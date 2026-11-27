open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let rec hooks : hooks =
  {
    check_expr = (fun env e -> Sema_expr.check_expr hooks env e);
    check_expr_any = (fun env e -> Sema_expr.check_expr_any hooks env e);
    check_call =
      (fun env e tcallee ptys ret callee args ->
        Sema_call.check_call hooks env e tcallee ptys ret callee args);
    check_collection_items = (fun env items -> Sema_lvalue.check_collection_items hooks env items);
    check_index_access =
      (fun env ~span target idx -> Sema_lvalue.check_index_access hooks env ~span target idx);
    check_lvalue = (fun env ~for_op lval -> Sema_lvalue.check_lvalue hooks env ~for_op lval);
    check_stmt =
      (fun env scope_vars loop_stack next_loop_id s ->
        Sema_stmt.check_stmt hooks env scope_vars loop_stack next_loop_id s);
    check_program;
  }

and check_program (prog : Ast.program) : (Typed_ast.program, Diagnostic.t list) result =
  let env : env = Hashtbl.create 16 in
  let root_scope = ref [] in
  let next_loop_id = ref 0 in
  let tstmts = ref [] in
  let all_errors = ref [] in
  let saved_root = !root_scope_ref in
  let saved_fun = !current_fun in
  let saved_outer = !fn_outer in
  let saved_user_types = Hashtbl.copy user_types in
  let saved_mod_cache = Hashtbl.copy module_cache in
  let saved_mod_order = !module_load_order in
  let saved_imp_stack = !import_stack in
  let saved_vis_map = Hashtbl.copy top_vis_map in
  let saved_mod_aliases = Hashtbl.copy module_aliases in
  let saved_lazy = Hashtbl.copy lazy_imports in
  let saved_gen = Sema_generics.snapshot () in
  let gen_env = env in
  Sema_generics.instantiate_type_hook := (fun sp name targs -> Sema_generics_inst.instantiate_type hooks gen_env sp name targs);
  Hashtbl.clear user_types;
  Hashtbl.clear module_cache;
  module_load_order := [];
  import_stack := [];
  Hashtbl.clear top_vis_map;
  Hashtbl.clear module_aliases;
  Hashtbl.clear lazy_imports;
  Sema_generics.clear_all ();
  root_scope_ref := Some root_scope;
  current_fun := RetNone;
  fn_outer := None;
  List.iter
    (fun stmt ->
      match hooks.check_stmt env root_scope [] next_loop_id stmt with
      | Ok ts -> tstmts := ts :: !tstmts
      | Error errs -> all_errors := List.rev_append errs !all_errors)
    prog;
  let used_mod_stmts = Sema_modules.collect_used_module_stmts () in
  let mono_stmts = List.rev !Sema_generics.pending_stmts in
  root_scope_ref := saved_root;
  current_fun := saved_fun;
  fn_outer := saved_outer;
  Hashtbl.clear user_types;
  Hashtbl.iter (Hashtbl.replace user_types) saved_user_types;
  Hashtbl.clear module_cache; Hashtbl.iter (Hashtbl.replace module_cache) saved_mod_cache;
  module_load_order := saved_mod_order;
  import_stack := saved_imp_stack;
  Hashtbl.clear top_vis_map; Hashtbl.iter (Hashtbl.replace top_vis_map) saved_vis_map;
  Hashtbl.clear module_aliases; Hashtbl.iter (Hashtbl.replace module_aliases) saved_mod_aliases;
  Hashtbl.clear lazy_imports; Hashtbl.iter (Hashtbl.replace lazy_imports) saved_lazy;
  Sema_generics.restore saved_gen;
  match List.rev !all_errors with
  | [] -> Ok (used_mod_stmts @ mono_stmts @ List.rev !tstmts)
  | errs -> Error errs

type entry_point =
  | EntryActNoArgs
  | EntryActArgs
  | EntryFunIntNoArgs
  | EntryFunIntArgs

let is_toplevel_decl_only (s : Typed_ast.stmt) : bool =
  match s.desc with
  | Typed_ast.TStmtTypeDecl _
  | Typed_ast.TStmtFunDecl _
  | Typed_ast.TStmtExtern _
  | Typed_ast.TStmtVarDecl _
  | Typed_ast.TStmtDestruct _ -> true
  | _ -> false

let detect_entry_point ?entry_name (prog : Typed_ast.program)
    : ((string * entry_point) option, Diagnostic.t list) result =
  let first_exec = List.find_opt (fun s -> not (is_toplevel_decl_only s)) prog in
  let target_name = match entry_name with Some n -> n | None -> "main" in
  let rec find_fn = function
    | [] -> None
    | { Typed_ast.desc = Typed_ast.TStmtFunDecl fd; _ } :: _ when fd.tf_name = target_name ->
      Some fd
    | _ :: rest -> find_fn rest
  in
  let match_sig (fd : Typed_ast.fun_decl) =
    match fd.tf_params, fd.tf_ret with
    | [], Ty.Void -> Some EntryActNoArgs
    | [ { tp_mode = Ty.ParamVal; tp_ty = Ty.DynArray Ty.String; _ } ], Ty.Void ->
      Some EntryActArgs
    | [], Ty.Int -> Some EntryFunIntNoArgs
    | [ { tp_mode = Ty.ParamVal; tp_ty = Ty.DynArray Ty.String; _ } ], Ty.Int ->
      Some EntryFunIntArgs
    | _ -> None
  in
  match entry_name, first_exec with
  | None, Some _ -> Ok None
  | None, None ->
    (match find_fn prog with
     | None -> Ok None
     | Some fd ->
       (match match_sig fd with
        | Some ep -> Ok (Some (target_name, ep))
        | None -> Ok None))
  | Some name, Some exec_stmt ->
    Error
      [
        Diagnostic.error
          ~hint:"remove top-level executable statements when invoking an explicit '--entry' function"
          ~code:"E0123" ~span:exec_stmt.span
          (Printf.sprintf "cannot invoke entry function '%s' in a file with top-level executable statements" name);
      ]
  | Some name, None ->
    (match find_fn prog with
     | None ->
       Error
         [
           Diagnostic.error
             ~hint:"declare a top-level 'act' or 'fun' matching one of: '()', '(args: string[])', '(): int', '(args: string[]): int'"
             ~code:"E0123" ~span:Sigma_common.Span.dummy
             (Printf.sprintf "entry point function '%s' not found at top level" name);
         ]
     | Some fd ->
       (match match_sig fd with
        | Some ep -> Ok (Some (name, ep))
        | None ->
          Error
            [
              Diagnostic.error
                ~hint:"supported entry signatures: 'act f()', 'act f(args: string[])', 'fun f(): int', 'fun f(args: string[]): int'"
                ~code:"E0123" ~span:fd.tf_name_span
                (Printf.sprintf "incompatible signature for entry point function '%s'" name);
            ]))

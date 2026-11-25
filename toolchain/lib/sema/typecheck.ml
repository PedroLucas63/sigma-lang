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
  Hashtbl.clear user_types;
  root_scope_ref := Some root_scope;
  current_fun := RetNone;
  fn_outer := None;
  List.iter
    (fun stmt ->
      match hooks.check_stmt env root_scope [] next_loop_id stmt with
      | Ok ts -> tstmts := ts :: !tstmts
      | Error errs -> all_errors := List.rev_append errs !all_errors)
    prog;
  root_scope_ref := saved_root;
  current_fun := saved_fun;
  fn_outer := saved_outer;
  Hashtbl.clear user_types;
  Hashtbl.iter (Hashtbl.replace user_types) saved_user_types;
  match List.rev !all_errors with
  | [] -> Ok (List.rev !tstmts)
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
  | Typed_ast.TStmtVarDecl _
  | Typed_ast.TStmtDestruct _ -> true
  | _ -> false

let detect_entry_point (prog : Typed_ast.program) : entry_point option =
  if not (List.for_all is_toplevel_decl_only prog) then None
  else
    let rec find_main = function
      | [] -> None
      | { Typed_ast.desc = Typed_ast.TStmtFunDecl fd; _ } :: _ when fd.tf_name = "main" ->
        (match fd.tf_params, fd.tf_ret with
         | [], Ty.Void -> Some EntryActNoArgs
         | [ { tp_mode = Ty.ParamVal; tp_ty = Ty.DynArray Ty.String; _ } ], Ty.Void ->
           Some EntryActArgs
         | [], Ty.Int -> Some EntryFunIntNoArgs
         | [ { tp_mode = Ty.ParamVal; tp_ty = Ty.DynArray Ty.String; _ } ], Ty.Int ->
           Some EntryFunIntArgs
         | _ -> None)
      | _ :: rest -> find_main rest
    in
    find_main prog

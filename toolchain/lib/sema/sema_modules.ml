open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let normalize_path (p : string) : string =
  let abs = if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p in
  let parts = String.split_on_char '/' abs in
  let rec walk acc = function
    | [] -> List.rev acc
    | "" :: rest when acc <> [] -> walk acc rest
    | "." :: rest -> walk acc rest
    | ".." :: rest -> (match acc with _ :: tl when tl <> [] -> walk tl rest | _ -> walk acc rest)
    | seg :: rest -> walk (seg :: acc) rest
  in
  let res = String.concat "/" (walk [] parts) in
  if String.length res = 0 || res.[0] <> '/' then "/" ^ res else res

let find_project_root (start_dir : string) : string =
  let rec up dir depth =
    if depth <= 0 then start_dir
    else if Sys.file_exists (Filename.concat dir "sigma.toml") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then start_dir else up parent (depth - 1)
  in
  up start_dir 6

let resolve_module_path ~(from_file : string) ~(is_path : bool) (target : string) : string option =
  let from_dir =
    if from_file = "" || from_file = "<dummy>" then Sys.getcwd () else Filename.dirname from_file
  in
  let proj_root = find_project_root from_dir in
  let cand_name = if is_path then target else target ^ ".sgm" in
  let candidates =
    [ Filename.concat from_dir cand_name; Filename.concat proj_root cand_name; cand_name ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> Some (normalize_path p)
  | None -> None

let rec mark_used (m : loaded_module) (sym : string) : unit =
  if not (Hashtbl.mem m.mod_used sym) then begin
    Hashtbl.replace m.mod_used sym ();
    match Hashtbl.find_opt m.mod_deps sym with
    | None -> ()
    | Some deps -> List.iter (mark_used m) deps
  end

let is_exported_vis = function
  | Ast.VisPublic | Ast.VisInternal -> true
  | Ast.VisProtected | Ast.VisPrivate -> false

let load_source_for_target ~span ~(is_path : bool) (target : string)
    : (string * Source.t, Diagnostic.t list) result =
  if Sema_remote_import.is_url target || Sema_remote_import.is_url span.Span.file then
    let eff_url =
      if Sema_remote_import.is_url target then target
      else
        let rel = if is_path then target else target ^ ".sgm" in
        Sema_remote_import.resolve_relative_url span.Span.file rel
    in
    match Sema_remote_import.fetch_url ~span eff_url with
    | Ok (canon_url, src) -> Ok (canon_url, src)
    | Error d -> Error [ d ]
  else
    match resolve_module_path ~from_file:span.Span.file ~is_path target with
    | None ->
      Error [ Diagnostic.error ~code:"E0129" ~span (Printf.sprintf "module '%s' not found" target) ]
    | Some canon_path ->
      (match Source.load canon_path with
       | Error msg ->
         Error [ Diagnostic.error ~code:"E0129" ~span
                   (Printf.sprintf "failed to load module '%s': %s" target msg) ]
       | Ok src -> Ok (canon_path, src))

let load_module hooks ~span ~(is_path : bool) (target : string)
    : (loaded_module, Diagnostic.t list) result =
  match load_source_for_target ~span ~is_path target with
  | Error es -> Error es
  | Ok (canon_path, source) ->
    if List.mem canon_path !import_stack then
      Error [ Diagnostic.error ~code:"E0129" ~span
                (Printf.sprintf "circular import detected involving '%s'" target) ]
    else
      match Hashtbl.find_opt module_cache canon_path with
      | Some m -> Ok m
      | None ->
        begin
          (match Lexer.tokenize source with
           | Error es -> Error es
           | Ok tokens ->
             (match Parser.parse tokens with
              | Error es -> Error es
              | Ok ast ->
                let saved_user_types = Hashtbl.copy user_types in
                let saved_vis_map = Hashtbl.copy top_vis_map in
                let saved_mod_aliases = Hashtbl.copy module_aliases in
                let saved_lazy = Hashtbl.copy lazy_imports in
                let saved_root = !root_scope_ref and saved_fun = !current_fun
                and saved_outer = !fn_outer and saved_type = !current_type_ctx
                and saved_ctor = !in_ctor and saved_scope = !current_scope_vars in
                Hashtbl.clear user_types;
                Hashtbl.clear top_vis_map;
                Hashtbl.clear module_aliases;
                Hashtbl.clear lazy_imports;
                import_stack := canon_path :: !import_stack;
                let mod_env : env = Hashtbl.create 16 in
                let mod_root = ref [] in
                let mod_loop_id = ref 0 in
                root_scope_ref := Some mod_root;
                current_fun := RetNone;
                fn_outer := None;
                current_type_ctx := None;
                in_ctor := false;
                let errs = ref [] in
                let tstmts = ref [] in
                List.iter
                  (fun s ->
                    match hooks.check_stmt mod_env mod_root [] mod_loop_id s with
                    | Ok ts -> tstmts := ts :: !tstmts
                    | Error es -> errs := List.rev_append es !errs)
                  ast;
                import_stack := List.tl !import_stack;
                let mod_types_snap = Hashtbl.copy user_types in
                let mod_vis_snap = Hashtbl.copy top_vis_map in
                Hashtbl.clear user_types; Hashtbl.iter (Hashtbl.replace user_types) saved_user_types;
                Hashtbl.clear top_vis_map; Hashtbl.iter (Hashtbl.replace top_vis_map) saved_vis_map;
                Hashtbl.clear module_aliases; Hashtbl.iter (Hashtbl.replace module_aliases) saved_mod_aliases;
                Hashtbl.clear lazy_imports; Hashtbl.iter (Hashtbl.replace lazy_imports) saved_lazy;
                root_scope_ref := saved_root; current_fun := saved_fun; fn_outer := saved_outer;
                current_type_ctx := saved_type; in_ctor := saved_ctor; current_scope_vars := saved_scope;
                match List.rev !errs with
                | _ :: _ as es -> Error es
                | [] ->
                  let mod_id = !next_mod_id in
                  incr next_mod_id;
                  let mod_prefix = Printf.sprintf "__m%d_" mod_id in
                  let rev_stmts = List.rev !tstmts in
                  let top_vals = Hashtbl.create 16 and top_types = Hashtbl.create 16 in
                  List.iter
                    (fun s ->
                      match s.Typed_ast.desc with
                      | Typed_ast.TStmtTypeDecl tud -> Hashtbl.replace top_types tud.tud_name ()
                      | _ -> List.iter (fun n -> Hashtbl.replace top_vals n ()) (Sema_dep_scan.stmt_defined_names s))
                    rev_stmts;
                  let mod_deps = Hashtbl.create 16 in
                  let mod_stmts =
                    List.map
                      (fun s ->
                        let defs = Sema_dep_scan.stmt_defined_names s in
                        let s', deps = Sema_dep_scan.rename_toplevel_stmt mod_prefix top_vals top_types s in
                        List.iter (fun d -> Hashtbl.replace mod_deps d deps) defs;
                        (defs, s'))
                      rev_stmts
                  in
                  let mod_exports = Hashtbl.create 16 in
                  Hashtbl.iter
                    (fun name (info : symbol_info) ->
                      if Hashtbl.mem top_vals name then
                        let vis = match Hashtbl.find_opt mod_vis_snap name with Some v -> v | None -> Ast.VisPrivate in
                        let pnames = Hashtbl.find_opt fun_param_names info.decl_span in
                        Hashtbl.replace mod_exports name
                          (vis, ExpVal { exp_mangled = mod_prefix ^ name; exp_ty = info.ty; exp_param_names = pnames }))
                    mod_env;
                  Hashtbl.iter
                    (fun tname tinfo ->
                      Hashtbl.replace mod_exports tname
                        (tinfo.ut_vis, ExpType { exp_mangled_type = tname; exp_info = tinfo }))
                    mod_types_snap;
                  let m =
                    { mod_id; mod_key = canon_path; mod_prefix; mod_exports; mod_stmts; mod_deps;
                      mod_used = Hashtbl.create 16 }
                  in
                  Hashtbl.replace module_cache canon_path m;
                  module_load_order := !module_load_order @ [ m ];
                  Ok m))
        end

let check_import_stmt hooks (env : env) (scope_vars : string list ref) (spec : Ast.import_spec)
    : (Typed_ast.stmt, Diagnostic.t list) result =
  match spec with
  | Ast.ImportModule { mod_path; mod_is_path; mod_span; mod_alias } ->
    (match load_module hooks ~span:mod_span ~is_path:mod_is_path mod_path with
     | Error es -> Error es
     | Ok m ->
       let alias =
         match mod_alias with
         | Some (a, _) -> a
         | None ->
           let b = Filename.basename mod_path in
           if Filename.check_suffix b ".sgm" then Filename.chop_extension b else b
       in
       Hashtbl.replace module_aliases alias m;
       Ok { Typed_ast.desc = Typed_ast.TStmtBlock []; span = mod_span })
  | Ast.ImportFrom { mod_path; mod_is_path; mod_span; mod_items } ->
    (match load_module hooks ~span:mod_span ~is_path:mod_is_path mod_path with
     | Error es -> Error es
     | Ok m ->
       let errs = ref [] in
       List.iter
         (fun (it : Ast.import_item) ->
           let local = match it.imp_alias with Some (a, _) -> a | None -> it.imp_name in
           match Hashtbl.find_opt m.mod_exports it.imp_name with
           | None ->
             errs := Diagnostic.error ~code:"E0130" ~span:it.imp_name_span
                       (Printf.sprintf "module '%s' has no symbol named '%s'" mod_path it.imp_name) :: !errs
           | Some (vis, _) when not (is_exported_vis vis) ->
             errs := Diagnostic.error ~code:"E0130" ~span:it.imp_name_span
                       (Printf.sprintf "symbol '%s' in module '%s' is %s and cannot be imported"
                          it.imp_name mod_path (vis_to_string vis)) :: !errs
           | Some (_, kind) ->
             Hashtbl.replace lazy_imports local (m, it.imp_name, kind);
             match kind with
             | ExpVal { exp_ty; exp_param_names; _ } ->
               Hashtbl.replace env local
                 { ty = exp_ty; decl_span = it.imp_name_span; is_const = true;
                   const_kind = DeepConst; is_ref_param = false; const_int = None };
               scope_vars := local :: !scope_vars;
               Option.iter (Hashtbl.replace var_param_names local) exp_param_names
             | ExpType { exp_info; _ } ->
               Hashtbl.replace user_types local exp_info)
         mod_items;
       match List.rev !errs with
       | [] -> Ok { Typed_ast.desc = Typed_ast.TStmtBlock []; span = mod_span }
       | es -> Error es)

let check_module_member_expr ~span (mod_alias : string) (member : string)
    : (Typed_ast.expr, Diagnostic.t) result option =
  match Hashtbl.find_opt module_aliases mod_alias with
  | None -> None
  | Some m ->
    Some
      (match Hashtbl.find_opt m.mod_exports member with
       | None ->
         Error (Diagnostic.error ~code:"E0130" ~span
                  (Printf.sprintf "module '%s' has no symbol named '%s'" mod_alias member))
       | Some (vis, _) when not (is_exported_vis vis) ->
         Error (Diagnostic.error ~code:"E0130" ~span
                  (Printf.sprintf "symbol '%s' in module '%s' is %s and inaccessible"
                     member mod_alias (vis_to_string vis)))
       | Some (_, ExpVal { exp_mangled; exp_ty; exp_param_names }) ->
         mark_used m member;
         Option.iter (Hashtbl.replace expr_param_names span) exp_param_names;
         Ok { Typed_ast.desc = Typed_ast.TExprIdent exp_mangled; ty = exp_ty; span }
       | Some (_, ExpType _) ->
         Error (Diagnostic.error ~code:"E0130" ~span
                  (Printf.sprintf "type '%s.%s' cannot be used as a value" mod_alias member)))

let collect_used_module_stmts () : Typed_ast.stmt list =
  List.concat_map
    (fun m ->
      List.filter_map
        (fun (defs, s) -> if List.exists (Hashtbl.mem m.mod_used) defs then Some s else None)
        m.mod_stmts)
    !module_load_order

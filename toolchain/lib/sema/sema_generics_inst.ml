open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_generics
open Sema_generics_subst

let instantiate_type hooks (env : env) (sp : Span.t) (name : string) (targs : Ty.t list)
    : (Ty.t, Diagnostic.t) result =
  match Hashtbl.find_opt generic_types name with
  | None ->
    Error
      (Diagnostic.error ~code:"E0101" ~span:sp
         (Printf.sprintf "type '%s' is not a generic type" name))
  | Some td ->
    let non_var = List.filter (fun (tp : Ast.type_param) -> not tp.tp_variadic) td.td_type_params in
    if List.length targs <> List.length non_var then
      Error
        (Diagnostic.error ~code:"E0141" ~span:sp
           (Printf.sprintf "generic type '%s' expects %d type argument(s), got %d"
              name (List.length non_var) (List.length targs)))
    else
      let bindings = List.map2 (fun (tp : Ast.type_param) ty -> (tp.tp_name, ty)) non_var targs in
      match check_where_clauses ~resolve_type_expr:(fun te init -> Sema_types.resolve_type_expr te init) td.td_where bindings with
      | Error e -> Error e
      | Ok () ->
        let mangled = mangle_name name targs in
        (match Hashtbl.find_opt instantiated_types mangled with
         | Some ty -> Ok ty
         | None ->
           let tmap = List.map (fun (k, ty) -> (k, ty_to_ast_type_expr sp ty)) bindings in
           let ctx = { tmap; variadic_param = None; var_aliases = [] } in
           let td' = subst_type_decl ctx td in
           let usp = unique_inst_span sp in
           let td_concrete =
             { td' with
               td_name = mangled;
               td_name_span = usp;
               td_type_params = [];
               td_where = [];
               td_ctors = List.map (fun (c : Ast.ctor_decl) -> { c with cd_name = mangled; cd_name_span = unique_inst_span sp }) td'.td_ctors }
           in
           let kind =
             match td.td_kind with
             | Ast.KindClass -> Ty.KindClass
             | Ast.KindStruct -> Ty.KindStruct
             | Ast.KindRecord -> Ty.KindRecord
           in
           let res_ty = Ty.User (kind, mangled) in
           Hashtbl.replace instantiated_types mangled res_ty;
           let root_env = Hashtbl.create 16 in
           Hashtbl.iter (fun k v -> Hashtbl.replace root_env k v) env;
           let scope_vars = match !root_scope_ref with Some r -> r | None -> ref [] in
           let next_loop_id = ref 0 in
           let stmt : Ast.stmt = { Ast.desc = Ast.StmtTypeDecl td_concrete; span = sp } in
           (match hooks.check_stmt root_env scope_vars [] next_loop_id stmt with
            | Error (e :: _) ->
              Hashtbl.remove instantiated_types mangled;
              Error e
            | Error [] ->
              Hashtbl.remove instantiated_types mangled;
              Error (Diagnostic.error ~code:"E0141" ~span:sp "failed to instantiate generic type")
            | Ok tstmt ->
              pending_stmts := tstmt :: !pending_stmts;
              Ok res_ty))

let instantiate_fun hooks (env : env) (sp : Span.t) (name : string) (targs : Ty.t list) (var_tys : Ty.t list)
    : (string * Ty.t, Diagnostic.t) result =
  match Hashtbl.find_opt generic_funs name with
  | None ->
    Error
      (Diagnostic.error ~code:"E0102" ~span:sp
         (Printf.sprintf "generic function '%s' not found" name))
  | Some fd ->
    let non_var = List.filter (fun (tp : Ast.type_param) -> not tp.tp_variadic) fd.fd_type_params in
    if List.length targs <> List.length non_var then
      Error
        (Diagnostic.error ~code:"E0141" ~span:sp
           (Printf.sprintf "generic function '%s' expects %d type argument(s), got %d"
              name (List.length non_var) (List.length targs)))
    else
      let bindings = List.map2 (fun (tp : Ast.type_param) ty -> (tp.tp_name, ty)) non_var targs in
      match check_where_clauses ~resolve_type_expr:(fun te init -> Sema_types.resolve_type_expr te init) fd.fd_where bindings with
      | Error e -> Error e
      | Ok () ->
        let all_mangle_tys = targs @ var_tys in
        let mangled = mangle_name name all_mangle_tys in
        (match Hashtbl.find_opt instantiated_funs mangled with
         | Some res -> Ok res
         | None ->
           let tmap = List.map (fun (k, ty) -> (k, ty_to_ast_type_expr sp ty)) bindings in
           let variadic_param =
             match List.find_opt (fun (p : Ast.fun_param) -> p.param_variadic) fd.fd_params with
             | Some vp -> Some (vp.param_name, vp.param_mode, List.map (ty_to_ast_type_expr sp) var_tys)
             | None -> None
           in
           let ctx = { tmap; variadic_param; var_aliases = [] } in
           let fd' = subst_fun_decl ctx fd in
           let fd_concrete = { fd' with fd_name = mangled; fd_name_span = unique_inst_span sp; fd_type_params = []; fd_where = [] } in
           let scope_vars = ref [] in
           let next_loop_id = ref 0 in
           let check_env = Hashtbl.copy env in
           let stmt : Ast.stmt = { Ast.desc = Ast.StmtFunDecl fd_concrete; span = sp } in
           (match hooks.check_stmt check_env scope_vars [] next_loop_id stmt with
            | Error (e :: _) -> Error e
            | Error [] -> Error (Diagnostic.error ~code:"E0141" ~span:sp "failed to instantiate generic function")
            | Ok tstmt ->
              let fty =
                match Hashtbl.find_opt check_env mangled with
                | Some sym -> sym.ty
                | None -> Ty.Void
              in
              Hashtbl.replace env mangled
                { ty = fty; decl_span = sp; is_const = true; const_kind = DeepConst; is_ref_param = false; const_int = None };
              Hashtbl.replace instantiated_funs mangled (mangled, fty);
              pending_stmts := tstmt :: !pending_stmts;
              Ok (mangled, fty)))

let resolve_type_args_no_wildcard (targs : Ast.type_expr list) : (Ty.t list, Diagnostic.t) result =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ast.TypeWildcard sp :: _ ->
      Error
        (Diagnostic.error ~code:"E0143" ~span:sp
           "wildcard '?' cannot be used as an explicit instantiation type argument")
    | te :: rest ->
      (match Sema_types.resolve_type_expr te None with
       | Error e -> Error e
       | Ok ty -> loop (ty :: acc) rest)
  in
  loop [] targs

let infer_new_type_args hooks (env : env) (sp : Span.t) (td : Ast.type_decl) (args : Ast.call_arg list)
    : (Ty.t list, Diagnostic.t) result =
  let non_var = List.filter (fun (tp : Ast.type_param) -> not tp.tp_variadic) td.td_type_params in
  let tp_names = List.map (fun (tp : Ast.type_param) -> tp.tp_name) non_var in
  let arg_tys_res =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | (a : Ast.call_arg) :: rest ->
        (match hooks.check_expr env a.arg_value with
         | Error e -> Error e
         | Ok te -> loop (te.ty :: acc) rest)
    in
    loop [] args
  in
  match arg_tys_res with
  | Error e -> Error e
  | Ok arg_tys ->
    let param_tes =
      match td.td_ctor_params with
      | Some cps -> List.map (fun (cp : Ast.ctor_param) -> cp.cp_type) cps
      | None ->
        match td.td_ctors with
        | c :: _ -> List.map (fun (cp : Ast.ctor_param) -> cp.cp_type) c.cd_params
        | [] -> List.map (fun (f : Ast.field_decl) -> f.field_type) td.td_fields
    in
    let rec match_l tes atys acc =
      match tes, atys with
      | te :: r1, ty :: r2 -> match_l r1 r2 (infer_from_type_expr tp_names te ty acc)
      | _, _ -> acc
    in
    let inferred = match_l param_tes arg_tys [] in
    let rec collect acc = function
      | [] -> Ok (List.rev acc)
      | (tp : Ast.type_param) :: rest ->
        (match List.assoc_opt tp.tp_name inferred with
         | Some ty -> collect (ty :: acc) rest
         | None ->
           Error
             (Diagnostic.error ~code:"E0141" ~span:sp
                (Printf.sprintf "cannot infer generic type argument '%s' for 'new %s'; specify it explicitly like 'new %s<...>'"
                   tp.tp_name td.td_name td.td_name)))
    in
    collect [] non_var

let register_generic_methods (owner : string) (methods : Ast.method_decl list) : Ast.method_decl list =
  let gen, conc = List.partition (fun (m : Ast.method_decl) -> m.md_fun.fd_type_params <> []) methods in
  List.iter (fun (m : Ast.method_decl) -> Hashtbl.replace generic_methods (owner, m.md_fun.fd_name) m) gen;
  conc

let instantiate_method hooks (env : env) (sp : Span.t) (owner : string) (mname : string)
    (targs : Ty.t list) (var_tys : Ty.t list) : (string, Diagnostic.t) result =
  match Hashtbl.find_opt generic_methods (owner, mname), Hashtbl.find_opt user_types owner with
  | Some md, Some info ->
    let non_var = List.filter (fun (tp : Ast.type_param) -> not tp.tp_variadic) md.md_fun.fd_type_params in
    if List.length targs <> List.length non_var then
      Error (Diagnostic.error ~code:"E0141" ~span:sp
               (Printf.sprintf "generic method '%s.%s' expects %d type argument(s), got %d"
                  owner mname (List.length non_var) (List.length targs)))
    else
      let bindings = List.map2 (fun (tp : Ast.type_param) ty -> (tp.tp_name, ty)) non_var targs in
      (match check_where_clauses ~resolve_type_expr:(fun te init -> Sema_types.resolve_type_expr te init) md.md_fun.fd_where bindings with
       | Error e -> Error e
       | Ok () ->
         let mangled_m = mangle_name mname (targs @ var_tys) in
         if List.exists (fun um -> um.um_name = mangled_m) info.ut_methods then Ok mangled_m
         else
           let tmap = List.map (fun (k, ty) -> (k, ty_to_ast_type_expr sp ty)) bindings in
           let variadic_param =
             match List.find_opt (fun (p : Ast.fun_param) -> p.param_variadic) md.md_fun.fd_params with
             | Some vp -> Some (vp.param_name, vp.param_mode, List.map (ty_to_ast_type_expr sp) var_tys)
             | None -> None
           in
           let fd' = subst_fun_decl { tmap; variadic_param; var_aliases = [] } md.md_fun in
           let md' = { md with md_fun = { fd' with fd_name = mangled_m; fd_name_span = unique_inst_span sp; fd_type_params = []; fd_where = [] } } in
           (match Sema_methods.check_method_sigs ~type_name:owner ~seen_members:(Hashtbl.create 1) [ md' ] with
            | Error (e :: _) -> Error e
            | Error [] -> Error (Diagnostic.error ~code:"E0141" ~span:sp "failed to check generic method")
            | Ok ums ->
              let loop_ref = ref 20000 in
              (match Sema_methods.check_method_bodies hooks env loop_ref ~type_name:owner ~kind:info.ut_kind [ md' ] ums with
               | Error (e :: _) -> Error e
               | Error [] -> Error (Diagnostic.error ~code:"E0141" ~span:sp "failed to check generic method body")
               | Ok (new_ums, tms) ->
                 Hashtbl.replace user_types owner { info with ut_methods = info.ut_methods @ new_ums };
                 List.iter (fun fd -> pending_stmts := { Typed_ast.desc = Typed_ast.TStmtFunDecl fd; span = sp } :: !pending_stmts) tms;
                 Ok mangled_m)))
  | _, _ ->
    Error (Diagnostic.error ~code:"E0130" ~span:sp (Printf.sprintf "generic method '%s.%s' not found" owner mname))


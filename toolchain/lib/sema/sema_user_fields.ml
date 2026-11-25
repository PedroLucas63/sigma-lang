open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let rec root_var_const_kind (env : env) (e : Typed_ast.expr) : const_kind option =
  match e.desc with
  | Typed_ast.TExprIdent name ->
    Option.map (fun info -> info.const_kind) (Hashtbl.find_opt env name)
  | Typed_ast.TExprField (sub, _, _) | Typed_ast.TExprIndex (sub, _) ->
    root_var_const_kind env sub
  | _ -> None

let check_field_access hooks (env : env) ~span (base : Ast.expr) (field : string)
    : (Typed_ast.expr, Diagnostic.t) result =
  let mod_opt =
    match base.desc with
    | Ast.ExprIdent mod_alias -> Sema_modules.check_module_member_expr ~span mod_alias field
    | _ -> None
  in
  match mod_opt with
  | Some res -> res
  | None ->
    let static_opt =
      match base.desc with
      | Ast.ExprIdent tname when not (Hashtbl.mem env tname) ->
        (match Hashtbl.find_opt user_types tname with
         | Some info ->
           (match Hashtbl.find_opt lazy_imports tname with
            | Some (m, orig_sym, ExpType _) -> Sema_modules.mark_used m orig_sym
            | _ -> ());
           Some info
         | None -> None)
      | _ -> None
    in
  match static_opt with
  | Some info -> Sema_methods.check_static_method_value ~span info field
  | None ->
    (match hooks.check_expr env base with
     | Error e -> Error e
     | Ok tbase ->
       (match tbase.ty with
        | Ty.User (kind, tname) ->
          (match Hashtbl.find_opt user_types tname with
           | Some info ->
             (match List.find_opt (fun f -> f.uf_name = field) info.ut_fields with
              | Some f ->
                (match f.uf_get with
                 | None ->
                   Error
                     (Diagnostic.error ~code:"E0124" ~span
                        (Printf.sprintf "property '%s' of '%s' has no 'get' accessor" field tname))
                 | Some v when not (is_vis_accessible_in_type tname v) ->
                   Error
                     (Diagnostic.error ~code:"E0124" ~span
                        (Printf.sprintf "cannot access %s property '%s' of '%s'" (vis_to_string v) field tname))
                 | Some _ ->
                   Ok { Typed_ast.desc = Typed_ast.TExprField (tbase, field, f.uf_idx); ty = f.uf_ty; span })
              | None ->
                (match List.find_opt (fun m -> m.um_name = field) info.ut_methods with
                 | Some m ->
                   let const_err =
                     if kind = Ty.KindStruct && m.um_mutates_this then
                       match root_var_const_kind env tbase with
                       | Some (DeepConst | ShallowConst) ->
                         Some
                           (Diagnostic.error ~code:"E0107" ~span
                              (Printf.sprintf "cannot call mutating method '%s' on immutable struct '%s'"
                                 field tname))
                       | _ -> None
                     else None
                   in
                   Sema_methods.check_instance_method_value ~span tbase info m const_err
                 | None ->
                   Error
                     (Diagnostic.error ~code:"E0119" ~span
                        (Printf.sprintf "type '%s' has no field or method named '%s'" tname field))))
           | None ->
             Error
               (Diagnostic.error ~code:"E0101" ~span:tbase.span
                  (Printf.sprintf "unknown type '%s'" tname)))
        | other ->
          Error
            (Diagnostic.error ~code:"E0119" ~span
               (Printf.sprintf "cannot access field '%s' on value of type '%s'" field (Ty.to_string other)))))

let check_field_lvalue hooks (env : env) ~span ~for_op (base : Ast.expr) (field : string)
    : (Typed_ast.lvalue * Ty.t * string, Diagnostic.t) result =
  match hooks.check_expr env base with
  | Error e -> Error e
  | Ok tbase ->
    (match tbase.ty with
     | Ty.User (kind, tname) ->
       (match Hashtbl.find_opt user_types tname with
        | None ->
          Error (Diagnostic.error ~code:"E0101" ~span:tbase.span (Printf.sprintf "unknown type '%s'" tname))
        | Some info ->
          (match List.find_opt (fun f -> f.uf_name = field) info.ut_fields with
           | None ->
             Error
               (Diagnostic.error ~code:"E0119" ~span
                  (Printf.sprintf "type '%s' has no field named '%s'" tname field))
           | Some f ->
             let is_this_in_ctor =
               !in_ctor && (match tbase.desc with Typed_ast.TExprIdent "this" -> true | _ -> false)
             in
             let check_accessors label =
               if is_this_in_ctor then
                 Ok (Typed_ast.TLValField (tbase, field, f.uf_idx), f.uf_ty, label)
               else
                 match f.uf_set with
               | None ->
                 if Option.is_some f.uf_init then
                   Error
                     (Diagnostic.error ~code:"E0125" ~span
                        (Printf.sprintf "property '%s' of '%s' is init-only and cannot be assigned after initialization"
                           field tname))
                 else
                   Error
                     (Diagnostic.error ~code:"E0125" ~span
                        (Printf.sprintf "property '%s' of '%s' is read-only and cannot be assigned" field tname))
               | Some v when not (is_vis_accessible_in_type tname v) ->
                 Error
                   (Diagnostic.error ~code:"E0124" ~span
                      (Printf.sprintf "cannot assign to property '%s' of '%s' because its 'set' accessor is %s"
                         field tname (vis_to_string v)))
               | Some _ ->
                 if for_op <> "=" && for_op <> "out" then
                   (match f.uf_get with
                    | None ->
                      Error
                        (Diagnostic.error ~code:"E0124" ~span
                           (Printf.sprintf "cannot read property '%s' of '%s' for '%s' because it has no 'get' accessor"
                              field tname for_op))
                    | Some gv when not (is_vis_accessible_in_type tname gv) ->
                      Error
                        (Diagnostic.error ~code:"E0124" ~span
                           (Printf.sprintf "cannot read %s property '%s' of '%s' for '%s'"
                              (vis_to_string gv) field tname for_op))
                    | Some _ -> Ok (Typed_ast.TLValField (tbase, field, f.uf_idx), f.uf_ty, label))
                 else Ok (Typed_ast.TLValField (tbase, field, f.uf_idx), f.uf_ty, label)
             in
             if kind = Ty.KindRecord then
               Error
                 (Diagnostic.error ~code:"E0107" ~span
                    (Printf.sprintf "cannot mutate field '%s' of immutable record '%s'" field tname))
             else if kind = Ty.KindStruct then
               (match root_var_const_kind env tbase with
                | Some (DeepConst | ShallowConst) ->
                  Error
                    (Diagnostic.error ~code:"E0107" ~span
                       (Printf.sprintf "cannot mutate field '%s' of immutable struct '%s'" field tname))
                | _ -> check_accessors "struct field")
             else check_accessors "class field"))
     | other ->
       Error
         (Diagnostic.error ~code:"E0119" ~span
            (Printf.sprintf "cannot assign to field '%s' of non-object type '%s'" field (Ty.to_string other))))

open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let check_field_access hooks (env : env) ~span (base : Ast.expr) (field : string)
    : (Typed_ast.expr, Diagnostic.t) result =
  match hooks.check_expr env base with
  | Error e -> Error e
  | Ok tbase ->
    (match tbase.ty with
     | Ty.User (_, tname) ->
       (match Hashtbl.find_opt user_types tname with
        | Some info ->
          (match List.find_opt (fun f -> f.uf_name = field) info.ut_fields with
           | Some f ->
             Ok { Typed_ast.desc = Typed_ast.TExprField (tbase, field, f.uf_idx); ty = f.uf_ty; span }
           | None ->
             Error
               (Diagnostic.error ~code:"E0119" ~span
                  (Printf.sprintf "type '%s' has no field named '%s'" tname field)))
        | None ->
          Error
            (Diagnostic.error ~code:"E0101" ~span:tbase.span
               (Printf.sprintf "unknown type '%s'" tname)))
     | other ->
       Error
         (Diagnostic.error ~code:"E0119" ~span
            (Printf.sprintf "cannot access field '%s' on value of type '%s'" field (Ty.to_string other))))

let rec root_var_const_kind (env : env) (e : Typed_ast.expr) : const_kind option =
  match e.desc with
  | Typed_ast.TExprIdent name ->
    Option.map (fun info -> info.const_kind) (Hashtbl.find_opt env name)
  | Typed_ast.TExprField (sub, _, _) | Typed_ast.TExprIndex (sub, _) ->
    root_var_const_kind env sub
  | _ -> None

let check_field_lvalue hooks (env : env) ~span ~for_op:_ (base : Ast.expr) (field : string)
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
                | _ -> Ok (Typed_ast.TLValField (tbase, field, f.uf_idx), f.uf_ty, "struct field"))
             else Ok (Typed_ast.TLValField (tbase, field, f.uf_idx), f.uf_ty, "class field")))
     | other ->
       Error
         (Diagnostic.error ~code:"E0119" ~span
            (Printf.sprintf "cannot assign to field '%s' of non-object type '%s'" field (Ty.to_string other))))

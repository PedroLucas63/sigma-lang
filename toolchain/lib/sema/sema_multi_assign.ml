open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let resolve_destruct_tys ~span (rhs_ty : Ty.t) (pat_len : int) : (Ty.t list, Diagnostic.t) result =
  match rhs_ty with
  | Ty.Tuple tys ->
    let val_len = List.length tys in
    if pat_len <> val_len then
      Error
        (Diagnostic.error
           ~hint:(Printf.sprintf "provide %d target(s) in the pattern (use '_' to ignore elements)" val_len)
           ~code:"E0104" ~span
           (Printf.sprintf "destructuring arity mismatch: pattern has %d elements, but '%s' has %d elements"
              pat_len (Ty.to_string rhs_ty) val_len))
    else Ok tys
  | Ty.User (_, name) ->
    (match Hashtbl.find_opt user_types name with
     | None ->
       Error (Diagnostic.error ~code:"E0101" ~span (Printf.sprintf "unknown type '%s'" name))
     | Some info ->
       let val_len = List.length info.ut_fields in
       if pat_len <> val_len then
         Error
           (Diagnostic.error
              ~hint:(Printf.sprintf "provide %d target(s) for fields of '%s' (use '_' to ignore)" val_len name)
              ~code:"E0104" ~span
              (Printf.sprintf "destructuring arity mismatch: pattern has %d elements, but '%s' has %d fields"
                 pat_len name val_len))
       else Ok (List.map (fun f -> f.uf_ty) info.ut_fields))
  | Ty.Array (elem_ty, n) ->
    if pat_len > n then
      Error
        (Diagnostic.error
           ~hint:(Printf.sprintf "provide at most %d target(s) for array of length %d" n n)
           ~code:"E0104" ~span
           (Printf.sprintf "destructuring arity mismatch: pattern has %d elements, but '%s' has %d elements"
              pat_len (Ty.to_string rhs_ty) n))
    else if pat_len = n then Ok (List.init n (fun _ -> elem_ty))
    else
      let prefix = List.init (pat_len - 1) (fun _ -> elem_ty) in
      let rest_ty = Ty.Array (elem_ty, n - pat_len + 1) in
      Ok (prefix @ [ rest_ty ])
  | _ ->
    Error
      (Diagnostic.error
         ~hint:"destructuring requires a tuple, array, or user-defined structure on the right-hand side"
         ~code:"E0104" ~span
         (Printf.sprintf "cannot destructure value of type '%s'" (Ty.to_string rhs_ty)))

let check_multi_assign hooks (env : env) (span : Span.t) (targets : Ast.multi_target list)
    (rhs : Ast.expr) : (Typed_ast.stmt, Diagnostic.t list) result =
  match hooks.check_expr env rhs with
  | Error err -> Error [ err ]
  | Ok trhs ->
    let pat_len = List.length targets in
    (match resolve_destruct_tys ~span trhs.ty pat_len with
     | Error err -> Error [ err ]
     | Ok elem_tys ->
       let errs = ref [] in
       let ttargets =
         List.map2
           (fun tgt expected_ty ->
             match tgt with
             | Ast.TargetDiscard _ -> Typed_ast.TTargetDiscard
             | Ast.TargetLVal lv ->
               (match hooks.check_lvalue env ~for_op:"=" lv with
                | Error err -> errs := err :: !errs; Typed_ast.TTargetDiscard
                | Ok (tlv, lv_ty, label) ->
                  if not (Ty.equal lv_ty expected_ty) then begin
                    errs :=
                      Diagnostic.error ~code:"E0104" ~span:(Ast.lvalue_span lv)
                        (Printf.sprintf "type mismatch in multiple assignment to %s: expected '%s', found '%s'"
                           label (Ty.to_string lv_ty) (Ty.to_string expected_ty))
                      :: !errs;
                    Typed_ast.TTargetDiscard
                  end else Typed_ast.TTargetLVal (tlv, lv_ty)))
           targets elem_tys
       in
       match List.rev !errs with
       | [] ->
         Ok { Typed_ast.desc = Typed_ast.TStmtMultiAssign { tma_targets = ttargets; tma_rhs = trhs }; span }
       | es -> Error es)

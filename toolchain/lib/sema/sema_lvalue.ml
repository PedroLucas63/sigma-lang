open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_ops

let check_collection_items hooks (env : env) (items : Ast.collection_item list)
    : ((Typed_ast.collection_item * Ty.t list) list, Diagnostic.t) result =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ast.ItemExpr sub :: rest ->
      (match hooks.check_expr env sub with
       | Error err -> Error err
       | Ok tsub -> loop ((Typed_ast.TItemExpr tsub, [ tsub.ty ]) :: acc) rest)
    | Ast.ItemSpread (sub, sp) :: rest ->
      (match hooks.check_expr env sub with
       | Error err -> Error err
       | Ok tsub ->
         (match tsub.ty with
          | Ty.Tuple elem_tys -> loop ((Typed_ast.TItemSpread tsub, elem_tys) :: acc) rest
          | Ty.Array (elem_ty, n) ->
            let tys = List.init n (fun _ -> elem_ty) in
            loop ((Typed_ast.TItemSpread tsub, tys) :: acc) rest
          | _ ->
            Error
              (Diagnostic.error
                 ~hint:"the spread operator '..' can only unpack tuples or arrays, e.g. '[..t, ..l, 50]'"
                 ~code:"E0104" ~span:sp
                 (Printf.sprintf "cannot spread value of type '%s' with '..' (expected a tuple or array)"
                    (Ty.to_string tsub.ty)))))
  in
  loop [] items

let check_index_access hooks (env : env) ~span:_ (target : Ast.expr) (idx : Ast.expr)
    : (Typed_ast.expr * Typed_ast.expr * Ty.t, Diagnostic.t) result =
  match hooks.check_expr env target with
  | Error err -> Error err
  | Ok ttarget ->
    (match hooks.check_expr env idx with
     | Error err -> Error err
     | Ok tidx ->
       if tidx.ty <> Ty.Int then
         Error
           (Diagnostic.error
              ~hint:"indices must be integer expressions of type 'int' (0-based)"
              ~code:"E0104" ~span:tidx.span
              (Printf.sprintf "index must be of type 'int', found '%s'" (Ty.to_string tidx.ty)))
       else
         (match ttarget.ty with
          | Ty.Tuple elem_tys ->
            let len = List.length elem_tys in
            (match eval_const_int env tidx with
             | Some k ->
               if k < 0L || k >= Int64.of_int len then
                 Error
                   (Diagnostic.error
                      ~hint:(Printf.sprintf "valid indices for this tuple are 0..%d" (len - 1))
                      ~code:"E0108" ~span:tidx.span
                      (Printf.sprintf "tuple index %Ld out of bounds for tuple of type '%s' (length %d)"
                         k (Ty.to_string ttarget.ty) len))
               else Ok (ttarget, tidx, List.nth elem_tys (Int64.to_int k))
             | None ->
               let first_ty = List.hd elem_tys in
               if List.for_all (Ty.equal first_ty) elem_tys then Ok (ttarget, tidx, first_ty)
               else
                 Error
                   (Diagnostic.error
                      ~hint:"use a constant integer literal index like 'tuple[0]' for heterogeneous tuples"
                      ~code:"E0104" ~span:tidx.span
                      "indexing a heterogeneous tuple requires a compile-time constant integer index"))
          | Ty.Array (elem_ty, len) ->
            (match eval_const_int env tidx with
             | Some k when k < 0L || k >= Int64.of_int len ->
               Error
                 (Diagnostic.error
                    ~hint:(Printf.sprintf "valid indices for '%s' are 0..%d" (Ty.to_string ttarget.ty) (len - 1))
                    ~code:"E0108" ~span:tidx.span
                    (Printf.sprintf "array index %Ld out of bounds for array of type '%s' (length %d)"
                       k (Ty.to_string ttarget.ty) len))
             | _ -> Ok (ttarget, tidx, elem_ty))
          | Ty.DynArray elem_ty ->
            Ok (ttarget, tidx, elem_ty)
          | _ ->
            Error
              (Diagnostic.error
                 ~hint:"indexing '[i]' is only supported on tuples and arrays"
                 ~code:"E0104" ~span:ttarget.span
                 (Printf.sprintf "cannot index value of type '%s' (expected a tuple or array)"
                    (Ty.to_string ttarget.ty)))))

let check_lvalue hooks (env : env) ~for_op (lval : Ast.lvalue)
    : (Typed_ast.lvalue * Ty.t * string, Diagnostic.t) result =
  match lval with
  | Ast.LValIdent (name, name_span) ->
    (match Hashtbl.find_opt env name with
     | None ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "declare '%s' first using 'var %s := ...;' or 'const %s := ...;'" name name name)
            ~code:"E0102" ~span:name_span
            (Printf.sprintf "use of undeclared variable '%s'" name))
     | Some info ->
       if info.const_kind = ShallowConst then
         let msg =
           if for_op = "++" || for_op = "--" then
             Printf.sprintf "cannot mutate parameter '%s' with '%s'; parameters are immutable by default (use 'ref %s' to allow mutation)" name for_op name
           else
             Printf.sprintf "cannot reassign parameter '%s'; parameters are immutable by default (use 'ref %s' to allow reassignment)" name name
         in
         Error
           (Diagnostic.error
              ~hint:(Printf.sprintf "use 'ref %s' in the function signature to allow modifying the caller's variable" name)
              ~code:"E0107" ~span:name_span msg)
       else if info.const_kind = DeepConst then
         let msg =
           if for_op = "++" || for_op = "--" then Printf.sprintf "cannot mutate constant '%s' with '%s'" name for_op
           else Printf.sprintf "cannot assign to constant '%s'" name
         in
         Error
           (Diagnostic.error
              ~hint:(Printf.sprintf "'%s' was declared as a constant at %s" name (Span.to_string info.decl_span))
              ~code:"E0107" ~span:name_span msg)
       else
         Ok (Typed_ast.TLValIdent name, info.ty, Printf.sprintf "variable '%s'" name))
  | Ast.LValIndex (base, idx, span) ->
    let rec check_chain_mutable (e : Ast.expr) : Diagnostic.t option =
      match e.desc with
      | Ast.ExprIdent name ->
        (match Hashtbl.find_opt env name with
         | Some info when info.const_kind = DeepConst ->
           Some
             (Diagnostic.error
                ~hint:(Printf.sprintf "'%s' was declared as a constant at %s" name (Span.to_string info.decl_span))
                ~code:"E0107" ~span
                (Printf.sprintf "cannot mutate element of constant '%s'" name))
         | _ -> None)
      | Ast.ExprIndex (sub_base, _) -> check_chain_mutable sub_base
      | _ -> None
    in
    (match check_index_access hooks env ~span base idx with
     | Error err -> Error err
     | Ok (tbase, tidx, elem_ty) ->
       let rec has_tuple_in_base (te : Typed_ast.expr) : bool =
         match te.ty with
         | Ty.Tuple _ -> true
         | _ ->
           (match te.desc with
            | Typed_ast.TExprIndex (inner, _) -> has_tuple_in_base inner
            | _ -> false)
       in
       if has_tuple_in_base tbase then
         Error
           (Diagnostic.error
              ~hint:"tuples are immutable; overwrite the entire tuple variable instead (e.g. 't = (...);') or use an array '[...]'"
              ~code:"E0107" ~span "cannot mutate tuple element: tuples are immutable")
       else
         (match check_chain_mutable base with
          | Some err -> Error err
          | None -> Ok (Typed_ast.TLValIndex (tbase, tidx), elem_ty, "indexed element")))
  | Ast.LValField (base, field, span) ->
    Sema_user_fields.check_field_lvalue hooks env ~span ~for_op base field

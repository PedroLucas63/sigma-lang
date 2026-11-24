open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_ops

let check_binary_expr hooks (env : env) (e_span : Span.t) op lhs rhs : (Typed_ast.expr, Diagnostic.t) result =
  match hooks.check_expr env lhs with
  | Error err -> Error err
  | Ok tlhs ->
    (match hooks.check_expr env rhs with
     | Error err -> Error err
     | Ok trhs ->
       let check_array_repeat elem_ty n count_expr =
         match eval_const_int env count_expr with
         | None ->
           Error
             (Diagnostic.error
                ~hint:"array repetition requires a compile-time constant integer so the static array size is known"
                ~code:"E0104" ~span:count_expr.span
                "array repetition multiplier must be a compile-time integer constant")
         | Some k when k <= 0L ->
           Error
             (Diagnostic.error
                ~hint:"use a positive repetition count >= 1, e.g. 'arr * 3'"
                ~code:"E0108" ~span:count_expr.span
                (Printf.sprintf "array repetition factor must be positive (> 0), found %Ld" k))
         | Some k ->
           Ok
             {
               Typed_ast.desc = Typed_ast.TExprBinary (op, tlhs, trhs);
               ty = Ty.Array (elem_ty, n * Int64.to_int k);
               span = e_span;
             }
       in
       match op, tlhs.ty, trhs.ty with
       | Ast.BinMul, Ty.Array (elem_ty, n), Ty.Int -> check_array_repeat elem_ty n trhs
       | Ast.BinMul, Ty.Int, Ty.Array (elem_ty, n) -> check_array_repeat elem_ty n tlhs
       | _ ->
         (match check_binary_type op tlhs.ty trhs.ty with
          | None ->
            let hint =
              if
                (tlhs.ty = Ty.Decimal && (trhs.ty = Ty.Float || trhs.ty = Ty.Double))
                || (trhs.ty = Ty.Decimal && (tlhs.ty = Ty.Float || tlhs.ty = Ty.Double))
              then
                "mixing binary floating-point ('float'/'double') with exact base-10 'decimal' is forbidden to prevent precision loss"
              else
                Printf.sprintf "operator '%s' is not defined for (%s, %s)"
                  (Ast.binop_to_string op) (Ty.to_string tlhs.ty) (Ty.to_string trhs.ty)
            in
            Error
              (Diagnostic.error ~hint ~code:"E0104" ~span:e_span
                 (Printf.sprintf "invalid binary operation: '%s' between '%s' and '%s'"
                    (Ast.binop_to_string op) (Ty.to_string tlhs.ty) (Ty.to_string trhs.ty)))
          | Some res_ty ->
            if (op = Ast.BinDiv || op = Ast.BinIntDiv || op = Ast.BinMod) && is_zero_literal trhs then
              Error
                (Diagnostic.error ~code:"E0105" ~span:e_span
                   (Printf.sprintf "division or modulo by zero in '%s' operation" (Ast.binop_to_string op)))
            else
              Ok { Typed_ast.desc = Typed_ast.TExprBinary (op, tlhs, trhs); ty = res_ty; span = e_span }))

let check_chain_cmp hooks (env : env) (e_span : Span.t) e1 op1 e2 op2 e3
    : (Typed_ast.expr, Diagnostic.t) result =
  match hooks.check_expr env e1, hooks.check_expr env e2, hooks.check_expr env e3 with
  | Error err, _, _ | _, Error err, _ | _, _, Error err -> Error err
  | Ok te1, Ok te2, Ok te3 ->
    (match check_binary_type op1 te1.ty te2.ty with
     | None ->
       Error
         (Diagnostic.error ~code:"E0104" ~span:(Span.merge te1.span te2.span)
            (Printf.sprintf "invalid relational operation: '%s' between '%s' and '%s'"
               (Ast.binop_to_string op1) (Ty.to_string te1.ty) (Ty.to_string te2.ty)))
     | Some _ ->
       (match check_binary_type op2 te2.ty te3.ty with
        | None ->
          Error
            (Diagnostic.error ~code:"E0104" ~span:(Span.merge te2.span te3.span)
               (Printf.sprintf "invalid relational operation: '%s' between '%s' and '%s'"
                  (Ast.binop_to_string op2) (Ty.to_string te2.ty) (Ty.to_string te3.ty)))
        | Some _ ->
          Ok { Typed_ast.desc = Typed_ast.TExprChainCmp (te1, op1, te2, op2, te3); ty = Ty.Bool; span = e_span }))

let check_assign_expr hooks (env : env) (e_span : Span.t) lval op rhs
    : (Typed_ast.expr, Diagnostic.t) result =
  match hooks.check_lvalue env ~for_op:(Ast.assign_op_to_string op) lval with
  | Error err -> Error err
  | Ok (tlval, lval_ty, lval_label) ->
    (match hooks.check_expr env rhs with
     | Error err -> Error err
     | Ok trhs ->
       (match binop_of_assign_op op with
        | None ->
          if Ty.equal lval_ty trhs.ty then begin
            (match tlval with
             | Typed_ast.TLValIdent target_name ->
               (match rhs.desc with
                | Ast.ExprIdent orig_name ->
                  (match Hashtbl.find_opt var_param_names orig_name with
                   | Some pnames -> Hashtbl.replace var_param_names target_name pnames
                   | None ->
                     (match Hashtbl.find_opt env orig_name with
                      | Some info ->
                        (match Hashtbl.find_opt fun_param_names info.decl_span with
                         | Some pnames -> Hashtbl.replace var_param_names target_name pnames
                         | None -> Hashtbl.remove var_param_names target_name)
                      | None -> Hashtbl.remove var_param_names target_name))
                | _ ->
                  (match Hashtbl.find_opt expr_param_names rhs.span with
                   | Some pnames -> Hashtbl.replace var_param_names target_name pnames
                   | None -> Hashtbl.remove var_param_names target_name))
             | _ -> ());
            Ok { Typed_ast.desc = Typed_ast.TExprAssign (tlval, op, trhs); ty = lval_ty; span = e_span }
          end else
            Error
              (Diagnostic.error
                 ~hint:
                   (Printf.sprintf "Sigma is strongly typed: cannot assign value of type '%s' to %s of type '%s'"
                      (Ty.to_string trhs.ty) lval_label (Ty.to_string lval_ty))
                 ~code:"E0104" ~span:e_span
                 (Printf.sprintf "type mismatch in assignment: %s has type '%s', found '%s'"
                    lval_label (Ty.to_string lval_ty) (Ty.to_string trhs.ty)))
        | Some binop ->
          let op_str = Ast.assign_op_to_string op in
          let eval_res =
            match binop, lval_ty, trhs.ty with
            | Ast.BinMul, Ty.Array (elem_ty, n), Ty.Int ->
              (match eval_const_int env trhs with
               | Some k when k > 0L -> Some (Ty.Array (elem_ty, n * Int64.to_int k))
               | _ -> None)
            | _ -> check_binary_type binop lval_ty trhs.ty
          in
          (match eval_res with
           | None ->
             Error
               (Diagnostic.error
                  ~hint:(Printf.sprintf "compound operator '%s' is not defined for (%s, %s)"
                           op_str (Ty.to_string lval_ty) (Ty.to_string trhs.ty))
                  ~code:"E0104" ~span:e_span
                  (Printf.sprintf "invalid compound assignment: '%s' between '%s' and '%s'"
                     op_str (Ty.to_string lval_ty) (Ty.to_string trhs.ty)))
           | Some res_ty ->
             if not (Ty.equal res_ty lval_ty) then
               let hint =
                 if op = Ast.AssignDiv && lval_ty = Ty.Int then
                   "in Sigma, '/' returns 'float'; use '//=' for integer division assignment on 'int'"
                 else
                   Printf.sprintf "operation produces '%s', which cannot be stored back into %s of type '%s'"
                     (Ty.to_string res_ty) lval_label (Ty.to_string lval_ty)
               in
               Error
                 (Diagnostic.error ~hint ~code:"E0104" ~span:e_span
                    (Printf.sprintf
                       "type mismatch in compound assignment '%s': result type '%s' does not match %s of type '%s'"
                       op_str (Ty.to_string res_ty) lval_label (Ty.to_string lval_ty)))
             else if (binop = Ast.BinDiv || binop = Ast.BinIntDiv || binop = Ast.BinMod) && is_zero_literal trhs then
               Error
                 (Diagnostic.error ~code:"E0105" ~span:e_span
                    (Printf.sprintf "division or modulo by zero in '%s' operation" op_str))
             else
               Ok { Typed_ast.desc = Typed_ast.TExprAssign (tlval, op, trhs); ty = lval_ty; span = e_span })))

let check_incdec_expr hooks (env : env) (e_span : Span.t) inc_op lval
    : (Typed_ast.expr, Diagnostic.t) result =
  let op_str = Ast.incdec_op_to_string inc_op in
  match hooks.check_lvalue env ~for_op:op_str lval with
  | Error err -> Error err
  | Ok (tlval, lval_ty, lval_label) ->
    (match lval_ty with
     | Ty.Int | Ty.Float | Ty.Double | Ty.Decimal | Ty.Char ->
       Ok { Typed_ast.desc = Typed_ast.TExprIncDec (inc_op, tlval); ty = lval_ty; span = e_span }
     | Ty.Bool | Ty.String | Ty.Tuple _ | Ty.Array _ | Ty.User _ | Ty.Fun _ | Ty.Void ->
       Error
         (Diagnostic.error
            ~hint:"'++' and '--' are supported on numeric types ('int', 'float', 'double', 'decimal') and 'char'"
            ~code:"E0104" ~span:e_span
            (Printf.sprintf "invalid operator '%s' for %s of type '%s'"
               op_str lval_label (Ty.to_string lval_ty))))

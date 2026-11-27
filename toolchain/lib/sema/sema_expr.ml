open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_ops

let check_expr hooks (env : env) (e : Ast.expr) : (Typed_ast.expr, Diagnostic.t) result =
  match hooks.check_expr_any env e with
  | Ok (te : Typed_ast.expr) when te.ty = Ty.Void ->
    Error
      (Diagnostic.error
         ~hint:"'act' procedures do not produce a value; call them as a standalone statement, e.g. 'my_act(...);'"
         ~code:"E0113" ~span:e.span
         "the result of an 'act' call cannot be used as a value")
  | r -> r

let check_expr_any hooks (env : env) (e : Ast.expr) : (Typed_ast.expr, Diagnostic.t) result =
  match e.desc with
  | Ast.ExprInt n -> Ok { Typed_ast.desc = Typed_ast.TExprInt n; ty = Ty.Int; span = e.span }
  | Ast.ExprFloat f -> Ok { Typed_ast.desc = Typed_ast.TExprFloat f; ty = Ty.Float; span = e.span }
  | Ast.ExprDouble d -> Ok { Typed_ast.desc = Typed_ast.TExprDouble d; ty = Ty.Double; span = e.span }
  | Ast.ExprDecimal m -> Ok { Typed_ast.desc = Typed_ast.TExprDecimal m; ty = Ty.Decimal; span = e.span }
  | Ast.ExprBool b -> Ok { Typed_ast.desc = Typed_ast.TExprBool b; ty = Ty.Bool; span = e.span }
  | Ast.ExprChar c -> Ok { Typed_ast.desc = Typed_ast.TExprChar c; ty = Ty.Char; span = e.span }
  | Ast.ExprString s -> Ok { Typed_ast.desc = Typed_ast.TExprString s; ty = Ty.String; span = e.span }
  | Ast.ExprInterp parts ->
    let rec check_parts acc = function
      | [] -> Ok (List.rev acc)
      | Ast.InterpLit s :: rest -> check_parts (Typed_ast.TInterpLit s :: acc) rest
      | Ast.InterpExpr (ie, fmt_opt) :: rest ->
        (match check_expr hooks env ie with
         | Error err -> Error err
         | Ok tie ->
           (match fmt_opt with
            | None -> check_parts (Typed_ast.TInterpExpr (tie, None) :: acc) rest
            | Some spec ->
              (match validate_format_spec spec tie.ty with
               | Error err -> Error err
               | Ok () -> check_parts (Typed_ast.TInterpExpr (tie, Some spec) :: acc) rest)))
    in
    (match check_parts [] parts with
     | Ok tparts -> Ok { Typed_ast.desc = Typed_ast.TExprInterp tparts; ty = Ty.String; span = e.span }
     | Error err -> Error err)
  | Ast.ExprInput { prompt; mode } -> Sema_expr_special.check_input_expr hooks env e.span prompt mode
  | Ast.ExprIdent name ->
    (match Hashtbl.find_opt env name with
     | Some info ->
       let actual_name =
         match Hashtbl.find_opt lazy_imports name with
         | Some (m, orig_sym, ExpVal { exp_mangled; _ }) ->
           Sema_modules.mark_used m orig_sym;
           exp_mangled
         | _ -> name
       in
       Ok { Typed_ast.desc = Typed_ast.TExprIdent actual_name; ty = info.ty; span = e.span }
     | None ->
       let not_found () =
         Error
           (Diagnostic.error
              ~hint:(Printf.sprintf "declare '%s' first using 'var %s := ...;'" name name)
              ~code:"E0102" ~span:e.span
              (Printf.sprintf "use of undeclared variable '%s'" name))
       in
       (match !current_type_ctx with
        | None -> not_found ()
        | Some (tname, _, is_static) ->
          (match Hashtbl.find_opt user_types tname with
           | None -> not_found ()
           | Some info ->
             if List.exists (fun f -> f.uf_name = name) info.ut_fields then begin
               if is_static then
                 Error (Diagnostic.error ~code:"E0128" ~span:e.span
                          (Printf.sprintf "cannot access instance field '%s' from a 'static' method" name))
               else
                 Sema_user_fields.check_field_access hooks env ~span:e.span
                   { Ast.desc = Ast.ExprThis; span = e.span } name
             end else
               match List.find_opt (fun m -> m.um_name = name) info.ut_methods with
               | None -> not_found ()
               | Some m when m.um_is_static ->
                 Sema_methods.check_static_method_value ~span:e.span info name
               | Some _ when is_static ->
                 Error (Diagnostic.error ~code:"E0128" ~span:e.span
                          (Printf.sprintf "cannot access instance method '%s' from a 'static' method" name))
               | Some _ ->
                 Sema_user_fields.check_field_access hooks env ~span:e.span
                   { Ast.desc = Ast.ExprThis; span = e.span } name)))
  | Ast.ExprThis ->
    (match !current_type_ctx with
     | None ->
       Error (Diagnostic.error ~code:"E0128" ~span:e.span "'this' can only be used inside an instance method")
     | Some (_, _, true) ->
       Error (Diagnostic.error ~code:"E0128" ~span:e.span "'this' cannot be used inside a 'static' method")
     | Some (tname, kind, false) ->
       Ok { Typed_ast.desc = Typed_ast.TExprIdent "this"; ty = Ty.User (kind, tname); span = e.span })
  | Ast.ExprTypeApp (callee, _, sp) ->
    Error (Diagnostic.error ~code:"E0002" ~span:sp
             (Printf.sprintf "explicit type arguments on '%s' must be followed by '(...)'"
                (match callee.desc with Ast.ExprIdent n -> n | _ -> "expression")))
  | Ast.ExprCall (callee, args) ->
    (match callee.desc with
     | Ast.ExprTypeApp (inner, targs, _) ->
       (match inner.desc with
        | Ast.ExprIdent name when Hashtbl.mem Sema_generics.generic_funs name ->
          Sema_generics_calls.check_generic_fun_call hooks env e name (Some targs) args
        | Ast.ExprIdent name when Hashtbl.mem Sema_generics.generic_types name ->
          Sema_generics_calls.check_generic_new hooks env ~span:e.span ~is_ctor:true name targs args None
        | Ast.ExprField (base, mname) ->
          (match Sema_generics_calls.check_generic_method_call hooks env e base mname (Some targs) args with
           | Some res -> res
           | None ->
             Error (Diagnostic.error ~code:"E0130" ~span:inner.span
                      (Printf.sprintf "generic method '%s' not found" mname)))
        | _ ->
          Error (Diagnostic.error ~code:"E0141" ~span:inner.span "target is not a generic function or method"))
     | Ast.ExprField (base, mname) ->
       (match Sema_generics_calls.check_generic_method_call hooks env e base mname None args with
        | Some res -> res
        | None ->
       (match Sema_methods.try_check_method_call hooks env ~span:e.span ~call_expr:e ~callee base mname args with
        | Some res -> res
        | None ->
          (match check_expr hooks env callee with
           | Error err -> Error err
           | Ok tcallee ->
             (match tcallee.ty with
              | Ty.Fun (ptys, ret) -> hooks.check_call env e tcallee ptys ret callee args
              | other ->
                Error
                  (Diagnostic.error
                     ~hint:"only values of function type ('fun'/'act') can be called"
                     ~code:"E0112" ~span:callee.span
                     (Printf.sprintf "cannot call a value of type '%s'" (Ty.to_string other)))))))
     | Ast.ExprIdent name when Hashtbl.mem Sema_generics.generic_funs name ->
       Sema_generics_calls.check_generic_fun_call hooks env e name None args
     | Ast.ExprIdent name ->
       (match Sema_user_types.check_record_ctor hooks env ~span:e.span name args with
        | Some res -> res
        | None ->
          let implicit_method_opt =
            if Hashtbl.mem env name then None
            else
              match !current_type_ctx with
              | Some (tname, _, false) ->
                (match Hashtbl.find_opt user_types tname with
                 | Some info when List.exists (fun m -> m.um_name = name && not m.um_is_static) info.ut_methods ->
                   Sema_methods.try_check_method_call hooks env ~span:e.span ~call_expr:e ~callee
                     { Ast.desc = Ast.ExprThis; span = callee.span } name args
                 | _ -> None)
              | _ -> None
          in
          (match implicit_method_opt with
           | Some res -> res
           | None ->
             (match check_expr hooks env callee with
              | Error err -> Error err
              | Ok tcallee ->
                (match tcallee.ty with
                 | Ty.Fun (ptys, ret) -> hooks.check_call env e tcallee ptys ret callee args
                 | other ->
                   Error
                     (Diagnostic.error
                        ~hint:"only values of function type ('fun'/'act') can be called"
                        ~code:"E0112" ~span:callee.span
                        (Printf.sprintf "cannot call a value of type '%s'" (Ty.to_string other)))))))
     | _ ->
       (match check_expr hooks env callee with
        | Error err -> Error err
        | Ok tcallee ->
          (match tcallee.ty with
           | Ty.Fun (ptys, ret) -> hooks.check_call env e tcallee ptys ret callee args
           | other ->
             Error
               (Diagnostic.error
                  ~hint:"only values of function type ('fun'/'act') can be called"
                  ~code:"E0112" ~span:callee.span
                  (Printf.sprintf "cannot call a value of type '%s'" (Ty.to_string other))))))
  | Ast.ExprStructInit (name, args) ->
    Sema_user_types.check_struct_init hooks env ~span:e.span name args
  | Ast.ExprNew { new_name; new_type_args; new_is_ctor; new_args; new_inits } ->
    (match Hashtbl.find_opt lazy_imports new_name with
     | Some (m, orig_sym, ExpType _) -> Sema_modules.mark_used m orig_sym
     | _ -> ());
    Sema_generics_calls.check_generic_new hooks env ~span:e.span ~is_ctor:new_is_ctor new_name new_type_args new_args new_inits
  | Ast.ExprField (base, field) ->
    Sema_user_fields.check_field_access hooks env ~span:e.span base field
  | Ast.ExprLambda lam -> Sema_expr_special.check_lambda_expr hooks env e.span lam
  | Ast.ExprTuple items ->
    (match hooks.check_collection_items env items with
     | Error err -> Error err
     | Ok checked ->
       let titems = List.map fst checked in
       let elem_tys = List.concat_map snd checked in
       Ok { Typed_ast.desc = Typed_ast.TExprTuple titems; ty = Ty.Tuple elem_tys; span = e.span })
  | Ast.ExprArray items ->
    if items = [] then
      Error
        (Diagnostic.error
           ~hint:"static arrays require at least 1 element (or declare with explicit size like 'var a : int[3];')"
           ~code:"E0108" ~span:e.span "empty array literal '[]' is not allowed")
    else
      (match hooks.check_collection_items env items with
       | Error err -> Error err
       | Ok checked ->
         let titems = List.map fst checked in
         let elem_tys = List.concat_map snd checked in
         match elem_tys with
         | [] -> Error (Diagnostic.error ~code:"E0108" ~span:e.span "array literal must contain at least 1 element")
         | first_ty :: rest_tys ->
           (match List.find_opt (fun t -> not (Ty.equal first_ty t)) rest_tys with
            | Some bad_ty ->
              Error
                (Diagnostic.error
                   ~hint:"all elements in an array must have the exact same type; use a tuple '(a, b)' for heterogeneous values"
                   ~code:"E0104" ~span:e.span
                   (Printf.sprintf "array elements must all have the same type: expected '%s', found '%s'"
                      (Ty.to_string first_ty) (Ty.to_string bad_ty)))
            | None ->
              Ok
                {
                  Typed_ast.desc = Typed_ast.TExprArray titems;
                  ty = Ty.Array (first_ty, List.length elem_tys);
                  span = e.span;
                }))
  | Ast.ExprIndex (target, idx) ->
    (match hooks.check_index_access env ~span:e.span target idx with
     | Error err -> Error err
     | Ok (ttarget, tidx, elem_ty) ->
       Ok { Typed_ast.desc = Typed_ast.TExprIndex (ttarget, tidx); ty = elem_ty; span = e.span })
  | Ast.ExprUnary (op, sub) ->
    (match check_expr hooks env sub with
     | Error err -> Error err
     | Ok tsub ->
       (match check_unary_type op tsub.ty with
        | Some res_ty -> Ok { Typed_ast.desc = Typed_ast.TExprUnary (op, tsub); ty = res_ty; span = e.span }
        | None ->
          Error
            (Diagnostic.error
               ~hint:"unary '+' and '-' are only supported on numeric types ('int', 'float', 'double', 'decimal')"
               ~code:"E0104" ~span:e.span
               (Printf.sprintf "invalid unary operator '%s' for operand of type '%s'"
                  (Ast.unop_to_string op) (Ty.to_string tsub.ty)))))
  | Ast.ExprBinary (op, lhs, rhs) -> Sema_expr_ops.check_binary_expr hooks env e.span op lhs rhs
  | Ast.ExprChainCmp (e1, op1, e2, op2, e3) -> Sema_expr_ops.check_chain_cmp hooks env e.span e1 op1 e2 op2 e3
  | Ast.ExprAssign (lval, op, rhs) -> Sema_expr_ops.check_assign_expr hooks env e.span lval op rhs
  | Ast.ExprIncDec (inc_op, lval) -> Sema_expr_ops.check_incdec_expr hooks env e.span inc_op lval

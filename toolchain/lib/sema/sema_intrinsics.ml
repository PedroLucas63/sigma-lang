open Sigma_diagnostics
open Sigma_syntax

type spec = {
  kind : Typed_ast.intrinsic_kind;
  params : (string * Ty.t) list;
  ret : Ty.t;
}

let table : (string * spec) list =
  [
    ("__PRINT_STRING", { kind = Typed_ast.IntrPrintString; params = [ ("value", Ty.String) ]; ret = Ty.Void });
    ("__PRINT_INT", { kind = Typed_ast.IntrPrintInt; params = [ ("value", Ty.Int) ]; ret = Ty.Void });
    ("__PRINT_FLOAT", { kind = Typed_ast.IntrPrintFloat; params = [ ("value", Ty.Float) ]; ret = Ty.Void });
    ("__PRINT_DOUBLE", { kind = Typed_ast.IntrPrintDouble; params = [ ("value", Ty.Double) ]; ret = Ty.Void });
    ("__PRINT_DECIMAL", { kind = Typed_ast.IntrPrintDecimal; params = [ ("value", Ty.Decimal) ]; ret = Ty.Void });
    ("__PRINT_BOOL", { kind = Typed_ast.IntrPrintBool; params = [ ("value", Ty.Bool) ]; ret = Ty.Void });
    ("__PRINT_CHAR", { kind = Typed_ast.IntrPrintChar; params = [ ("value", Ty.Char) ]; ret = Ty.Void });
    ("__PRINT_NEWLINE", { kind = Typed_ast.IntrPrintNewline; params = []; ret = Ty.Void });
    ("__READ_LINE", { kind = Typed_ast.IntrReadLine; params = []; ret = Ty.String });
    ("__READ_BLOCK", { kind = Typed_ast.IntrReadBlock; params = []; ret = Ty.String });
  ]

let lookup (name : string) : spec option = List.assoc_opt name table

let is_intrinsic (name : string) : bool = Option.is_some (lookup name)

let check_call check_expr (call_expr : Ast.expr) (name : string) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result =
  match lookup name with
  | None ->
    Error (Diagnostic.error ~code:"E0102" ~span:call_expr.span
             (Printf.sprintf "use of undeclared intrinsic '%s'" name))
  | Some spec ->
    let expected_n = List.length spec.params in
    let actual_n = List.length args in
    if actual_n <> expected_n then
      Error
        (Diagnostic.error
           ~hint:(Printf.sprintf "'%s' expects %d argument(s)" name expected_n)
           ~code:"E0110" ~span:call_expr.span
           (Printf.sprintf "wrong number of arguments for '%s': expected %d, found %d" name expected_n actual_n))
    else
      let rec check_args acc = function
        | [], [] -> Ok (List.rev acc)
        | (a : Ast.call_arg) :: rest_a, (pname, pty) :: rest_p ->
          let name_ok =
            match a.arg_name with
            | None -> Ok ()
            | Some (nm, nsp) ->
              if nm = pname then Ok ()
              else
                Error (Diagnostic.error
                         ~hint:(Printf.sprintf "parameter is named '%s'" pname)
                         ~code:"E0110" ~span:nsp
                         (Printf.sprintf "unknown parameter name '%s'" nm))
          in
          (match name_ok with
           | Error err -> Error err
           | Ok () ->
             (match a.arg_mode with
              | Ast.ArgRef ->
                Error (Diagnostic.error ~code:"E0116" ~span:a.arg_value.span
                         (Printf.sprintf "parameter '%s' is not marked 'ref'; remove 'ref'" pname))
              | Ast.ArgOut | Ast.ArgOutVar _ ->
                Error (Diagnostic.error ~code:"E0116" ~span:a.arg_value.span
                         (Printf.sprintf "parameter '%s' is not marked 'out'; remove 'out'" pname))
              | Ast.ArgVal ->
                (match check_expr a.arg_value with
                 | Error err -> Error err
                 | Ok (texpr : Typed_ast.expr) ->
                   if Ty.equal texpr.ty pty then check_args (texpr :: acc) (rest_a, rest_p)
                   else
                     Error
                       (Diagnostic.error
                          ~hint:(Printf.sprintf "expected '%s' for parameter '%s' of '%s'"
                                   (Ty.to_string pty) pname name)
                          ~code:"E0104" ~span:a.arg_value.span
                          (Printf.sprintf "type mismatch: expected '%s', got '%s'"
                             (Ty.to_string pty) (Ty.to_string texpr.ty))))))
        | _ -> assert false
      in
      (match check_args [] (args, spec.params) with
       | Error err -> Error err
       | Ok targs ->
         Ok { Typed_ast.desc = Typed_ast.TExprIntrinsic (spec.kind, targs); ty = spec.ret; span = call_expr.span })

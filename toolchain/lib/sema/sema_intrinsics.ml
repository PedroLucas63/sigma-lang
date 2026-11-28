open Sigma_diagnostics
open Sigma_syntax
open Sema_env

type spec = {
  kind : Typed_ast.intrinsic_kind;
  params : (string * Ty.param_mode * Ty.t) list;
  ret : Ty.t;
}

let table : (string * spec) list =
  [
    ("__PRINT_STRING", { kind = Typed_ast.IntrPrintString; params = [ ("value", Ty.ParamVal, Ty.String) ]; ret = Ty.Void });
    ("__PRINT_INT", { kind = Typed_ast.IntrPrintInt; params = [ ("value", Ty.ParamVal, Ty.Int) ]; ret = Ty.Void });
    ("__PRINT_FLOAT", { kind = Typed_ast.IntrPrintFloat; params = [ ("value", Ty.ParamVal, Ty.Float) ]; ret = Ty.Void });
    ("__PRINT_DOUBLE", { kind = Typed_ast.IntrPrintDouble; params = [ ("value", Ty.ParamVal, Ty.Double) ]; ret = Ty.Void });
    ("__PRINT_DECIMAL", { kind = Typed_ast.IntrPrintDecimal; params = [ ("value", Ty.ParamVal, Ty.Decimal) ]; ret = Ty.Void });
    ("__PRINT_BOOL", { kind = Typed_ast.IntrPrintBool; params = [ ("value", Ty.ParamVal, Ty.Bool) ]; ret = Ty.Void });
    ("__PRINT_CHAR", { kind = Typed_ast.IntrPrintChar; params = [ ("value", Ty.ParamVal, Ty.Char) ]; ret = Ty.Void });
    ("__PRINT_NEWLINE", { kind = Typed_ast.IntrPrintNewline; params = []; ret = Ty.Void });
    ("__READ_LINE", { kind = Typed_ast.IntrReadLine; params = []; ret = Ty.String });
    ("__READ_BLOCK", { kind = Typed_ast.IntrReadBlock; params = []; ret = Ty.String });
    ("__PARSE_INT", { kind = Typed_ast.IntrParseInt; params = [ ("value", Ty.ParamVal, Ty.String) ]; ret = Ty.Int });
    ("__PARSE_FLOAT", { kind = Typed_ast.IntrParseFloat; params = [ ("value", Ty.ParamVal, Ty.String) ]; ret = Ty.Float });
    ("__PARSE_DOUBLE", { kind = Typed_ast.IntrParseDouble; params = [ ("value", Ty.ParamVal, Ty.String) ]; ret = Ty.Double });
    ("__PARSE_DECIMAL", { kind = Typed_ast.IntrParseDecimal; params = [ ("value", Ty.ParamVal, Ty.String) ]; ret = Ty.Decimal });
    ("__PARSE_BOOL", { kind = Typed_ast.IntrParseBool; params = [ ("value", Ty.ParamVal, Ty.String) ]; ret = Ty.Bool });
    ("__PARSE_CHAR", { kind = Typed_ast.IntrParseChar; params = [ ("value", Ty.ParamVal, Ty.String) ]; ret = Ty.Char });
    ("__TRY_PARSE_INT", { kind = Typed_ast.IntrTryParseInt; params = [ ("value", Ty.ParamVal, Ty.String); ("result", Ty.ParamOut, Ty.Int) ]; ret = Ty.Bool });
    ("__TRY_PARSE_FLOAT", { kind = Typed_ast.IntrTryParseFloat; params = [ ("value", Ty.ParamVal, Ty.String); ("result", Ty.ParamOut, Ty.Float) ]; ret = Ty.Bool });
    ("__TRY_PARSE_DOUBLE", { kind = Typed_ast.IntrTryParseDouble; params = [ ("value", Ty.ParamVal, Ty.String); ("result", Ty.ParamOut, Ty.Double) ]; ret = Ty.Bool });
    ("__TRY_PARSE_DECIMAL", { kind = Typed_ast.IntrTryParseDecimal; params = [ ("value", Ty.ParamVal, Ty.String); ("result", Ty.ParamOut, Ty.Decimal) ]; ret = Ty.Bool });
    ("__TRY_PARSE_BOOL", { kind = Typed_ast.IntrTryParseBool; params = [ ("value", Ty.ParamVal, Ty.String); ("result", Ty.ParamOut, Ty.Bool) ]; ret = Ty.Bool });
    ("__TRY_PARSE_CHAR", { kind = Typed_ast.IntrTryParseChar; params = [ ("value", Ty.ParamVal, Ty.String); ("result", Ty.ParamOut, Ty.Char) ]; ret = Ty.Bool });
  ]

let lookup (name : string) : spec option = List.assoc_opt name table

let is_intrinsic (name : string) : bool = Option.is_some (lookup name)

let () = is_intrinsic_fn := is_intrinsic

let check_call hooks (env : env) (call_expr : Ast.expr) (name : string) (args : Ast.call_arg list)
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
      let ptys = List.map (fun (_, pmode, pty) -> { Ty.pmode; pty }) spec.params in
      let pnames = List.map (fun (nm, _, _) -> nm) spec.params in
      let synth_callee_ast : Ast.expr = { Ast.desc = Ast.ExprThis; span = call_expr.span } in
      let synth_tcallee : Typed_ast.expr =
        { Typed_ast.desc = Typed_ast.TExprIdent name; ty = Ty.Fun (ptys, spec.ret); span = call_expr.span }
      in
      Hashtbl.replace expr_param_names call_expr.span pnames;
      let res = hooks.check_call env call_expr synth_tcallee ptys spec.ret synth_callee_ast args in
      Hashtbl.remove expr_param_names call_expr.span;
      match res with
      | Error err -> Error err
      | Ok { Typed_ast.desc = Typed_ast.TExprCall { args = tcas; _ }; _ } ->
        let ordered = List.sort (fun (a : Typed_ast.typed_call_arg) (b : Typed_ast.typed_call_arg) -> compare a.tca_idx b.tca_idx) tcas in
        Ok { Typed_ast.desc = Typed_ast.TExprIntrinsic (spec.kind, ordered); ty = spec.ret; span = call_expr.span }
      | Ok other -> Ok other

open Sigma_common
open Sigma_diagnostics

type state = {
  tokens : Token.t array;
  mutable idx : int;
}

let cur st : Token.t =
  if st.idx < Array.length st.tokens then st.tokens.(st.idx)
  else st.tokens.(Array.length st.tokens - 1)

let bump st =
  if st.idx < Array.length st.tokens - 1 then st.idx <- st.idx + 1

let is_eof st =
  match (cur st).kind with
  | Token.EOF -> true
  | _ -> false

let sync_to_semi st =
  while not (is_eof st) && (cur st).kind <> Token.SEMI do
    bump st
  done;
  if (cur st).kind = Token.SEMI then bump st

let parse_expr st : (Ast.expr, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.INT n ->
    bump st;
    Ok { Ast.desc = Ast.ExprInt n; span = tok.span }
  | Token.FLOAT f ->
    bump st;
    Ok { Ast.desc = Ast.ExprFloat f; span = tok.span }
  | Token.DOUBLE d ->
    bump st;
    Ok { Ast.desc = Ast.ExprDouble d; span = tok.span }
  | Token.DECIMAL m ->
    bump st;
    Ok { Ast.desc = Ast.ExprDecimal m; span = tok.span }
  | Token.BOOL b ->
    bump st;
    Ok { Ast.desc = Ast.ExprBool b; span = tok.span }
  | Token.MINUS ->
    bump st;
    let next_tok = cur st in
    (match next_tok.kind with
     | Token.INT n ->
       bump st;
       let span = Span.merge tok.span next_tok.span in
       Ok { Ast.desc = Ast.ExprInt (Int64.neg n); span }
     | Token.FLOAT f ->
       bump st;
       let span = Span.merge tok.span next_tok.span in
       Ok { Ast.desc = Ast.ExprFloat (-.f); span }
     | Token.DOUBLE d ->
       bump st;
       let span = Span.merge tok.span next_tok.span in
       Ok { Ast.desc = Ast.ExprDouble (-.d); span }
     | Token.DECIMAL m ->
       bump st;
       let span = Span.merge tok.span next_tok.span in
       Ok { Ast.desc = Ast.ExprDecimal (-.m); span }
     | _ ->
       Error
         (Diagnostic.error
            ~code:"E0002"
            ~span:next_tok.span
            (Printf.sprintf "expected numeric literal after '-', found %s"
               (Token.kind_to_string next_tok.kind))))
  | Token.IDENT name ->
    bump st;
    Ok { Ast.desc = Ast.ExprIdent name; span = tok.span }
  | _ ->
    Error
      (Diagnostic.error
         ~code:"E0002"
         ~span:tok.span
         (Printf.sprintf "expected expression, found %s"
            (Token.kind_to_string tok.kind)))

let expect_semi st start_span : (Span.t, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.SEMI ->
    bump st;
    Ok (Span.merge start_span tok.span)
  | _ ->
    Error
      (Diagnostic.error
         ~hint:"every statement in Sigma must end with ';'"
         ~code:"E0003"
         ~span:tok.span
         (Printf.sprintf "expected ';' at end of statement, found %s"
            (Token.kind_to_string tok.kind)))

let parse_var_decl st var_tok : (Ast.stmt, Diagnostic.t) result =
  let name_tok = cur st in
  match name_tok.kind with
  | Token.IDENT name ->
    bump st;
    let op_tok = cur st in
    (match op_tok.kind with
     | Token.COLON_EQUAL ->
       bump st;
       (match parse_expr st with
        | Error e -> Error e
        | Ok init ->
          (match expect_semi st var_tok.Token.span with
           | Error e -> Error e
           | Ok full_span ->
             let decl =
               { Ast.name; name_span = name_tok.span; ty_annot = None; init = Some init }
             in
             Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }))
     | Token.COLON ->
       bump st;
       let ty_tok = cur st in
       (match ty_tok.kind with
        | Token.IDENT ty_name ->
          bump st;
          let ty_annot = Some (ty_name, ty_tok.span) in
          let after_ty = cur st in
          (match after_ty.kind with
           | Token.EQUAL ->
             bump st;
             (match parse_expr st with
              | Error e -> Error e
              | Ok init ->
                (match expect_semi st var_tok.span with
                 | Error e -> Error e
                 | Ok full_span ->
                   let decl =
                     { Ast.name; name_span = name_tok.span; ty_annot; init = Some init }
                   in
                   Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }))
           | Token.SEMI ->
             bump st;
             let full_span = Span.merge var_tok.span after_ty.span in
             let decl =
               { Ast.name; name_span = name_tok.span; ty_annot; init = None }
             in
             Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }
           | _ ->
             Error
               (Diagnostic.error
                  ~hint:"use 'var x : int = 10;' or 'var x : int;'"
                  ~code:"E0003"
                  ~span:after_ty.span
                  (Printf.sprintf "expected '=' or ';' after type '%s', found %s"
                     ty_name (Token.kind_to_string after_ty.kind))))
        | _ ->
          Error
            (Diagnostic.error
               ~code:"E0002"
               ~span:ty_tok.span
               (Printf.sprintf "expected type name after ':', found %s"
                  (Token.kind_to_string ty_tok.kind))))
     | Token.EQUAL ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "use 'var %s := <value>;' for type inference or 'var %s : int = <value>;'" name name)
            ~code:"E0002"
            ~span:op_tok.span
            "cannot use '=' without an explicit type annotation in 'var' declaration")
     | _ ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "use 'var %s : <type> [= <value>];' or 'var %s := <value>;'" name name)
            ~code:"E0002"
            ~span:op_tok.span
            (Printf.sprintf "expected ':' or ':=' after variable name '%s', found %s"
               name (Token.kind_to_string op_tok.kind))))
  | _ ->
    Error
      (Diagnostic.error
         ~code:"E0002"
         ~span:name_tok.span
         (Printf.sprintf "expected variable name after 'var', found %s"
            (Token.kind_to_string name_tok.kind)))

let parse_stmt st : (Ast.stmt, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.KW_VAR ->
    bump st;
    parse_var_decl st tok
  | _ ->
    (match parse_expr st with
     | Error e -> Error e
     | Ok expr ->
       (match expect_semi st expr.span with
        | Error e -> Error e
        | Ok full_span ->
          Ok { Ast.desc = Ast.StmtExpr expr; span = full_span }))

let parse (token_list : Token.t list) : (Ast.program, Diagnostic.t list) result =
  let tokens = Array.of_list token_list in
  let st = { tokens; idx = 0 } in
  let stmts = ref [] in
  let errors = ref [] in
  while not (is_eof st) do
    match parse_stmt st with
    | Ok s -> stmts := s :: !stmts
    | Error err ->
      errors := err :: !errors;
      sync_to_semi st
  done;
  match List.rev !errors with
  | [] -> Ok (List.rev !stmts)
  | errs -> Error errs

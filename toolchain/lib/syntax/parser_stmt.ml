open Sigma_common
open Sigma_diagnostics
open Parser_state

let parse_call_args hooks st fn_name fn_tok ~require_semi : (Ast.expr list * Span.t, Diagnostic.t) result =
  let open_tok = cur st in
  match open_tok.kind with
  | Token.LPAREN ->
    bump st;
    let rec parse_args acc =
      match hooks.parse_expr st with
      | Error e -> Error e
      | Ok arg ->
        let acc = arg :: acc in
        let next_tok = cur st in
        (match next_tok.kind with
         | Token.COMMA -> bump st; parse_args acc
         | Token.RPAREN -> bump st; Ok (List.rev acc, next_tok.span)
         | _ ->
           err_at next_tok ~hint:(Printf.sprintf "use '%s(a, b);' and close with ')'" fn_name)
             (Printf.sprintf "expected ')' or ',' after argument in '%s', found %s"
                fn_name (Token.kind_to_string next_tok.kind)))
    in
    let args_res =
      if (cur st).kind = Token.RPAREN then begin
        let close_tok = cur st in
        bump st;
        Ok ([], close_tok.span)
      end else parse_args []
    in
    (match args_res with
     | Error e -> Error e
     | Ok (args, close_sp) ->
       (match finish_inline_stmt st fn_tok.Token.span close_sp ~require_semi with
        | Error e -> Error e
        | Ok full_span -> Ok (args, full_span)))
  | _ ->
    err_at open_tok ~hint:(Printf.sprintf "call '%s' with parentheses, e.g. '%s(...);'" fn_name fn_name)
      (Printf.sprintf "expected '(' after '%s', found %s" fn_name (Token.kind_to_string open_tok.kind))

let parse_print_stmt hooks st fn_tok ~newline ~require_semi : (Ast.stmt, Diagnostic.t) result =
  let fn_name = if newline then "println" else "print" in
  match parse_call_args hooks st fn_name fn_tok ~require_semi with
  | Error e -> Error e
  | Ok (args, full_span) -> Ok { Ast.desc = Ast.StmtPrint { newline; args }; span = full_span }

let parse_assert_stmt hooks st fn_tok ~require_semi : (Ast.stmt, Diagnostic.t) result =
  match parse_call_args hooks st "assert" fn_tok ~require_semi with
  | Error e -> Error e
  | Ok (args, full_span) ->
    (match args with
     | [ actual ] ->
       Ok { Ast.desc = Ast.StmtAssert { actual; expected = None; message = None }; span = full_span }
     | [ actual; expected ] ->
       Ok { Ast.desc = Ast.StmtAssert { actual; expected = Some expected; message = None }; span = full_span }
     | [ actual; expected; msg ] ->
       Ok { Ast.desc = Ast.StmtAssert { actual; expected = Some expected; message = Some msg }; span = full_span }
     | _ ->
       Error
         (Diagnostic.error
            ~hint:"use 'assert(cond);', 'assert(actual, expected);', or 'assert(actual, expected, \"message\");'"
            ~code:"E0002" ~span:full_span
            (Printf.sprintf "'assert' expects 1, 2, or 3 arguments, found %d" (List.length args))))

let parse_assert_error_stmt hooks st fn_tok ~require_semi : (Ast.stmt, Diagnostic.t) result =
  match parse_call_args hooks st "assert_error" fn_tok ~require_semi with
  | Error e -> Error e
  | Ok (args, full_span) ->
    let extract_str (e : Ast.expr) (arg_desc : string) =
      match e.desc with
      | Ast.ExprString s -> Ok (s, e.span)
      | _ ->
        Error
          (Diagnostic.error
             ~hint:"use a string literal, e.g. 'assert_error(\"E0106\", \"const x : int;\");'"
             ~code:"E0002" ~span:e.span
             (Printf.sprintf "'assert_error' requires a string literal for %s" arg_desc))
    in
    (match args with
     | [ code_expr; snippet_expr ] ->
       (match extract_str code_expr "expected error code", extract_str snippet_expr "code snippet" with
        | Error e, _ | _, Error e -> Error e
        | Ok (expected_code, code_span), Ok (snippet, snippet_span) ->
          Ok
            {
              Ast.desc = Ast.StmtAssertError { expected_code; code_span; snippet; snippet_span; message = None };
              span = full_span;
            })
     | [ code_expr; snippet_expr; msg_expr ] ->
       (match extract_str code_expr "expected error code", extract_str snippet_expr "code snippet" with
        | Error e, _ | _, Error e -> Error e
        | Ok (expected_code, code_span), Ok (snippet, snippet_span) ->
          Ok
            {
              Ast.desc =
                Ast.StmtAssertError
                  { expected_code; code_span; snippet; snippet_span; message = Some msg_expr };
              span = full_span;
            })
     | _ ->
       Error
         (Diagnostic.error
            ~hint:"use 'assert_error(\"E0106\", \"const x : int;\");' or 'assert_error(\"E0106\", \"const x : int;\", \"message\");'"
            ~code:"E0002" ~span:full_span
            (Printf.sprintf "'assert_error' expects 2 or 3 arguments, found %d" (List.length args))))

let parse_return_stmt hooks st kw_tok : (Ast.stmt, Diagnostic.t) result =
  let t = cur st in
  if t.kind = Token.SEMI then begin
    bump st;
    Ok { Ast.desc = Ast.StmtReturn None; span = Span.merge kw_tok.Token.span t.span }
  end else
    match hooks.parse_expr st with
    | Error e -> Error e
    | Ok e ->
      (match expect_semi st kw_tok.Token.span with
       | Error err -> Error err
       | Ok full_span -> Ok { Ast.desc = Ast.StmtReturn (Some e); span = full_span })

let parse_break_or_continue_stmt st kw_tok ~is_break ~require_semi : (Ast.stmt, Diagnostic.t) result =
  let label_opt, end_sp =
    let next_tok = cur st in
    match next_tok.kind with
    | Token.IDENT lbl -> bump st; (Some (lbl, next_tok.span), next_tok.span)
    | _ -> (None, kw_tok.Token.span)
  in
  match finish_inline_stmt st kw_tok.Token.span end_sp ~require_semi with
  | Error e -> Error e
  | Ok full_span ->
    let desc = if is_break then Ast.StmtBreak label_opt else Ast.StmtContinue label_opt in
    Ok { Ast.desc; span = full_span }

let parse_block_stmt hooks st open_brace_tok : (Ast.stmt, Diagnostic.t) result =
  let stmts = ref [] in
  let first_err = ref None in
  while not (is_eof st) && (cur st).kind <> Token.RBRACE && !first_err = None do
    match hooks.parse_stmt st with
    | Ok s -> stmts := s :: !stmts
    | Error err -> first_err := Some err
  done;
  match !first_err with
  | Some err -> Error err
  | None ->
    let close_tok = cur st in
    (match close_tok.kind with
     | Token.RBRACE ->
       bump st;
       Ok { Ast.desc = Ast.StmtBlock (List.rev !stmts); span = Span.merge open_brace_tok.Token.span close_tok.span }
     | _ -> err_at open_brace_tok "unclosed block '{' (missing closing '}')")

let rec parse_non_decl_stmt hooks st : (Ast.stmt, Diagnostic.t) result =
  let tok = cur st in
  let nd_parser = parse_non_decl_stmt hooks in
  match tok.kind with
  | Token.KW_VAR | Token.KW_CONST ->
    let kw_str = if tok.kind = Token.KW_VAR then "variable declaration 'var'" else "constant declaration 'const'" in
    let kw_name = if tok.kind = Token.KW_VAR then "var" else "const" in
    err_at tok ~hint:(Printf.sprintf "wrap the declaration in braces, e.g. 'if (cond) { %s x := ...; }'" kw_name)
      (Printf.sprintf "%s is not allowed directly inside an unbraced branch" kw_str)
  | (Token.KW_FUN | Token.KW_ACT) when (peek_next st).kind <> Token.LPAREN ->
    let kw = if tok.kind = Token.KW_FUN then "fun" else "act" in
    err_at tok ~hint:(Printf.sprintf "wrap the declaration in braces, e.g. 'if (cond) { %s f(...) ... }'" kw)
      (Printf.sprintf "'%s' declaration is not allowed directly inside an unbraced branch" kw)
  | Token.KW_RETURN -> bump st; parse_return_stmt hooks st tok
  | Token.KW_IF -> bump st; hooks.parse_if_stmt st tok ~stmt_parser:nd_parser
  | Token.KW_WHILE -> bump st; hooks.parse_while_stmt st tok.span ~label:None ~stmt_parser:nd_parser
  | Token.KW_FOR -> bump st; hooks.parse_for_stmt st tok.span ~label:None ~stmt_parser:nd_parser
  | Token.KW_FOREACH -> bump st; hooks.parse_foreach_stmt st tok.span ~label:None ~stmt_parser:nd_parser
  | Token.IDENT lbl when (peek_next st).kind = Token.COLON ->
    bump st;
    let colon_tok = cur st in
    bump st;
    let loop_tok = cur st in
    let label = Some (lbl, tok.span) in
    (match loop_tok.kind with
     | Token.KW_WHILE -> bump st; hooks.parse_while_stmt st tok.span ~label ~stmt_parser:nd_parser
     | Token.KW_FOR -> bump st; hooks.parse_for_stmt st tok.span ~label ~stmt_parser:nd_parser
     | Token.KW_FOREACH -> bump st; hooks.parse_foreach_stmt st tok.span ~label ~stmt_parser:nd_parser
     | _ ->
       Error
         (Diagnostic.error
            ~hint:"labels can only annotate loops, e.g. 'loop1: for (var i := 0; i < 10; i++) { ... }'"
            ~code:"E0002" ~span:(Span.merge tok.span colon_tok.span)
            (Printf.sprintf "label '%s:' must be followed by 'while', 'for', or 'foreach', found %s"
               lbl (Token.kind_to_string loop_tok.kind))))
  | Token.KW_BREAK -> bump st; parse_break_or_continue_stmt st tok ~is_break:true ~require_semi:true
  | Token.KW_CONTINUE -> bump st; parse_break_or_continue_stmt st tok ~is_break:false ~require_semi:true
  | Token.LBRACE -> bump st; parse_block_stmt hooks st tok
  | Token.KW_PRINT -> bump st; parse_print_stmt hooks st tok ~newline:false ~require_semi:true
  | Token.KW_PRINTLN -> bump st; parse_print_stmt hooks st tok ~newline:true ~require_semi:true
  | Token.KW_ASSERT -> bump st; parse_assert_stmt hooks st tok ~require_semi:true
  | Token.KW_ASSERT_ERROR -> bump st; parse_assert_error_stmt hooks st tok ~require_semi:true
  | _ ->
    (match hooks.parse_expr st with
     | Error e -> Error e
     | Ok expr ->
       (match expect_semi st expr.span with
        | Error e -> Error e
        | Ok full_span -> Ok { Ast.desc = Ast.StmtExpr expr; span = full_span }))

let parse_stmt hooks st : (Ast.stmt, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.KW_VAR -> bump st; hooks.parse_decl st tok ~is_const:false
  | Token.KW_CONST -> bump st; hooks.parse_decl st tok ~is_const:true
  | Token.KW_FUN when (peek_next st).kind <> Token.LPAREN ->
    bump st;
    Parser_decl.parse_fun_decl hooks st tok ~is_act:false
  | Token.KW_ACT when (peek_next st).kind <> Token.LPAREN ->
    bump st;
    Parser_decl.parse_fun_decl hooks st tok ~is_act:true
  | _ -> parse_non_decl_stmt hooks st

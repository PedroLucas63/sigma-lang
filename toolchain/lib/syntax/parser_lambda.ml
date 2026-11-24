open Sigma_common
open Sigma_diagnostics
open Parser_state

let rec parse_lambda_inline_stmt hooks st ~is_act : (Ast.stmt, Diagnostic.t) result =
  let tok = cur st in
  let inline_parser s = parse_lambda_inline_stmt hooks s ~is_act in
  match tok.kind with
  | Token.LBRACE ->
    bump st;
    hooks.parse_block_stmt st tok
  | Token.KW_VAR | Token.KW_CONST ->
    let kw_str = if tok.kind = Token.KW_VAR then "variable declaration 'var'" else "constant declaration 'const'" in
    let kw_name = if tok.kind = Token.KW_VAR then "var" else "const" in
    err_at tok ~hint:(Printf.sprintf "wrap the declaration in braces, e.g. '=> { %s x := ...; }'" kw_name)
      (Printf.sprintf "%s is not allowed directly inside an unbraced lambda body" kw_str)
  | (Token.KW_FUN | Token.KW_ACT) when (peek_next st).kind <> Token.LPAREN ->
    let kw = if tok.kind = Token.KW_FUN then "fun" else "act" in
    err_at tok ~hint:(Printf.sprintf "wrap the declaration in braces, e.g. '=> { %s f(...) ... }'" kw)
      (Printf.sprintf "'%s' declaration is not allowed directly inside an unbraced lambda body" kw)
  | Token.KW_RETURN ->
    bump st;
    let nt = cur st in
    (match nt.kind with
     | Token.SEMI | Token.COMMA | Token.RPAREN | Token.RBRACE | Token.RBRACKET | Token.KW_ELSE | Token.EOF ->
       (match finish_inline_stmt st tok.span tok.span ~require_semi:false with
        | Error err -> Error err
        | Ok full_span -> Ok { Ast.desc = Ast.StmtReturn None; span = full_span })
     | _ ->
       (match hooks.parse_expr st with
        | Error e -> Error e
        | Ok e ->
          (match finish_inline_stmt st tok.span e.span ~require_semi:false with
           | Error err -> Error err
           | Ok full_span -> Ok { Ast.desc = Ast.StmtReturn (Some e); span = full_span })))
  | Token.KW_IF -> bump st; hooks.parse_if_stmt st tok ~stmt_parser:inline_parser
  | Token.KW_WHILE -> bump st; hooks.parse_while_stmt st tok.span ~label:None ~stmt_parser:inline_parser
  | Token.KW_FOR -> bump st; hooks.parse_for_stmt st tok.span ~label:None ~stmt_parser:inline_parser
  | Token.KW_FOREACH -> bump st; hooks.parse_foreach_stmt st tok.span ~label:None ~stmt_parser:inline_parser
  | Token.IDENT lbl when (peek_next st).kind = Token.COLON ->
    bump st;
    let colon_tok = cur st in
    bump st;
    let loop_tok = cur st in
    let label = Some (lbl, tok.span) in
    (match loop_tok.kind with
     | Token.KW_WHILE -> bump st; hooks.parse_while_stmt st tok.span ~label ~stmt_parser:inline_parser
     | Token.KW_FOR -> bump st; hooks.parse_for_stmt st tok.span ~label ~stmt_parser:inline_parser
     | Token.KW_FOREACH -> bump st; hooks.parse_foreach_stmt st tok.span ~label ~stmt_parser:inline_parser
     | _ ->
       Error
         (Diagnostic.error
            ~hint:"labels can only annotate loops, e.g. 'loop1: for (var i := 0; i < 10; i++) { ... }'"
            ~code:"E0002"
            ~span:(Span.merge tok.span colon_tok.span)
            (Printf.sprintf "label '%s:' must be followed by 'while', 'for', or 'foreach', found %s"
               lbl (Token.kind_to_string loop_tok.kind))))
  | Token.KW_BREAK -> bump st; hooks.parse_break_or_continue_stmt st tok ~is_break:true ~require_semi:false
  | Token.KW_CONTINUE -> bump st; hooks.parse_break_or_continue_stmt st tok ~is_break:false ~require_semi:false
  | Token.KW_PRINT -> bump st; hooks.parse_print_stmt st tok ~newline:false ~require_semi:false
  | Token.KW_PRINTLN -> bump st; hooks.parse_print_stmt st tok ~newline:true ~require_semi:false
  | Token.KW_ASSERT -> bump st; hooks.parse_assert_stmt st tok ~require_semi:false
  | Token.KW_ASSERT_ERROR -> bump st; hooks.parse_assert_error_stmt st tok ~require_semi:false
  | _ ->
    (match hooks.parse_expr st with
     | Error e -> Error e
     | Ok expr ->
       (match finish_inline_stmt st expr.span expr.span ~require_semi:false with
        | Error err -> Error err
        | Ok full_span ->
          if is_act then Ok { Ast.desc = Ast.StmtExpr expr; span = full_span }
          else Ok { Ast.desc = Ast.StmtReturn (Some expr); span = full_span }))

let parse_lambda_expr hooks st kw_tok ~is_act : (Ast.expr, Diagnostic.t) result =
  let kw_name = if is_act then "act" else "fun" in
  let lp = cur st in
  if lp.kind <> Token.LPAREN then
    err_at lp
      (Printf.sprintf "expected '(' after anonymous '%s', found %s" kw_name
         (Token.kind_to_string lp.kind))
  else begin
    bump st;
    match Parser_decl.parse_fun_params st with
    | Error e -> Error e
    | Ok ps ->
      let ret_res =
        if (cur st).kind = Token.COLON then begin
          bump st;
          match Parser_types.parse_type_expr st with
          | Ok te -> Ok (Some te)
          | Error e -> Error e
        end else
          Ok None
      in
      (match ret_res with
       | Error e -> Error e
       | Ok ret ->
         let arrow_tok = cur st in
         if arrow_tok.kind <> Token.FAT_ARROW then
           err_at arrow_tok
             (Printf.sprintf "expected '=>' in anonymous '%s', found %s" kw_name
                (Token.kind_to_string arrow_tok.kind))
         else begin
           bump st;
           if (cur st).kind = Token.LBRACE then begin
             let ob = cur st in
             bump st;
             match hooks.parse_block_stmt st ob with
             | Error e -> Error e
             | Ok { Ast.desc = Ast.StmtBlock body; span } ->
               Ok
                 {
                   Ast.desc =
                     Ast.ExprLambda
                       { Ast.lam_is_act = is_act; lam_params = ps; lam_ret = ret; lam_body = body };
                   span = Span.merge kw_tok.Token.span span;
                 }
             | Ok _ -> err_at ob "internal error: lambda block must be a block"
           end else
             match parse_lambda_inline_stmt hooks st ~is_act with
             | Error e -> Error e
             | Ok body_stmt ->
               Ok
                 {
                   Ast.desc =
                     Ast.ExprLambda
                       { Ast.lam_is_act = is_act; lam_params = ps; lam_ret = ret; lam_body = [ body_stmt ] };
                   span = Span.merge kw_tok.Token.span body_stmt.span;
                 }
         end)
  end

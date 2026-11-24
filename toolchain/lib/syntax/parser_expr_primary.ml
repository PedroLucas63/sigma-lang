open Sigma_common
open Sigma_diagnostics
open Parser_state

let parse_collection_item hooks st : (Ast.collection_item, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.DOT_DOT ->
    bump st;
    (match hooks.parse_expr st with
     | Error e -> Error e
     | Ok sub -> Ok (Ast.ItemSpread (sub, Span.merge tok.span sub.span)))
  | _ ->
    (match hooks.parse_expr st with
     | Error e -> Error e
     | Ok sub -> Ok (Ast.ItemExpr sub))

let rec parse_collection_rest hooks st ~close_kind ~close_str (acc : Ast.collection_item list)
    : (Ast.collection_item list * Span.t, Diagnostic.t) result =
  let tok = cur st in
  if tok.kind = close_kind then begin
    bump st;
    Ok (List.rev acc, tok.span)
  end else
    match parse_collection_item hooks st with
    | Error e -> Error e
    | Ok item ->
      let acc = item :: acc in
      let next_tok = cur st in
      if next_tok.kind = Token.COMMA then begin
        bump st;
        parse_collection_rest hooks st ~close_kind ~close_str acc
      end else if next_tok.kind = close_kind then begin
        bump st;
        Ok (List.rev acc, next_tok.span)
      end else
        err_at next_tok
          (Printf.sprintf "expected '%s' or ',' in collection literal, found %s"
             close_str (Token.kind_to_string next_tok.kind))

let parse_interp_parts hooks segments str_span : (Ast.expr, Diagnostic.t) result =
  let rec convert acc = function
    | [] -> Ok (List.rev acc)
    | Token.SegLit s :: rest -> convert (Ast.InterpLit s :: acc) rest
    | Token.SegTokens (sub_toks, fmt_opt, seg_span) :: rest ->
      let sub_st = { tokens = Array.of_list sub_toks; idx = 0 } in
      if is_eof sub_st then
        Error (Diagnostic.error ~code:"E0002" ~span:seg_span "empty interpolation '${}' in string literal")
      else
        (match hooks.parse_expr sub_st with
         | Error e -> Error e
         | Ok ie ->
           if not (is_eof sub_st) then
             let extra = cur sub_st in
             err_at extra
               (Printf.sprintf "unexpected token %s inside string interpolation '${...}'"
                  (Token.kind_to_string extra.kind))
           else
             match fmt_opt with
             | None -> convert (Ast.InterpExpr (ie, None) :: acc) rest
             | Some (raw_spec, fmt_span) ->
               (match Parser_format.parse_format_spec raw_spec fmt_span with
                | Error e -> Error e
                | Ok spec -> convert (Ast.InterpExpr (ie, Some spec) :: acc) rest))
  in
  match convert [] segments with
  | Ok parts -> Ok { Ast.desc = Ast.ExprInterp parts; span = str_span }
  | Error e -> Error e

let parse_input_expr st tok : (Ast.expr, Diagnostic.t) result =
  let open_tok = cur st in
  if open_tok.kind <> Token.LPAREN then
    err_at open_tok
      ~hint:"call 'input' with parentheses, e.g. 'input()', 'input(int)', or 'input(\"${int}-${int}\")'"
      (Printf.sprintf "expected '(' after 'input', found %s" (Token.kind_to_string open_tok.kind))
  else begin
    bump st;
    if (cur st).kind = Token.RPAREN then begin
      let close_tok = cur st in
      bump st;
      let span = Span.merge tok.Token.span close_tok.span in
      Ok { Ast.desc = Ast.ExprInput { prompt = None; mode = Ast.InputLine }; span }
    end else
      match Parser_format.parse_input_mode_args st ~kw_name:"input" with
      | Error e -> Error e
      | Ok mode ->
        let close_tok = cur st in
        if close_tok.kind = Token.RPAREN then begin
          bump st;
          let span = Span.merge tok.Token.span close_tok.span in
          Ok { Ast.desc = Ast.ExprInput { prompt = None; mode }; span }
        end else
          err_at close_tok ~hint:"close 'input(...)' with ')'"
            (Printf.sprintf "expected ')' to close 'input(...)', found %s"
               (Token.kind_to_string close_tok.kind))
  end

let parse_prompt_expr hooks st tok : (Ast.expr, Diagnostic.t) result =
  let open_tok = cur st in
  if open_tok.kind <> Token.LPAREN then
    err_at open_tok
      ~hint:"call 'prompt' with parentheses and a prompt message, e.g. 'prompt(\"Name: \")' or 'prompt(\"Age: \", int)'"
      (Printf.sprintf "expected '(' after 'prompt', found %s" (Token.kind_to_string open_tok.kind))
  else begin
    bump st;
    if (cur st).kind = Token.RPAREN then begin
      let close_tok = cur st in
      bump st;
      Error
        (Diagnostic.error
           ~hint:"pass a prompt string as the first argument (e.g. 'prompt(\"Enter: \")'), or use 'input()' to read without a prompt"
           ~code:"E0002"
           ~span:(Span.merge tok.Token.span close_tok.span)
           "'prompt()' requires a prompt message argument")
    end else
      match hooks.parse_expr st with
      | Error e -> Error e
      | Ok prompt_expr ->
        let next_tok = cur st in
        if next_tok.kind = Token.RPAREN then begin
          bump st;
          let span = Span.merge tok.Token.span next_tok.span in
          Ok { Ast.desc = Ast.ExprInput { prompt = Some prompt_expr; mode = Ast.InputLine }; span }
        end else if next_tok.kind = Token.COMMA then begin
          bump st;
          match Parser_format.parse_input_mode_args st ~kw_name:"prompt" with
          | Error e -> Error e
          | Ok mode ->
            let close_tok = cur st in
            if close_tok.kind = Token.RPAREN then begin
              bump st;
              let span = Span.merge tok.Token.span close_tok.span in
              Ok { Ast.desc = Ast.ExprInput { prompt = Some prompt_expr; mode }; span }
            end else
              err_at close_tok ~hint:"close 'prompt(...)' with ')'"
                (Printf.sprintf "expected ')' to close 'prompt(...)', found %s"
                   (Token.kind_to_string close_tok.kind))
        end else
          err_at next_tok ~hint:"use 'prompt(msg)' or 'prompt(msg, type_or_mask)'"
            (Printf.sprintf "expected ')' or ',' after prompt message in 'prompt(...)', found %s"
               (Token.kind_to_string next_tok.kind))
  end

let parse_primary hooks st : (Ast.expr, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.INT n -> bump st; Ok { Ast.desc = Ast.ExprInt n; span = tok.span }
  | Token.FLOAT f -> bump st; Ok { Ast.desc = Ast.ExprFloat f; span = tok.span }
  | Token.DOUBLE d -> bump st; Ok { Ast.desc = Ast.ExprDouble d; span = tok.span }
  | Token.DECIMAL m -> bump st; Ok { Ast.desc = Ast.ExprDecimal m; span = tok.span }
  | Token.BOOL b -> bump st; Ok { Ast.desc = Ast.ExprBool b; span = tok.span }
  | Token.CHAR c -> bump st; Ok { Ast.desc = Ast.ExprChar c; span = tok.span }
  | Token.STRING s -> bump st; Ok { Ast.desc = Ast.ExprString s; span = tok.span }
  | Token.INTERP_STRING segments ->
    bump st;
    parse_interp_parts hooks segments tok.span
  | Token.KW_INPUT ->
    bump st;
    parse_input_expr st tok
  | Token.KW_PROMPT ->
    bump st;
    parse_prompt_expr hooks st tok
  | Token.IDENT name ->
    bump st;
    if (cur st).kind = Token.LBRACE then
      Parser_type_decl.parse_struct_init hooks st name tok.span
    else
      Ok { Ast.desc = Ast.ExprIdent name; span = tok.span }
  | Token.KW_FUN ->
    bump st;
    hooks.parse_lambda_expr st tok ~is_act:false
  | Token.KW_ACT ->
    bump st;
    hooks.parse_lambda_expr st tok ~is_act:true
  | Token.KW_NEW ->
    bump st;
    Parser_type_decl.parse_new_expr hooks st tok
  | Token.LPAREN ->
    bump st;
    if (cur st).kind = Token.RPAREN then begin
      let close_tok = cur st in
      bump st;
      Error
        (Diagnostic.error ~hint:"provide at least one element, e.g. '(10, 20)'" ~code:"E0002"
           ~span:(Span.merge tok.span close_tok.span) "empty tuple '()' is not supported")
    end else if (cur st).kind = Token.DOT_DOT then begin
      match parse_collection_item hooks st with
      | Error e -> Error e
      | Ok first_item ->
        let next_tok = cur st in
        if next_tok.kind = Token.COMMA then begin
          bump st;
          match parse_collection_rest hooks st ~close_kind:Token.RPAREN ~close_str:")" [ first_item ] with
          | Error e -> Error e
          | Ok (items, close_span) ->
            Ok { Ast.desc = Ast.ExprTuple items; span = Span.merge tok.span close_span }
        end else if next_tok.kind = Token.RPAREN then begin
          bump st;
          Ok { Ast.desc = Ast.ExprTuple [ first_item ]; span = Span.merge tok.span next_tok.span }
        end else
          err_at next_tok
            (Printf.sprintf "expected ')' or ',' in tuple literal, found %s"
               (Token.kind_to_string next_tok.kind))
    end else
      (match hooks.parse_expr st with
       | Error e -> Error e
       | Ok inner ->
         let next_tok = cur st in
         (match next_tok.kind with
          | Token.RPAREN ->
            bump st;
            Ok { inner with span = Span.merge tok.span next_tok.span }
          | Token.COMMA ->
            bump st;
            (match parse_collection_rest hooks st ~close_kind:Token.RPAREN ~close_str:")"
                     [ Ast.ItemExpr inner ] with
             | Error e -> Error e
             | Ok (items, close_span) ->
               Ok { Ast.desc = Ast.ExprTuple items; span = Span.merge tok.span close_span })
          | _ ->
            err_at next_tok
              (Printf.sprintf "expected ')' to close parenthesized expression or ',' for tuple, found %s"
                 (Token.kind_to_string next_tok.kind))))
  | Token.LBRACKET ->
    bump st;
    if (cur st).kind = Token.RBRACKET then begin
      let close_tok = cur st in
      bump st;
      Ok { Ast.desc = Ast.ExprArray []; span = Span.merge tok.span close_tok.span }
    end else
      (match parse_collection_item hooks st with
       | Error e -> Error e
       | Ok first_item ->
         let next_tok = cur st in
         if next_tok.kind = Token.COMMA then begin
           bump st;
           match parse_collection_rest hooks st ~close_kind:Token.RBRACKET ~close_str:"]" [ first_item ] with
           | Error e -> Error e
           | Ok (items, close_span) ->
             Ok { Ast.desc = Ast.ExprArray items; span = Span.merge tok.span close_span }
         end else if next_tok.kind = Token.RBRACKET then begin
           bump st;
           Ok { Ast.desc = Ast.ExprArray [ first_item ]; span = Span.merge tok.span next_tok.span }
         end else
           err_at next_tok
             (Printf.sprintf "expected ']' or ',' in array literal, found %s"
                (Token.kind_to_string next_tok.kind)))
  | Token.KW_PRINT | Token.KW_PRINTLN ->
    let fn_name = if tok.kind = Token.KW_PRINTLN then "println" else "print" in
    err_at tok ~hint:(Printf.sprintf "use '%s(...);' as a standalone statement" fn_name)
      (Printf.sprintf "'%s' is a void output statement and cannot be used inside an expression" fn_name)
  | Token.KW_ASSERT | Token.KW_ASSERT_ERROR ->
    let fn_name = if tok.kind = Token.KW_ASSERT_ERROR then "assert_error" else "assert" in
    err_at tok ~hint:(Printf.sprintf "use '%s(...);' as a standalone statement" fn_name)
      (Printf.sprintf "'%s' is a verification statement and cannot be used inside an expression" fn_name)
  | _ ->
    err_at tok (Printf.sprintf "expected expression, found %s" (Token.kind_to_string tok.kind))

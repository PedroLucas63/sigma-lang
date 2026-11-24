open Sigma_common
open Sigma_diagnostics
open Parser_state

let kind_kw = function
  | Ast.KindClass -> "class"
  | Ast.KindStruct -> "struct"
  | Ast.KindRecord -> "record"

let parse_field_spec hooks st ~stop_kind ~require_semi ~ctx_name : (Ast.field_decl, Diagnostic.t) result =
  let name_tok = cur st in
  match name_tok.kind with
  | Token.IDENT field_name ->
    bump st;
    if (cur st).kind <> Token.COLON then
      err_at (cur st)
        ~hint:(Printf.sprintf "annotate field type, e.g. '%s: int'" field_name)
        (Printf.sprintf "expected ':' after field name '%s' in %s, found %s"
           field_name ctx_name (Token.kind_to_string (cur st).kind))
    else begin
      bump st;
      match Parser_types.parse_type_expr st with
      | Error e -> Error e
      | Ok field_type ->
        let default_res =
          if (cur st).kind = Token.EQUAL then begin
            bump st;
            match hooks.parse_expr st with
            | Error e -> Error e
            | Ok e -> Ok (Some e)
          end else Ok None
        in
        (match default_res with
         | Error e -> Error e
         | Ok field_default ->
           if require_semi then begin
             match expect_semi st name_tok.span with
             | Error e -> Error e
             | Ok full_span ->
               Ok { Ast.field_name; field_span = full_span; field_type; field_default }
           end else
             let end_sp =
               match field_default with
               | Some e -> e.span
               | None -> Ast.type_expr_span field_type
             in
             if (cur st).kind = stop_kind || (cur st).kind = Token.COMMA then
               Ok { Ast.field_name; field_span = Span.merge name_tok.span end_sp; field_type; field_default }
             else
               err_at (cur st)
                 (Printf.sprintf "expected ',' or ')' after field '%s' in %s, found %s"
                    field_name ctx_name (Token.kind_to_string (cur st).kind)))
    end
  | _ ->
    err_at name_tok
      (Printf.sprintf "expected field name in %s, found %s" ctx_name (Token.kind_to_string name_tok.kind))

let parse_class_or_struct hooks st kw_tok ~kind name name_tok : (Ast.stmt, Diagnostic.t) result =
  let kw = kind_kw kind in
  let open_tok = cur st in
  if open_tok.kind <> Token.LBRACE then
    err_at open_tok ~hint:(Printf.sprintf "declare fields inside '{ ... }', e.g. '%s %s { x: int; }'" kw name)
      (Printf.sprintf "expected '{' after '%s %s', found %s" kw name (Token.kind_to_string open_tok.kind))
  else begin
    bump st;
    let rec loop acc =
      let tok = cur st in
      if tok.kind = Token.RBRACE then begin
        bump st;
        let td : Ast.type_decl =
          { td_kind = kind; td_name = name; td_name_span = name_tok.Token.span; td_fields = List.rev acc }
        in
        Ok { Ast.desc = Ast.StmtTypeDecl td; span = Span.merge kw_tok.Token.span tok.span }
      end else if is_eof st then
        err_at open_tok (Printf.sprintf "unclosed '{' in %s '%s'" kw name)
      else
        match parse_field_spec hooks st ~stop_kind:Token.RBRACE ~require_semi:true ~ctx_name:kw with
        | Error e -> Error e
        | Ok f -> loop (f :: acc)
    in
    loop []
  end

let parse_record hooks st kw_tok name name_tok : (Ast.stmt, Diagnostic.t) result =
  let open_tok = cur st in
  if open_tok.kind <> Token.LPAREN then
    err_at open_tok ~hint:(Printf.sprintf "declare record members in parentheses, e.g. 'record %s(x: int) {}'" name)
      (Printf.sprintf "expected '(' after 'record %s', found %s" name (Token.kind_to_string open_tok.kind))
  else begin
    bump st;
    let rec loop acc =
      let tok = cur st in
      if tok.kind = Token.RPAREN then begin
        bump st;
        Ok (List.rev acc, tok.span)
      end else
        match parse_field_spec hooks st ~stop_kind:Token.RPAREN ~require_semi:false ~ctx_name:"record" with
        | Error e -> Error e
        | Ok f ->
          let acc = f :: acc in
          let nt = cur st in
          if nt.kind = Token.COMMA then (bump st; loop acc)
          else if nt.kind = Token.RPAREN then (bump st; Ok (List.rev acc, nt.span))
          else
            err_at nt
              (Printf.sprintf "expected ',' or ')' in record '%s', found %s" name (Token.kind_to_string nt.kind))
    in
    match loop [] with
    | Error e -> Error e
    | Ok (fields, _rparen_sp) ->
      let next_tok = cur st in
      let finish_span_res =
        match next_tok.kind with
        | Token.SEMI ->
          bump st;
          Ok next_tok.span
        | Token.LBRACE ->
          bump st;
          let close_tok = cur st in
          if close_tok.kind = Token.RBRACE then (bump st; Ok close_tok.span)
          else err_at close_tok ~hint:"records currently only support an empty body '{}' or ';'"
                 (Printf.sprintf "expected '}' in record '%s', found %s" name (Token.kind_to_string close_tok.kind))
        | _ ->
          err_at next_tok ~hint:(Printf.sprintf "end record with '{}' or ';', e.g. 'record %s(...) {}'" name)
            (Printf.sprintf "expected '{}' or ';' after record '%s(...)', found %s"
               name (Token.kind_to_string next_tok.kind))
      in
      (match finish_span_res with
       | Error e -> Error e
       | Ok end_sp ->
         let td : Ast.type_decl =
           { td_kind = Ast.KindRecord; td_name = name; td_name_span = name_tok.Token.span; td_fields = fields }
         in
         Ok { Ast.desc = Ast.StmtTypeDecl td; span = Span.merge kw_tok.Token.span end_sp })
  end

let parse_type_decl hooks st kw_tok ~(kind : Ast.user_type_kind) : (Ast.stmt, Diagnostic.t) result =
  let kw = kind_kw kind in
  let name_tok = cur st in
  match name_tok.kind with
  | Token.IDENT name ->
    bump st;
    (match kind with
     | Ast.KindRecord -> parse_record hooks st kw_tok name name_tok
     | Ast.KindClass | Ast.KindStruct -> parse_class_or_struct hooks st kw_tok ~kind name name_tok)
  | _ ->
    err_at name_tok
      (Printf.sprintf "expected type name after '%s', found %s" kw (Token.kind_to_string name_tok.kind))

let parse_call_arg_list hooks st ~close_kind ~close_str ~ctx_label =
  bump st;
  let rec loop acc =
    let tok = cur st in
    if tok.kind = close_kind then begin
      bump st;
      Ok (List.rev acc, tok.span)
    end else begin
      let named =
        match tok.kind with
        | Token.IDENT n when (peek_next st).kind = Token.EQUAL ->
          bump st;
          bump st;
          Some (n, tok.span)
        | _ -> None
      in
      match hooks.parse_expr st with
      | Error e -> Error e
      | Ok v ->
        let arg : Ast.call_arg = { arg_name = named; arg_mode = Ast.ArgVal; arg_value = v } in
        let acc = arg :: acc in
        let nt = cur st in
        if nt.kind = Token.COMMA then (bump st; loop acc)
        else if nt.kind = close_kind then (bump st; Ok (List.rev acc, nt.span))
        else
          err_at nt
            (Printf.sprintf "expected ',' or '%s' in %s, found %s"
               close_str ctx_label (Token.kind_to_string nt.kind))
    end
  in
  loop []

let parse_struct_init hooks st type_name name_span : (Ast.expr, Diagnostic.t) result =
  match parse_call_arg_list hooks st ~close_kind:Token.RBRACE ~close_str:"}" ~ctx_label:(type_name ^ " { ... }") with
  | Error e -> Error e
  | Ok (args, end_sp) ->
    Ok ({ Ast.desc = Ast.ExprStructInit (type_name, args); span = Span.merge name_span end_sp } : Ast.expr)

let parse_new_expr hooks st kw_tok : (Ast.expr, Diagnostic.t) result =
  let name_tok = cur st in
  match name_tok.kind with
  | Token.IDENT new_name ->
    bump st;
    let next_tok = cur st in
    (match next_tok.kind with
     | Token.LBRACE ->
       (match parse_call_arg_list hooks st ~close_kind:Token.RBRACE ~close_str:"}" ~ctx_label:("new " ^ new_name ^ " { ... }") with
        | Error e -> Error e
        | Ok (new_args, end_sp) ->
          Ok ({ Ast.desc = Ast.ExprNew { new_name; new_is_ctor = false; new_args }; span = Span.merge kw_tok.Token.span end_sp } : Ast.expr))
     | Token.LPAREN ->
       (match parse_call_arg_list hooks st ~close_kind:Token.RPAREN ~close_str:")" ~ctx_label:("new " ^ new_name ^ "(...)") with
        | Error e -> Error e
        | Ok (new_args, end_sp) ->
          Ok ({ Ast.desc = Ast.ExprNew { new_name; new_is_ctor = true; new_args }; span = Span.merge kw_tok.Token.span end_sp } : Ast.expr))
     | _ ->
       err_at next_tok ~hint:(Printf.sprintf "use 'new %s { ... }' or 'new %s(...)'" new_name new_name)
         (Printf.sprintf "expected '{' or '(' after 'new %s', found %s" new_name (Token.kind_to_string next_tok.kind)))
  | _ ->
    err_at name_tok ~hint:"use 'new TypeName { ... }' or 'new TypeName(...)'"
      (Printf.sprintf "expected type name after 'new', found %s" (Token.kind_to_string name_tok.kind))

let try_parse_multi_assign hooks st open_tok : (Ast.stmt, Diagnostic.t) result option =
  let saved = st.idx in
  bump st;
  let rec parse_targets acc =
    let t = cur st in
    let item_res =
      match t.kind with
      | Token.IDENT "_" -> bump st; Ok (Ast.TargetDiscard t.span)
      | _ ->
        (match hooks.parse_expr st with
         | Error e -> Error e
         | Ok e ->
           (match expr_to_lvalue e with
            | Some lv -> Ok (Ast.TargetLVal lv)
            | None -> Error (Diagnostic.error ~code:"E0002" ~span:e.span "invalid target in multiple assignment")))
    in
    match item_res with
    | Error _ -> None
    | Ok item ->
      let acc = item :: acc in
      let nt = cur st in
      if nt.kind = Token.COMMA then begin
        bump st;
        if (cur st).kind = Token.RPAREN then (bump st; Some (List.rev acc))
        else parse_targets acc
      end else if nt.kind = Token.RPAREN then begin
        bump st;
        Some (List.rev acc)
      end else None
  in
  match parse_targets [] with
  | Some targets when List.length targets >= 2 && (cur st).kind = Token.EQUAL ->
    bump st;
    Some
      (match hooks.parse_expr st with
       | Error e -> Error e
       | Ok ma_rhs ->
         (match expect_semi st open_tok.Token.span with
          | Error e -> Error e
          | Ok full_sp ->
            Ok ({ Ast.desc = Ast.StmtMultiAssign { ma_targets = targets; ma_rhs }; span = full_sp } : Ast.stmt)))
  | _ ->
    st.idx <- saved;
    None

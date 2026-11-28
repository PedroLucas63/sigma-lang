open Sigma_common
open Sigma_diagnostics
open Parser_state

let kind_kw = function
  | Ast.KindClass -> "class"
  | Ast.KindStruct -> "struct"
  | Ast.KindRecord -> "record"

let parse_field_spec hooks st ~vis_opt ~stop_kind ~require_semi ~ctx_name : (Ast.field_decl, Diagnostic.t) result =
  let vis_opt = match vis_opt with Some v -> Some v | None -> Parser_accessors.parse_opt_visibility st in
  let name_tok = cur st in
  let start_sp = match vis_opt with Some (_, sp) -> sp | None -> name_tok.span in
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
        let acc_res =
          if require_semi && (cur st).kind = Token.LBRACE then
            match Parser_accessors.parse_accessor_blk hooks st with
            | Error e -> Error e
            | Ok (accs, end_sp) -> Ok (Some accs, Some end_sp)
          else Ok (None, None)
        in
        (match acc_res with
         | Error e -> Error e
         | Ok (field_accessors, acc_end_sp) ->
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
              let field_vis = Option.map fst vis_opt in
              let field_is_positional = not require_semi in
              if require_semi then begin
                match field_accessors, field_default, acc_end_sp with
                | Some _, None, Some end_sp ->
                  let full_span =
                    if (cur st).kind = Token.SEMI then
                      let semi_tok = cur st in
                      bump st;
                      Span.merge start_sp semi_tok.span
                    else Span.merge start_sp end_sp
                  in
                  Ok { Ast.field_vis; field_is_positional; field_name; field_span = full_span;
                       field_type; field_accessors; field_default }
                | _ ->
                  (match expect_semi st start_sp with
                   | Error e -> Error e
                   | Ok full_span ->
                     Ok { Ast.field_vis; field_is_positional; field_name; field_span = full_span;
                          field_type; field_accessors; field_default })
              end else
                let end_sp =
                  match field_default with
                  | Some e -> e.span
                  | None -> Ast.type_expr_span field_type
                in
                if (cur st).kind = stop_kind || (cur st).kind = Token.COMMA then
                  Ok { Ast.field_vis; field_is_positional; field_name;
                       field_span = Span.merge start_sp end_sp;
                       field_type; field_accessors; field_default }
                else
                  err_at (cur st)
                    (Printf.sprintf "expected ',' or ')' after field '%s' in %s, found %s"
                       field_name ctx_name (Token.kind_to_string (cur st).kind))))
    end
  | _ ->
    err_at name_tok
      (Printf.sprintf "expected field name in %s, found %s" ctx_name (Token.kind_to_string name_tok.kind))

let parse_member hooks st ~ctx_name
    : ([`Field of Ast.field_decl | `Ctor of Ast.ctor_decl | `Method of Ast.method_decl], Diagnostic.t) result =
  let vis_opt = Parser_accessors.parse_opt_visibility st in
  let md_vis = Option.map fst vis_opt in
  let is_static, static_sp =
    if (cur st).kind = Token.KW_STATIC then
      let t = cur st in (bump st; (true, Some t.span))
    else (false, None)
  in
  let kw_tok = cur st in
  match kw_tok.kind with
  | Token.KW_FUN | Token.KW_ACT ->
    let is_act = kw_tok.kind = Token.KW_ACT in
    bump st;
    (match Parser_decl.parse_fun_decl hooks st kw_tok ~is_act with
     | Error e -> Error e
     | Ok { Ast.desc = Ast.StmtFunDecl md_fun; span } ->
       let sp0 =
         match vis_opt, static_sp with
         | Some (_, sp), _ -> sp | None, Some sp -> sp | None, None -> span
       in
       Ok (`Method { Ast.md_vis; md_is_static = is_static; md_fun; md_span = Span.merge sp0 span })
     | Ok _ -> err_at kw_tok "internal error: expected method declaration")
  | Token.IDENT ctor_name when (not is_static) && (peek_next st).kind = Token.LPAREN ->
    bump st;
    (match Parser_accessors.parse_ctor_decl hooks st ~vis_opt kw_tok ctor_name with
     | Error e -> Error e
     | Ok c -> Ok (`Ctor c))
  | _ ->
    if is_static then
      err_at kw_tok ~hint:"'static' can only precede 'fun' or 'act' methods"
        (Printf.sprintf "expected 'fun' or 'act' after 'static', found %s" (Token.kind_to_string kw_tok.kind))
    else
      (match parse_field_spec hooks st ~vis_opt ~stop_kind:Token.RBRACE ~require_semi:true ~ctx_name with
       | Error e -> Error e
       | Ok f -> Ok (`Field f))

let parse_class_or_struct hooks st ~td_vis ~start_span ~kind ~td_type_params name name_tok : (Ast.stmt, Diagnostic.t) result =
  let kw = kind_kw kind in
  let ctor_params_res =
    if (cur st).kind = Token.LPAREN then
      match Parser_accessors.parse_ctor_params hooks st ~ctx_name:(kw ^ " " ^ name) with
      | Error e -> Error e
      | Ok (cps, _) -> Ok (Some cps)
    else Ok None
  in
  match ctor_params_res with
  | Error e -> Error e
  | Ok td_ctor_params ->
    (match Parser_generics.parse_opt_where_clauses ~parse_type_expr:Parser_types.parse_type_expr st with
     | Error e -> Error e
     | Ok td_where ->
       let open_tok = cur st in
       if open_tok.kind <> Token.LBRACE then
         err_at open_tok ~hint:(Printf.sprintf "declare members inside '{ ... }', e.g. '%s %s { public x: int; }'" kw name)
           (Printf.sprintf "expected '{' after '%s %s', found %s" kw name (Token.kind_to_string open_tok.kind))
       else begin
         bump st;
         let rec loop fields ctors methods =
           let tok = cur st in
           if tok.kind = Token.RBRACE then begin
             bump st;
             let td : Ast.type_decl =
               { td_vis; td_kind = kind; td_name = name; td_name_span = name_tok.Token.span;
                 td_type_params; td_where; td_ctor_params;
                 td_fields = List.rev fields; td_ctors = List.rev ctors; td_methods = List.rev methods }
             in
             Ok { Ast.desc = Ast.StmtTypeDecl td; span = Span.merge start_span tok.span }
           end else if is_eof st then err_at open_tok (Printf.sprintf "unclosed '{' in %s '%s'" kw name)
           else
             match parse_member hooks st ~ctx_name:kw with
             | Error e -> Error e
             | Ok (`Field f) -> loop (f :: fields) ctors methods
             | Ok (`Ctor c) -> loop fields (c :: ctors) methods
             | Ok (`Method m) -> loop fields ctors (m :: methods)
         in
         loop [] [] []
       end)

let parse_record hooks st ~td_vis ~start_span ~td_type_params name name_tok : (Ast.stmt, Diagnostic.t) result =
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
        match parse_field_spec hooks st ~vis_opt:None ~stop_kind:Token.RPAREN ~require_semi:false ~ctx_name:"record" with
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
    | Ok (param_fields, _rparen_sp) ->
      (match Parser_generics.parse_opt_where_clauses ~parse_type_expr:Parser_types.parse_type_expr st with
       | Error e -> Error e
       | Ok td_where ->
         let next_tok = cur st in
         let finish_res =
           match next_tok.kind with
           | Token.SEMI -> bump st; Ok ([], [], next_tok.span)
           | Token.LBRACE ->
             bump st;
             let rec body_loop fields methods =
               let tok = cur st in
               if tok.kind = Token.RBRACE then (bump st; Ok (List.rev fields, List.rev methods, tok.span))
               else if is_eof st then err_at next_tok (Printf.sprintf "unclosed '{' in record '%s'" name)
               else
                 match parse_member hooks st ~ctx_name:"record" with
                 | Error e -> Error e
                 | Ok (`Field f) -> body_loop (f :: fields) methods
                 | Ok (`Ctor c) -> Error (Diagnostic.error ~code:"E0002" ~span:c.cd_name_span "records do not support custom constructors in body")
                 | Ok (`Method m) -> body_loop fields (m :: methods)
             in
             body_loop [] []
           | _ ->
             err_at next_tok ~hint:(Printf.sprintf "end record with '{}' or ';', e.g. 'record %s(...) {}'" name)
               (Printf.sprintf "expected '{}' or ';' after record '%s(...)', found %s" name (Token.kind_to_string next_tok.kind))
         in
         (match finish_res with
          | Error e -> Error e
          | Ok (body_fields, body_methods, end_sp) ->
            let td : Ast.type_decl =
              { td_vis; td_kind = Ast.KindRecord; td_name = name; td_name_span = name_tok.Token.span;
                td_type_params; td_where; td_ctor_params = None; td_fields = param_fields @ body_fields;
                td_ctors = []; td_methods = body_methods }
            in
            Ok { Ast.desc = Ast.StmtTypeDecl td; span = Span.merge start_span end_sp }))
  end

let parse_type_decl hooks st ?(td_vis = None) ?start_span kw_tok ~(kind : Ast.user_type_kind)
    : (Ast.stmt, Diagnostic.t) result =
  let kw = kind_kw kind in
  let start_span = match start_span with Some sp -> sp | None -> kw_tok.Token.span in
  let name_tok = cur st in
  match name_tok.kind with
  | Token.IDENT name ->
    bump st;
    (match Parser_generics.parse_opt_type_params st with
     | Error e -> Error e
     | Ok td_type_params ->
       (match kind with
        | Ast.KindRecord -> parse_record hooks st ~td_vis ~start_span ~td_type_params name name_tok
        | Ast.KindClass | Ast.KindStruct ->
          parse_class_or_struct hooks st ~td_vis ~start_span ~kind ~td_type_params name name_tok))
  | _ ->
    err_at name_tok
      (Printf.sprintf "expected type name after '%s', found %s" kw (Token.kind_to_string name_tok.kind))

let parse_struct_init hooks st type_name name_span : (Ast.expr, Diagnostic.t) result =
  match Parser_accessors.parse_call_arg_list hooks st ~close_kind:Token.RBRACE ~close_str:"}" ~ctx_label:(type_name ^ " { ... }") with
  | Error e -> Error e
  | Ok (args, end_sp) ->
    Ok ({ Ast.desc = Ast.ExprStructInit (type_name, args); span = Span.merge name_span end_sp } : Ast.expr)

let parse_new_expr hooks st kw_tok : (Ast.expr, Diagnostic.t) result =
  let name_tok = cur st in
  match name_tok.kind with
  | Token.IDENT new_name ->
    bump st;
    let targs_res =
      if (cur st).kind = Token.LT then
        match Parser_generics.parse_type_args ~parse_type_expr:Parser_types.parse_type_expr st with
        | Error e -> Error e
        | Ok (targs, _) -> Ok targs
      else Ok []
    in
    (match targs_res with
     | Error e -> Error e
     | Ok new_type_args ->
       let next_tok = cur st in
       (match next_tok.kind with
        | Token.LBRACE ->
          (match Parser_accessors.parse_call_arg_list hooks st ~close_kind:Token.RBRACE ~close_str:"}" ~ctx_label:("new " ^ new_name ^ " { ... }") with
           | Error e -> Error e
           | Ok (new_args, end_sp) ->
             Ok ({ Ast.desc = Ast.ExprNew { new_name; new_type_args; new_is_ctor = false; new_args; new_inits = None };
                   span = Span.merge kw_tok.Token.span end_sp } : Ast.expr))
        | Token.LPAREN ->
          (match Parser_accessors.parse_call_arg_list hooks st ~close_kind:Token.RPAREN ~close_str:")" ~ctx_label:("new " ^ new_name ^ "(...)") with
           | Error e -> Error e
           | Ok (new_args, rparen_sp) ->
             if (cur st).kind = Token.LBRACE then
               (match Parser_accessors.parse_call_arg_list hooks st ~close_kind:Token.RBRACE ~close_str:"}" ~ctx_label:("new " ^ new_name ^ "(...) { ... }") with
                | Error e -> Error e
                | Ok (inits, end_sp) ->
                  Ok ({ Ast.desc = Ast.ExprNew { new_name; new_type_args; new_is_ctor = true; new_args; new_inits = Some inits };
                        span = Span.merge kw_tok.Token.span end_sp } : Ast.expr))
             else
               Ok ({ Ast.desc = Ast.ExprNew { new_name; new_type_args; new_is_ctor = true; new_args; new_inits = None };
                     span = Span.merge kw_tok.Token.span rparen_sp } : Ast.expr))
        | _ ->
          err_at next_tok ~hint:(Printf.sprintf "use 'new %s { ... }' or 'new %s(...)'" new_name new_name)
            (Printf.sprintf "expected '{' or '(' after 'new %s', found %s" new_name (Token.kind_to_string next_tok.kind))))
  | _ ->
    err_at name_tok ~hint:"use 'new TypeName { ... }' or 'new TypeName(...)'"
      (Printf.sprintf "expected type name after 'new', found %s" (Token.kind_to_string name_tok.kind))

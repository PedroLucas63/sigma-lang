open Sigma_common
open Sigma_diagnostics
open Parser_state

let parse_extern_item st : (Ast.extern_item, Diagnostic.t) result =
  let kw_tok = cur st in
  let is_act_res =
    match kw_tok.kind with
    | Token.KW_FUN -> bump st; Ok false
    | Token.KW_ACT -> bump st; Ok true
    | _ ->
      err_at kw_tok ~hint:"use 'fun name(...): type;' or 'act name(...);' inside an extern block"
        (Printf.sprintf "expected 'fun' or 'act' in extern declaration, found %s"
           (Token.kind_to_string kw_tok.kind))
  in
  match is_act_res with
  | Error e -> Error e
  | Ok ext_is_act ->
    let name_tok = cur st in
    (match name_tok.kind with
     | Token.IDENT ext_name ->
       bump st;
       let lp = cur st in
       if lp.kind <> Token.LPAREN then
         err_at lp
           (Printf.sprintf "expected '(' after extern %s '%s', found %s"
              (if ext_is_act then "act" else "fun")
              ext_name (Token.kind_to_string lp.kind))
       else begin
         bump st;
         match Parser_decl.parse_fun_params st with
         | Error e -> Error e
         | Ok ext_params ->
           let ret_res =
             if ext_is_act then Ok None
             else if (cur st).kind = Token.COLON then begin
               bump st;
               match Parser_types.parse_type_expr st with
               | Ok te -> Ok (Some te)
               | Error e -> Error e
             end else
               err_at (cur st)
                 (Printf.sprintf "expected ':' and a return type for extern fun '%s'" ext_name)
           in
           (match ret_res with
            | Error e -> Error e
            | Ok ext_ret ->
              let sym_res =
                if (cur st).kind = Token.KW_AS then begin
                  bump st;
                  let sym_tok = cur st in
                  match sym_tok.kind with
                  | Token.STRING s -> bump st; Ok (Some s)
                  | _ ->
                    err_at sym_tok ~hint:"use 'as \"c_symbol_name\"'"
                      (Printf.sprintf "expected string literal after 'as', found %s"
                         (Token.kind_to_string sym_tok.kind))
                end else Ok None
              in
              (match sym_res with
               | Error e -> Error e
               | Ok ext_symbol ->
                 (match expect_semi st kw_tok.Token.span with
                  | Error e -> Error e
                  | Ok ext_span ->
                    Ok
                      {
                        Ast.ext_is_act;
                        ext_name;
                        ext_name_span = name_tok.span;
                        ext_params;
                        ext_ret;
                        ext_symbol;
                        ext_span;
                      })))
       end
     | _ ->
       err_at name_tok
         (Printf.sprintf "expected function or action name, found %s"
            (Token.kind_to_string name_tok.kind)))

let parse_extern_stmt st kw_tok : (Ast.stmt, Diagnostic.t) result =
  let lang_tok = cur st in
  match lang_tok.kind with
  | Token.STRING ext_lang ->
    bump st;
    let from_res =
      if (cur st).kind = Token.KW_FROM then begin
        bump st;
        let path_tok = cur st in
        match path_tok.kind with
        | Token.STRING p -> bump st; Ok (Some p, Some path_tok.span)
        | _ ->
          err_at path_tok ~hint:"use 'from \"path/to/lib.c\"' or 'from \"m\"'"
            (Printf.sprintf "expected string literal after 'from', found %s"
               (Token.kind_to_string path_tok.kind))
      end else Ok (None, None)
    in
    (match from_res with
     | Error e -> Error e
     | Ok (ext_from, ext_from_span) ->
       if (cur st).kind = Token.LBRACE then begin
         bump st;
         let rec loop acc =
           let t = cur st in
           if t.kind = Token.RBRACE then begin
             bump st;
             Ok (List.rev acc, t.span)
           end else if t.kind = Token.EOF then
             err_at t ~hint:"close the extern block with '}'" "unterminated 'extern' block"
           else
             match parse_extern_item st with
             | Error e -> Error e
             | Ok item -> loop (item :: acc)
         in
         match loop [] with
         | Error e -> Error e
         | Ok (ext_items, close_span) ->
           Ok
             {
               Ast.desc =
                 Ast.StmtExtern
                   { ext_lang; ext_lang_span = lang_tok.span; ext_from; ext_from_span; ext_items };
               span = Span.merge kw_tok.Token.span close_span;
             }
       end else
         match parse_extern_item st with
         | Error e -> Error e
         | Ok item ->
           Ok
             {
               Ast.desc =
                 Ast.StmtExtern
                   {
                     ext_lang;
                     ext_lang_span = lang_tok.span;
                     ext_from;
                     ext_from_span;
                     ext_items = [ item ];
                   };
               span = Span.merge kw_tok.Token.span item.ext_span;
             })
  | _ ->
    err_at lang_tok ~hint:"specify the foreign ABI language, e.g. 'extern \"C\" ...'"
      (Printf.sprintf "expected language string literal after 'extern', found %s"
         (Token.kind_to_string lang_tok.kind))

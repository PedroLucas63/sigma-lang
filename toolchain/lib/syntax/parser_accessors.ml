open Sigma_common
open Sigma_diagnostics
open Parser_state

let parse_opt_visibility st : (Ast.visibility * Span.t) option =
  let tok = cur st in
  match tok.kind with
  | Token.KW_PUBLIC -> bump st; Some (Ast.VisPublic, tok.span)
  | Token.KW_PROTECTED -> bump st; Some (Ast.VisProtected, tok.span)
  | Token.KW_PRIVATE -> bump st; Some (Ast.VisPrivate, tok.span)
  | Token.KW_INTERNAL -> bump st; Some (Ast.VisInternal, tok.span)
  | _ -> None

let parse_accessor_blk st : (Ast.accessor_spec list * Span.t, Diagnostic.t) result =
  let open_tok = cur st in
  bump st;
  let rec loop acc =
    let tok = cur st in
    if tok.kind = Token.RBRACE then begin
      bump st;
      if acc = [] then
        err_at open_tok ~hint:"declare at least one accessor, e.g. '{ get; set; }' or '{ get; }'"
          "property accessor block cannot be empty"
      else Ok (List.rev acc, tok.span)
    end else if is_eof st then
      err_at open_tok "unclosed '{' in property accessor block"
    else
      let vis_opt = parse_opt_visibility st in
      let acc_tok = cur st in
      let kind_res =
        match acc_tok.kind with
        | Token.IDENT "get" -> Ok Ast.AccGet
        | Token.IDENT "set" -> Ok Ast.AccSet
        | Token.IDENT "init" -> Ok Ast.AccInit
        | _ ->
          err_at acc_tok ~hint:"expected 'get', 'set', or 'init' inside accessor block"
            (Printf.sprintf "unexpected token %s in accessor block" (Token.kind_to_string acc_tok.kind))
      in
      match kind_res with
      | Error e -> Error e
      | Ok acc_kind ->
        bump st;
        let sp0 = match vis_opt with Some (_, sp) -> sp | None -> acc_tok.span in
        (match expect_semi st sp0 with
         | Error e -> Error e
         | Ok acc_span ->
           let spec : Ast.accessor_spec =
             { acc_kind; acc_vis = Option.map fst vis_opt; acc_span }
           in
           loop (spec :: acc))
  in
  loop []

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

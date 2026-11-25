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
          bump st; bump st; Some (n, tok.span)
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

let parse_ctor_params hooks st ~ctx_name : (Ast.ctor_param list * Span.t, Diagnostic.t) result =
  bump st;
  let rec loop acc =
    let tok = cur st in
    if tok.kind = Token.RPAREN then (bump st; Ok (List.rev acc, tok.span))
    else
      match tok.kind with
      | Token.IDENT cp_name ->
        bump st;
        if (cur st).kind <> Token.COLON then
          err_at (cur st) (Printf.sprintf "expected ':' after parameter '%s' in %s" cp_name ctx_name)
        else begin
          bump st;
          match Parser_types.parse_type_expr st with
          | Error e -> Error e
          | Ok cp_type ->
            let def_res =
              if (cur st).kind = Token.EQUAL then begin
                bump st;
                match hooks.parse_expr st with Error e -> Error e | Ok e -> Ok (Some e)
              end else Ok None
            in
            (match def_res with
             | Error e -> Error e
             | Ok cp_default ->
               let end_sp = match cp_default with Some e -> e.span | None -> Ast.type_expr_span cp_type in
               let cp : Ast.ctor_param = { cp_name; cp_span = Span.merge tok.span end_sp; cp_type; cp_default } in
               let acc = cp :: acc in
               let nt = cur st in
               if nt.kind = Token.COMMA then (bump st; loop acc)
               else if nt.kind = Token.RPAREN then (bump st; Ok (List.rev acc, nt.span))
               else err_at nt (Printf.sprintf "expected ',' or ')' in %s parameters" ctx_name))
        end
      | _ ->
        err_at tok (Printf.sprintf "expected parameter name in %s, found %s" ctx_name (Token.kind_to_string tok.kind))
  in
  loop []

let parse_ctor_decl hooks st ~vis_opt name_tok cd_name : (Ast.ctor_decl, Diagnostic.t) result =
  let sp0 = match vis_opt with Some (_, sp) -> sp | None -> name_tok.Token.span in
  match parse_ctor_params hooks st ~ctx_name:("constructor " ^ cd_name) with
  | Error e -> Error e
  | Ok (cd_params, _) ->
    let chain_res =
      if (cur st).kind = Token.COLON then begin
        bump st;
        let this_tok = cur st in
        if this_tok.kind <> Token.KW_THIS then
          err_at this_tok ~hint:"delegate to another constructor using ': this(...)'"
            (Printf.sprintf "expected 'this' after ':' in constructor '%s'" cd_name)
        else begin
          bump st;
          if (cur st).kind <> Token.LPAREN then
            err_at (cur st) (Printf.sprintf "expected '(' after ': this' in constructor '%s'" cd_name)
          else
            match parse_call_arg_list hooks st ~close_kind:Token.RPAREN ~close_str:")" ~ctx_label:"this(...)" with
            | Error e -> Error e
            | Ok (cargs, _) -> Ok (Some cargs)
        end
      end else Ok None
    in
    (match chain_res with
     | Error e -> Error e
     | Ok cd_chain ->
       let ob = cur st in
       if ob.kind <> Token.LBRACE then
         err_at ob (Printf.sprintf "expected '{' to start body of constructor '%s'" cd_name)
       else begin
         bump st;
         match hooks.parse_block_stmt st ob with
         | Error e -> Error e
         | Ok { Ast.desc = Ast.StmtBlock cd_body; span } ->
           Ok { Ast.cd_vis = Option.map fst vis_opt; cd_name; cd_name_span = name_tok.Token.span;
                cd_params; cd_chain; cd_body; cd_span = Span.merge sp0 span }
         | Ok _ -> err_at ob "internal error: constructor body must be a block"
       end)

open Sigma_common
open Sigma_diagnostics
open Parser_state

let consume_gt st : (Span.t, Diagnostic.t) result =
  if st.pending_gt > 0 then begin
    st.pending_gt <- st.pending_gt - 1;
    let tok = cur st in
    let sp = tok.span in
    if st.pending_gt = 0 then bump st;
    Ok sp
  end else
    let tok = cur st in
    match tok.kind with
    | Token.GT -> bump st; Ok tok.span
    | Token.RSHIFT ->
      st.pending_gt <- 1;
      Ok tok.span
    | _ ->
      err_at tok
        (Printf.sprintf "expected '>' to close type arguments, found %s"
           (Token.kind_to_string tok.kind))

let at_gt st : bool =
  st.pending_gt > 0
  || match (cur st).kind with
     | Token.GT | Token.RSHIFT -> true
     | _ -> false

let parse_opt_type_params st : (Ast.type_param list, Diagnostic.t) result =
  if (cur st).kind <> Token.LT then Ok []
  else begin
    let open_tok = cur st in
    bump st;
    if (cur st).kind = Token.GT then
      err_at open_tok "type parameter list '<>' cannot be empty"
    else
      let rec loop acc =
        let t = cur st in
        match t.kind with
        | Token.IDENT name ->
          bump st;
          let tp_variadic, end_sp =
            if (cur st).kind = Token.ELLIPSIS then begin
              let e_tok = cur st in
              bump st;
              (true, Span.merge t.span e_tok.span)
            end else
              (false, t.span)
          in
          let tp = { Ast.tp_name = name; tp_span = end_sp; tp_variadic } in
          let acc = tp :: acc in
          let nt = cur st in
          if nt.kind = Token.COMMA then (bump st; loop acc)
          else if nt.kind = Token.GT then (bump st; Ok (List.rev acc))
          else
            err_at nt
              (Printf.sprintf "expected ',' or '>' in type parameter list, found %s"
                 (Token.kind_to_string nt.kind))
        | _ ->
          err_at t
            (Printf.sprintf "expected type parameter name, found %s"
               (Token.kind_to_string t.kind))
      in
      loop []
  end

let parse_type_args ~parse_type_expr st : (Ast.type_expr list * Span.t, Diagnostic.t) result =
  let open_tok = cur st in
  bump st;
  if at_gt st then
    err_at open_tok "type argument list '<>' cannot be empty"
  else
    let rec loop acc =
      let t = cur st in
      let item_res =
        if t.kind = Token.QUESTION then begin
          bump st;
          Ok (Ast.TypeWildcard t.span)
        end else
          parse_type_expr st
      in
      match item_res with
      | Error e -> Error e
      | Ok te ->
        let acc = te :: acc in
        if (cur st).kind = Token.COMMA then begin
          bump st;
          loop acc
        end else
          match consume_gt st with
          | Error e -> Error e
          | Ok close_sp -> Ok (List.rev acc, close_sp)
    in
    loop []

let try_parse_expr_type_args ~parse_type_expr st : (Ast.type_expr list * Span.t, Diagnostic.t) result option =
  if (cur st).kind <> Token.LT then None
  else begin
    let saved_idx = st.idx in
    let saved_gt = st.pending_gt in
    match parse_type_args ~parse_type_expr st with
    | Ok (args, end_sp) when (cur st).kind = Token.LPAREN ->
      Some (Ok (args, end_sp))
    | _ ->
      st.idx <- saved_idx;
      st.pending_gt <- saved_gt;
      None
  end

let parse_constraint ~parse_type_expr st : (Ast.type_constraint, Diagnostic.t) result =
  let t = cur st in
  match t.kind with
  | Token.IDENT "numeric" -> bump st; Ok Ast.ConstraintNumeric
  | Token.KW_CLASS -> bump st; Ok Ast.ConstraintClass
  | Token.KW_STRUCT -> bump st; Ok Ast.ConstraintStruct
  | Token.KW_RECORD -> bump st; Ok Ast.ConstraintRecord
  | Token.LPAREN ->
    bump st;
    let rec parse_union acc =
      match parse_type_expr st with
      | Error e -> Error e
      | Ok te ->
        let acc = te :: acc in
        let nt = cur st in
        if nt.kind = Token.PIPE then (bump st; parse_union acc)
        else if nt.kind = Token.RPAREN then (bump st; Ok (Ast.ConstraintTypes (List.rev acc)))
        else
          err_at nt
            (Printf.sprintf "expected '|' or ')' in type constraint, found %s"
               (Token.kind_to_string nt.kind))
    in
    parse_union []
  | _ ->
    match parse_type_expr st with
    | Error e -> Error e
    | Ok te -> Ok (Ast.ConstraintTypes [ te ])

let parse_opt_where_clauses ~parse_type_expr st : (Ast.where_clause list, Diagnostic.t) result =
  match (cur st).kind with
  | Token.IDENT "where" ->
    let where_tok = cur st in
    bump st;
    let rec loop acc =
      let p_tok = cur st in
      match p_tok.kind with
      | Token.IDENT pname ->
        bump st;
        (match (cur st).kind with
         | Token.IDENT "is" ->
           bump st;
           (match parse_constraint ~parse_type_expr st with
            | Error e -> Error e
            | Ok c ->
              let wc = { Ast.wc_param = pname; wc_param_span = p_tok.span; wc_constraint = c; wc_span = Span.merge where_tok.span p_tok.span } in
              let acc = wc :: acc in
              if (cur st).kind = Token.COMMA then (bump st; loop acc)
              else Ok (List.rev acc))
         | _ ->
           err_at (cur st)
             (Printf.sprintf "expected 'is' after type parameter '%s' in where clause" pname))
      | _ ->
        err_at p_tok
          (Printf.sprintf "expected type parameter name after 'where', found %s"
             (Token.kind_to_string p_tok.kind))
    in
    loop []
  | _ -> Ok []

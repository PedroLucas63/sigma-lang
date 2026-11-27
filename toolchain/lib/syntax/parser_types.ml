open Sigma_common
open Sigma_diagnostics
open Parser_state

let rec parse_type_expr st : (Ast.type_expr, Diagnostic.t) result =
  match parse_base_type st with
  | Error e -> Error e
  | Ok base0 ->
    (match parse_fn_suffixes st base0 with
     | Error e -> Error e
     | Ok base -> parse_type_dims st base)

and parse_fn_suffixes st (base : Ast.type_expr) : (Ast.type_expr, Diagnostic.t) result =
  match base with
  | (Ast.TypeNamed _ | Ast.TypeGeneric _ | Ast.TypeFun _) when (cur st).kind = Token.LPAREN ->
    bump st;
    let rec params acc =
      let t = cur st in
      if t.kind = Token.RPAREN then begin
        bump st;
        Ok (List.rev acc, t.span)
      end else
        let pmode =
          let tok = cur st in
          if tok.kind = Token.KW_REF then (bump st; Ast.ParamRef)
          else if tok.kind = Token.KW_OUT then (bump st; Ast.ParamOut)
          else Ast.ParamVal
        in
        match parse_type_expr st with
        | Error e -> Error e
        | Ok te ->
          let acc = (pmode, te) :: acc in
          let nt = cur st in
          if nt.kind = Token.COMMA then begin
            bump st;
            params acc
          end else if nt.kind = Token.RPAREN then begin
            bump st;
            Ok (List.rev acc, nt.span)
          end else
            err_at nt
              (Printf.sprintf "expected ')' or ',' in function type annotation, found %s"
                 (Token.kind_to_string nt.kind))
    in
    (match params [] with
     | Error e -> Error e
     | Ok (ps, close_span) ->
       parse_fn_suffixes st
         (Ast.TypeFun (base, ps, Span.merge (Ast.type_expr_span base) close_span)))
  | _ -> Ok base

and parse_type_dims st (base : Ast.type_expr) : (Ast.type_expr, Diagnostic.t) result =
  let base_span = Ast.type_expr_span base in
  let expect_rbracket tok sign_n =
    let close_tok = cur st in
    if close_tok.kind = Token.RBRACKET then begin
      bump st;
      Ok (Some sign_n, Span.merge tok.Token.span close_tok.Token.span)
    end else
      err_at close_tok
        (Printf.sprintf "expected ']' after array size, found %s"
           (Token.kind_to_string close_tok.kind))
  in
  let rec parse_dims acc =
    let tok = cur st in
    match tok.kind with
    | Token.LBRACKET ->
      bump st;
      let next_tok = cur st in
      (match next_tok.kind with
       | Token.RBRACKET ->
         bump st;
         parse_dims ((None, Span.merge tok.span next_tok.span) :: acc)
       | Token.INT n ->
         bump st;
         (match expect_rbracket tok (Int64.to_int n) with
          | Error e -> Error e
          | Ok dim -> parse_dims (dim :: acc))
       | Token.MINUS ->
         bump st;
         (match (cur st).kind with
          | Token.INT n ->
            bump st;
            (match expect_rbracket tok (- (Int64.to_int n)) with
             | Error e -> Error e
             | Ok dim -> parse_dims (dim :: acc))
          | _ -> err_at (cur st) "expected integer literal after '-' in array size")
       | _ ->
         err_at next_tok ~hint:"use an integer size like 'int[3]' or omit size with 'int[]'"
           (Printf.sprintf "expected array length integer or ']', found %s"
              (Token.kind_to_string next_tok.kind)))
    | _ -> Ok (List.rev acc)
  in
  match parse_dims [] with
  | Error e -> Error e
  | Ok dims ->
    Ok
      (List.fold_right
         (fun (sz_opt, br_span) acc ->
           Ast.TypeArray (acc, sz_opt, Span.merge base_span br_span))
         dims base)

and parse_base_type st : (Ast.type_expr, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.IDENT ty_name ->
    bump st;
    if (cur st).kind = Token.LT then
      (match Parser_generics.parse_type_args ~parse_type_expr st with
       | Error e -> Error e
       | Ok (args, close_sp) ->
         Ok (Ast.TypeGeneric (ty_name, args, Span.merge tok.span close_sp)))
    else
      Ok (Ast.TypeNamed (ty_name, tok.span))
  | Token.QUESTION ->
    bump st;
    Ok (Ast.TypeWildcard tok.span)
  | Token.LPAREN ->
    bump st;
    let rec parse_tuple_types acc =
      if (cur st).kind = Token.RPAREN then begin
        let close_tok = cur st in
        bump st;
        Ok (List.rev acc, close_tok.span)
      end else
        match parse_type_expr st with
        | Error e -> Error e
        | Ok te ->
          let acc = te :: acc in
          let next_tok = cur st in
          if next_tok.kind = Token.COMMA then begin
            bump st;
            parse_tuple_types acc
          end else if next_tok.kind = Token.RPAREN then begin
            bump st;
            Ok (List.rev acc, next_tok.span)
          end else
            err_at next_tok
              (Printf.sprintf "expected ')' or ',' in tuple type annotation, found %s"
                 (Token.kind_to_string next_tok.kind))
    in
    (match parse_tuple_types [] with
     | Error e -> Error e
     | Ok ([], close_span) ->
       Error
         (Diagnostic.error
            ~code:"E0002"
            ~span:(Span.merge tok.span close_span)
            "empty tuple type '()' is not supported")
     | Ok (tys, close_span) ->
       Ok (Ast.TypeTuple (tys, Span.merge tok.span close_span)))
  | _ ->
    err_at tok
      (Printf.sprintf "expected type name after ':', found %s"
         (Token.kind_to_string tok.kind))

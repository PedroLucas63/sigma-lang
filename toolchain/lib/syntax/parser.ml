open Sigma_common
open Sigma_diagnostics

type state = {
  tokens : Token.t array;
  mutable idx : int;
}

let cur st : Token.t =
  if st.idx < Array.length st.tokens then st.tokens.(st.idx)
  else st.tokens.(Array.length st.tokens - 1)

let bump st =
  if st.idx < Array.length st.tokens - 1 then st.idx <- st.idx + 1

let is_eof st =
  match (cur st).kind with
  | Token.EOF -> true
  | _ -> false

let sync_to_semi st =
  while not (is_eof st) && (cur st).kind <> Token.SEMI do
    bump st
  done;
  if (cur st).kind = Token.SEMI then bump st

let rec parse_expr st : (Ast.expr, Diagnostic.t) result =
  parse_logical_or st

and parse_logical_or st : (Ast.expr, Diagnostic.t) result =
  match parse_logical_xor st with
  | Error e -> Error e
  | Ok lhs ->
    let rec loop (acc : Ast.expr) : (Ast.expr, Diagnostic.t) result =
      let tok = cur st in
      match tok.kind with
      | Token.KW_OR ->
        bump st;
        (match parse_logical_xor st with
         | Error e -> Error e
         | Ok rhs ->
           let span = Span.merge acc.span rhs.span in
           loop { Ast.desc = Ast.ExprBinary (Ast.BinOr, acc, rhs); span })
      | _ -> Ok acc
    in
    loop lhs

and parse_logical_xor st : (Ast.expr, Diagnostic.t) result =
  match parse_logical_and st with
  | Error e -> Error e
  | Ok lhs ->
    let rec loop (acc : Ast.expr) : (Ast.expr, Diagnostic.t) result =
      let tok = cur st in
      match tok.kind with
      | Token.KW_XOR ->
        bump st;
        (match parse_logical_and st with
         | Error e -> Error e
         | Ok rhs ->
           let span = Span.merge acc.span rhs.span in
           loop { Ast.desc = Ast.ExprBinary (Ast.BinXor, acc, rhs); span })
      | _ -> Ok acc
    in
    loop lhs

and parse_logical_and st : (Ast.expr, Diagnostic.t) result =
  match parse_logical_not st with
  | Error e -> Error e
  | Ok lhs ->
    let rec loop (acc : Ast.expr) : (Ast.expr, Diagnostic.t) result =
      let tok = cur st in
      match tok.kind with
      | Token.KW_AND ->
        bump st;
        (match parse_logical_not st with
         | Error e -> Error e
         | Ok rhs ->
           let span = Span.merge acc.span rhs.span in
           loop { Ast.desc = Ast.ExprBinary (Ast.BinAnd, acc, rhs); span })
      | _ -> Ok acc
    in
    loop lhs

and parse_logical_not st : (Ast.expr, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.KW_NOT ->
    bump st;
    (match parse_logical_not st with
     | Error e -> Error e
     | Ok sub ->
       let span = Span.merge tok.span sub.span in
       Ok { Ast.desc = Ast.ExprUnary (Ast.UnNot, sub); span })
  | _ -> parse_shift st

and parse_shift st : (Ast.expr, Diagnostic.t) result =
  match parse_additive st with
  | Error e -> Error e
  | Ok lhs ->
    let rec loop (acc : Ast.expr) : (Ast.expr, Diagnostic.t) result =
      let tok = cur st in
      let op_opt =
        match tok.kind with
        | Token.LSHIFT -> Some Ast.BinShl
        | Token.RSHIFT -> Some Ast.BinShr
        | _ -> None
      in
      match op_opt with
      | None -> Ok acc
      | Some op ->
        bump st;
        (match parse_additive st with
         | Error e -> Error e
         | Ok rhs ->
           let span = Span.merge acc.span rhs.span in
           loop { Ast.desc = Ast.ExprBinary (op, acc, rhs); span })
    in
    loop lhs

and parse_additive st : (Ast.expr, Diagnostic.t) result =
  match parse_mult st with
  | Error e -> Error e
  | Ok lhs ->
    let rec loop (acc : Ast.expr) : (Ast.expr, Diagnostic.t) result =
      let tok = cur st in
      let op_opt =
        match tok.kind with
        | Token.PLUS -> Some Ast.BinAdd
        | Token.MINUS -> Some Ast.BinSub
        | _ -> None
      in
      match op_opt with
      | None -> Ok acc
      | Some op ->
        bump st;
        (match parse_mult st with
         | Error e -> Error e
         | Ok rhs ->
           let span = Span.merge acc.span rhs.span in
           loop { Ast.desc = Ast.ExprBinary (op, acc, rhs); span })
    in
    loop lhs

and parse_mult st : (Ast.expr, Diagnostic.t) result =
  match parse_unary st with
  | Error e -> Error e
  | Ok lhs ->
    let rec loop (acc : Ast.expr) : (Ast.expr, Diagnostic.t) result =
      let tok = cur st in
      let op_opt =
        match tok.kind with
        | Token.STAR -> Some Ast.BinMul
        | Token.SLASH -> Some Ast.BinDiv
        | Token.SLASH_SLASH -> Some Ast.BinIntDiv
        | Token.PERCENT -> Some Ast.BinMod
        | _ -> None
      in
      match op_opt with
      | None -> Ok acc
      | Some op ->
        bump st;
        (match parse_unary st with
         | Error e -> Error e
         | Ok rhs ->
           let span = Span.merge acc.span rhs.span in
           loop { Ast.desc = Ast.ExprBinary (op, acc, rhs); span })
    in
    loop lhs

and parse_unary st : (Ast.expr, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.PLUS ->
    bump st;
    (match parse_unary st with
     | Error e -> Error e
     | Ok sub ->
       let span = Span.merge tok.span sub.span in
       Ok { Ast.desc = Ast.ExprUnary (Ast.UnPos, sub); span })
  | Token.MINUS ->
    bump st;
    (match parse_unary st with
     | Error e -> Error e
     | Ok sub ->
       let span = Span.merge tok.span sub.span in
       Ok { Ast.desc = Ast.ExprUnary (Ast.UnNeg, sub); span })
  | _ -> parse_power st

and parse_power st : (Ast.expr, Diagnostic.t) result =
  match parse_primary st with
  | Error e -> Error e
  | Ok lhs ->
    let tok = cur st in
    (match tok.kind with
     | Token.CARET ->
       bump st;
       (match parse_unary st with
        | Error e -> Error e
        | Ok rhs ->
          let span = Span.merge lhs.span rhs.span in
          Ok { Ast.desc = Ast.ExprBinary (Ast.BinPow, lhs, rhs); span })
     | _ -> Ok lhs)

and parse_primary st : (Ast.expr, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.INT n ->
    bump st;
    Ok { Ast.desc = Ast.ExprInt n; span = tok.span }
  | Token.FLOAT f ->
    bump st;
    Ok { Ast.desc = Ast.ExprFloat f; span = tok.span }
  | Token.DOUBLE d ->
    bump st;
    Ok { Ast.desc = Ast.ExprDouble d; span = tok.span }
  | Token.DECIMAL m ->
    bump st;
    Ok { Ast.desc = Ast.ExprDecimal m; span = tok.span }
  | Token.BOOL b ->
    bump st;
    Ok { Ast.desc = Ast.ExprBool b; span = tok.span }
  | Token.CHAR c ->
    bump st;
    Ok { Ast.desc = Ast.ExprChar c; span = tok.span }
  | Token.STRING s ->
    bump st;
    Ok { Ast.desc = Ast.ExprString s; span = tok.span }
  | Token.INTERP_STRING segments ->
    bump st;
    let rec convert_segments acc = function
      | [] -> Ok (List.rev acc)
      | Token.SegLit s :: rest ->
        convert_segments (Ast.InterpLit s :: acc) rest
      | Token.SegTokens (sub_toks, seg_span) :: rest ->
        let sub_arr = Array.of_list sub_toks in
        let sub_st = { tokens = sub_arr; idx = 0 } in
        if is_eof sub_st then
          Error
            (Diagnostic.error
               ~code:"E0002"
               ~span:seg_span
               "empty interpolation '${}' in string literal")
        else
          (match parse_expr sub_st with
           | Error e -> Error e
           | Ok ie ->
             if not (is_eof sub_st) then
               let extra = cur sub_st in
               Error
                 (Diagnostic.error
                    ~code:"E0002"
                    ~span:extra.span
                    (Printf.sprintf "unexpected token %s inside string interpolation '${...}'"
                       (Token.kind_to_string extra.kind)))
             else
               convert_segments (Ast.InterpExpr ie :: acc) rest)
    in
    (match convert_segments [] segments with
     | Ok parts -> Ok { Ast.desc = Ast.ExprInterp parts; span = tok.span }
     | Error e -> Error e)
  | Token.IDENT name ->
    bump st;
    Ok { Ast.desc = Ast.ExprIdent name; span = tok.span }
  | Token.LPAREN ->
    bump st;
    (match parse_expr st with
     | Error e -> Error e
     | Ok inner ->
       let close_tok = cur st in
       (match close_tok.kind with
        | Token.RPAREN ->
          bump st;
          let span = Span.merge tok.span close_tok.span in
          Ok { inner with span }
        | _ ->
          Error
            (Diagnostic.error
               ~code:"E0002"
               ~span:close_tok.span
               (Printf.sprintf "expected ')' to close parenthesized expression, found %s"
                  (Token.kind_to_string close_tok.kind)))))
  | _ ->
    Error
      (Diagnostic.error
         ~code:"E0002"
         ~span:tok.span
         (Printf.sprintf "expected expression, found %s"
            (Token.kind_to_string tok.kind)))

let expect_semi st start_span : (Span.t, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.SEMI ->
    bump st;
    Ok (Span.merge start_span tok.span)
  | _ ->
    Error
      (Diagnostic.error
         ~hint:"every statement in Sigma must end with ';'"
         ~code:"E0003"
         ~span:tok.span
         (Printf.sprintf "expected ';' at end of statement, found %s"
            (Token.kind_to_string tok.kind)))

let parse_var_decl st var_tok : (Ast.stmt, Diagnostic.t) result =
  let name_tok = cur st in
  match name_tok.kind with
  | Token.IDENT name ->
    bump st;
    let op_tok = cur st in
    (match op_tok.kind with
     | Token.COLON_EQUAL ->
       bump st;
       (match parse_expr st with
        | Error e -> Error e
        | Ok init ->
          (match expect_semi st var_tok.Token.span with
           | Error e -> Error e
           | Ok full_span ->
             let decl =
               { Ast.name; name_span = name_tok.span; ty_annot = None; init = Some init }
             in
             Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }))
     | Token.COLON ->
       bump st;
       let ty_tok = cur st in
       (match ty_tok.kind with
        | Token.IDENT ty_name ->
          bump st;
          let ty_annot = Some (ty_name, ty_tok.span) in
          let after_ty = cur st in
          (match after_ty.kind with
           | Token.EQUAL ->
             bump st;
             (match parse_expr st with
              | Error e -> Error e
              | Ok init ->
                (match expect_semi st var_tok.span with
                 | Error e -> Error e
                 | Ok full_span ->
                   let decl =
                     { Ast.name; name_span = name_tok.span; ty_annot; init = Some init }
                   in
                   Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }))
           | Token.SEMI ->
             bump st;
             let full_span = Span.merge var_tok.span after_ty.span in
             let decl =
               { Ast.name; name_span = name_tok.span; ty_annot; init = None }
             in
             Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }
           | _ ->
             Error
               (Diagnostic.error
                  ~hint:"use 'var x : int = 10;' or 'var x : int;'"
                  ~code:"E0003"
                  ~span:after_ty.span
                  (Printf.sprintf "expected '=' or ';' after type '%s', found %s"
                     ty_name (Token.kind_to_string after_ty.kind))))
        | _ ->
          Error
            (Diagnostic.error
               ~code:"E0002"
               ~span:ty_tok.span
               (Printf.sprintf "expected type name after ':', found %s"
                  (Token.kind_to_string ty_tok.kind))))
     | Token.EQUAL ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "use 'var %s := <value>;' for type inference or 'var %s : int = <value>;'" name name)
            ~code:"E0002"
            ~span:op_tok.span
            "cannot use '=' without an explicit type annotation in 'var' declaration")
     | _ ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "use 'var %s : <type> [= <value>];' or 'var %s := <value>;'" name name)
            ~code:"E0002"
            ~span:op_tok.span
            (Printf.sprintf "expected ':' or ':=' after variable name '%s', found %s"
               name (Token.kind_to_string op_tok.kind))))
  | _ ->
    Error
      (Diagnostic.error
         ~code:"E0002"
         ~span:name_tok.span
         (Printf.sprintf "expected variable name after 'var', found %s"
            (Token.kind_to_string name_tok.kind)))

let parse_stmt st : (Ast.stmt, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.KW_VAR ->
    bump st;
    parse_var_decl st tok
  | _ ->
    (match parse_expr st with
     | Error e -> Error e
     | Ok expr ->
       (match expect_semi st expr.span with
        | Error e -> Error e
        | Ok full_span ->
          Ok { Ast.desc = Ast.StmtExpr expr; span = full_span }))

let parse (token_list : Token.t list) : (Ast.program, Diagnostic.t list) result =
  let tokens = Array.of_list token_list in
  let st = { tokens; idx = 0 } in
  let stmts = ref [] in
  let errors = ref [] in
  while not (is_eof st) do
    match parse_stmt st with
    | Ok s -> stmts := s :: !stmts
    | Error err ->
      errors := err :: !errors;
      sync_to_semi st
  done;
  match List.rev !errors with
  | [] -> Ok (List.rev !stmts)
  | errs -> Error errs

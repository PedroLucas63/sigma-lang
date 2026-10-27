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

let match_assign_op (k : Token.kind) : Ast.assign_op option =
  match k with
  | Token.EQUAL -> Some Ast.AssignEq
  | Token.PLUS_EQ -> Some Ast.AssignAdd
  | Token.MINUS_EQ -> Some Ast.AssignSub
  | Token.STAR_EQ -> Some Ast.AssignMul
  | Token.SLASH_EQ -> Some Ast.AssignDiv
  | Token.SLASH_SLASH_EQ -> Some Ast.AssignIntDiv
  | Token.PERCENT_EQ -> Some Ast.AssignMod
  | Token.CARET_EQ -> Some Ast.AssignPow
  | Token.KW_AND_EQ -> Some Ast.AssignAnd
  | Token.KW_OR_EQ -> Some Ast.AssignOr
  | Token.KW_XOR_EQ -> Some Ast.AssignXor
  | Token.LSHIFT_EQ -> Some Ast.AssignShl
  | Token.RSHIFT_EQ -> Some Ast.AssignShr
  | _ -> None

let expr_to_lvalue (e : Ast.expr) : Ast.lvalue option =
  match e.desc with
  | Ast.ExprIdent name -> Some (Ast.LValIdent (name, e.span))
  | Ast.ExprIndex (base, idx) -> Some (Ast.LValIndex (base, idx, e.span))
  | _ -> None

let rec parse_expr st : (Ast.expr, Diagnostic.t) result =
  parse_assignment st

and parse_assignment st : (Ast.expr, Diagnostic.t) result =
  match parse_logical_or st with
  | Error e -> Error e
  | Ok lhs ->
    (match match_assign_op (cur st).kind with
     | None -> Ok lhs
     | Some op ->
       bump st;
       (match parse_assignment st with
        | Error e -> Error e
        | Ok rhs ->
          (match expr_to_lvalue lhs with
           | Some lval ->
             let span = Span.merge lhs.span rhs.span in
             Ok { Ast.desc = Ast.ExprAssign (lval, op, rhs); span }
           | None ->
             let op_str = Ast.assign_op_to_string op in
             Error
               (Diagnostic.error
                  ~hint:(Printf.sprintf "assign to a declared variable or array element, e.g. 'x %s <expr>;' or 'arr[0] %s <expr>;'" op_str op_str)
                  ~code:"E0002"
                  ~span:lhs.span
                  (Printf.sprintf "invalid assignment target: left-hand side of '%s' must be a variable or indexed expression"
                     op_str)))))

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
  | _ -> parse_relational st

and match_rel_op (k : Token.kind) : Ast.binop option =
  match k with
  | Token.EQ_EQ -> Some Ast.BinEq
  | Token.BANG_EQ -> Some Ast.BinNe
  | Token.LT -> Some Ast.BinLt
  | Token.GT -> Some Ast.BinGt
  | Token.LT_EQ -> Some Ast.BinLe
  | Token.GT_EQ -> Some Ast.BinGe
  | _ -> None

and parse_relational st : (Ast.expr, Diagnostic.t) result =
  match parse_shift st with
  | Error e -> Error e
  | Ok e1 ->
    (match match_rel_op (cur st).kind with
     | None -> Ok e1
     | Some op1 ->
       bump st;
       (match parse_shift st with
        | Error e -> Error e
        | Ok e2 ->
          (match match_rel_op (cur st).kind with
           | None ->
             let span = Span.merge e1.span e2.span in
             Ok { Ast.desc = Ast.ExprBinary (op1, e1, e2); span }
           | Some op2 ->
             bump st;
             (match parse_shift st with
              | Error e -> Error e
              | Ok e3 ->
                let tok3 = cur st in
                (match match_rel_op tok3.kind with
                 | Some _ ->
                   Error
                     (Diagnostic.error
                        ~hint:"chained relational expressions support at most 3 operands (e.g. 'a < b <= c'); combine further conditions with 'and'"
                        ~code:"E0002"
                        ~span:tok3.span
                        "relational chain exceeds maximum of 3 operands")
                 | None ->
                   let span = Span.merge e1.span e3.span in
                   Ok { Ast.desc = Ast.ExprChainCmp (e1, op1, e2, op2, e3); span })))))

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
  | Token.PLUS_PLUS | Token.MINUS_MINUS ->
    let inc_op =
      if tok.kind = Token.PLUS_PLUS then Ast.PreInc else Ast.PreDec
    in
    let op_str = Ast.incdec_op_to_string inc_op in
    bump st;
    (match parse_unary st with
     | Error e -> Error e
     | Ok sub ->
       (match expr_to_lvalue sub with
        | Some lval ->
          let span = Span.merge tok.span sub.span in
          Ok { Ast.desc = Ast.ExprIncDec (inc_op, lval); span }
        | None ->
          Error
            (Diagnostic.error
               ~hint:(Printf.sprintf "apply '%s' directly to a variable or array element, e.g. '%sx' or '%sarr[0]'" op_str op_str op_str)
               ~code:"E0002"
               ~span:sub.span
               (Printf.sprintf "operand of prefix '%s' must be a variable or indexed expression"
                  op_str))))
  | _ -> parse_power st

and parse_power st : (Ast.expr, Diagnostic.t) result =
  match parse_postfix st with
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

and parse_postfix st : (Ast.expr, Diagnostic.t) result =
  match parse_primary st with
  | Error e -> Error e
  | Ok lhs ->
    let rec loop (acc : Ast.expr) : (Ast.expr, Diagnostic.t) result =
      let tok = cur st in
      match tok.kind with
      | Token.LBRACKET ->
        bump st;
        (match parse_expr st with
         | Error e -> Error e
         | Ok idx_expr ->
           let close_tok = cur st in
           (match close_tok.kind with
            | Token.RBRACKET ->
              bump st;
              let span = Span.merge acc.span close_tok.span in
              loop { Ast.desc = Ast.ExprIndex (acc, idx_expr); span }
            | _ ->
              Error
                (Diagnostic.error
                   ~hint:"close index access with ']', e.g. 'arr[0]'"
                   ~code:"E0002"
                   ~span:close_tok.span
                   (Printf.sprintf "expected ']' after index expression, found %s"
                      (Token.kind_to_string close_tok.kind)))))
      | Token.PLUS_PLUS | Token.MINUS_MINUS ->
        let inc_op =
          if tok.kind = Token.PLUS_PLUS then Ast.PostInc else Ast.PostDec
        in
        let op_str = Ast.incdec_op_to_string inc_op in
        bump st;
        (match expr_to_lvalue acc with
         | Some lval ->
           let span = Span.merge acc.span tok.span in
           loop { Ast.desc = Ast.ExprIncDec (inc_op, lval); span }
         | None ->
           Error
             (Diagnostic.error
                ~hint:(Printf.sprintf "apply '%s' directly to a variable or array element, e.g. 'x%s' or 'arr[0]%s'" op_str op_str op_str)
                ~code:"E0002"
                ~span:acc.span
                (Printf.sprintf "operand of postfix '%s' must be a variable or indexed expression"
                   op_str)))
      | _ -> Ok acc
    in
    loop lhs

and parse_collection_item st : (Ast.collection_item, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.DOT_DOT ->
    bump st;
    (match parse_expr st with
     | Error e -> Error e
     | Ok sub ->
       Ok (Ast.ItemSpread (sub, Span.merge tok.span sub.span)))
  | _ ->
    (match parse_expr st with
     | Error e -> Error e
     | Ok sub -> Ok (Ast.ItemExpr sub))

and parse_collection_rest st ~close_kind ~close_str (acc : Ast.collection_item list)
    : (Ast.collection_item list * Span.t, Diagnostic.t) result =
  let tok = cur st in
  if tok.kind = close_kind then begin
    bump st;
    Ok (List.rev acc, tok.span)
  end else
    match parse_collection_item st with
    | Error e -> Error e
    | Ok item ->
      let acc = item :: acc in
      let next_tok = cur st in
      if next_tok.kind = Token.COMMA then begin
        bump st;
        parse_collection_rest st ~close_kind ~close_str acc
      end else if next_tok.kind = close_kind then begin
        bump st;
        Ok (List.rev acc, next_tok.span)
      end else
        Error
          (Diagnostic.error
             ~code:"E0002"
             ~span:next_tok.span
             (Printf.sprintf "expected '%s' or ',' in collection literal, found %s"
                close_str (Token.kind_to_string next_tok.kind)))

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
    if (cur st).kind = Token.RPAREN then begin
      let close_tok = cur st in
      bump st;
      Error
        (Diagnostic.error
           ~hint:"provide at least one element, e.g. '(10, 20)'"
           ~code:"E0002"
           ~span:(Span.merge tok.span close_tok.span)
           "empty tuple '()' is not supported")
    end else if (cur st).kind = Token.DOT_DOT then begin
      match parse_collection_item st with
      | Error e -> Error e
      | Ok first_item ->
        let next_tok = cur st in
        if next_tok.kind = Token.COMMA then begin
          bump st;
          match parse_collection_rest st ~close_kind:Token.RPAREN ~close_str:")" [ first_item ] with
          | Error e -> Error e
          | Ok (items, close_span) ->
            let span = Span.merge tok.span close_span in
            Ok { Ast.desc = Ast.ExprTuple items; span }
        end else if next_tok.kind = Token.RPAREN then begin
          bump st;
          let span = Span.merge tok.span next_tok.span in
          Ok { Ast.desc = Ast.ExprTuple [ first_item ]; span }
        end else
          Error
            (Diagnostic.error
               ~code:"E0002"
               ~span:next_tok.span
               (Printf.sprintf "expected ')' or ',' in tuple literal, found %s"
                  (Token.kind_to_string next_tok.kind)))
    end else
      (match parse_expr st with
       | Error e -> Error e
       | Ok inner ->
         let next_tok = cur st in
         (match next_tok.kind with
          | Token.RPAREN ->
            bump st;
            let span = Span.merge tok.span next_tok.span in
            Ok { inner with span }
          | Token.COMMA ->
            bump st;
            (match parse_collection_rest st ~close_kind:Token.RPAREN ~close_str:")" [ Ast.ItemExpr inner ] with
             | Error e -> Error e
             | Ok (items, close_span) ->
               let span = Span.merge tok.span close_span in
               Ok { Ast.desc = Ast.ExprTuple items; span })
          | _ ->
            Error
              (Diagnostic.error
                 ~code:"E0002"
                 ~span:next_tok.span
                 (Printf.sprintf "expected ')' to close parenthesized expression or ',' for tuple, found %s"
                    (Token.kind_to_string next_tok.kind)))))
  | Token.LBRACKET ->
    bump st;
    if (cur st).kind = Token.RBRACKET then begin
      let close_tok = cur st in
      bump st;
      let span = Span.merge tok.span close_tok.span in
      Ok { Ast.desc = Ast.ExprArray []; span }
    end else
      (match parse_collection_item st with
       | Error e -> Error e
       | Ok first_item ->
         let next_tok = cur st in
         if next_tok.kind = Token.COMMA then begin
           bump st;
           match parse_collection_rest st ~close_kind:Token.RBRACKET ~close_str:"]" [ first_item ] with
           | Error e -> Error e
           | Ok (items, close_span) ->
             let span = Span.merge tok.span close_span in
             Ok { Ast.desc = Ast.ExprArray items; span }
         end else if next_tok.kind = Token.RBRACKET then begin
           bump st;
           let span = Span.merge tok.span next_tok.span in
           Ok { Ast.desc = Ast.ExprArray [ first_item ]; span }
         end else
           Error
             (Diagnostic.error
                ~code:"E0002"
                ~span:next_tok.span
                (Printf.sprintf "expected ']' or ',' in array literal, found %s"
                   (Token.kind_to_string next_tok.kind))))
  | Token.KW_PRINT | Token.KW_PRINTLN ->
    let fn_name = if tok.kind = Token.KW_PRINTLN then "println" else "print" in
    Error
      (Diagnostic.error
         ~hint:(Printf.sprintf "use '%s(...);' as a standalone statement" fn_name)
         ~code:"E0002"
         ~span:tok.span
         (Printf.sprintf "'%s' is a void output statement and cannot be used inside an expression"
            fn_name))
  | Token.KW_ASSERT | Token.KW_ASSERT_ERROR ->
    let fn_name = if tok.kind = Token.KW_ASSERT_ERROR then "assert_error" else "assert" in
    Error
      (Diagnostic.error
         ~hint:(Printf.sprintf "use '%s(...);' as a standalone statement" fn_name)
         ~code:"E0002"
         ~span:tok.span
         (Printf.sprintf "'%s' is a verification statement and cannot be used inside an expression"
            fn_name))
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

let rec parse_type_expr st : (Ast.type_expr, Diagnostic.t) result =
  match parse_base_type st with
  | Error e -> Error e
  | Ok base ->
    let base_span = Ast.type_expr_span base in
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
           let close_tok = cur st in
           if close_tok.kind = Token.RBRACKET then begin
             bump st;
             parse_dims ((Some (Int64.to_int n), Span.merge tok.span close_tok.span) :: acc)
           end else
             Error
               (Diagnostic.error
                  ~code:"E0002"
                  ~span:close_tok.span
                  (Printf.sprintf "expected ']' after array size, found %s"
                     (Token.kind_to_string close_tok.kind)))
         | Token.MINUS ->
           bump st;
           (match (cur st).kind with
            | Token.INT n ->
              bump st;
              let close_tok = cur st in
              if close_tok.kind = Token.RBRACKET then begin
                bump st;
                parse_dims ((Some (- (Int64.to_int n)), Span.merge tok.span close_tok.span) :: acc)
              end else
                Error
                  (Diagnostic.error
                     ~code:"E0002"
                     ~span:close_tok.span
                     (Printf.sprintf "expected ']' after array size, found %s"
                        (Token.kind_to_string close_tok.kind)))
            | _ ->
              Error
                (Diagnostic.error
                   ~code:"E0002"
                   ~span:(cur st).span
                   "expected integer literal after '-' in array size"))
         | _ ->
           Error
             (Diagnostic.error
                ~hint:"use an integer size like 'int[3]' or omit size with 'int[]'"
                ~code:"E0002"
                ~span:next_tok.span
                (Printf.sprintf "expected array length integer or ']', found %s"
                   (Token.kind_to_string next_tok.kind))))
      | _ -> Ok (List.rev acc)
    in
    (match parse_dims [] with
     | Error e -> Error e
     | Ok dims ->
       Ok
         (List.fold_right
            (fun (sz_opt, br_span) acc ->
              Ast.TypeArray (acc, sz_opt, Span.merge base_span br_span))
            dims base))

and parse_base_type st : (Ast.type_expr, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.IDENT ty_name ->
    bump st;
    Ok (Ast.TypeNamed (ty_name, tok.span))
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
            Error
              (Diagnostic.error
                 ~code:"E0002"
                 ~span:next_tok.span
                 (Printf.sprintf "expected ')' or ',' in tuple type annotation, found %s"
                    (Token.kind_to_string next_tok.kind)))
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
    Error
      (Diagnostic.error
         ~code:"E0002"
         ~span:tok.span
         (Printf.sprintf "expected type name after ':', found %s"
            (Token.kind_to_string tok.kind)))

let parse_destruct_names st open_tok : ((string * Span.t) list, Diagnostic.t) result =
  let rec loop acc =
    if (cur st).kind = Token.RPAREN then begin
      bump st;
      Ok (List.rev acc)
    end else
      let tok = cur st in
      match tok.kind with
      | Token.IDENT name ->
        bump st;
        let acc = (name, tok.span) :: acc in
        let next_tok = cur st in
        if next_tok.kind = Token.COMMA then begin
          bump st;
          loop acc
        end else if next_tok.kind = Token.RPAREN then begin
          bump st;
          Ok (List.rev acc)
        end else
          Error
            (Diagnostic.error
               ~code:"E0002"
               ~span:next_tok.span
               (Printf.sprintf "expected ')' or ',' in destructuring pattern, found %s"
                  (Token.kind_to_string next_tok.kind)))
      | _ ->
        Error
          (Diagnostic.error
             ~hint:"use variable names or '_' inside destructuring, e.g. 'var (a, b, _) := tuple;'"
             ~code:"E0002"
             ~span:tok.span
             (Printf.sprintf "expected variable identifier in destructuring pattern, found %s"
                (Token.kind_to_string tok.kind)))
  in
  match loop [] with
  | Error e -> Error e
  | Ok [] ->
    Error
      (Diagnostic.error
         ~code:"E0002"
         ~span:open_tok.Token.span
         "destructuring pattern cannot be empty '()'")
  | Ok names -> Ok names

let parse_decl st kw_tok ~is_const : (Ast.stmt, Diagnostic.t) result =
  let kw_name = if is_const then "const" else "var" in
  let entity_name = if is_const then "constant" else "variable" in
  let name_tok = cur st in
  match name_tok.kind with
  | Token.LPAREN ->
    bump st;
    (match parse_destruct_names st name_tok with
     | Error e -> Error e
     | Ok names ->
       let op_tok = cur st in
       (match op_tok.kind with
        | Token.COLON_EQUAL ->
          bump st;
          (match parse_expr st with
           | Error e -> Error e
           | Ok init ->
             (match expect_semi st kw_tok.Token.span with
              | Error e -> Error e
              | Ok full_span ->
                let decl = { Ast.names; is_const; ty_annot = None; init = Some init } in
                Ok { Ast.desc = Ast.StmtDestruct decl; span = full_span }))
        | Token.COLON ->
          bump st;
          (match parse_type_expr st with
           | Error e -> Error e
           | Ok ty_expr ->
             let ty_annot = Some ty_expr in
             let ty_str = Ast.type_expr_to_string ty_expr in
             let after_ty = cur st in
             (match after_ty.kind with
              | Token.EQUAL ->
                bump st;
                (match parse_expr st with
                 | Error e -> Error e
                 | Ok init ->
                   (match expect_semi st kw_tok.span with
                    | Error e -> Error e
                    | Ok full_span ->
                      let decl = { Ast.names; is_const; ty_annot; init = Some init } in
                      Ok { Ast.desc = Ast.StmtDestruct decl; span = full_span }))
              | Token.SEMI ->
                bump st;
                let full_span = Span.merge kw_tok.span after_ty.span in
                if is_const then
                  Error
                    (Diagnostic.error
                       ~code:"E0106"
                       ~span:full_span
                       "constant destructuring declaration requires an initializing expression")
                else
                  let decl = { Ast.names; is_const = false; ty_annot; init = None } in
                  Ok { Ast.desc = Ast.StmtDestruct decl; span = full_span }
              | _ ->
                let expected_msg = if is_const then "expected '=' after" else "expected '=' or ';' after" in
                Error
                  (Diagnostic.error
                     ~code:"E0003"
                     ~span:after_ty.span
                     (Printf.sprintf "%s type '%s', found %s"
                        expected_msg ty_str (Token.kind_to_string after_ty.kind)))))
        | _ ->
          Error
            (Diagnostic.error
               ~hint:(Printf.sprintf "use '%s (a, b) := <tuple>;' or '%s (a, b) : (int, int) = <tuple>;'" kw_name kw_name)
               ~code:"E0002"
               ~span:op_tok.span
               (Printf.sprintf "expected ':' or ':=' after destructuring pattern, found %s"
                  (Token.kind_to_string op_tok.kind)))))
  | Token.IDENT name ->
    bump st;
    let op_tok = cur st in
    (match op_tok.kind with
     | Token.COLON_EQUAL ->
       bump st;
       (match parse_expr st with
        | Error e -> Error e
        | Ok init ->
          (match expect_semi st kw_tok.Token.span with
           | Error e -> Error e
           | Ok full_span ->
             let decl =
               { Ast.name; name_span = name_tok.span; is_const; ty_annot = None; init = Some init }
             in
             Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }))
     | Token.COLON ->
       bump st;
       (match parse_type_expr st with
        | Error e -> Error e
        | Ok ty_expr ->
          let ty_annot = Some ty_expr in
          let ty_name = Ast.type_expr_to_string ty_expr in
          let after_ty = cur st in
          (match after_ty.kind with
           | Token.EQUAL ->
             bump st;
             (match parse_expr st with
              | Error e -> Error e
              | Ok init ->
                (match expect_semi st kw_tok.span with
                 | Error e -> Error e
                 | Ok full_span ->
                   let decl =
                     { Ast.name; name_span = name_tok.span; is_const; ty_annot; init = Some init }
                   in
                   Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }))
           | Token.SEMI ->
             bump st;
             let full_span = Span.merge kw_tok.span after_ty.span in
             if is_const then
               Error
                 (Diagnostic.error
                    ~hint:(Printf.sprintf "provide a value, e.g. 'const %s : %s = <value>;'" name ty_name)
                    ~code:"E0106"
                    ~span:full_span
                    (Printf.sprintf "constant '%s' requires an initializing expression" name))
             else
               let decl =
                 { Ast.name; name_span = name_tok.span; is_const = false; ty_annot; init = None }
               in
               Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }
           | _ ->
             let hint =
               if is_const then
                 Printf.sprintf "use 'const %s : %s = <value>;'" name ty_name
               else
                 Printf.sprintf "use 'var %s : %s = <value>;' or 'var %s : %s;'" name ty_name name ty_name
             in
             let expected_msg = if is_const then "expected '=' after" else "expected '=' or ';' after" in
             Error
               (Diagnostic.error
                  ~hint
                  ~code:"E0003"
                  ~span:after_ty.span
                  (Printf.sprintf "%s type '%s', found %s"
                     expected_msg ty_name (Token.kind_to_string after_ty.kind)))))
     | Token.EQUAL ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "use '%s %s := <value>;' for type inference or '%s %s : int = <value>;'" kw_name name kw_name name)
            ~code:"E0002"
            ~span:op_tok.span
            (Printf.sprintf "cannot use '=' without an explicit type annotation in '%s' declaration" kw_name))
     | _ ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "use '%s %s : <type> = <value>;' or '%s %s := <value>;'" kw_name name kw_name name)
            ~code:"E0002"
            ~span:op_tok.span
            (Printf.sprintf "expected ':' or ':=' after %s name '%s', found %s"
               entity_name name (Token.kind_to_string op_tok.kind))))
  | _ ->
    Error
      (Diagnostic.error
         ~code:"E0002"
         ~span:name_tok.span
         (Printf.sprintf "expected %s name after '%s', found %s"
            entity_name kw_name (Token.kind_to_string name_tok.kind)))

let parse_print_stmt st fn_tok ~newline : (Ast.stmt, Diagnostic.t) result =
  let fn_name = if newline then "println" else "print" in
  let open_tok = cur st in
  match open_tok.kind with
  | Token.LPAREN ->
    bump st;
    let rec parse_args acc =
      match parse_expr st with
      | Error e -> Error e
      | Ok arg ->
        let acc = arg :: acc in
        let next_tok = cur st in
        (match next_tok.kind with
         | Token.COMMA ->
           bump st;
           parse_args acc
         | Token.RPAREN ->
           bump st;
           Ok (List.rev acc)
         | _ ->
           Error
             (Diagnostic.error
                ~hint:(Printf.sprintf "use '%s(a, b);' and close with ')'" fn_name)
                ~code:"E0002"
                ~span:next_tok.span
                (Printf.sprintf "expected ')' or ',' after argument in '%s', found %s"
                   fn_name (Token.kind_to_string next_tok.kind))))
    in
    let args_res =
      if (cur st).kind = Token.RPAREN then begin
        bump st;
        Ok []
      end else
        parse_args []
    in
    (match args_res with
     | Error e -> Error e
     | Ok args ->
       (match expect_semi st fn_tok.Token.span with
        | Error e -> Error e
        | Ok full_span ->
          Ok { Ast.desc = Ast.StmtPrint { newline; args }; span = full_span }))
  | _ ->
    Error
      (Diagnostic.error
         ~hint:(Printf.sprintf "call '%s' with parentheses, e.g. '%s(expr);'" fn_name fn_name)
         ~code:"E0002"
         ~span:open_tok.span
         (Printf.sprintf "expected '(' after '%s', found %s"
            fn_name (Token.kind_to_string open_tok.kind)))

let parse_call_args st fn_name fn_tok : (Ast.expr list * Span.t, Diagnostic.t) result =
  let open_tok = cur st in
  match open_tok.kind with
  | Token.LPAREN ->
    bump st;
    let rec parse_args acc =
      match parse_expr st with
      | Error e -> Error e
      | Ok arg ->
        let acc = arg :: acc in
        let next_tok = cur st in
        (match next_tok.kind with
         | Token.COMMA ->
           bump st;
           parse_args acc
         | Token.RPAREN ->
           bump st;
           Ok (List.rev acc)
         | _ ->
           Error
             (Diagnostic.error
                ~hint:(Printf.sprintf "use '%s(a, b);' and close with ')'" fn_name)
                ~code:"E0002"
                ~span:next_tok.span
                (Printf.sprintf "expected ')' or ',' after argument in '%s', found %s"
                   fn_name (Token.kind_to_string next_tok.kind))))
    in
    let args_res =
      if (cur st).kind = Token.RPAREN then begin
        bump st;
        Ok []
      end else
        parse_args []
    in
    (match args_res with
     | Error e -> Error e
     | Ok args ->
       (match expect_semi st fn_tok.Token.span with
        | Error e -> Error e
        | Ok full_span -> Ok (args, full_span)))
  | _ ->
    Error
      (Diagnostic.error
         ~hint:(Printf.sprintf "call '%s' with parentheses, e.g. '%s(...);'" fn_name fn_name)
         ~code:"E0002"
         ~span:open_tok.span
         (Printf.sprintf "expected '(' after '%s', found %s"
            fn_name (Token.kind_to_string open_tok.kind)))

let parse_assert_stmt st fn_tok : (Ast.stmt, Diagnostic.t) result =
  match parse_call_args st "assert" fn_tok with
  | Error e -> Error e
  | Ok (args, full_span) ->
    (match args with
     | [ actual ] ->
       Ok { Ast.desc = Ast.StmtAssert { actual; expected = None; message = None }; span = full_span }
     | [ actual; expected ] ->
       Ok { Ast.desc = Ast.StmtAssert { actual; expected = Some expected; message = None }; span = full_span }
     | [ actual; expected; msg ] ->
       Ok { Ast.desc = Ast.StmtAssert { actual; expected = Some expected; message = Some msg }; span = full_span }
     | _ ->
       Error
         (Diagnostic.error
            ~hint:"use 'assert(cond);', 'assert(actual, expected);', or 'assert(actual, expected, \"message\");'"
            ~code:"E0002"
            ~span:full_span
            (Printf.sprintf "'assert' expects 1, 2, or 3 arguments, found %d" (List.length args))))

let parse_assert_error_stmt st fn_tok : (Ast.stmt, Diagnostic.t) result =
  match parse_call_args st "assert_error" fn_tok with
  | Error e -> Error e
  | Ok (args, full_span) ->
    let extract_str (e : Ast.expr) (arg_desc : string) =
      match e.desc with
      | Ast.ExprString s -> Ok (s, e.span)
      | _ ->
        Error
          (Diagnostic.error
             ~hint:"use a string literal, e.g. 'assert_error(\"E0106\", \"const x : int;\");'"
             ~code:"E0002"
             ~span:e.span
             (Printf.sprintf "'assert_error' requires a string literal for %s" arg_desc))
    in
    (match args with
     | [ code_expr; snippet_expr ] ->
       (match extract_str code_expr "expected error code", extract_str snippet_expr "code snippet" with
        | Error e, _ | _, Error e -> Error e
        | Ok (expected_code, code_span), Ok (snippet, snippet_span) ->
          Ok
            {
              Ast.desc =
                Ast.StmtAssertError
                  { expected_code; code_span; snippet; snippet_span; message = None };
              span = full_span;
            })
     | [ code_expr; snippet_expr; msg_expr ] ->
       (match extract_str code_expr "expected error code", extract_str snippet_expr "code snippet" with
        | Error e, _ | _, Error e -> Error e
        | Ok (expected_code, code_span), Ok (snippet, snippet_span) ->
          Ok
            {
              Ast.desc =
                Ast.StmtAssertError
                  { expected_code; code_span; snippet; snippet_span; message = Some msg_expr };
              span = full_span;
            })
     | _ ->
       Error
         (Diagnostic.error
            ~hint:"use 'assert_error(\"E0106\", \"const x : int;\");' or 'assert_error(\"E0106\", \"const x : int;\", \"message\");'"
            ~code:"E0002"
            ~span:full_span
            (Printf.sprintf "'assert_error' expects 2 or 3 arguments, found %d" (List.length args))))

let rec parse_stmt st : (Ast.stmt, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.KW_VAR ->
    bump st;
    parse_decl st tok ~is_const:false
  | Token.KW_CONST ->
    bump st;
    parse_decl st tok ~is_const:true
  | _ -> parse_non_decl_stmt st

and parse_non_decl_stmt st : (Ast.stmt, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.KW_VAR | Token.KW_CONST ->
    let kw_str = if tok.kind = Token.KW_VAR then "variable declaration 'var'" else "constant declaration 'const'" in
    let kw_name = if tok.kind = Token.KW_VAR then "var" else "const" in
    Error
      (Diagnostic.error
         ~hint:(Printf.sprintf "wrap the declaration in braces, e.g. 'if (cond) { %s x := ...; }'" kw_name)
         ~code:"E0002"
         ~span:tok.span
         (Printf.sprintf "%s is not allowed directly inside an unbraced branch" kw_str))
  | Token.KW_IF ->
    bump st;
    parse_if_stmt st tok
  | Token.LBRACE ->
    bump st;
    parse_block_stmt st tok
  | Token.KW_PRINT ->
    bump st;
    parse_print_stmt st tok ~newline:false
  | Token.KW_PRINTLN ->
    bump st;
    parse_print_stmt st tok ~newline:true
  | Token.KW_ASSERT ->
    bump st;
    parse_assert_stmt st tok
  | Token.KW_ASSERT_ERROR ->
    bump st;
    parse_assert_error_stmt st tok
  | _ ->
    (match parse_expr st with
     | Error e -> Error e
     | Ok expr ->
       (match expect_semi st expr.span with
        | Error e -> Error e
        | Ok full_span ->
          Ok { Ast.desc = Ast.StmtExpr expr; span = full_span }))

and parse_if_stmt st if_tok : (Ast.stmt, Diagnostic.t) result =
  let open_tok = cur st in
  match open_tok.kind with
  | Token.LPAREN ->
    bump st;
    (match parse_expr st with
     | Error e -> Error e
     | Ok cond ->
       let close_tok = cur st in
       (match close_tok.kind with
        | Token.RPAREN ->
          bump st;
          (match parse_non_decl_stmt st with
           | Error e -> Error e
           | Ok then_branch ->
             if (cur st).kind = Token.KW_ELSE then begin
               bump st;
               match parse_non_decl_stmt st with
               | Error e -> Error e
               | Ok else_stmt ->
                 let full_span = Span.merge if_tok.Token.span else_stmt.span in
                 Ok
                   {
                     Ast.desc =
                       Ast.StmtIf { cond; then_branch; else_branch = Some else_stmt };
                     span = full_span;
                   }
             end else
               let full_span = Span.merge if_tok.Token.span then_branch.span in
               Ok
                 {
                   Ast.desc = Ast.StmtIf { cond; then_branch; else_branch = None };
                   span = full_span;
                 })
        | _ ->
          Error
            (Diagnostic.error
               ~code:"E0002"
               ~span:close_tok.span
               (Printf.sprintf "expected ')' after 'if' condition, found %s"
                  (Token.kind_to_string close_tok.kind)))))
  | _ ->
    Error
      (Diagnostic.error
         ~hint:"wrap the condition in parentheses, e.g. 'if (x == 10) ...'"
         ~code:"E0002"
         ~span:open_tok.span
         (Printf.sprintf "expected '(' after 'if', found %s"
            (Token.kind_to_string open_tok.kind)))

and parse_block_stmt st open_brace_tok : (Ast.stmt, Diagnostic.t) result =
  let stmts = ref [] in
  let first_err = ref None in
  while not (is_eof st) && (cur st).kind <> Token.RBRACE && !first_err = None do
    match parse_stmt st with
    | Ok s -> stmts := s :: !stmts
    | Error err -> first_err := Some err
  done;
  match !first_err with
  | Some err -> Error err
  | None ->
    let close_tok = cur st in
    (match close_tok.kind with
     | Token.RBRACE ->
       bump st;
       let full_span = Span.merge open_brace_tok.Token.span close_tok.span in
       Ok { Ast.desc = Ast.StmtBlock (List.rev !stmts); span = full_span }
     | _ ->
       Error
         (Diagnostic.error
            ~code:"E0002"
            ~span:open_brace_tok.span
            "unclosed block '{' (missing closing '}')"))

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

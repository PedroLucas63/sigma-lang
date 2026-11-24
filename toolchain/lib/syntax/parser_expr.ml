open Sigma_common
open Sigma_diagnostics
open Parser_state

let parse_fn_call_args hooks st : (Ast.call_arg list * Span.t, Diagnostic.t) result =
  let rec loop acc =
    let tok = cur st in
    if tok.kind = Token.RPAREN then begin
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
      let t = cur st in
      let arg_mode_res : (Ast.arg_mode * Ast.expr, Diagnostic.t) result =
        if t.kind = Token.KW_REF then begin
          bump st;
          match hooks.parse_expr st with
          | Error e -> Error e
          | Ok e -> Ok (Ast.ArgRef, e)
        end else if t.kind = Token.KW_OUT then begin
          bump st;
          if (cur st).kind = Token.KW_VAR then begin
            bump st;
            let id_tok = cur st in
            match id_tok.kind with
            | Token.IDENT vname ->
              bump st;
              let ty_res =
                if (cur st).kind = Token.COLON then begin
                  bump st;
                  match Parser_types.parse_type_expr st with
                  | Ok te -> Ok (Some te)
                  | Error e -> Error e
                end else Ok None
              in
              (match ty_res with
               | Error e -> Error e
               | Ok ty_opt ->
                 let dummy : Ast.expr = { desc = Ast.ExprIdent vname; span = id_tok.span } in
                 Ok (Ast.ArgOutVar { var_name = vname; var_span = id_tok.span; var_type = ty_opt }, dummy))
            | _ ->
              err_at id_tok
                (Printf.sprintf "expected identifier after 'out var', found %s"
                   (Token.kind_to_string id_tok.kind))
          end else
            match hooks.parse_expr st with
            | Error e -> Error e
            | Ok e -> Ok (Ast.ArgOut, e)
        end else
          match hooks.parse_expr st with
          | Error e -> Error e
          | Ok e -> Ok (Ast.ArgVal, e)
      in
      match arg_mode_res with
      | Error e -> Error e
      | Ok (mode, v) ->
        let acc = { Ast.arg_name = named; arg_mode = mode; arg_value = v } :: acc in
        let nt = cur st in
        if nt.kind = Token.COMMA then begin
          bump st;
          loop acc
        end else if nt.kind = Token.RPAREN then begin
          bump st;
          Ok (List.rev acc, nt.span)
        end else
          err_at nt
            (Printf.sprintf "expected ')' or ',' in call arguments, found %s"
               (Token.kind_to_string nt.kind))
    end
  in
  loop []

let parse_left_assoc st ~next_level ~match_op =
  match next_level st with
  | Error e -> Error e
  | Ok (lhs : Ast.expr) ->
    let rec loop (acc : Ast.expr) =
      match match_op (cur st).kind with
      | None -> Ok acc
      | Some op ->
        bump st;
        (match next_level st with
         | Error e -> Error e
         | Ok (rhs : Ast.expr) ->
           let span = Span.merge acc.span rhs.span in
           loop { Ast.desc = Ast.ExprBinary (op, acc, rhs); span })
    in
    loop lhs

let match_rel_op (k : Token.kind) : Ast.binop option =
  match k with
  | Token.EQ_EQ -> Some Ast.BinEq
  | Token.BANG_EQ -> Some Ast.BinNe
  | Token.LT -> Some Ast.BinLt
  | Token.GT -> Some Ast.BinGt
  | Token.LT_EQ -> Some Ast.BinLe
  | Token.GT_EQ -> Some Ast.BinGe
  | _ -> None

let rec parse_expr hooks st : (Ast.expr, Diagnostic.t) result =
  parse_assignment hooks st

and parse_assignment hooks st : (Ast.expr, Diagnostic.t) result =
  match parse_logical_or hooks st with
  | Error e -> Error e
  | Ok lhs ->
    (match match_assign_op (cur st).kind with
     | None -> Ok lhs
     | Some op ->
       bump st;
       (match parse_assignment hooks st with
        | Error e -> Error e
        | Ok rhs ->
          (match expr_to_lvalue lhs with
           | Some lval ->
             Ok { Ast.desc = Ast.ExprAssign (lval, op, rhs); span = Span.merge lhs.span rhs.span }
           | None ->
             let op_str = Ast.assign_op_to_string op in
             Error
               (Diagnostic.error
                  ~hint:(Printf.sprintf "assign to a declared variable or array element, e.g. 'x %s <expr>;' or 'arr[0] %s <expr>;'" op_str op_str)
                  ~code:"E0002"
                  ~span:lhs.span
                  (Printf.sprintf "invalid assignment target: left-hand side of '%s' must be a variable or indexed expression"
                     op_str)))))

and parse_logical_or hooks st =
  parse_left_assoc st
    ~next_level:(parse_logical_xor hooks)
    ~match_op:(function Token.KW_OR -> Some Ast.BinOr | _ -> None)

and parse_logical_xor hooks st =
  parse_left_assoc st
    ~next_level:(parse_logical_and hooks)
    ~match_op:(function Token.KW_XOR -> Some Ast.BinXor | _ -> None)

and parse_logical_and hooks st =
  parse_left_assoc st
    ~next_level:(parse_logical_not hooks)
    ~match_op:(function Token.KW_AND -> Some Ast.BinAnd | _ -> None)

and parse_logical_not hooks st : (Ast.expr, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.KW_NOT ->
    bump st;
    (match parse_logical_not hooks st with
     | Error e -> Error e
     | Ok sub -> Ok { Ast.desc = Ast.ExprUnary (Ast.UnNot, sub); span = Span.merge tok.span sub.span })
  | _ -> parse_relational hooks st

and parse_relational hooks st : (Ast.expr, Diagnostic.t) result =
  match parse_shift hooks st with
  | Error e -> Error e
  | Ok e1 ->
    (match match_rel_op (cur st).kind with
     | None -> Ok e1
     | Some op1 ->
       bump st;
       (match parse_shift hooks st with
        | Error e -> Error e
        | Ok e2 ->
          (match match_rel_op (cur st).kind with
           | None -> Ok { Ast.desc = Ast.ExprBinary (op1, e1, e2); span = Span.merge e1.span e2.span }
           | Some op2 ->
             bump st;
             (match parse_shift hooks st with
              | Error e -> Error e
              | Ok e3 ->
                let tok3 = cur st in
                (match match_rel_op tok3.kind with
                 | Some _ ->
                   err_at tok3
                     ~hint:"chained relational expressions support at most 3 operands (e.g. 'a < b <= c'); combine further conditions with 'and'"
                     "relational chain exceeds maximum of 3 operands"
                 | None ->
                   Ok { Ast.desc = Ast.ExprChainCmp (e1, op1, e2, op2, e3); span = Span.merge e1.span e3.span })))))

and parse_shift hooks st =
  parse_left_assoc st
    ~next_level:(parse_additive hooks)
    ~match_op:(function Token.LSHIFT -> Some Ast.BinShl | Token.RSHIFT -> Some Ast.BinShr | _ -> None)

and parse_additive hooks st =
  parse_left_assoc st
    ~next_level:(parse_mult hooks)
    ~match_op:(function Token.PLUS -> Some Ast.BinAdd | Token.MINUS -> Some Ast.BinSub | _ -> None)

and parse_mult hooks st =
  parse_left_assoc st
    ~next_level:(parse_unary hooks)
    ~match_op:(function
      | Token.STAR -> Some Ast.BinMul
      | Token.SLASH -> Some Ast.BinDiv
      | Token.SLASH_SLASH -> Some Ast.BinIntDiv
      | Token.PERCENT -> Some Ast.BinMod
      | _ -> None)

and parse_unary hooks st : (Ast.expr, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.PLUS | Token.MINUS ->
    let op = if tok.kind = Token.PLUS then Ast.UnPos else Ast.UnNeg in
    bump st;
    (match parse_unary hooks st with
     | Error e -> Error e
     | Ok sub -> Ok { Ast.desc = Ast.ExprUnary (op, sub); span = Span.merge tok.span sub.span })
  | Token.PLUS_PLUS | Token.MINUS_MINUS ->
    let inc_op = if tok.kind = Token.PLUS_PLUS then Ast.PreInc else Ast.PreDec in
    let op_str = Ast.incdec_op_to_string inc_op in
    bump st;
    (match parse_unary hooks st with
     | Error e -> Error e
     | Ok sub ->
       (match expr_to_lvalue sub with
        | Some lval ->
          Ok { Ast.desc = Ast.ExprIncDec (inc_op, lval); span = Span.merge tok.span sub.span }
        | None ->
          Error
            (Diagnostic.error
               ~hint:(Printf.sprintf "apply '%s' directly to a variable or array element, e.g. '%sx' or '%sarr[0]'" op_str op_str op_str)
               ~code:"E0002"
               ~span:sub.span
               (Printf.sprintf "operand of prefix '%s' must be a variable or indexed expression" op_str))))
  | _ -> parse_power hooks st

and parse_power hooks st : (Ast.expr, Diagnostic.t) result =
  match parse_postfix hooks st with
  | Error e -> Error e
  | Ok lhs ->
    (match (cur st).kind with
     | Token.CARET ->
       bump st;
       (match parse_unary hooks st with
        | Error e -> Error e
        | Ok rhs ->
          Ok { Ast.desc = Ast.ExprBinary (Ast.BinPow, lhs, rhs); span = Span.merge lhs.span rhs.span })
     | _ -> Ok lhs)

and parse_postfix hooks st : (Ast.expr, Diagnostic.t) result =
  match Parser_expr_primary.parse_primary hooks st with
  | Error e -> Error e
  | Ok lhs ->
    let rec loop (acc : Ast.expr) : (Ast.expr, Diagnostic.t) result =
      let tok = cur st in
      match tok.kind with
      | Token.LBRACKET ->
        bump st;
        (match hooks.parse_expr st with
         | Error e -> Error e
         | Ok idx_expr ->
           let close_tok = cur st in
           if close_tok.kind = Token.RBRACKET then begin
             bump st;
             loop { Ast.desc = Ast.ExprIndex (acc, idx_expr); span = Span.merge acc.span close_tok.span }
           end else
             err_at close_tok ~hint:"close index access with ']', e.g. 'arr[0]'"
               (Printf.sprintf "expected ']' after index expression, found %s"
                  (Token.kind_to_string close_tok.kind)))
      | Token.LPAREN ->
        bump st;
        (match parse_fn_call_args hooks st with
         | Error e -> Error e
         | Ok (args, close_span) ->
           loop { Ast.desc = Ast.ExprCall (acc, args); span = Span.merge acc.span close_span })
      | Token.PLUS_PLUS | Token.MINUS_MINUS ->
        let inc_op = if tok.kind = Token.PLUS_PLUS then Ast.PostInc else Ast.PostDec in
        let op_str = Ast.incdec_op_to_string inc_op in
        bump st;
        (match expr_to_lvalue acc with
         | Some lval ->
           loop { Ast.desc = Ast.ExprIncDec (inc_op, lval); span = Span.merge acc.span tok.span }
         | None ->
           Error
             (Diagnostic.error
                ~hint:(Printf.sprintf "apply '%s' directly to a variable or array element, e.g. 'x%s' or 'arr[0]%s'" op_str op_str op_str)
                ~code:"E0002"
                ~span:acc.span
                (Printf.sprintf "operand of postfix '%s' must be a variable or indexed expression" op_str)))
      | _ -> Ok acc
    in
    loop lhs

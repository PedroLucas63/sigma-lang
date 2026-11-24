open Sigma_common
open Sigma_diagnostics

type state = {
  tokens : Token.t array;
  mutable idx : int;
}

type hooks = {
  parse_expr : state -> (Ast.expr, Diagnostic.t) result;
  parse_stmt : state -> (Ast.stmt, Diagnostic.t) result;
  parse_non_decl_stmt : state -> (Ast.stmt, Diagnostic.t) result;
  parse_block_stmt : state -> Token.t -> (Ast.stmt, Diagnostic.t) result;
  parse_lambda_expr : state -> Token.t -> is_act:bool -> (Ast.expr, Diagnostic.t) result;
  parse_decl : state -> Token.t -> is_const:bool -> (Ast.stmt, Diagnostic.t) result;
  parse_destruct_names : state -> Token.t -> ((string * Span.t) list, Diagnostic.t) result;
  parse_if_stmt :
    state ->
    Token.t ->
    stmt_parser:(state -> (Ast.stmt, Diagnostic.t) result) ->
    (Ast.stmt, Diagnostic.t) result;
  parse_while_stmt :
    state ->
    Span.t ->
    label:(string * Span.t) option ->
    stmt_parser:(state -> (Ast.stmt, Diagnostic.t) result) ->
    (Ast.stmt, Diagnostic.t) result;
  parse_for_stmt :
    state ->
    Span.t ->
    label:(string * Span.t) option ->
    stmt_parser:(state -> (Ast.stmt, Diagnostic.t) result) ->
    (Ast.stmt, Diagnostic.t) result;
  parse_foreach_stmt :
    state ->
    Span.t ->
    label:(string * Span.t) option ->
    stmt_parser:(state -> (Ast.stmt, Diagnostic.t) result) ->
    (Ast.stmt, Diagnostic.t) result;
  parse_print_stmt : state -> Token.t -> newline:bool -> require_semi:bool -> (Ast.stmt, Diagnostic.t) result;
  parse_assert_stmt : state -> Token.t -> require_semi:bool -> (Ast.stmt, Diagnostic.t) result;
  parse_assert_error_stmt : state -> Token.t -> require_semi:bool -> (Ast.stmt, Diagnostic.t) result;
  parse_break_or_continue_stmt :
    state -> Token.t -> is_break:bool -> require_semi:bool -> (Ast.stmt, Diagnostic.t) result;
}

let cur st : Token.t =
  if st.idx < Array.length st.tokens then st.tokens.(st.idx)
  else st.tokens.(Array.length st.tokens - 1)

let peek_next st : Token.t =
  if st.idx + 1 < Array.length st.tokens then st.tokens.(st.idx + 1)
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

let err_at ?hint ?(code = "E0002") (tok : Token.t) msg =
  Error (Diagnostic.error ?hint ~code ~span:tok.span msg)

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

let finish_inline_stmt st start_span (end_span : Span.t) ~(require_semi : bool)
    : (Span.t, Diagnostic.t) result =
  if require_semi then expect_semi st start_span
  else begin
    let full = Span.merge start_span end_span in
    if (cur st).kind = Token.SEMI && (peek_next st).kind = Token.KW_ELSE then begin
      let s_tok = cur st in
      bump st;
      Ok (Span.merge start_span s_tok.span)
    end else
      Ok full
  end

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
  | Ast.ExprField (base, field) -> Some (Ast.LValField (base, field, e.span))
  | _ -> None

open Sigma_common
open Sigma_diagnostics
open Lexer_state

let keyword_or_ident st word =
  match word with
  | "var" -> Token.KW_VAR
  | "const" -> Token.KW_CONST
  | "if" -> Token.KW_IF
  | "else" -> Token.KW_ELSE
  | "while" -> Token.KW_WHILE
  | "for" -> Token.KW_FOR
  | "foreach" -> Token.KW_FOREACH
  | "in" -> Token.KW_IN
  | "break" -> Token.KW_BREAK
  | "continue" -> Token.KW_CONTINUE
  | "fun" -> Token.KW_FUN
  | "act" -> Token.KW_ACT
  | "return" -> Token.KW_RETURN
  | "ref" -> Token.KW_REF
  | "out" -> Token.KW_OUT
  | "class" -> Token.KW_CLASS
  | "struct" -> Token.KW_STRUCT
  | "record" -> Token.KW_RECORD
  | "new" -> Token.KW_NEW
  | "public" -> Token.KW_PUBLIC
  | "protected" -> Token.KW_PROTECTED
  | "private" -> Token.KW_PRIVATE
  | "internal" -> Token.KW_INTERNAL
  | "input" -> Token.KW_INPUT
  | "prompt" -> Token.KW_PROMPT
  | "print" -> Token.KW_PRINT
  | "println" -> Token.KW_PRINTLN
  | "assert" -> Token.KW_ASSERT
  | "assert_error" -> Token.KW_ASSERT_ERROR
  | "and" ->
    if peek st = '=' && peek_next st <> '=' then (advance st; Token.KW_AND_EQ)
    else Token.KW_AND
  | "or" ->
    if peek st = '=' && peek_next st <> '=' then (advance st; Token.KW_OR_EQ)
    else Token.KW_OR
  | "xor" ->
    if peek st = '=' && peek_next st <> '=' then (advance st; Token.KW_XOR_EQ)
    else Token.KW_XOR
  | "not" -> Token.KW_NOT
  | "true" -> Token.BOOL true
  | "false" -> Token.BOOL false
  | _ -> Token.IDENT word

let read_ident st tokens start_pos =
  let buf = Buffer.create 16 in
  while not (is_eof st) && is_alnum (peek st) do
    Buffer.add_char buf (peek st);
    advance st
  done;
  let kind = keyword_or_ident st (Buffer.contents buf) in
  emit_token st tokens kind start_pos (cur_pos st)

let read_symbol st tokens errors start_pos c =
  let emit k = emit_token st tokens k start_pos (cur_pos st) in
  match c with
  | ':' ->
    advance st;
    if peek st = '=' then (advance st; emit Token.COLON_EQUAL) else emit Token.COLON
  | '=' ->
    advance st;
    if peek st = '=' then (advance st; emit Token.EQ_EQ)
    else if peek st = '>' then (advance st; emit Token.FAT_ARROW)
    else emit Token.EQUAL
  | '!' when peek_next st = '=' ->
    advance st; advance st; emit Token.BANG_EQ
  | '+' ->
    advance st;
    if peek st = '+' then (advance st; emit Token.PLUS_PLUS)
    else if peek st = '=' then (advance st; emit Token.PLUS_EQ)
    else emit Token.PLUS
  | '-' ->
    advance st;
    if peek st = '-' then (advance st; emit Token.MINUS_MINUS)
    else if peek st = '=' then (advance st; emit Token.MINUS_EQ)
    else emit Token.MINUS
  | '*' ->
    advance st;
    if peek st = '=' then (advance st; emit Token.STAR_EQ) else emit Token.STAR
  | '/' ->
    advance st;
    if peek st = '/' then begin
      advance st;
      if peek st = '=' then (advance st; emit Token.SLASH_SLASH_EQ)
      else emit Token.SLASH_SLASH
    end else if peek st = '=' then (advance st; emit Token.SLASH_EQ)
    else emit Token.SLASH
  | '^' ->
    advance st;
    if peek st = '=' then (advance st; emit Token.CARET_EQ) else emit Token.CARET
  | '%' ->
    advance st;
    if peek st = '=' then (advance st; emit Token.PERCENT_EQ) else emit Token.PERCENT
  | '<' ->
    advance st;
    if peek st = '<' then begin
      advance st;
      if peek st = '=' then (advance st; emit Token.LSHIFT_EQ) else emit Token.LSHIFT
    end else if peek st = '=' then (advance st; emit Token.LT_EQ)
    else emit Token.LT
  | '>' ->
    advance st;
    if peek st = '>' then begin
      advance st;
      if peek st = '=' then (advance st; emit Token.RSHIFT_EQ) else emit Token.RSHIFT
    end else if peek st = '=' then (advance st; emit Token.GT_EQ)
    else emit Token.GT
  | '(' -> advance st; emit Token.LPAREN
  | ')' -> advance st; emit Token.RPAREN
  | '{' -> advance st; emit Token.LBRACE
  | '}' -> advance st; emit Token.RBRACE
  | '[' -> advance st; emit Token.LBRACKET
  | ']' -> advance st; emit Token.RBRACKET
  | '.' when peek_next st = '.' ->
    advance st; advance st; emit Token.DOT_DOT
  | '.' -> advance st; emit Token.DOT
  | ',' -> advance st; emit Token.COMMA
  | ';' -> advance st; emit Token.SEMI
  | _ ->
    advance st;
    add_error st errors start_pos (cur_pos st)
      (Printf.sprintf "unexpected character '%c'" c)

let rec tokenize_range (src : Source.t) (start_pos : Span.pos) (end_offset : int)
    : (Token.t list, Diagnostic.t list) result =
  let st =
    {
      src;
      len = end_offset;
      offset = start_pos.offset;
      line = start_pos.line;
      col = start_pos.col;
    }
  in
  let tokens = ref [] in
  let errors = ref [] in
  while not (is_eof st) do
    skip_whitespace_and_comments st errors;
    if not (is_eof st) then begin
      let start_pos = cur_pos st in
      let c = peek st in
      if is_alpha c then read_ident st tokens start_pos
      else if is_digit c then Lexer_literals.read_number st tokens errors start_pos
      else if c = '\'' then Lexer_literals.read_char st tokens errors start_pos
      else if c = '"' then Lexer_literals.read_string ~tokenize_range st tokens errors start_pos
      else read_symbol st tokens errors start_pos c
    end
  done;
  let eof_pos = cur_pos st in
  emit_token st tokens Token.EOF eof_pos eof_pos;
  match List.rev !errors with
  | [] -> Ok (List.rev !tokens)
  | errs -> Error errs

let tokenize (src : Source.t) : (Token.t list, Diagnostic.t list) result =
  tokenize_range src { offset = 0; line = 1; col = 1 } (String.length src.content)

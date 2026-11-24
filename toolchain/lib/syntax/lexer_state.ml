open Sigma_common
open Sigma_diagnostics

type state = {
  src : Source.t;
  len : int;
  mutable offset : int;
  mutable line : int;
  mutable col : int;
}

let cur_pos st : Span.pos =
  { offset = st.offset; line = st.line; col = st.col }

let is_eof st = st.offset >= st.len

let peek st =
  if is_eof st then '\000' else st.src.content.[st.offset]

let peek_next st =
  if st.offset + 1 >= st.len then '\000' else st.src.content.[st.offset + 1]

let advance st =
  if not (is_eof st) then begin
    let c = st.src.content.[st.offset] in
    st.offset <- st.offset + 1;
    if c = '\n' then begin
      st.line <- st.line + 1;
      st.col <- 1
    end else
      st.col <- st.col + 1
  end

let is_alpha c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_digit c =
  c >= '0' && c <= '9'

let is_alnum c = is_alpha c || is_digit c

let emit_token st tokens kind start_pos end_pos =
  let span = Span.make ~file:st.src.filename ~start_pos ~end_pos in
  tokens := { Token.kind; span } :: !tokens

let add_error ?hint st errors start_pos end_pos msg =
  let span = Span.make ~file:st.src.filename ~start_pos ~end_pos in
  errors := Diagnostic.error ?hint ~code:"E0001" ~span msg :: !errors

let skip_whitespace_and_comments st errors =
  let rec loop () =
    if not (is_eof st) then
      match peek st with
      | ' ' | '\t' | '\r' | '\n' ->
        advance st;
        loop ()
      | '#' ->
        while not (is_eof st) && peek st <> '\n' do
          advance st
        done;
        loop ()
      | '/' when peek_next st = '*' ->
        let start_pos = cur_pos st in
        advance st;
        advance st;
        let closed = ref false in
        while not (is_eof st) && not !closed do
          if peek st = '*' && peek_next st = '/' then begin
            advance st;
            advance st;
            closed := true
          end else
            advance st
        done;
        if not !closed then
          add_error st errors start_pos (cur_pos st)
            "unterminated block comment (missing closing '*/')"
        else
          loop ()
      | _ -> ()
  in
  loop ()

let utf8_char_byte_len (c : char) : int =
  let b = Char.code c in
  if b < 0x80 then 1
  else if b >= 0xC0 && b <= 0xDF then 2
  else if b >= 0xE0 && b <= 0xEF then 3
  else if b >= 0xF0 && b <= 0xF7 then 4
  else 1

let read_escape st : (string, Diagnostic.t) result =
  let esc_start = cur_pos st in
  if is_eof st then
    let span = Span.make ~file:st.src.filename ~start_pos:esc_start ~end_pos:esc_start in
    Error (Diagnostic.error ~code:"E0001" ~span "incomplete escape sequence at end of file")
  else
    let ch = peek st in
    advance st;
    match ch with
    | 'n' -> Ok "\n"
    | 'r' -> Ok "\r"
    | 't' -> Ok "\t"
    | '\\' -> Ok "\\"
    | '\'' -> Ok "'"
    | '"' -> Ok "\""
    | '0' -> Ok "\000"
    | '$' -> Ok "$"
    | _ ->
      let span = Span.make ~file:st.src.filename ~start_pos:esc_start ~end_pos:(cur_pos st) in
      Error
        (Diagnostic.error
           ~hint:"supported escape sequences: \\n, \\r, \\t, \\\\, \\', \\\", \\0, \\$"
           ~code:"E0001"
           ~span
           (Printf.sprintf "unknown escape sequence '\\%c'" ch))

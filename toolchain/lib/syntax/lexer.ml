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

let skip_whitespace_and_comments st =
  let rec loop () =
    if not (is_eof st) then
      match peek st with
      | ' ' | '\t' | '\r' | '\n' ->
        advance st;
        loop ()
      | '/' when peek_next st = '/' ->
        while not (is_eof st) && peek st <> '\n' do
          advance st
        done;
        loop ()
      | _ -> ()
  in
  loop ()

let tokenize (src : Source.t) : (Token.t list, Diagnostic.t list) result =
  let st = { src; len = String.length src.content; offset = 0; line = 1; col = 1 } in
  let tokens = ref [] in
  let errors = ref [] in
  let emit kind start_pos end_pos =
    let span = Span.make ~file:src.filename ~start_pos ~end_pos in
    tokens := { Token.kind; span } :: !tokens
  in
  while not (is_eof st) do
    skip_whitespace_and_comments st;
    if not (is_eof st) then begin
      let start_pos = cur_pos st in
      let c = peek st in
      if is_alpha c then begin
        let buf = Buffer.create 16 in
        while not (is_eof st) && is_alnum (peek st) do
          Buffer.add_char buf (peek st);
          advance st
        done;
        let end_pos = cur_pos st in
        let word = Buffer.contents buf in
        let kind =
          match word with
          | "var" -> Token.KW_VAR
          | "true" -> Token.BOOL true
          | "false" -> Token.BOOL false
          | _ -> Token.IDENT word
        in
        emit kind start_pos end_pos
      end else if is_digit c then begin
        let raw_buf = Buffer.create 16 in
        let digits_buf = Buffer.create 16 in
        let prev_underscore = ref false in
        let consecutive_underscore = ref false in
        while not (is_eof st) && (is_digit (peek st) || peek st = '_') do
          let ch = peek st in
          Buffer.add_char raw_buf ch;
          if ch = '_' then begin
            if !prev_underscore then consecutive_underscore := true;
            prev_underscore := true
          end else begin
            Buffer.add_char digits_buf ch;
            prev_underscore := false
          end;
          advance st
        done;
        let end_pos = cur_pos st in
        let raw = Buffer.contents raw_buf in
        let clean_digits = Buffer.contents digits_buf in
        let span = Span.make ~file:src.filename ~start_pos ~end_pos in
        if !prev_underscore then
          errors :=
            Diagnostic.error
              ~code:"E0001"
              ~span
              (Printf.sprintf
                 "invalid trailing underscore '_' in integer literal '%s'"
                 raw)
            :: !errors
        else if !consecutive_underscore then
          errors :=
            Diagnostic.error
              ~code:"E0001"
              ~span
              (Printf.sprintf
                 "consecutive underscores '__' are not allowed in integer literal '%s'"
                 raw)
            :: !errors
        else
          match Int64.of_string_opt clean_digits with
          | Some n -> emit (Token.INT n) start_pos end_pos
          | None ->
            errors :=
              Diagnostic.error
                ~code:"E0001"
                ~span
                (Printf.sprintf
                   "integer literal '%s' overflows 64-bit signed integer"
                   raw)
              :: !errors
      end else begin
        match c with
        | ':' ->
          advance st;
          if peek st = '=' then begin
            advance st;
            emit Token.COLON_EQUAL start_pos (cur_pos st)
          end else
            emit Token.COLON start_pos (cur_pos st)
        | '=' ->
          advance st;
          emit Token.EQUAL start_pos (cur_pos st)
        | '-' ->
          advance st;
          emit Token.MINUS start_pos (cur_pos st)
        | ';' ->
          advance st;
          emit Token.SEMI start_pos (cur_pos st)
        | _ ->
          advance st;
          let end_pos = cur_pos st in
          let span = Span.make ~file:src.filename ~start_pos ~end_pos in
          errors :=
            Diagnostic.error
              ~code:"E0001"
              ~span
              (Printf.sprintf "unexpected character '%c'" c)
            :: !errors
      end
    end
  done;
  let eof_pos = cur_pos st in
  emit Token.EOF eof_pos eof_pos;
  match List.rev !errors with
  | [] -> Ok (List.rev !tokens)
  | errs -> Error errs

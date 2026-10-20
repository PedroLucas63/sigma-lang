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

let round_f32 (f : float) : float =
  Int32.float_of_bits (Int32.bits_of_float f)

type digit_seq_status =
  | Digits_ok
  | Digits_trailing_underscore
  | Digits_consecutive_underscore

let read_digits st raw_buf clean_buf : digit_seq_status =
  let prev_underscore = ref false in
  let consecutive_underscore = ref false in
  while not (is_eof st) && (is_digit (peek st) || peek st = '_') do
    let ch = peek st in
    Buffer.add_char raw_buf ch;
    if ch = '_' then begin
      if !prev_underscore then consecutive_underscore := true;
      prev_underscore := true
    end else begin
      Buffer.add_char clean_buf ch;
      prev_underscore := false
    end;
    advance st
  done;
  if !prev_underscore then Digits_trailing_underscore
  else if !consecutive_underscore then Digits_consecutive_underscore
  else Digits_ok

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
  let emit kind s_pos e_pos =
    let span = Span.make ~file:src.filename ~start_pos:s_pos ~end_pos:e_pos in
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
        let clean_buf = Buffer.create 16 in
        let int_status = read_digits st raw_buf clean_buf in
        let has_dot = ref false in
        let has_exp = ref false in
        let custom_err : string option ref = ref None in
        let set_err msg =
          match !custom_err with
          | None -> custom_err := Some msg
          | Some _ -> ()
        in
        (match int_status with
         | Digits_ok -> ()
         | Digits_trailing_underscore ->
           if not (is_eof st) && peek st = '.' then
             set_err "invalid underscore '_' immediately before '.' in numeric literal"
           else
             set_err "invalid trailing underscore '_' in numeric literal"
         | Digits_consecutive_underscore ->
           set_err "consecutive underscores '__' are not allowed in numeric literal");
        if not (is_eof st) && peek st = '.' then begin
          has_dot := true;
          Buffer.add_char raw_buf '.';
          Buffer.add_char clean_buf '.';
          advance st;
          if not (is_eof st) && peek st = '_' then begin
            set_err "invalid underscore '_' immediately after '.' in floating-point literal";
            while not (is_eof st) && (is_digit (peek st) || peek st = '_') do
              Buffer.add_char raw_buf (peek st);
              advance st
            done
          end else if not (is_eof st) && is_digit (peek st) then begin
            match read_digits st raw_buf clean_buf with
            | Digits_ok -> ()
            | Digits_trailing_underscore ->
              set_err "invalid trailing underscore '_' in fractional part of literal"
            | Digits_consecutive_underscore ->
              set_err "consecutive underscores '__' are not allowed in fractional part of literal"
          end else
            Buffer.add_char clean_buf '0'
        end;
        if not (is_eof st) && (peek st = 'e' || peek st = 'E') then begin
          has_exp := true;
          let e_ch = peek st in
          Buffer.add_char raw_buf e_ch;
          Buffer.add_char clean_buf e_ch;
          advance st;
          if not (is_eof st) && (peek st = '+' || peek st = '-') then begin
            let s_ch = peek st in
            Buffer.add_char raw_buf s_ch;
            Buffer.add_char clean_buf s_ch;
            advance st
          end;
          if is_eof st then
            set_err "incomplete exponent in floating-point literal"
          else if peek st = '_' then begin
            set_err "invalid leading underscore '_' in exponent of floating-point literal";
            while not (is_eof st) && (is_digit (peek st) || peek st = '_') do
              Buffer.add_char raw_buf (peek st);
              advance st
            done
          end else if is_digit (peek st) then begin
            match read_digits st raw_buf clean_buf with
            | Digits_ok -> ()
            | Digits_trailing_underscore ->
              set_err "invalid trailing underscore '_' in exponent of floating-point literal"
            | Digits_consecutive_underscore ->
              set_err "consecutive underscores '__' are not allowed in exponent of floating-point literal"
          end else
            set_err "expected digits in exponent of floating-point literal"
        end;
        if not (is_eof st) && peek st = '.' then begin
          set_err "unexpected second '.' in numeric literal";
          while not (is_eof st) && (is_alnum (peek st) || peek st = '.') do
            Buffer.add_char raw_buf (peek st);
            advance st
          done
        end;
        let suffix_buf = Buffer.create 4 in
        while not (is_eof st) && is_alnum (peek st) do
          let ch = peek st in
          Buffer.add_char raw_buf ch;
          Buffer.add_char suffix_buf ch;
          advance st
        done;
        let suffix = Buffer.contents suffix_buf in
        let end_pos = cur_pos st in
        let raw = Buffer.contents raw_buf in
        let clean = Buffer.contents clean_buf in
        let span = Span.make ~file:src.filename ~start_pos ~end_pos in
        match !custom_err with
        | Some msg ->
          errors :=
            Diagnostic.error
              ~code:"E0001"
              ~span
              (Printf.sprintf "%s '%s'" msg raw)
            :: !errors
        | None ->
          (match suffix with
           | "" when not !has_dot && not !has_exp ->
             (match Int64.of_string_opt clean with
              | Some n -> emit (Token.INT n) start_pos end_pos
              | None ->
                errors :=
                  Diagnostic.error
                    ~code:"E0001"
                    ~span
                    (Printf.sprintf
                       "integer literal '%s' overflows 64-bit signed integer"
                       raw)
                  :: !errors)
           | "" | "f" ->
             (match float_of_string_opt clean with
              | Some f when Float.is_finite f ->
                let f32 = round_f32 f in
                if Float.is_finite f32 then
                  emit (Token.FLOAT f32) start_pos end_pos
                else
                  errors :=
                    Diagnostic.error
                      ~code:"E0001"
                      ~span
                      (Printf.sprintf "float literal '%s' overflows 32-bit float" raw)
                    :: !errors
              | _ ->
                errors :=
                  Diagnostic.error
                    ~code:"E0001"
                    ~span
                    (Printf.sprintf "invalid float literal '%s'" raw)
                  :: !errors)
           | "d" ->
             (match float_of_string_opt clean with
              | Some d when Float.is_finite d ->
                emit (Token.DOUBLE d) start_pos end_pos
              | _ ->
                errors :=
                  Diagnostic.error
                    ~code:"E0001"
                    ~span
                    (Printf.sprintf "double literal '%s' overflows 64-bit double" raw)
                  :: !errors)
           | "m" ->
             (match Decimal.of_string_opt clean with
              | Some m ->
                emit (Token.DECIMAL m) start_pos end_pos
              | None ->
                errors :=
                  Diagnostic.error
                    ~code:"E0001"
                    ~span
                    (Printf.sprintf
                       "decimal literal '%s' overflows 128-bit base-10 decimal (~38 significant digits)"
                       raw)
                  :: !errors)
           | bad_suffix ->
             errors :=
               Diagnostic.error
                 ~hint:"valid floating-point type suffixes are 'f' (float), 'd' (double), and 'm' (decimal)"
                 ~code:"E0001"
                 ~span
                 (Printf.sprintf
                    "invalid suffix '%s' on numeric literal '%s'"
                    bad_suffix raw)
               :: !errors)
      end else if c = '\'' then begin
        advance st;
        let char_buf = Buffer.create 4 in
        let cp_count = ref 0 in
        let esc_err = ref None in
        while not (is_eof st) && peek st <> '\'' && peek st <> '\n' && peek st <> '\r' do
          if peek st = '\\' then begin
            advance st;
            incr cp_count;
            match read_escape st with
            | Ok s -> Buffer.add_string char_buf s
            | Error e -> esc_err := Some e
          end else begin
            let n_bytes = utf8_char_byte_len (peek st) in
            incr cp_count;
            for _ = 1 to n_bytes do
              if not (is_eof st) && peek st <> '\'' && peek st <> '\n' && peek st <> '\r' then begin
                Buffer.add_char char_buf (peek st);
                advance st
              end
            done
          end
        done;
        if is_eof st || peek st <> '\'' then begin
          let span = Span.make ~file:src.filename ~start_pos ~end_pos:(cur_pos st) in
          errors :=
            Diagnostic.error
              ~code:"E0001"
              ~span
              "unterminated character literal (missing closing \"'\")"
            :: !errors
        end else begin
          advance st;
          let end_pos = cur_pos st in
          let span = Span.make ~file:src.filename ~start_pos ~end_pos in
          match !esc_err with
          | Some e -> errors := e :: !errors
          | None ->
            if !cp_count = 0 then
              errors :=
                Diagnostic.error
                  ~code:"E0001"
                  ~span
                  "empty character literal ''"
                :: !errors
            else if !cp_count > 1 then
              errors :=
                Diagnostic.error
                  ~hint:"use double quotes \"...\" for multi-character strings"
                  ~code:"E0001"
                  ~span
                  "character literal may only contain a single Unicode character"
                :: !errors
            else
              emit (Token.CHAR (Buffer.contents char_buf)) start_pos end_pos
        end
      end else if c = '"' then begin
        advance st;
        let lit_buf = Buffer.create 32 in
        let segments = ref [] in
        let has_interp = ref false in
        let str_failed = ref false in
        while not (is_eof st) && peek st <> '"' && peek st <> '\n' && peek st <> '\r' do
          if peek st = '\\' then begin
            advance st;
            match read_escape st with
            | Ok s -> Buffer.add_string lit_buf s
            | Error e ->
              str_failed := true;
              errors := e :: !errors
          end else if peek st = '$' && peek_next st = '{' then begin
            has_interp := true;
            if Buffer.length lit_buf > 0 then begin
              segments := Token.SegLit (Buffer.contents lit_buf) :: !segments;
              Buffer.clear lit_buf
            end;
            let interp_start_pos = cur_pos st in
            advance st;
            advance st;
            let expr_start_pos = cur_pos st in
            while not (is_eof st) && peek st <> '}' && peek st <> '"' && peek st <> '\n' && peek st <> '\r' do
              advance st
            done;
            let expr_end_offset = st.offset in
            if is_eof st || peek st <> '}' then begin
              str_failed := true;
              let span = Span.make ~file:src.filename ~start_pos:interp_start_pos ~end_pos:(cur_pos st) in
              errors :=
                Diagnostic.error
                  ~code:"E0001"
                  ~span
                  "unterminated '${...}' interpolation in string literal"
                :: !errors
            end else begin
              advance st;
              let seg_span = Span.make ~file:src.filename ~start_pos:interp_start_pos ~end_pos:(cur_pos st) in
              match tokenize_range src expr_start_pos expr_end_offset with
              | Ok sub_toks ->
                segments := Token.SegTokens (sub_toks, seg_span) :: !segments
              | Error sub_errs ->
                str_failed := true;
                errors := List.rev_append sub_errs !errors
            end
          end else begin
            Buffer.add_char lit_buf (peek st);
            advance st
          end
        done;
        if is_eof st || peek st <> '"' then begin
          let span = Span.make ~file:src.filename ~start_pos ~end_pos:(cur_pos st) in
          errors :=
            Diagnostic.error
              ~code:"E0001"
              ~span
              "unterminated string literal (missing closing '\"')"
            :: !errors
        end else begin
          advance st;
          let end_pos = cur_pos st in
          if not !str_failed then begin
            if not !has_interp then
              emit (Token.STRING (Buffer.contents lit_buf)) start_pos end_pos
            else begin
              if Buffer.length lit_buf > 0 then
                segments := Token.SegLit (Buffer.contents lit_buf) :: !segments;
              emit (Token.INTERP_STRING (List.rev !segments)) start_pos end_pos
            end
          end
        end
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

let tokenize (src : Source.t) : (Token.t list, Diagnostic.t list) result =
  tokenize_range src { offset = 0; line = 1; col = 1 } (String.length src.content)

open Sigma_common
open Lexer_state

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

let read_number st tokens errors start_pos =
  let raw_buf = Buffer.create 16 in
  let clean_buf = Buffer.create 16 in
  let int_status = read_digits st raw_buf clean_buf in
  let has_dot = ref false in
  let has_exp = ref false in
  let custom_err : string option ref = ref None in
  let set_err msg =
    if Option.is_none !custom_err then custom_err := Some msg
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
  match !custom_err with
  | Some msg -> add_error st errors start_pos end_pos (Printf.sprintf "%s '%s'" msg raw)
  | None ->
    (match suffix with
     | "" when not !has_dot && not !has_exp ->
       (match Int64.of_string_opt clean with
        | Some n -> emit_token st tokens (Token.INT n) start_pos end_pos
        | None ->
          add_error st errors start_pos end_pos
            (Printf.sprintf "integer literal '%s' overflows 64-bit signed integer" raw))
     | "" | "f" ->
       (match float_of_string_opt clean with
        | Some f when Float.is_finite f ->
          let f32 = round_f32 f in
          if Float.is_finite f32 then emit_token st tokens (Token.FLOAT f32) start_pos end_pos
          else add_error st errors start_pos end_pos
                 (Printf.sprintf "float literal '%s' overflows 32-bit float" raw)
        | _ ->
          add_error st errors start_pos end_pos (Printf.sprintf "invalid float literal '%s'" raw))
     | "d" ->
       (match float_of_string_opt clean with
        | Some d when Float.is_finite d -> emit_token st tokens (Token.DOUBLE d) start_pos end_pos
        | _ ->
          add_error st errors start_pos end_pos
            (Printf.sprintf "double literal '%s' overflows 64-bit double" raw))
     | "m" ->
       (match Decimal.of_string_opt clean with
        | Some m -> emit_token st tokens (Token.DECIMAL m) start_pos end_pos
        | None ->
          add_error st errors start_pos end_pos
            (Printf.sprintf
               "decimal literal '%s' overflows 128-bit base-10 decimal (~38 significant digits)"
               raw))
     | bad_suffix ->
       add_error
         ~hint:"valid floating-point type suffixes are 'f' (float), 'd' (double), and 'm' (decimal)"
         st errors start_pos end_pos
         (Printf.sprintf "invalid suffix '%s' on numeric literal '%s'" bad_suffix raw))

let read_char st tokens errors start_pos =
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
  if is_eof st || peek st <> '\'' then
    add_error st errors start_pos (cur_pos st)
      "unterminated character literal (missing closing \"'\")"
  else begin
    advance st;
    let end_pos = cur_pos st in
    match !esc_err with
    | Some e -> errors := e :: !errors
    | None ->
      if !cp_count = 0 then
        add_error st errors start_pos end_pos "empty character literal ''"
      else if !cp_count > 1 then
        add_error ~hint:"use double quotes \"...\" for multi-character strings"
          st errors start_pos end_pos
          "character literal may only contain a single Unicode character"
      else
        emit_token st tokens (Token.CHAR (Buffer.contents char_buf)) start_pos end_pos
  end

let read_string ~tokenize_range st tokens errors start_pos =
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
      let depth = ref 0 in
      let in_char = ref false in
      let colon_split : (int * Span.pos) option ref = ref None in
      while not (is_eof st) && peek st <> '}' && peek st <> '"' && peek st <> '\n' && peek st <> '\r' do
        let ch = peek st in
        if !in_char then begin
          if ch = '\\' && not (is_eof st) then begin
            advance st;
            if not (is_eof st) then advance st
          end else begin
            if ch = '\'' then in_char := false;
            advance st
          end
        end else if Option.is_none !colon_split && !depth = 0 && ch = ':' && peek_next st <> '=' then begin
          let c_offset = st.offset in
          advance st;
          colon_split := Some (c_offset, cur_pos st)
        end else begin
          if ch = '\'' then in_char := true
          else if ch = '(' || ch = '[' then incr depth
          else if (ch = ')' || ch = ']') && !depth > 0 then decr depth;
          advance st
        end
      done;
      let expr_end_offset = st.offset in
      let expr_end_pos = cur_pos st in
      if is_eof st || peek st <> '}' then begin
        str_failed := true;
        add_error st errors interp_start_pos (cur_pos st)
          "unterminated '${...}' interpolation in string literal"
      end else begin
        advance st;
        let seg_span = Span.make ~file:st.src.filename ~start_pos:interp_start_pos ~end_pos:(cur_pos st) in
        let tok_end_offset, fmt_opt =
          match !colon_split with
          | None -> (expr_end_offset, None)
          | Some (c_offset, fmt_start_pos) ->
            let raw_fmt =
              String.sub st.src.content fmt_start_pos.offset (expr_end_offset - fmt_start_pos.offset)
            in
            let fmt_span = Span.make ~file:st.src.filename ~start_pos:fmt_start_pos ~end_pos:expr_end_pos in
            (c_offset, Some (raw_fmt, fmt_span))
        in
        match tokenize_range st.src expr_start_pos tok_end_offset with
        | Ok sub_toks ->
          segments := Token.SegTokens (sub_toks, fmt_opt, seg_span) :: !segments
        | Error sub_errs ->
          str_failed := true;
          errors := List.rev_append sub_errs !errors
      end
    end else begin
      Buffer.add_char lit_buf (peek st);
      advance st
    end
  done;
  if is_eof st || peek st <> '"' then
    add_error st errors start_pos (cur_pos st)
      "unterminated string literal (missing closing '\"')"
  else begin
    advance st;
    let end_pos = cur_pos st in
    if not !str_failed then begin
      if not !has_interp then
        emit_token st tokens (Token.STRING (Buffer.contents lit_buf)) start_pos end_pos
      else begin
        if Buffer.length lit_buf > 0 then
          segments := Token.SegLit (Buffer.contents lit_buf) :: !segments;
        emit_token st tokens (Token.INTERP_STRING (List.rev !segments)) start_pos end_pos
      end
    end
  end

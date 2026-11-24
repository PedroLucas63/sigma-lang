open Sigma_common
open Sigma_diagnostics
open Parser_state

let utf8_chars (s : string) : string list =
  let len = String.length s in
  let rec loop i acc =
    if i >= len then List.rev acc
    else
      let b = Char.code s.[i] in
      let clen =
        if b < 0x80 then 1
        else if b land 0xE0 = 0xC0 then 2
        else if b land 0xF0 = 0xE0 then 3
        else 4
      in
      let clen = if i + clen > len then len - i else clen in
      loop (i + clen) (String.sub s i clen :: acc)
  in
  loop 0 []

let is_ascii_digit_str (s : string) : bool =
  String.length s = 1 && s.[0] >= '0' && s.[0] <= '9'

let parse_align_char (s : string) : Ast.align option =
  match s with
  | "<" -> Some Ast.AlignLeft
  | ">" -> Some Ast.AlignRight
  | "^" -> Some Ast.AlignCenter
  | _ -> None

let take_digits (chars : string list) : string * string list =
  let buf = Buffer.create 8 in
  let rec loop = function
    | c :: rest when is_ascii_digit_str c ->
      Buffer.add_string buf c;
      loop rest
    | rest -> (Buffer.contents buf, rest)
  in
  loop chars

let parse_format_spec (raw_spec : string) (fmt_span : Span.t)
    : (Ast.format_spec, Diagnostic.t) result =
  let trimmed = String.trim raw_spec in
  if String.length trimmed = 0 then
    Error
      (Diagnostic.error
         ~hint:"provide a format specifier after ':', e.g. '${x:.2}', '${x:>10}', or remove ':'"
         ~code:"E0002"
         ~span:fmt_span
         "empty format specifier after ':' inside string interpolation")
  else
    let chars = utf8_chars trimmed in
    let fill_opt, align, chars =
      match chars with
      | f :: a :: rest when parse_align_char a <> None ->
        (Some f, parse_align_char a, rest)
      | a :: rest when parse_align_char a <> None ->
        (None, parse_align_char a, rest)
      | _ -> (None, None, chars)
    in
    let sign_plus, chars =
      match chars with
      | "+" :: rest -> (true, rest)
      | _ -> (false, chars)
    in
    let zero_pad, chars =
      match chars with
      | "0" :: rest -> (true, rest)
      | _ -> (false, chars)
    in
    let width_digits, chars = take_digits chars in
    let width =
      if width_digits = "" then None else Some (int_of_string width_digits)
    in
    let prec_res =
      match chars with
      | "." :: rest ->
        let prec_digits, rest2 = take_digits rest in
        if prec_digits = "" then
          Error
            (Diagnostic.error
               ~hint:"specify the number of digits after '.', e.g. '${pi:.2}'"
               ~code:"E0002"
               ~span:fmt_span
               (Printf.sprintf "invalid format specifier ':%s': expected precision digits after '.'"
                  trimmed))
        else Ok (Some (int_of_string prec_digits), rest2)
      | _ -> Ok (None, chars)
    in
    match prec_res with
    | Error e -> Error e
    | Ok (precision, leftover) ->
      if leftover <> [] then
        Error
          (Diagnostic.error
             ~hint:"valid format syntax is '[fill][<|>|^][+][0][width][.precision]', e.g. '${x:.2}', '${x:>10}', '${x:*^10}', '${x:+08}'"
             ~code:"E0002"
             ~span:fmt_span
             (Printf.sprintf "invalid format specifier ':%s' in string interpolation"
                trimmed))
      else
        let fill =
          match fill_opt with
          | Some f -> f
          | None -> if zero_pad && align = None then "0" else " "
        in
        Ok { Ast.fill; align; sign_plus; zero_pad; width; precision; span = fmt_span }

let parse_mask_segments (segments : Token.interp_segment list) (str_span : Span.t)
    : (Ast.input_mode, Diagnostic.t) result =
  let rec loop acc has_type = function
    | [] ->
      if not has_type then
        Error
          (Diagnostic.error
             ~hint:"include at least one '${type}' placeholder, e.g. '\"${int}.${int}\"'"
             ~code:"E0002"
             ~span:str_span
             "input mask must contain at least one '${type}' placeholder")
      else Ok (Ast.InputMask (List.rev acc))
    | Token.SegLit "" :: rest -> loop acc has_type rest
    | Token.SegLit s :: rest -> loop (Ast.MaskLit s :: acc) has_type rest
    | Token.SegTokens (sub_toks, fmt_opt, seg_span) :: rest ->
      (match fmt_opt with
       | Some (_, fmt_span) ->
         Error
           (Diagnostic.error
              ~hint:"use plain type placeholders like '${int}' or '${float}' in input masks"
              ~code:"E0002"
              ~span:fmt_span
              "format specifiers are not allowed inside input mask placeholders")
       | None ->
         (match sub_toks with
          | [ { Token.kind = Token.EOF; _ } ] ->
            Error
              (Diagnostic.error
                 ~hint:"specify a type inside '${...}', e.g. '${int}' or '${string}'"
                 ~code:"E0002"
                 ~span:seg_span
                 "empty placeholder '${}' in input mask")
          | [ { Token.kind = Token.IDENT ty_name; span = ty_span }; { Token.kind = Token.EOF; _ } ] ->
            loop (Ast.MaskType (ty_name, ty_span) :: acc) true rest
          | _ ->
            Error
              (Diagnostic.error
                 ~hint:"input mask placeholders must be primitive type names like '${int}', '${float}', '${string}'"
                 ~code:"E0002"
                 ~span:seg_span
                 "expected primitive type name inside input mask placeholder '${...}'")))
  in
  loop [] false segments

let parse_input_mode_args st ~kw_name : (Ast.input_mode, Diagnostic.t) result =
  let tok = cur st in
  match tok.kind with
  | Token.STRING _ ->
    if kw_name = "input" then
      err_at tok
        ~hint:"use 'prompt(\"...\")' to display a prompt message, or include '${type}' placeholders for a mask like 'input(\"${int}-${int}\")'"
        "string literal in 'input(...)' must contain at least one '${type}' mask placeholder; to display a prompt message, use 'prompt(...)'"
    else
      err_at tok
        ~hint:"include at least one '${type}' placeholder in the mask string, e.g. 'prompt(\"CPF: \", \"${int}.${int}.${int}-${int}\")'"
        "mask string in 'prompt(...)' must contain at least one '${type}' placeholder"
  | Token.INTERP_STRING segments ->
    bump st;
    parse_mask_segments segments tok.span
  | Token.IDENT first_ty ->
    bump st;
    let rec loop acc =
      let next_tok = cur st in
      match next_tok.kind with
      | Token.COMMA ->
        bump st;
        let t_tok = cur st in
        (match t_tok.kind with
         | Token.IDENT ty_name ->
           bump st;
           loop ((ty_name, t_tok.span) :: acc)
         | _ ->
           err_at t_tok
             ~hint:(Printf.sprintf "pass primitive type names separated by commas, e.g. '%s(int, float)'" kw_name)
             (Printf.sprintf "expected type name after ',' in '%s(...)', found %s"
                kw_name (Token.kind_to_string t_tok.kind)))
      | _ -> Ok (Ast.InputTypes (List.rev acc))
    in
    loop [ (first_ty, tok.span) ]
  | _ ->
    err_at tok
      ~hint:(Printf.sprintf "pass a primitive type (e.g. '%s(int)'), multiple types ('%s(int, float)'), or a mask string ('%s(\"${int}/${int}\")')" kw_name kw_name kw_name)
      (Printf.sprintf "expected type name or mask string in '%s(...)', found %s"
         kw_name (Token.kind_to_string tok.kind))

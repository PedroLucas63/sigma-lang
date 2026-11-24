open Sigma_common
open Sigma_syntax
open Vm_math
open Vm_state

let utf8_char_list (s : string) : string list =
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
      let clen = min clen (len - i) in
      loop (i + clen) (String.sub s i clen :: acc)
  in
  loop 0 []

let utf8_code_point_len (s : string) : int =
  let len = String.length s in
  let rec loop i count =
    if i >= len then count
    else
      let b = Char.code s.[i] in
      let clen =
        if b < 0x80 then 1
        else if b land 0xE0 = 0xC0 then 2
        else if b land 0xF0 = 0xE0 then 3
        else 4
      in
      loop (i + clen) (count + 1)
  in
  loop 0 0

let utf8_truncate (s : string) (max_chars : int) : string =
  if max_chars <= 0 then ""
  else
    let chars = utf8_char_list s in
    let rec take n acc = function
      | [] -> String.concat "" (List.rev acc)
      | _ when n <= 0 -> String.concat "" (List.rev acc)
      | c :: rest -> take (n - 1) (c :: acc) rest
    in
    take max_chars [] chars

let format_decimal_unsigned (raw : string) (prec_opt : int option) : string =
  match prec_opt with
  | None -> raw
  | Some p ->
    let int_part, frac_part =
      match String.index_opt raw '.' with
      | None -> (raw, "")
      | Some idx ->
        (String.sub raw 0 idx, String.sub raw (idx + 1) (String.length raw - idx - 1))
    in
    let flen = String.length frac_part in
    if flen <= p then
      if p = 0 then int_part else int_part ^ "." ^ frac_part ^ String.make (p - flen) '0'
    else
      let kept_frac = String.sub frac_part 0 p in
      let combined = Bytes.of_string (int_part ^ kept_frac) in
      let round_up = frac_part.[p] >= '5' in
      let final_digits =
        if not round_up then Bytes.to_string combined
        else begin
          let i = ref (Bytes.length combined - 1) in
          let carry = ref true in
          while !i >= 0 && !carry do
            let d = Char.code (Bytes.get combined !i) - Char.code '0' + 1 in
            if d = 10 then (Bytes.set combined !i '0'; decr i)
            else (Bytes.set combined !i (Char.chr (Char.code '0' + d)); carry := false)
          done;
          if !carry then "1" ^ Bytes.to_string combined else Bytes.to_string combined
        end
      in
      if p = 0 then final_digits
      else
        let dlen = String.length final_digits in
        String.sub final_digits 0 (dlen - p) ^ "." ^ String.sub final_digits (dlen - p) p

let format_value (spec : Ast.format_spec) (v : value) : string =
  let sign_str, digits_str =
    match v with
    | VInt n ->
      let is_neg = n < 0L in
      let abs_s = if n = Int64.min_int then "9223372036854775808" else Int64.to_string (Int64.abs n) in
      let sign = if is_neg then "-" else if spec.sign_plus then "+" else "" in
      (sign, abs_s)
    | VFloat f ->
      let is_neg = Float.sign_bit f && not (Float.is_nan f) in
      let abs_f = Float.abs f in
      let body =
        match spec.precision with
        | Some p -> Printf.sprintf "%.*f" p abs_f
        | None -> Printf.sprintf "%.6g" abs_f
      in
      let sign = if is_neg then "-" else if spec.sign_plus then "+" else "" in
      (sign, body)
    | VDouble d ->
      let is_neg = Float.sign_bit d && not (Float.is_nan d) in
      let abs_d = Float.abs d in
      let body =
        match spec.precision with
        | Some p -> Printf.sprintf "%.*f" p abs_d
        | None -> Printf.sprintf "%.15g" abs_d
      in
      let sign = if is_neg then "-" else if spec.sign_plus then "+" else "" in
      (sign, body)
    | VDecimal m ->
      let is_neg = m.neg && not (Decimal.is_zero m) in
      let raw = Decimal.to_string (if is_neg then Decimal.negate m else m) in
      let body = format_decimal_unsigned raw spec.precision in
      let sign = if is_neg then "-" else if spec.sign_plus then "+" else "" in
      (sign, body)
    | VString s ->
      let body = match spec.precision with Some p -> utf8_truncate s p | None -> s in
      ("", body)
    | _ -> ("", value_to_string v)
  in
  if spec.zero_pad && spec.align = None then
    let cur_len = String.length sign_str + utf8_code_point_len digits_str in
    match spec.width with
    | Some w when w > cur_len -> sign_str ^ String.make (w - cur_len) '0' ^ digits_str
    | _ -> sign_str ^ digits_str
  else
    let full_body = sign_str ^ digits_str in
    let cur_len = utf8_code_point_len full_body in
    match spec.width with
    | None -> full_body
    | Some w when w <= cur_len -> full_body
    | Some w ->
      let pad_total = w - cur_len in
      (match spec.align with
       | Some Ast.AlignLeft -> full_body ^ repeat_str spec.fill pad_total
       | Some Ast.AlignCenter ->
         let left_pad = pad_total / 2 in
         repeat_str spec.fill left_pad ^ full_body ^ repeat_str spec.fill (pad_total - left_pad)
       | Some Ast.AlignRight | None -> repeat_str spec.fill pad_total ^ full_body)

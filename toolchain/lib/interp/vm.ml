open Sigma_common
open Sigma_syntax
open Sigma_sema
open Sigma_ir

type value =
  | VInt of int64
  | VFloat of float
  | VDouble of float
  | VDecimal of Decimal.t
  | VBool of bool
  | VChar of string
  | VString of string
  | VTuple of value array
  | VArray of value array

let round_f32 (f : float) : float =
  Int32.float_of_bits (Int32.bits_of_float f)

let rec value_to_string = function
  | VInt n -> Int64.to_string n
  | VFloat f -> Printf.sprintf "%.6g" f
  | VDouble d -> Printf.sprintf "%.15g" d
  | VDecimal m -> Decimal.to_string m
  | VBool b -> if b then "true" else "false"
  | VChar c -> c
  | VString s -> s
  | VTuple arr ->
    "(" ^ String.concat ", " (List.map value_to_string (Array.to_list arr)) ^ ")"
  | VArray arr ->
    "[" ^ String.concat ", " (List.map value_to_string (Array.to_list arr)) ^ "]"

let decode_utf8 (s : string) : int =
  let len = String.length s in
  if len = 0 then 0
  else
    let b0 = Char.code s.[0] in
    if b0 < 0x80 then b0
    else if b0 < 0xE0 && len >= 2 then
      ((b0 land 0x1F) lsl 6) lor (Char.code s.[1] land 0x3F)
    else if b0 < 0xF0 && len >= 3 then
      ((b0 land 0x0F) lsl 12)
      lor ((Char.code s.[1] land 0x3F) lsl 6)
      lor (Char.code s.[2] land 0x3F)
    else if len >= 4 then
      ((b0 land 0x07) lsl 18)
      lor ((Char.code s.[1] land 0x3F) lsl 12)
      lor ((Char.code s.[2] land 0x3F) lsl 6)
      lor (Char.code s.[3] land 0x3F)
    else b0

let encode_utf8 (cp : int) : (string, string) result =
  if cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF) then
    Error (Printf.sprintf "Invalid Unicode code point U+%04X in char arithmetic" cp)
  else if cp < 0x80 then
    Ok (String.make 1 (Char.chr cp))
  else if cp < 0x800 then
    let b = Bytes.create 2 in
    Bytes.set b 0 (Char.chr (0xC0 lor (cp lsr 6)));
    Bytes.set b 1 (Char.chr (0x80 lor (cp land 0x3F)));
    Ok (Bytes.unsafe_to_string b)
  else if cp < 0x10000 then
    let b = Bytes.create 3 in
    Bytes.set b 0 (Char.chr (0xE0 lor (cp lsr 12)));
    Bytes.set b 1 (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Bytes.set b 2 (Char.chr (0x80 lor (cp land 0x3F)));
    Ok (Bytes.unsafe_to_string b)
  else
    let b = Bytes.create 4 in
    Bytes.set b 0 (Char.chr (0xF0 lor (cp lsr 18)));
    Bytes.set b 1 (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Bytes.set b 2 (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Bytes.set b 3 (Char.chr (0x80 lor (cp land 0x3F)));
    Ok (Bytes.unsafe_to_string b)

let pow_i64 (base : int64) (exp : int64) : int64 =
  if exp < 0L then
    if base = 1L then 1L
    else if base = -1L then
      if Int64.rem exp 2L = 0L then 1L else -1L
    else 0L
  else
    let b = ref base in
    let e = ref exp in
    let acc = ref 1L in
    while !e > 0L do
      if Int64.logand !e 1L <> 0L then acc := Int64.mul !acc !b;
      b := Int64.mul !b !b;
      e := Int64.shift_right_logical !e 1
    done;
    !acc

let mod_euclid_i64 (a : int64) (b : int64) : (int64, string) result =
  if b = 0L then Error "Modulo by zero"
  else
    let r = Int64.rem a b in
    let abs_b = if b = Int64.min_int then Int64.max_int else Int64.abs b in
    Ok (if r < 0L then Int64.add r abs_b else r)

let mod_euclid_float (a : float) (b : float) : (float, string) result =
  if b = 0.0 then Error "Modulo by zero"
  else
    let abs_b = Float.abs b in
    let r = Float.rem a abs_b in
    let r = if r < 0.0 then r +. abs_b else r in
    Ok (if r = 0.0 then 0.0 else r)

type assertion_result = {
  passed : bool;
  span : Span.t;
  expected : string;
  actual : string;
  message : string;
}

type test_report = {
  stdout : string;
  passed_count : int;
  failed_count : int;
  assertions : assertion_result list;
}

let rec value_equal (a : value) (b : value) : bool =
  match a, b with
  | VInt x, VInt y -> x = y
  | VFloat x, VFloat y -> x = y
  | VDouble x, VDouble y -> x = y
  | VDecimal x, VDecimal y -> Decimal.compare x y = 0
  | VBool x, VBool y -> x = y
  | VChar x, VChar y -> decode_utf8 x = decode_utf8 y
  | VString x, VString y -> String.equal x y
  | VTuple xs, VTuple ys | VArray xs, VArray ys ->
    let len = Array.length xs in
    if len <> Array.length ys then false
    else
      let rec loop i =
        if i >= len then true
        else if value_equal xs.(i) ys.(i) then loop (i + 1)
        else false
      in
      loop 0
  | _ -> false

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
      let clen = if i + clen > len then len - i else clen in
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

let repeat_str (s : string) (n : int) : string =
  if n <= 0 then ""
  else
    let buf = Buffer.create (String.length s * n) in
    for _ = 1 to n do
      Buffer.add_string buf s
    done;
    Buffer.contents buf

let format_decimal_unsigned (raw : string) (prec_opt : int option) : string =
  match prec_opt with
  | None -> raw
  | Some p ->
    let int_part, frac_part =
      match String.index_opt raw '.' with
      | None -> (raw, "")
      | Some idx ->
        ( String.sub raw 0 idx,
          String.sub raw (idx + 1) (String.length raw - idx - 1) )
    in
    let flen = String.length frac_part in
    if flen <= p then
      if p = 0 then int_part
      else int_part ^ "." ^ frac_part ^ String.make (p - flen) '0'
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
            if d = 10 then begin
              Bytes.set combined !i '0';
              decr i
            end else begin
              Bytes.set combined !i (Char.chr (Char.code '0' + d));
              carry := false
            end
          done;
          if !carry then "1" ^ Bytes.to_string combined
          else Bytes.to_string combined
        end
      in
      if p = 0 then final_digits
      else
        let dlen = String.length final_digits in
        String.sub final_digits 0 (dlen - p)
        ^ "."
        ^ String.sub final_digits (dlen - p) p

let format_value (spec : Ast.format_spec) (v : value) : string =
  let sign_str, digits_str =
    match v with
    | VInt n ->
      let is_neg = n < 0L in
      let abs_s =
        if n = Int64.min_int then "9223372036854775808"
        else Int64.to_string (Int64.abs n)
      in
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
      let body =
        match spec.precision with
        | Some p -> utf8_truncate s p
        | None -> s
      in
      ("", body)
    | _ -> ("", value_to_string v)
  in
  if spec.zero_pad && spec.align = None then
    let cur_len = String.length sign_str + utf8_code_point_len digits_str in
    match spec.width with
    | Some w when w > cur_len ->
      sign_str ^ String.make (w - cur_len) '0' ^ digits_str
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
       | Some Ast.AlignLeft ->
         full_body ^ repeat_str spec.fill pad_total
       | Some Ast.AlignCenter ->
         let left_pad = pad_total / 2 in
         let right_pad = pad_total - left_pad in
         repeat_str spec.fill left_pad ^ full_body ^ repeat_str spec.fill right_pad
       | Some Ast.AlignRight | None ->
         repeat_str spec.fill pad_total ^ full_body)

type state = {
  mutable ip : int;
  mutable stack : value list;
  globals : (string, value) Hashtbl.t;
  mutable halted : bool;
  mutable collect_assertions : bool;
  mutable assertions_rev : assertion_result list;
  mutable stdin_lines : string list option;
}

let create_state ?(collect_assertions = false) ?stdin_lines () =
  {
    ip = 0;
    stack = [];
    globals = Hashtbl.create 16;
    halted = false;
    collect_assertions;
    assertions_rev = [];
    stdin_lines;
  }

let strip_cr (s : string) : string =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\r' then String.sub s 0 (len - 1) else s

let read_next_line (st : state) : (string, string) result =
  match st.stdin_lines with
  | Some (line :: rest) ->
    st.stdin_lines <- Some rest;
    Ok (strip_cr line)
  | Some [] ->
    Error "Runtime error: unexpected end of input (EOF) while reading from stdin"
  | None ->
    (try
       flush stdout;
       let line = input_line stdin in
       Ok (strip_cr line)
     with End_of_file ->
       Error "Runtime error: unexpected end of input (EOF) while reading from stdin")

let is_strict_base10_int (s : string) : bool =
  let len = String.length s in
  if len = 0 then false
  else
    let start = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
    if start >= len then false
    else
      let rec check i =
        if i >= len then true
        else if s.[i] >= '0' && s.[i] <= '9' then check (i + 1)
        else false
      in
      check start

let parse_prim_value (ty : Ty.t) (raw : string) : (value, string) result =
  match ty with
  | Ty.Int ->
    let s = String.trim raw in
    if is_strict_base10_int s then
      match Int64.of_string_opt s with
      | Some n -> Ok (VInt n)
      | None -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'int'" raw)
    else Error (Printf.sprintf "Runtime error: failed to parse %S as 'int'" raw)
  | Ty.Float ->
    let s = String.trim raw in
    if s = "" then Error (Printf.sprintf "Runtime error: failed to parse %S as 'float'" raw)
    else
      (match Float.of_string_opt s with
       | Some f when not (Float.is_nan f || Float.is_infinite f) ->
         Ok (VFloat (round_f32 f))
       | _ -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'float'" raw))
  | Ty.Double ->
    let s = String.trim raw in
    if s = "" then Error (Printf.sprintf "Runtime error: failed to parse %S as 'double'" raw)
    else
      (match Float.of_string_opt s with
       | Some d when not (Float.is_nan d || Float.is_infinite d) ->
         Ok (VDouble d)
       | _ -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'double'" raw))
  | Ty.Decimal ->
    let s = String.trim raw in
    let s_clean =
      if String.length s > 1 && s.[0] = '+' then
        String.sub s 1 (String.length s - 1)
      else s
    in
    (match Decimal.of_string_opt s_clean with
     | Some m -> Ok (VDecimal m)
     | None -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'decimal'" raw))
  | Ty.Bool ->
    let s = String.trim raw in
    (match s with
     | "true" -> Ok (VBool true)
     | "false" -> Ok (VBool false)
     | _ -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'bool' (expected 'true' or 'false')" raw))
  | Ty.Char ->
    if utf8_code_point_len raw = 1 then Ok (VChar raw)
    else
      let trimmed = String.trim raw in
      if utf8_code_point_len trimmed = 1 then Ok (VChar trimmed)
      else Error (Printf.sprintf "Runtime error: failed to parse %S as 'char' (expected a single character)" raw)
  | Ty.String -> Ok (VString raw)
  | Ty.Tuple _ | Ty.Array _ ->
    Error "Runtime error: composite types are not supported in primitive input parsing"

let split_whitespace_tokens (s : string) : string list =
  let len = String.length s in
  let is_ws c = c = ' ' || c = '\t' in
  let rec skip_ws i =
    if i < len && is_ws s.[i] then skip_ws (i + 1) else i
  in
  let rec take_word i =
    if i < len && not (is_ws s.[i]) then take_word (i + 1) else i
  in
  let rec loop i acc =
    let start = skip_ws i in
    if start >= len then List.rev acc
    else
      let stop = take_word start in
      loop stop (String.sub s start (stop - start) :: acc)
  in
  loop 0 []

let find_substring_from (haystack : string) (start_pos : int) (needle : string) : int option =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  if nlen = 0 then Some start_pos
  else if start_pos + nlen > hlen then None
  else
    let rec loop i =
      if i + nlen > hlen then None
      else if String.sub haystack i nlen = needle then Some i
      else loop (i + 1)
    in
    loop start_pos

let exec_input (st : state) (spec : Ir.input_spec) : (value, string) result =
  match read_next_line st with
  | Error msg -> Error msg
  | Ok line ->
    (match spec with
     | Ir.IrInputLine -> Ok (VString line)
     | Ir.IrInputTypes [ single_ty ] ->
       let raw = if single_ty = Ty.String then String.trim line else line in
       parse_prim_value single_ty raw
     | Ir.IrInputTypes tys ->
       let toks = split_whitespace_tokens line in
       let expected_n = List.length tys in
       let actual_n = List.length toks in
       if actual_n <> expected_n then
         Error
           (Printf.sprintf
              "Runtime error: input expected %d whitespace-separated values, found %d in %S"
              expected_n actual_n line)
       else
         let rec parse_all acc ty_l tok_l =
           match ty_l, tok_l with
           | [], [] -> Ok (VTuple (Array.of_list (List.rev acc)))
           | ty :: rest_ty, tok :: rest_tok ->
             (match parse_prim_value ty tok with
              | Error msg -> Error msg
              | Ok v -> parse_all (v :: acc) rest_ty rest_tok)
           | _ -> Error "Runtime error: internal input token mismatch"
         in
         parse_all [] tys toks
     | Ir.IrInputMask parts ->
       let hlen = String.length line in
       let rec match_parts pos acc = function
         | [] ->
           if pos <> hlen then
             Error
               (Printf.sprintf
                  "Runtime error: unexpected trailing input %S after matching mask in %S"
                  (String.sub line pos (hlen - pos))
                  line)
           else
             let vals = List.rev acc in
             (match vals with
              | [ single ] -> Ok single
              | _ -> Ok (VTuple (Array.of_list vals)))
         | Ir.IrMaskLit lit :: rest ->
           let llen = String.length lit in
           if pos + llen <= hlen && String.sub line pos llen = lit then
             match_parts (pos + llen) acc rest
           else
             Error
               (Printf.sprintf
                  "Runtime error: input %S does not match mask delimiter %S at position %d"
                  line lit pos)
         | Ir.IrMaskType ty :: rest ->
           (match rest with
            | [] ->
              let sub = String.sub line pos (hlen - pos) in
              (match parse_prim_value ty sub with
               | Error msg -> Error msg
               | Ok v -> match_parts hlen (v :: acc) [])
            | Ir.IrMaskLit next_lit :: _ ->
              (match find_substring_from line pos next_lit with
               | None ->
                 Error
                   (Printf.sprintf
                      "Runtime error: input %S is missing mask delimiter %S"
                      line next_lit)
               | Some found_idx ->
                 let sub = String.sub line pos (found_idx - pos) in
                 (match parse_prim_value ty sub with
                  | Error msg -> Error msg
                  | Ok v -> match_parts found_idx (v :: acc) rest))
            | Ir.IrMaskType _ :: _ ->
              Error "Runtime error: invalid adjacent mask placeholders")
       in
       match_parts 0 [] parts)

let step ~out_buf (prog : Ir.program) (st : state) : (unit, string) result =
  if st.halted || st.ip >= Array.length prog then begin
    st.halted <- true;
    Ok ()
  end else begin
    let instr = prog.(st.ip) in
    st.ip <- st.ip + 1;
    match instr.op with
    | Ir.OP_CONST_I64 n ->
      st.stack <- VInt n :: st.stack;
      Ok ()
    | Ir.OP_CONST_F32 f ->
      st.stack <- VFloat f :: st.stack;
      Ok ()
    | Ir.OP_CONST_F64 d ->
      st.stack <- VDouble d :: st.stack;
      Ok ()
    | Ir.OP_CONST_DEC m ->
      st.stack <- VDecimal m :: st.stack;
      Ok ()
    | Ir.OP_CONST_BOOL b ->
      st.stack <- VBool b :: st.stack;
      Ok ()
    | Ir.OP_CONST_CHAR c ->
      st.stack <- VChar c :: st.stack;
      Ok ()
    | Ir.OP_CONST_STR s ->
      st.stack <- VString s :: st.stack;
      Ok ()
    | Ir.OP_MAKE_TUPLE count ->
      let rec pop_n n acc stack =
        if n <= 0 then Some (acc, stack)
        else
          match stack with
          | v :: rest -> pop_n (n - 1) (v :: acc) rest
          | [] -> None
      in
      (match pop_n count [] st.stack with
       | Some (items, rest) ->
         st.stack <- VTuple (Array.of_list items) :: rest;
         Ok ()
       | None -> Error "VM stack underflow on OP_MAKE_TUPLE")
    | Ir.OP_MAKE_ARRAY count ->
      let rec pop_n n acc stack =
        if n <= 0 then Some (acc, stack)
        else
          match stack with
          | v :: rest -> pop_n (n - 1) (v :: acc) rest
          | [] -> None
      in
      (match pop_n count [] st.stack with
       | Some (items, rest) ->
         st.stack <- VArray (Array.of_list items) :: rest;
         Ok ()
       | None -> Error "VM stack underflow on OP_MAKE_ARRAY")
    | Ir.OP_SPREAD ->
      (match st.stack with
       | (VTuple arr | VArray arr) :: rest ->
         st.stack <- rest;
         Array.iter (fun item -> st.stack <- item :: st.stack) arr;
         Ok ()
       | _ -> Error "VM type error or stack underflow on OP_SPREAD")
    | Ir.OP_INDEX_GET ->
      (match st.stack with
       | VInt idx :: (VTuple arr | VArray arr) :: rest ->
         let len = Array.length arr in
         if idx < 0L || idx >= Int64.of_int len then
           Error
             (Printf.sprintf "Runtime error: index %Ld out of bounds (length %d)" idx len)
         else begin
           st.stack <- arr.(Int64.to_int idx) :: rest;
           Ok ()
         end
       | _ -> Error "VM type error or stack underflow on OP_INDEX_GET")
    | Ir.OP_INDEX_SET ->
      (match st.stack with
       | v :: VInt idx :: VArray arr :: rest ->
         let len = Array.length arr in
         if idx < 0L || idx >= Int64.of_int len then
           Error
             (Printf.sprintf "Runtime error: index %Ld out of bounds (length %d)" idx len)
         else begin
           arr.(Int64.to_int idx) <- v;
           st.stack <- v :: rest;
           Ok ()
         end
       | _ -> Error "VM type error or stack underflow on OP_INDEX_SET")
    | Ir.OP_ARRAY_CONCAT ->
      (match st.stack with
       | VArray b :: VArray a :: rest ->
         st.stack <- VArray (Array.append a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_ARRAY_CONCAT")
    | Ir.OP_ARRAY_REPEAT ->
      (match st.stack with
       | VInt count :: VArray a :: rest ->
         let n = Int64.to_int count in
         let len = Array.length a in
         if n <= 0 || len = 0 then begin
           st.stack <- VArray [||] :: rest;
           Ok ()
         end else begin
           let out = Array.init (len * n) (fun i -> a.(i mod len)) in
           st.stack <- VArray out :: rest;
           Ok ()
         end
       | _ -> Error "VM type error on OP_ARRAY_REPEAT")
    | Ir.OP_TO_STR ->
      (match st.stack with
       | v :: rest ->
         st.stack <- VString (value_to_string v) :: rest;
         Ok ()
       | [] -> Error "VM stack underflow on OP_TO_STR")
    | Ir.OP_FMT_SPEC spec ->
      (match st.stack with
       | v :: rest ->
         st.stack <- VString (format_value spec v) :: rest;
         Ok ()
       | [] -> Error "VM stack underflow on OP_FMT_SPEC")
    | Ir.OP_CONCAT_STR count ->
      let rec pop_n n acc stack =
        if n <= 0 then Some (acc, stack)
        else
          match stack with
          | VString s :: rest -> pop_n (n - 1) (s :: acc) rest
          | _ -> None
      in
      (match pop_n count [] st.stack with
       | Some (parts, rest) ->
         st.stack <- VString (String.concat "" parts) :: rest;
         Ok ()
       | None -> Error "VM stack underflow or type error on OP_CONCAT_STR")
    | Ir.OP_INPUT spec ->
      (match exec_input st spec with
       | Ok v ->
         st.stack <- v :: st.stack;
         Ok ()
       | Error msg -> Error msg)
    | Ir.OP_I64_TO_F32 ->
      (match st.stack with
       | VInt n :: rest ->
         st.stack <- VFloat (round_f32 (Int64.to_float n)) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_I64_TO_F32")
    | Ir.OP_I64_TO_F64 ->
      (match st.stack with
       | VInt n :: rest ->
         st.stack <- VDouble (Int64.to_float n) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_I64_TO_F64")
    | Ir.OP_I64_TO_DEC ->
      (match st.stack with
       | VInt n :: rest ->
         st.stack <- VDecimal (Decimal.of_int64 n) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_I64_TO_DEC")
    | Ir.OP_F32_TO_F64 ->
      (match st.stack with
       | VFloat f :: rest ->
         st.stack <- VDouble f :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_F32_TO_F64")
    | Ir.OP_NEG_I64 ->
      (match st.stack with
       | VInt n :: rest ->
         st.stack <- VInt (Int64.neg n) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_NEG_I64")
    | Ir.OP_NEG_F32 ->
      (match st.stack with
       | VFloat f :: rest ->
         st.stack <- VFloat (round_f32 (-.f)) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_NEG_F32")
    | Ir.OP_NEG_F64 ->
      (match st.stack with
       | VDouble d :: rest ->
         st.stack <- VDouble (-.d) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_NEG_F64")
    | Ir.OP_NEG_DEC ->
      (match st.stack with
       | VDecimal m :: rest ->
         st.stack <- VDecimal (Decimal.negate m) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_NEG_DEC")
    | Ir.OP_ADD_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         st.stack <- VInt (Int64.add a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_ADD_I64")
    | Ir.OP_ADD_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest ->
         st.stack <- VFloat (round_f32 (a +. b)) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_ADD_F32")
    | Ir.OP_ADD_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest ->
         st.stack <- VDouble (a +. b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_ADD_F64")
    | Ir.OP_ADD_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest ->
         st.stack <- VDecimal (Decimal.add a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_ADD_DEC")
    | Ir.OP_SUB_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         st.stack <- VInt (Int64.sub a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_SUB_I64")
    | Ir.OP_SUB_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest ->
         st.stack <- VFloat (round_f32 (a -. b)) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_SUB_F32")
    | Ir.OP_SUB_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest ->
         st.stack <- VDouble (a -. b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_SUB_F64")
    | Ir.OP_SUB_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest ->
         st.stack <- VDecimal (Decimal.sub a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_SUB_DEC")
    | Ir.OP_MUL_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         st.stack <- VInt (Int64.mul a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_MUL_I64")
    | Ir.OP_MUL_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest ->
         st.stack <- VFloat (round_f32 (a *. b)) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_MUL_F32")
    | Ir.OP_MUL_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest ->
         st.stack <- VDouble (a *. b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_MUL_F64")
    | Ir.OP_MUL_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest ->
         st.stack <- VDecimal (Decimal.mul a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_MUL_DEC")
    | Ir.OP_DIV_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest ->
         if b = 0.0 then Error "Division by zero"
         else begin
           st.stack <- VFloat (round_f32 (a /. b)) :: rest;
           Ok ()
         end
       | _ -> Error "VM type error on OP_DIV_F32")
    | Ir.OP_DIV_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest ->
         if b = 0.0 then Error "Division by zero"
         else begin
           st.stack <- VDouble (a /. b) :: rest;
           Ok ()
         end
       | _ -> Error "VM type error on OP_DIV_F64")
    | Ir.OP_DIV_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest ->
         (match Decimal.div a b with
          | Ok res ->
            st.stack <- VDecimal res :: rest;
            Ok ()
          | Error msg -> Error msg)
       | _ -> Error "VM type error on OP_DIV_DEC")
    | Ir.OP_IDIV_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         if b = 0L then Error "Division by zero"
         else begin
           st.stack <- VInt (Int64.div a b) :: rest;
           Ok ()
         end
       | _ -> Error "VM type error on OP_IDIV_I64")
    | Ir.OP_IDIV_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest ->
         if b = 0.0 then Error "Division by zero"
         else begin
           st.stack <- VInt (Int64.of_float (Float.trunc (round_f32 (a /. b)))) :: rest;
           Ok ()
         end
       | _ -> Error "VM type error on OP_IDIV_F32")
    | Ir.OP_IDIV_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest ->
         if b = 0.0 then Error "Division by zero"
         else begin
           st.stack <- VInt (Int64.of_float (Float.trunc (a /. b))) :: rest;
           Ok ()
         end
       | _ -> Error "VM type error on OP_IDIV_F64")
    | Ir.OP_IDIV_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest ->
         (match Decimal.div_trunc_i64 a b with
          | Ok res ->
            st.stack <- VInt res :: rest;
            Ok ()
          | Error msg -> Error msg)
       | _ -> Error "VM type error on OP_IDIV_DEC")
    | Ir.OP_MOD_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         (match mod_euclid_i64 a b with
          | Ok res ->
            st.stack <- VInt res :: rest;
            Ok ()
          | Error msg -> Error msg)
       | _ -> Error "VM type error on OP_MOD_I64")
    | Ir.OP_MOD_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest ->
         (match mod_euclid_float a b with
          | Ok res ->
            st.stack <- VFloat (round_f32 res) :: rest;
            Ok ()
          | Error msg -> Error msg)
       | _ -> Error "VM type error on OP_MOD_F32")
    | Ir.OP_MOD_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest ->
         (match mod_euclid_float a b with
          | Ok res ->
            st.stack <- VDouble res :: rest;
            Ok ()
          | Error msg -> Error msg)
       | _ -> Error "VM type error on OP_MOD_F64")
    | Ir.OP_MOD_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest ->
         (match Decimal.rem a b with
          | Ok res ->
            st.stack <- VDecimal res :: rest;
            Ok ()
          | Error msg -> Error msg)
       | _ -> Error "VM type error on OP_MOD_DEC")
    | Ir.OP_POW_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         st.stack <- VInt (pow_i64 a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_POW_I64")
    | Ir.OP_POW_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest ->
         st.stack <- VFloat (round_f32 (Float.pow a b)) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_POW_F32")
    | Ir.OP_POW_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest ->
         st.stack <- VDouble (Float.pow a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_POW_F64")
    | Ir.OP_POW_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest ->
         st.stack <- VDecimal (Decimal.pow a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_POW_DEC")
    | Ir.OP_CHAR_ADD_I64 ->
      (match st.stack with
       | VInt n :: VChar c :: rest ->
         let cp = decode_utf8 c + Int64.to_int n in
         (match encode_utf8 cp with
          | Ok res ->
            st.stack <- VChar res :: rest;
            Ok ()
          | Error msg -> Error msg)
       | _ -> Error "VM type error on OP_CHAR_ADD_I64")
    | Ir.OP_CHAR_SUB_I64 ->
      (match st.stack with
       | VInt n :: VChar c :: rest ->
         let cp = decode_utf8 c - Int64.to_int n in
         (match encode_utf8 cp with
          | Ok res ->
            st.stack <- VChar res :: rest;
            Ok ()
          | Error msg -> Error msg)
       | _ -> Error "VM type error on OP_CHAR_SUB_I64")
    | Ir.OP_CHAR_DIFF ->
      (match st.stack with
       | VChar b :: VChar a :: rest ->
         let diff = Int64.of_int (decode_utf8 a - decode_utf8 b) in
         st.stack <- VInt diff :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_CHAR_DIFF")
    | Ir.OP_REPEAT_STR ->
      (match st.stack with
       | VInt n :: VString s :: rest ->
         if n <= 0L || String.length s = 0 then begin
           st.stack <- VString "" :: rest;
           Ok ()
         end else begin
           let count = Int64.to_int n in
           let buf = Buffer.create (String.length s * count) in
           for _ = 1 to count do
             Buffer.add_string buf s
           done;
           st.stack <- VString (Buffer.contents buf) :: rest;
           Ok ()
         end
       | _ -> Error "VM type error on OP_REPEAT_STR")
    | Ir.OP_NOT_BOOL ->
      (match st.stack with
       | VBool b :: rest ->
         st.stack <- VBool (not b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_NOT_BOOL")
    | Ir.OP_AND_BOOL ->
      (match st.stack with
       | VBool b :: VBool a :: rest ->
         st.stack <- VBool (a && b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_AND_BOOL")
    | Ir.OP_OR_BOOL ->
      (match st.stack with
       | VBool b :: VBool a :: rest ->
         st.stack <- VBool (a || b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_OR_BOOL")
    | Ir.OP_XOR_BOOL ->
      (match st.stack with
       | VBool b :: VBool a :: rest ->
         st.stack <- VBool (a <> b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_XOR_BOOL")
    | Ir.OP_NOT_I64 ->
      (match st.stack with
       | VInt a :: rest ->
         st.stack <- VInt (Int64.lognot a) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_NOT_I64")
    | Ir.OP_AND_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         st.stack <- VInt (Int64.logand a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_AND_I64")
    | Ir.OP_OR_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         st.stack <- VInt (Int64.logor a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_OR_I64")
    | Ir.OP_XOR_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         st.stack <- VInt (Int64.logxor a b) :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_XOR_I64")
    | Ir.OP_SHL_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         let res =
           if b < 0L || b >= 64L then 0L
           else Int64.shift_left a (Int64.to_int b)
         in
         st.stack <- VInt res :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_SHL_I64")
    | Ir.OP_SHR_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest ->
         let res =
           if b < 0L then 0L
           else if b >= 64L then if a < 0L then -1L else 0L
           else Int64.shift_right a (Int64.to_int b)
         in
         st.stack <- VInt res :: rest;
         Ok ()
       | _ -> Error "VM type error on OP_SHR_I64")
    | Ir.OP_EQ_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest -> st.stack <- VBool (a = b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_EQ_I64")
    | Ir.OP_EQ_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest -> st.stack <- VBool (a = b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_EQ_F32")
    | Ir.OP_EQ_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest -> st.stack <- VBool (a = b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_EQ_F64")
    | Ir.OP_EQ_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest -> st.stack <- VBool (Decimal.compare a b = 0) :: rest; Ok ()
       | _ -> Error "VM type error on OP_EQ_DEC")
    | Ir.OP_EQ_BOOL ->
      (match st.stack with
       | VBool b :: VBool a :: rest -> st.stack <- VBool (a = b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_EQ_BOOL")
    | Ir.OP_EQ_CHAR ->
      (match st.stack with
       | VChar b :: VChar a :: rest -> st.stack <- VBool (decode_utf8 a = decode_utf8 b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_EQ_CHAR")
    | Ir.OP_EQ_STR ->
      (match st.stack with
       | VString b :: VString a :: rest -> st.stack <- VBool (String.equal a b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_EQ_STR")
    | Ir.OP_EQ_OBJ ->
      (match st.stack with
       | b :: a :: rest -> st.stack <- VBool (value_equal a b) :: rest; Ok ()
       | _ -> Error "VM stack underflow on OP_EQ_OBJ")
    | Ir.OP_NE_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest -> st.stack <- VBool (a <> b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_NE_I64")
    | Ir.OP_NE_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest -> st.stack <- VBool (a <> b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_NE_F32")
    | Ir.OP_NE_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest -> st.stack <- VBool (a <> b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_NE_F64")
    | Ir.OP_NE_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest -> st.stack <- VBool (Decimal.compare a b <> 0) :: rest; Ok ()
       | _ -> Error "VM type error on OP_NE_DEC")
    | Ir.OP_NE_BOOL ->
      (match st.stack with
       | VBool b :: VBool a :: rest -> st.stack <- VBool (a <> b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_NE_BOOL")
    | Ir.OP_NE_CHAR ->
      (match st.stack with
       | VChar b :: VChar a :: rest -> st.stack <- VBool (decode_utf8 a <> decode_utf8 b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_NE_CHAR")
    | Ir.OP_NE_STR ->
      (match st.stack with
       | VString b :: VString a :: rest -> st.stack <- VBool (not (String.equal a b)) :: rest; Ok ()
       | _ -> Error "VM type error on OP_NE_STR")
    | Ir.OP_NE_OBJ ->
      (match st.stack with
       | b :: a :: rest -> st.stack <- VBool (not (value_equal a b)) :: rest; Ok ()
       | _ -> Error "VM stack underflow on OP_NE_OBJ")
    | Ir.OP_LT_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest -> st.stack <- VBool (a < b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LT_I64")
    | Ir.OP_LT_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest -> st.stack <- VBool (a < b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LT_F32")
    | Ir.OP_LT_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest -> st.stack <- VBool (a < b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LT_F64")
    | Ir.OP_LT_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest -> st.stack <- VBool (Decimal.compare a b < 0) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LT_DEC")
    | Ir.OP_LT_CHAR ->
      (match st.stack with
       | VChar b :: VChar a :: rest -> st.stack <- VBool (decode_utf8 a < decode_utf8 b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LT_CHAR")
    | Ir.OP_GT_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest -> st.stack <- VBool (a > b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GT_I64")
    | Ir.OP_GT_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest -> st.stack <- VBool (a > b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GT_F32")
    | Ir.OP_GT_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest -> st.stack <- VBool (a > b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GT_F64")
    | Ir.OP_GT_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest -> st.stack <- VBool (Decimal.compare a b > 0) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GT_DEC")
    | Ir.OP_GT_CHAR ->
      (match st.stack with
       | VChar b :: VChar a :: rest -> st.stack <- VBool (decode_utf8 a > decode_utf8 b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GT_CHAR")
    | Ir.OP_LE_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest -> st.stack <- VBool (a <= b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LE_I64")
    | Ir.OP_LE_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest -> st.stack <- VBool (a <= b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LE_F32")
    | Ir.OP_LE_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest -> st.stack <- VBool (a <= b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LE_F64")
    | Ir.OP_LE_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest -> st.stack <- VBool (Decimal.compare a b <= 0) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LE_DEC")
    | Ir.OP_LE_CHAR ->
      (match st.stack with
       | VChar b :: VChar a :: rest -> st.stack <- VBool (decode_utf8 a <= decode_utf8 b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_LE_CHAR")
    | Ir.OP_GE_I64 ->
      (match st.stack with
       | VInt b :: VInt a :: rest -> st.stack <- VBool (a >= b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GE_I64")
    | Ir.OP_GE_F32 ->
      (match st.stack with
       | VFloat b :: VFloat a :: rest -> st.stack <- VBool (a >= b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GE_F32")
    | Ir.OP_GE_F64 ->
      (match st.stack with
       | VDouble b :: VDouble a :: rest -> st.stack <- VBool (a >= b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GE_F64")
    | Ir.OP_GE_DEC ->
      (match st.stack with
       | VDecimal b :: VDecimal a :: rest -> st.stack <- VBool (Decimal.compare a b >= 0) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GE_DEC")
    | Ir.OP_GE_CHAR ->
      (match st.stack with
       | VChar b :: VChar a :: rest -> st.stack <- VBool (decode_utf8 a >= decode_utf8 b) :: rest; Ok ()
       | _ -> Error "VM type error on OP_GE_CHAR")
    | Ir.OP_JUMP target ->
      st.ip <- target;
      Ok ()
    | Ir.OP_JUMP_IF_FALSE target ->
      (match st.stack with
       | VBool cond :: rest ->
         st.stack <- rest;
         if not cond then st.ip <- target;
         Ok ()
       | _ -> Error "VM type error on OP_JUMP_IF_FALSE")
    | Ir.OP_JUMP_IF_TRUE target ->
      (match st.stack with
       | VBool cond :: rest ->
         st.stack <- rest;
         if cond then st.ip <- target;
         Ok ()
       | _ -> Error "VM type error on OP_JUMP_IF_TRUE")
    | Ir.OP_SET_TEMP id ->
      (match st.stack with
       | v :: rest ->
         st.stack <- rest;
         Hashtbl.replace st.globals (Printf.sprintf "$tmp_%d" id) v;
         Ok ()
       | [] -> Error "VM stack underflow on OP_SET_TEMP")
    | Ir.OP_GET_TEMP id ->
      (match Hashtbl.find_opt st.globals (Printf.sprintf "$tmp_%d" id) with
       | Some v ->
         st.stack <- v :: st.stack;
         Ok ()
       | None -> Error "VM undefined temporary slot")
    | Ir.OP_DEF_GLOBAL_I64 name
    | Ir.OP_DEF_GLOBAL_F32 name
    | Ir.OP_DEF_GLOBAL_F64 name
    | Ir.OP_DEF_GLOBAL_DEC name
    | Ir.OP_DEF_GLOBAL_BOOL name
    | Ir.OP_DEF_GLOBAL_CHAR name
    | Ir.OP_DEF_GLOBAL_STR name
    | Ir.OP_DEF_GLOBAL_OBJ name
    | Ir.OP_SET_GLOBAL_I64 name
    | Ir.OP_SET_GLOBAL_F32 name
    | Ir.OP_SET_GLOBAL_F64 name
    | Ir.OP_SET_GLOBAL_DEC name
    | Ir.OP_SET_GLOBAL_BOOL name
    | Ir.OP_SET_GLOBAL_CHAR name
    | Ir.OP_SET_GLOBAL_STR name
    | Ir.OP_SET_GLOBAL_OBJ name ->
      (match st.stack with
       | v :: rest ->
         st.stack <- rest;
         Hashtbl.replace st.globals name v;
         Ok ()
       | [] -> Error "VM stack underflow on OP_DEF/SET_GLOBAL")
    | Ir.OP_GET_GLOBAL_I64 name
    | Ir.OP_GET_GLOBAL_F32 name
    | Ir.OP_GET_GLOBAL_F64 name
    | Ir.OP_GET_GLOBAL_DEC name
    | Ir.OP_GET_GLOBAL_BOOL name
    | Ir.OP_GET_GLOBAL_CHAR name
    | Ir.OP_GET_GLOBAL_STR name
    | Ir.OP_GET_GLOBAL_OBJ name ->
      (match Hashtbl.find_opt st.globals name with
       | Some v ->
         st.stack <- v :: st.stack;
         Ok ()
       | None -> Error (Printf.sprintf "VM undefined global '%s'" name))
    | Ir.OP_UNDEF_GLOBAL name ->
      Hashtbl.remove st.globals name;
      Ok ()
    | Ir.OP_POP ->
      (match st.stack with
       | _ :: rest ->
         st.stack <- rest;
         Ok ()
       | [] -> Error "VM stack underflow on OP_POP")
    | Ir.OP_PRINT_I64 ->
      (match st.stack with
       | VInt n :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (Printf.sprintf "%Ld" n);
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_I64"
       | [] -> Error "VM stack underflow on OP_PRINT_I64")
    | Ir.OP_PRINT_F32 ->
      (match st.stack with
       | VFloat f :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (Printf.sprintf "%.6g" f);
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_F32"
       | [] -> Error "VM stack underflow on OP_PRINT_F32")
    | Ir.OP_PRINT_F64 ->
      (match st.stack with
       | VDouble d :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (Printf.sprintf "%.15g" d);
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_F64"
       | [] -> Error "VM stack underflow on OP_PRINT_F64")
    | Ir.OP_PRINT_DEC ->
      (match st.stack with
       | VDecimal m :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (Decimal.to_string m);
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_DEC"
       | [] -> Error "VM stack underflow on OP_PRINT_DEC")
    | Ir.OP_PRINT_BOOL ->
      (match st.stack with
       | VBool b :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (if b then "true" else "false");
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_BOOL"
       | [] -> Error "VM stack underflow on OP_PRINT_BOOL")
    | Ir.OP_PRINT_CHAR ->
      (match st.stack with
       | VChar c :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf c;
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_CHAR"
       | [] -> Error "VM stack underflow on OP_PRINT_CHAR")
    | Ir.OP_PRINT_STR ->
      (match st.stack with
       | VString s :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf s;
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_STR"
       | [] -> Error "VM stack underflow on OP_PRINT_STR")
    | Ir.OP_PRINT_OBJ ->
      (match st.stack with
       | ((VTuple _ | VArray _) as v) :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (value_to_string v);
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_OBJ"
       | [] -> Error "VM stack underflow on OP_PRINT_OBJ")
    | Ir.OP_PRINT_SPACE ->
      Buffer.add_char out_buf ' ';
      Ok ()
    | Ir.OP_PRINT_NEWLINE ->
      Buffer.add_char out_buf '\n';
      Ok ()
    | Ir.OP_ASSERT_EQ ->
      (match st.stack with
       | VString msg :: expected_val :: actual_val :: rest ->
         st.stack <- rest;
         let passed = value_equal actual_val expected_val in
         let expected_s = value_to_string expected_val in
         let actual_s = value_to_string actual_val in
         let res =
           { passed; span = instr.span; expected = expected_s; actual = actual_s; message = msg }
         in
         st.assertions_rev <- res :: st.assertions_rev;
         if passed || st.collect_assertions then
           Ok ()
         else
           let msg_part = if msg = "" then "" else Printf.sprintf " (%s)" msg in
           Error
             (Printf.sprintf "assertion failed at %s%s: expected %s, got %s"
                (Span.to_string instr.span) msg_part expected_s actual_s)
       | _ -> Error "VM stack underflow or type error on OP_ASSERT_EQ")
    | Ir.OP_ASSERT_ERR { expected_code; actual_codes; passed } ->
      (match st.stack with
       | VString msg :: rest ->
         st.stack <- rest;
         let actual_s =
           if actual_codes = [] then "no error" else String.concat ", " actual_codes
         in
         let res =
           {
             passed;
             span = instr.span;
             expected = expected_code;
             actual = actual_s;
             message = msg;
           }
         in
         st.assertions_rev <- res :: st.assertions_rev;
         if passed || st.collect_assertions then
           Ok ()
         else
           let msg_part = if msg = "" then "" else Printf.sprintf " (%s)" msg in
           Error
             (Printf.sprintf "assert_error failed at %s%s: expected '%s', got %s"
                (Span.to_string instr.span) msg_part expected_code actual_s)
       | _ -> Error "VM stack underflow or type error on OP_ASSERT_ERR")
    | Ir.OP_HALT ->
      st.halted <- true;
      Ok ()
  end

let run ?stdin_lines (prog : Ir.program) : (string, string) result =
  let st = create_state ?stdin_lines () in
  let out_buf = Buffer.create 64 in
  let rec loop () =
    if st.halted then Ok (Buffer.contents out_buf)
    else
      match step ~out_buf prog st with
      | Ok () -> loop ()
      | Error msg -> Error msg
  in
  loop ()

let run_tests ?stdin_lines (prog : Ir.program) : (test_report, string) result =
  let st = create_state ~collect_assertions:true ?stdin_lines () in
  let out_buf = Buffer.create 64 in
  let rec loop () =
    if st.halted then begin
      let assertions = List.rev st.assertions_rev in
      let passed_count = List.length (List.filter (fun a -> a.passed) assertions) in
      let failed_count = List.length (List.filter (fun a -> not a.passed) assertions) in
      Ok
        {
          stdout = Buffer.contents out_buf;
          passed_count;
          failed_count;
          assertions;
        }
    end else
      match step ~out_buf prog st with
      | Ok () -> loop ()
      | Error msg -> Error msg
  in
  loop ()

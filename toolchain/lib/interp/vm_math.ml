let round_f32 (f : float) : float =
  Int32.float_of_bits (Int32.bits_of_float f)

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
  else if cp < 0x80 then Ok (String.make 1 (Char.chr cp))
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
    else if base = -1L then if Int64.rem exp 2L = 0L then 1L else -1L
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

let repeat_str (s : string) (n : int) : string =
  if n <= 0 then ""
  else
    let buf = Buffer.create (String.length s * n) in
    for _ = 1 to n do
      Buffer.add_string buf s
    done;
    Buffer.contents buf

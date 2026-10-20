type t = {
  neg : bool;
  scale : int;
  hi : int64;
  lo : int64;
}

type u128 = int64 * int64

let u128_zero : u128 = (0L, 0L)
let u128_ten : u128 = (0L, 10L)
let u128_max : u128 = (-1L, -1L)

let u128_is_zero ((hi, lo) : u128) : bool =
  hi = 0L && lo = 0L

let u128_cmp ((ah, al) : u128) ((bh, bl) : u128) : int =
  let c = Int64.unsigned_compare ah bh in
  if c <> 0 then c else Int64.unsigned_compare al bl

let u128_add ((ah, al) : u128) ((bh, bl) : u128) : u128 =
  let lo = Int64.add al bl in
  let carry = if Int64.unsigned_compare lo al < 0 then 1L else 0L in
  let hi = Int64.add (Int64.add ah bh) carry in
  (hi, lo)

let u128_sub ((ah, al) : u128) ((bh, bl) : u128) : u128 =
  let borrow = if Int64.unsigned_compare al bl < 0 then 1L else 0L in
  let lo = Int64.sub al bl in
  let hi = Int64.sub (Int64.sub ah bh) borrow in
  (hi, lo)

let u128_bit ((hi, lo) : u128) (i : int) : bool =
  if i >= 64 then
    Int64.logand (Int64.shift_right_logical hi (i - 64)) 1L <> 0L
  else
    Int64.logand (Int64.shift_right_logical lo i) 1L <> 0L

let u128_set_bit ((hi, lo) : u128) (i : int) : u128 =
  if i >= 64 then
    (Int64.logor hi (Int64.shift_left 1L (i - 64)), lo)
  else
    (hi, Int64.logor lo (Int64.shift_left 1L i))

let u128_shl1 ((hi, lo) : u128) (bit0 : bool) : u128 =
  let carry = Int64.shift_right_logical lo 63 in
  let lo' = Int64.logor (Int64.shift_left lo 1) (if bit0 then 1L else 0L) in
  let hi' = Int64.logor (Int64.shift_left hi 1) carry in
  (hi', lo')

let u128_divrem (a : u128) (b : u128) : u128 * u128 =
  if u128_is_zero b then (u128_zero, u128_zero)
  else
    let q = ref u128_zero in
    let r = ref u128_zero in
    for i = 127 downto 0 do
      r := u128_shl1 !r (u128_bit a i);
      if u128_cmp !r b >= 0 then (
        r := u128_sub !r b;
        q := u128_set_bit !q i
      )
    done;
    (!q, !r)

let u128_div a b = fst (u128_divrem a b)
let u128_rem a b = snd (u128_divrem a b)

let u128_mul (a : u128) (b : u128) : u128 =
  let res = ref u128_zero in
  for i = 127 downto 0 do
    res := u128_shl1 !res false;
    if u128_bit b i then res := u128_add !res a
  done;
  !res

let u128_max_div10 : u128 = u128_div u128_max u128_ten
let u128_max_mul10_add9 : u128 =
  u128_div (u128_sub u128_max (0L, 9L)) u128_ten

let mul10_add (x : u128) (d : int) : u128 option =
  if u128_cmp x u128_max_div10 > 0 then None
  else
    let x10 = u128_mul x u128_ten in
    let d128 = (0L, Int64.of_int d) in
    let sum = u128_add x10 d128 in
    if u128_cmp sum x10 < 0 then None else Some sum

let divrem10 (x : u128) : u128 * int =
  let q, (_, r_lo) = u128_divrem x u128_ten in
  (q, Int64.to_int r_lo)

let zero = { neg = false; scale = 0; hi = 0L; lo = 0L }

let is_zero d = d.hi = 0L && d.lo = 0L

let negate d =
  if is_zero d then zero else { d with neg = not d.neg }

let equal a b =
  a.neg = b.neg && a.scale = b.scale && a.hi = b.hi && a.lo = b.lo

let make_normalized (neg : bool) (scale : int) (coeff : u128) : t =
  if u128_is_zero coeff then zero
  else
    let c = ref coeff in
    let sc = ref scale in
    while !sc < 0 && u128_cmp !c u128_max_div10 <= 0 do
      c := u128_mul !c u128_ten;
      incr sc
    done;
    if !sc < 0 then sc := 0;
    let keep_stripping = ref true in
    while !sc > 0 && !keep_stripping do
      let q, r = divrem10 !c in
      if r = 0 then (
        c := q;
        decr sc
      ) else keep_stripping := false
    done;
    let hi, lo = !c in
    if hi = 0L && lo = 0L then zero
    else { neg; scale = !sc; hi; lo }

let of_int64 (n : int64) : t =
  if n = 0L then zero
  else
    let neg = n < 0L in
    let abs_lo =
      if n = Int64.min_int then 0x8000_0000_0000_0000L
      else Int64.abs n
    in
    make_normalized neg 0 (0L, abs_lo)

let align_scales (a : t) (b : t) : u128 * u128 * int =
  let ac = ref (a.hi, a.lo) in
  let asc = ref a.scale in
  let bc = ref (b.hi, b.lo) in
  let bsc = ref b.scale in
  while !asc < !bsc do
    if u128_cmp !ac u128_max_div10 <= 0 then (
      ac := u128_mul !ac u128_ten;
      incr asc
    ) else (
      bc := fst (divrem10 !bc);
      decr bsc
    )
  done;
  while !bsc < !asc do
    if u128_cmp !bc u128_max_div10 <= 0 then (
      bc := u128_mul !bc u128_ten;
      incr bsc
    ) else (
      ac := fst (divrem10 !ac);
      decr asc
    )
  done;
  (!ac, !bc, !asc)

let add (a : t) (b : t) : t =
  if is_zero a then b
  else if is_zero b then a
  else
    let ac, bc, sc = align_scales a b in
    if a.neg = b.neg then
      make_normalized a.neg sc (u128_add ac bc)
    else
      let cmp = u128_cmp ac bc in
      if cmp = 0 then zero
      else if cmp > 0 then make_normalized a.neg sc (u128_sub ac bc)
      else make_normalized b.neg sc (u128_sub bc ac)

let sub (a : t) (b : t) : t =
  add a (negate b)

let mul (a : t) (b : t) : t =
  if is_zero a || is_zero b then zero
  else
    let ac = ref (a.hi, a.lo) in
    let asc = ref a.scale in
    let bc = ref (b.hi, b.lo) in
    let bsc = ref b.scale in
    while
      not (u128_is_zero !bc)
      && u128_cmp !ac (u128_div u128_max !bc) > 0
      && (!asc > 0 || !bsc > 0)
    do
      if !asc >= !bsc then (
        ac := fst (divrem10 !ac);
        decr asc
      ) else (
        bc := fst (divrem10 !bc);
        decr bsc
      )
    done;
    make_normalized (a.neg <> b.neg) (!asc + !bsc) (u128_mul !ac !bc)

let div (a : t) (b : t) : (t, string) result =
  if is_zero b then Error "Division by zero"
  else if is_zero a then Ok zero
  else
    let neg = a.neg <> b.neg in
    let ac = (a.hi, a.lo) in
    let bc = (b.hi, b.lo) in
    let q0, r0 = u128_divrem ac bc in
    let q = ref q0 in
    let rem = ref r0 in
    let sc = ref (a.scale - b.scale) in
    while
      not (u128_is_zero !rem)
      && !sc < 28
      && u128_cmp !q u128_max_mul10_add9 <= 0
      && u128_cmp !rem u128_max_div10 <= 0
    do
      let rem10 = u128_mul !rem u128_ten in
      let d, r_next = u128_divrem rem10 bc in
      q := u128_add (u128_mul !q u128_ten) d;
      rem := r_next;
      incr sc
    done;
    Ok (make_normalized neg !sc !q)

let div_trunc_i64 (a : t) (b : t) : (int64, string) result =
  if is_zero b then Error "Division by zero"
  else if is_zero a then Ok 0L
  else
    let ac, bc, _ = align_scales a b in
    let (_, q_lo) = u128_div ac bc in
    let neg = a.neg <> b.neg in
    if neg then Ok (Int64.neg q_lo) else Ok q_lo

let rem (a : t) (b : t) : (t, string) result =
  if is_zero b then Error "Modulo by zero"
  else if is_zero a then Ok zero
  else
    let ac, bc, sc = align_scales a b in
    let r_mag = u128_rem ac bc in
    if u128_is_zero r_mag then Ok zero
    else if not a.neg then Ok (make_normalized false sc r_mag)
    else Ok (make_normalized false sc (u128_sub bc r_mag))

let split_once ch s =
  match String.index_opt s ch with
  | None -> (s, None)
  | Some idx ->
    let left = String.sub s 0 idx in
    let right = String.sub s (idx + 1) (String.length s - idx - 1) in
    (left, Some right)

let of_string_opt (s : string) : t option =
  let len = String.length s in
  if len = 0 then None
  else
    let neg, start_idx =
      if s.[0] = '-' then (true, 1)
      else if s.[0] = '+' then (false, 1)
      else (false, 0)
    in
    let body = String.sub s start_idx (len - start_idx) in
    let mantissa_str, exp_opt =
      match split_once 'e' body with
      | m, Some e -> (m, Some e)
      | _, None -> split_once 'E' body
    in
    let int_part, frac_part =
      match split_once '.' mantissa_str with
      | i, Some f -> (i, f)
      | i, None -> (i, "")
    in
    let exp_val_opt =
      match exp_opt with
      | None -> Some 0
      | Some e_str -> int_of_string_opt e_str
    in
    match exp_val_opt with
    | None -> None
    | Some exp_val ->
      let raw_digits = int_part ^ frac_part in
      let frac_len = String.length frac_part in
      let target_scale = frac_len - exp_val in
      let d_len = ref (String.length raw_digits) in
      let scale_ref = ref target_scale in
      while !d_len > 1 && !scale_ref > 0 && raw_digits.[!d_len - 1] = '0' do
        decr d_len;
        decr scale_ref
      done;
      if !scale_ref < -38 || !scale_ref > 100 then None
      else
        let rec feed_digits i coeff =
          if i >= !d_len then Some coeff
          else
            let c = raw_digits.[i] in
            if c < '0' || c > '9' then None
            else
              match mul10_add coeff (Char.code c - Char.code '0') with
              | None -> None
              | Some coeff' -> feed_digits (i + 1) coeff'
        in
        let rec mul_pow10 k coeff =
          if k <= 0 then Some coeff
          else
            match mul10_add coeff 0 with
            | None -> None
            | Some coeff' -> mul_pow10 (k - 1) coeff'
        in
        match feed_digits 0 u128_zero with
        | None -> None
        | Some base_coeff ->
          let final_coeff_opt, final_scale =
            if !scale_ref < 0 then
              (mul_pow10 (- !scale_ref) base_coeff, 0)
            else
              (Some base_coeff, !scale_ref)
          in
          (match final_coeff_opt with
           | None -> None
           | Some coeff ->
             Some (make_normalized neg final_scale coeff))

let to_string (d : t) : string =
  let coeff = (d.hi, d.lo) in
  if u128_is_zero coeff then "0"
  else
    let rec extract l acc =
      if u128_is_zero l then acc
      else
        let q, r = divrem10 l in
        extract q (Char.chr (Char.code '0' + r) :: acc)
    in
    let digits = extract coeff [] in
    let num_digits = List.length digits in
    let digits_str = String.of_seq (List.to_seq digits) in
    let body =
      if d.scale = 0 then digits_str
      else if num_digits <= d.scale then
        let zeros = String.make (d.scale - num_digits) '0' in
        "0." ^ zeros ^ digits_str
      else
        let split_pos = num_digits - d.scale in
        let left = String.sub digits_str 0 split_pos in
        let right = String.sub digits_str split_pos d.scale in
        left ^ "." ^ right
    in
    if d.neg then "-" ^ body else body

let to_float (d : t) : float =
  match float_of_string_opt (to_string d) with
  | Some f -> f
  | None -> 0.0

let pow (a : t) (b : t) : t =
  if b.scale = 0 && b.hi = 0L && Int64.unsigned_compare b.lo 1000L <= 0 then
    let exp = ref (Int64.to_int b.lo) in
    let base = ref a in
    let acc = ref (of_int64 1L) in
    while !exp > 0 do
      if !exp land 1 = 1 then acc := mul !acc !base;
      base := mul !base !base;
      exp := !exp lsr 1
    done;
    if b.neg then
      match div (of_int64 1L) !acc with
      | Ok inv -> inv
      | Error _ -> zero
    else !acc
  else
    let rf = Float.pow (to_float a) (to_float b) in
    match of_string_opt (Printf.sprintf "%.15g" rf) with
    | Some res -> res
    | None -> zero

let pp fmt d = Format.fprintf fmt "%s" (to_string d)

type t = {
  neg : bool;
  scale : int;
  hi : int64;
  lo : int64;
}

let zero = { neg = false; scale = 0; hi = 0L; lo = 0L }

let is_zero d = d.hi = 0L && d.lo = 0L

let negate d = if is_zero d then zero else { d with neg = not d.neg }

let equal a b = a.neg = b.neg && a.scale = b.scale && a.hi = b.hi && a.lo = b.lo

let make_normalized (neg : bool) (scale : int) (coeff : U128.t) : t =
  if U128.is_zero coeff then zero
  else
    let c = ref coeff in
    let sc = ref scale in
    while !sc < 0 && U128.cmp !c U128.max_div10 <= 0 do
      c := U128.mul !c U128.ten;
      incr sc
    done;
    if !sc < 0 then sc := 0;
    let keep_stripping = ref true in
    while !sc > 0 && !keep_stripping do
      let q, r = U128.divrem10 !c in
      if r = 0 then (c := q; decr sc) else keep_stripping := false
    done;
    let hi, lo = !c in
    if hi = 0L && lo = 0L then zero else { neg; scale = !sc; hi; lo }

let of_int64 (n : int64) : t =
  if n = 0L then zero
  else
    let neg = n < 0L in
    let abs_lo = if n = Int64.min_int then 0x8000_0000_0000_0000L else Int64.abs n in
    make_normalized neg 0 (0L, abs_lo)

let align_scales (a : t) (b : t) : U128.t * U128.t * int =
  let ac = ref (a.hi, a.lo) and asc = ref a.scale in
  let bc = ref (b.hi, b.lo) and bsc = ref b.scale in
  while !asc < !bsc do
    if U128.cmp !ac U128.max_div10 <= 0 then (ac := U128.mul !ac U128.ten; incr asc)
    else (bc := fst (U128.divrem10 !bc); decr bsc)
  done;
  while !bsc < !asc do
    if U128.cmp !bc U128.max_div10 <= 0 then (bc := U128.mul !bc U128.ten; incr bsc)
    else (ac := fst (U128.divrem10 !ac); decr asc)
  done;
  (!ac, !bc, !asc)

let compare (a : t) (b : t) : int =
  if is_zero a && is_zero b then 0
  else if is_zero a then if b.neg then 1 else -1
  else if is_zero b then if a.neg then -1 else 1
  else if a.neg <> b.neg then if a.neg then -1 else 1
  else
    let ac, bc, _ = align_scales a b in
    let c = U128.cmp ac bc in
    if a.neg then -c else c

let add (a : t) (b : t) : t =
  if is_zero a then b
  else if is_zero b then a
  else
    let ac, bc, sc = align_scales a b in
    if a.neg = b.neg then make_normalized a.neg sc (U128.add ac bc)
    else
      let cmp = U128.cmp ac bc in
      if cmp = 0 then zero
      else if cmp > 0 then make_normalized a.neg sc (U128.sub ac bc)
      else make_normalized b.neg sc (U128.sub bc ac)

let sub (a : t) (b : t) : t = add a (negate b)

let mul (a : t) (b : t) : t =
  if is_zero a || is_zero b then zero
  else
    let ac = ref (a.hi, a.lo) and asc = ref a.scale in
    let bc = ref (b.hi, b.lo) and bsc = ref b.scale in
    while
      not (U128.is_zero !bc)
      && U128.cmp !ac (U128.div U128.max_val !bc) > 0
      && (!asc > 0 || !bsc > 0)
    do
      if !asc >= !bsc then (ac := fst (U128.divrem10 !ac); decr asc)
      else (bc := fst (U128.divrem10 !bc); decr bsc)
    done;
    make_normalized (a.neg <> b.neg) (!asc + !bsc) (U128.mul !ac !bc)

let div (a : t) (b : t) : (t, string) result =
  if is_zero b then Error "Division by zero"
  else if is_zero a then Ok zero
  else
    let neg = a.neg <> b.neg in
    let q0, r0 = U128.divrem (a.hi, a.lo) (b.hi, b.lo) in
    let q = ref q0 and rem = ref r0 and sc = ref (a.scale - b.scale) in
    while
      not (U128.is_zero !rem)
      && !sc < 28
      && U128.cmp !q U128.max_mul10_add9 <= 0
      && U128.cmp !rem U128.max_div10 <= 0
    do
      let d, r_next = U128.divrem (U128.mul !rem U128.ten) (b.hi, b.lo) in
      q := U128.add (U128.mul !q U128.ten) d;
      rem := r_next;
      incr sc
    done;
    Ok (make_normalized neg !sc !q)

let div_trunc_i64 (a : t) (b : t) : (int64, string) result =
  if is_zero b then Error "Division by zero"
  else if is_zero a then Ok 0L
  else
    let ac, bc, _ = align_scales a b in
    let (_, q_lo) = U128.div ac bc in
    if a.neg <> b.neg then Ok (Int64.neg q_lo) else Ok q_lo

let rem (a : t) (b : t) : (t, string) result =
  if is_zero b then Error "Modulo by zero"
  else if is_zero a then Ok zero
  else
    let ac, bc, sc = align_scales a b in
    let r_mag = U128.rem ac bc in
    if U128.is_zero r_mag then Ok zero
    else if not a.neg then Ok (make_normalized false sc r_mag)
    else Ok (make_normalized false sc (U128.sub bc r_mag))

let split_once ch s =
  match String.index_opt s ch with
  | None -> (s, None)
  | Some idx -> (String.sub s 0 idx, Some (String.sub s (idx + 1) (String.length s - idx - 1)))

let of_string_opt (s : string) : t option =
  let len = String.length s in
  if len = 0 then None
  else
    let neg, start_idx =
      if s.[0] = '-' then (true, 1) else if s.[0] = '+' then (false, 1) else (false, 0)
    in
    let body = String.sub s start_idx (len - start_idx) in
    let mantissa_str, exp_opt =
      match split_once 'e' body with m, Some e -> (m, Some e) | _, None -> split_once 'E' body
    in
    let int_part, frac_part =
      match split_once '.' mantissa_str with i, Some f -> (i, f) | i, None -> (i, "")
    in
    match (match exp_opt with None -> Some 0 | Some e_str -> int_of_string_opt e_str) with
    | None -> None
    | Some exp_val ->
      let raw_digits = int_part ^ frac_part in
      let d_len = ref (String.length raw_digits) in
      let scale_ref = ref (String.length frac_part - exp_val) in
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
            else match U128.mul10_add coeff (Char.code c - Char.code '0') with
              | None -> None
              | Some coeff' -> feed_digits (i + 1) coeff'
        in
        let rec mul_pow10 k coeff =
          if k <= 0 then Some coeff
          else match U128.mul10_add coeff 0 with
            | None -> None
            | Some coeff' -> mul_pow10 (k - 1) coeff'
        in
        match feed_digits 0 U128.zero with
        | None -> None
        | Some base_coeff ->
          let final_coeff_opt, final_scale =
            if !scale_ref < 0 then (mul_pow10 (- !scale_ref) base_coeff, 0)
            else (Some base_coeff, !scale_ref)
          in
          Option.map (make_normalized neg final_scale) final_coeff_opt

let to_string (d : t) : string =
  let coeff = (d.hi, d.lo) in
  if U128.is_zero coeff then "0"
  else
    let rec extract l acc =
      if U128.is_zero l then acc
      else let q, r = U128.divrem10 l in extract q (Char.chr (Char.code '0' + r) :: acc)
    in
    let digits = extract coeff [] in
    let num_digits = List.length digits in
    let digits_str = String.of_seq (List.to_seq digits) in
    let body =
      if d.scale = 0 then digits_str
      else if num_digits <= d.scale then "0." ^ String.make (d.scale - num_digits) '0' ^ digits_str
      else
        let split_pos = num_digits - d.scale in
        String.sub digits_str 0 split_pos ^ "." ^ String.sub digits_str split_pos d.scale
    in
    if d.neg then "-" ^ body else body

let to_float (d : t) : float = Option.value ~default:0.0 (float_of_string_opt (to_string d))

let pow (a : t) (b : t) : t =
  if b.scale = 0 && b.hi = 0L && Int64.unsigned_compare b.lo 1000L <= 0 then
    let exp = ref (Int64.to_int b.lo) and base = ref a and acc = ref (of_int64 1L) in
    while !exp > 0 do
      if !exp land 1 = 1 then acc := mul !acc !base;
      base := mul !base !base;
      exp := !exp lsr 1
    done;
    if b.neg then Result.value ~default:zero (div (of_int64 1L) !acc) else !acc
  else
    let rf = Float.pow (to_float a) (to_float b) in
    Option.value ~default:zero (of_string_opt (Printf.sprintf "%.15g" rf))

let pp fmt d = Format.fprintf fmt "%s" (to_string d)

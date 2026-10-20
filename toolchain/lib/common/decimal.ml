type t = {
  neg : bool;
  scale : int;
  hi : int64;
  lo : int64;
}

type u128_limbs = int64 * int64 * int64 * int64

let mask32 = 0xFFFF_FFFFL

let limbs_zero : u128_limbs = (0L, 0L, 0L, 0L)

let is_limbs_zero ((w3, w2, w1, w0) : u128_limbs) =
  w3 = 0L && w2 = 0L && w1 = 0L && w0 = 0L

let limbs_to_hi_lo ((w3, w2, w1, w0) : u128_limbs) : int64 * int64 =
  let hi = Int64.logor (Int64.shift_left w3 32) (Int64.logand w2 mask32) in
  let lo = Int64.logor (Int64.shift_left w1 32) (Int64.logand w0 mask32) in
  (hi, lo)

let limbs_of_hi_lo (hi : int64) (lo : int64) : u128_limbs =
  let w3 = Int64.logand (Int64.shift_right_logical hi 32) mask32 in
  let w2 = Int64.logand hi mask32 in
  let w1 = Int64.logand (Int64.shift_right_logical lo 32) mask32 in
  let w0 = Int64.logand lo mask32 in
  (w3, w2, w1, w0)

let mul10_add ((w3, w2, w1, w0) : u128_limbs) (d : int) : u128_limbs option =
  let t0 = Int64.add (Int64.mul w0 10L) (Int64.of_int d) in
  let w0' = Int64.logand t0 mask32 and c0 = Int64.shift_right_logical t0 32 in
  let t1 = Int64.add (Int64.mul w1 10L) c0 in
  let w1' = Int64.logand t1 mask32 and c1 = Int64.shift_right_logical t1 32 in
  let t2 = Int64.add (Int64.mul w2 10L) c1 in
  let w2' = Int64.logand t2 mask32 and c2 = Int64.shift_right_logical t2 32 in
  let t3 = Int64.add (Int64.mul w3 10L) c2 in
  let w3' = Int64.logand t3 mask32 and c3 = Int64.shift_right_logical t3 32 in
  if c3 <> 0L then None else Some (w3', w2', w1', w0')

let divrem10 ((w3, w2, w1, w0) : u128_limbs) : u128_limbs * int =
  let q3 = Int64.div w3 10L and r3 = Int64.rem w3 10L in
  let n2 = Int64.logor (Int64.shift_left r3 32) w2 in
  let q2 = Int64.div n2 10L and r2 = Int64.rem n2 10L in
  let n1 = Int64.logor (Int64.shift_left r2 32) w1 in
  let q1 = Int64.div n1 10L and r1 = Int64.rem n1 10L in
  let n0 = Int64.logor (Int64.shift_left r1 32) w0 in
  let q0 = Int64.div n0 10L and r0 = Int64.rem n0 10L in
  ((q3, q2, q1, q0), Int64.to_int r0)

let zero = { neg = false; scale = 0; hi = 0L; lo = 0L }

let negate d =
  if d.hi = 0L && d.lo = 0L then d else { d with neg = not d.neg }

let equal a b =
  a.neg = b.neg && a.scale = b.scale && a.hi = b.hi && a.lo = b.lo

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
      (* target_scale = frac_len - exp_val *)
      let target_scale = frac_len - exp_val in
      (* Strip trailing zeros while target_scale > 0 to normalize *)
      let d_len = ref (String.length raw_digits) in
      let scale_ref = ref target_scale in
      while !d_len > 1 && !scale_ref > 0 && raw_digits.[!d_len - 1] = '0' do
        decr d_len;
        decr scale_ref
      done;
      if !scale_ref < -38 || !scale_ref > 100 then None
      else
        let rec feed_digits i limbs =
          if i >= !d_len then Some limbs
          else
            let c = raw_digits.[i] in
            if c < '0' || c > '9' then None
            else
              match mul10_add limbs (Char.code c - Char.code '0') with
              | None -> None
              | Some limbs' -> feed_digits (i + 1) limbs'
        in
        let rec mul_pow10 k limbs =
          if k <= 0 then Some limbs
          else
            match mul10_add limbs 0 with
            | None -> None
            | Some limbs' -> mul_pow10 (k - 1) limbs'
        in
        match feed_digits 0 limbs_zero with
        | None -> None
        | Some base_limbs ->
          let final_limbs_opt, final_scale =
            if !scale_ref < 0 then
              (mul_pow10 (- !scale_ref) base_limbs, 0)
            else
              (Some base_limbs, !scale_ref)
          in
          (match final_limbs_opt with
           | None -> None
           | Some limbs ->
             if is_limbs_zero limbs then Some zero
             else
               let rec normalize l sc =
                 if sc <= 0 then (l, 0)
                 else
                   let q, r = divrem10 l in
                   if r = 0 then normalize q (sc - 1) else (l, sc)
               in
               let norm_limbs, norm_scale = normalize limbs final_scale in
               let hi, lo = limbs_to_hi_lo norm_limbs in
               Some { neg; scale = norm_scale; hi; lo })

let to_string (d : t) : string =
  let limbs = limbs_of_hi_lo d.hi d.lo in
  if is_limbs_zero limbs then "0"
  else
    let rec extract l acc =
      if is_limbs_zero l then acc
      else
        let q, r = divrem10 l in
        extract q (Char.chr (Char.code '0' + r) :: acc)
    in
    let digits = extract limbs [] in
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

let pp fmt d = Format.fprintf fmt "%s" (to_string d)

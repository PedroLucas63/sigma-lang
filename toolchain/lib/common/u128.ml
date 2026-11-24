type t = int64 * int64

let zero : t = (0L, 0L)
let ten : t = (0L, 10L)
let max_val : t = (-1L, -1L)

let is_zero ((hi, lo) : t) : bool = hi = 0L && lo = 0L

let cmp ((ah, al) : t) ((bh, bl) : t) : int =
  let c = Int64.unsigned_compare ah bh in
  if c <> 0 then c else Int64.unsigned_compare al bl

let add ((ah, al) : t) ((bh, bl) : t) : t =
  let lo = Int64.add al bl in
  let carry = if Int64.unsigned_compare lo al < 0 then 1L else 0L in
  (Int64.add (Int64.add ah bh) carry, lo)

let sub ((ah, al) : t) ((bh, bl) : t) : t =
  let borrow = if Int64.unsigned_compare al bl < 0 then 1L else 0L in
  let lo = Int64.sub al bl in
  (Int64.sub (Int64.sub ah bh) borrow, lo)

let bit ((hi, lo) : t) (i : int) : bool =
  if i >= 64 then Int64.logand (Int64.shift_right_logical hi (i - 64)) 1L <> 0L
  else Int64.logand (Int64.shift_right_logical lo i) 1L <> 0L

let set_bit ((hi, lo) : t) (i : int) : t =
  if i >= 64 then (Int64.logor hi (Int64.shift_left 1L (i - 64)), lo)
  else (hi, Int64.logor lo (Int64.shift_left 1L i))

let shl1 ((hi, lo) : t) (bit0 : bool) : t =
  let carry = Int64.shift_right_logical lo 63 in
  let lo' = Int64.logor (Int64.shift_left lo 1) (if bit0 then 1L else 0L) in
  let hi' = Int64.logor (Int64.shift_left hi 1) carry in
  (hi', lo')

let divrem (a : t) (b : t) : t * t =
  if is_zero b then (zero, zero)
  else
    let q = ref zero in
    let r = ref zero in
    for i = 127 downto 0 do
      r := shl1 !r (bit a i);
      if cmp !r b >= 0 then (
        r := sub !r b;
        q := set_bit !q i)
    done;
    (!q, !r)

let div a b = fst (divrem a b)
let rem a b = snd (divrem a b)

let mul (a : t) (b : t) : t =
  let res = ref zero in
  for i = 127 downto 0 do
    res := shl1 !res false;
    if bit b i then res := add !res a
  done;
  !res

let max_div10 : t = div max_val ten
let max_mul10_add9 : t = div (sub max_val (0L, 9L)) ten

let mul10_add (x : t) (d : int) : t option =
  if cmp x max_div10 > 0 then None
  else
    let x10 = mul x ten in
    let sum = add x10 (0L, Int64.of_int d) in
    if cmp sum x10 < 0 then None else Some sum

let divrem10 (x : t) : t * int =
  let q, (_, r_lo) = divrem x ten in
  (q, Int64.to_int r_lo)

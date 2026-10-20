open Sigma_common
open Sigma_ir

type value =
  | VInt of int64
  | VFloat of float
  | VDouble of float
  | VDecimal of Decimal.t
  | VBool of bool
  | VChar of string
  | VString of string

let round_f32 (f : float) : float =
  Int32.float_of_bits (Int32.bits_of_float f)

let value_to_string = function
  | VInt n -> Int64.to_string n
  | VFloat f -> Printf.sprintf "%.6g" f
  | VDouble d -> Printf.sprintf "%.15g" d
  | VDecimal m -> Decimal.to_string m
  | VBool b -> if b then "true" else "false"
  | VChar c -> c
  | VString s -> s

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

type state = {
  mutable ip : int;
  mutable stack : value list;
  globals : (string, value) Hashtbl.t;
  mutable halted : bool;
}

let create_state () =
  { ip = 0; stack = []; globals = Hashtbl.create 16; halted = false }

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
    | Ir.OP_TO_STR ->
      (match st.stack with
       | v :: rest ->
         st.stack <- VString (value_to_string v) :: rest;
         Ok ()
       | [] -> Error "VM stack underflow on OP_TO_STR")
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
    | Ir.OP_DEF_GLOBAL_I64 name
    | Ir.OP_DEF_GLOBAL_F32 name
    | Ir.OP_DEF_GLOBAL_F64 name
    | Ir.OP_DEF_GLOBAL_DEC name
    | Ir.OP_DEF_GLOBAL_BOOL name
    | Ir.OP_DEF_GLOBAL_CHAR name
    | Ir.OP_DEF_GLOBAL_STR name ->
      (match st.stack with
       | v :: rest ->
         st.stack <- rest;
         Hashtbl.replace st.globals name v;
         Ok ()
       | [] -> Error "VM stack underflow on OP_DEF_GLOBAL")
    | Ir.OP_GET_GLOBAL_I64 name
    | Ir.OP_GET_GLOBAL_F32 name
    | Ir.OP_GET_GLOBAL_F64 name
    | Ir.OP_GET_GLOBAL_DEC name
    | Ir.OP_GET_GLOBAL_BOOL name
    | Ir.OP_GET_GLOBAL_CHAR name
    | Ir.OP_GET_GLOBAL_STR name ->
      (match Hashtbl.find_opt st.globals name with
       | Some v ->
         st.stack <- v :: st.stack;
         Ok ()
       | None -> Error (Printf.sprintf "VM undefined global '%s'" name))
    | Ir.OP_PRINT_I64 ->
      (match st.stack with
       | VInt n :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (Printf.sprintf "%Ld\n" n);
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_I64"
       | [] -> Error "VM stack underflow on OP_PRINT_I64")
    | Ir.OP_PRINT_F32 ->
      (match st.stack with
       | VFloat f :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (Printf.sprintf "%.6g\n" f);
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_F32"
       | [] -> Error "VM stack underflow on OP_PRINT_F32")
    | Ir.OP_PRINT_F64 ->
      (match st.stack with
       | VDouble d :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (Printf.sprintf "%.15g\n" d);
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_F64"
       | [] -> Error "VM stack underflow on OP_PRINT_F64")
    | Ir.OP_PRINT_DEC ->
      (match st.stack with
       | VDecimal m :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (Decimal.to_string m ^ "\n");
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_DEC"
       | [] -> Error "VM stack underflow on OP_PRINT_DEC")
    | Ir.OP_PRINT_BOOL ->
      (match st.stack with
       | VBool b :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (if b then "true\n" else "false\n");
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_BOOL"
       | [] -> Error "VM stack underflow on OP_PRINT_BOOL")
    | Ir.OP_PRINT_CHAR ->
      (match st.stack with
       | VChar c :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (c ^ "\n");
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_CHAR"
       | [] -> Error "VM stack underflow on OP_PRINT_CHAR")
    | Ir.OP_PRINT_STR ->
      (match st.stack with
       | VString s :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (s ^ "\n");
         Ok ()
       | _ :: _ -> Error "VM type error on OP_PRINT_STR"
       | [] -> Error "VM stack underflow on OP_PRINT_STR")
    | Ir.OP_HALT ->
      st.halted <- true;
      Ok ()
  end

let run (prog : Ir.program) : (string, string) result =
  let st = create_state () in
  let out_buf = Buffer.create 64 in
  let rec loop () =
    if st.halted then Ok (Buffer.contents out_buf)
    else
      match step ~out_buf prog st with
      | Ok () -> loop ()
      | Error msg -> Error msg
  in
  loop ()

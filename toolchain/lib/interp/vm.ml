open Sigma_ir

type value =
  | VInt of int64
  | VFloat of float
  | VDouble of float
  | VDecimal of float
  | VBool of bool

let value_to_string = function
  | VInt n -> Int64.to_string n
  | VFloat f -> Printf.sprintf "%.6g" f
  | VDouble d -> Printf.sprintf "%.15g" d
  | VDecimal m -> Printf.sprintf "%.15g" m
  | VBool b -> if b then "true" else "false"

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
    | Ir.OP_DEF_GLOBAL_I64 name
    | Ir.OP_DEF_GLOBAL_F32 name
    | Ir.OP_DEF_GLOBAL_F64 name
    | Ir.OP_DEF_GLOBAL_DEC name
    | Ir.OP_DEF_GLOBAL_BOOL name ->
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
    | Ir.OP_GET_GLOBAL_BOOL name ->
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
         Buffer.add_string out_buf (Printf.sprintf "%.15g\n" m);
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

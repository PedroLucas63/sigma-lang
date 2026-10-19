open Sigma_ir

type state = {
  mutable ip : int;
  mutable stack : int64 list;
  globals : (string, int64) Hashtbl.t;
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
      st.stack <- n :: st.stack;
      Ok ()
    | Ir.OP_DEF_GLOBAL_I64 name ->
      (match st.stack with
       | v :: rest ->
         st.stack <- rest;
         Hashtbl.replace st.globals name v;
         Ok ()
       | [] -> Error "VM stack underflow on OP_DEF_GLOBAL_I64")
    | Ir.OP_GET_GLOBAL_I64 name ->
      (match Hashtbl.find_opt st.globals name with
       | Some v ->
         st.stack <- v :: st.stack;
         Ok ()
       | None -> Error (Printf.sprintf "VM undefined global '%s'" name))
    | Ir.OP_PRINT_I64 ->
      (match st.stack with
       | v :: rest ->
         st.stack <- rest;
         Buffer.add_string out_buf (Printf.sprintf "%Ld\n" v);
         Ok ()
       | [] -> Error "VM stack underflow on OP_PRINT_I64")
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

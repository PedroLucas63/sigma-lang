open Sigma_common
open Sigma_ir
open Vm_state
open Vm_format
open Vm_input

let pop_n count stack =
  let rec loop n acc s =
    if n <= 0 then Some (acc, s)
    else match s with v :: rest -> loop (n - 1) (v :: acc) rest | [] -> None
  in
  loop count [] stack

let exec_call st idxs =
  let n = List.length idxs in
  match pop_n n st.stack with
  | Some (argv, VFun f :: rest) ->
    let len = Array.length f.bound in
    let remaining =
      Array.of_list (List.filter (fun k -> Option.is_none f.bound.(k)) (List.init len Fun.id))
    in
    let bound = Array.copy f.bound in
    List.iter2 (fun idx v -> bound.(remaining.(idx)) <- Some v) idxs argv;
    if Array.for_all Option.is_some bound then begin
      if List.length st.frames >= max_call_depth then
        Error "Runtime error: call stack overflow (recursion too deep)"
      else begin
        let vars = Hashtbl.create 8 in
        Array.iteri
          (fun k nm ->
            match bound.(k) with
            | Some (VRef r) -> Hashtbl.replace vars nm r
            | Some v -> Hashtbl.replace vars nm (ref v)
            | None -> ())
          f.pnames;
        st.frames <-
          { vars; f_env = f.env; self_fn = Some f; ret_ip = st.ip; saved_stack = rest } :: st.frames;
        st.stack <- rest;
        st.ip <- f.entry;
        Ok ()
      end
    end else begin
      st.stack <- VFun { f with bound } :: rest;
      Ok ()
    end
  | _ -> Error "VM type error or stack underflow on OP_CALL"

let exec_core_op ~out_buf (st : state) (instr : Ir.instr) : (unit, string) result =
  match instr.op with
  | Ir.OP_CONST_I64 n -> st.stack <- VInt n :: st.stack; Ok ()
  | Ir.OP_CONST_F32 f -> st.stack <- VFloat f :: st.stack; Ok ()
  | Ir.OP_CONST_F64 d -> st.stack <- VDouble d :: st.stack; Ok ()
  | Ir.OP_CONST_DEC m -> st.stack <- VDecimal m :: st.stack; Ok ()
  | Ir.OP_CONST_BOOL b -> st.stack <- VBool b :: st.stack; Ok ()
  | Ir.OP_CONST_CHAR c -> st.stack <- VChar c :: st.stack; Ok ()
  | Ir.OP_CONST_STR s -> st.stack <- VString s :: st.stack; Ok ()
  | Ir.OP_MAKE_TUPLE count ->
    (match pop_n count st.stack with
     | Some (items, rest) -> st.stack <- VTuple (Array.of_list items) :: rest; Ok ()
     | None -> Error "VM stack underflow on OP_MAKE_TUPLE")
  | Ir.OP_MAKE_ARRAY count ->
    (match pop_n count st.stack with
     | Some (items, rest) -> st.stack <- VArray (Array.of_list items) :: rest; Ok ()
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
         Error (Printf.sprintf "Runtime error: index %Ld out of bounds (length %d)" idx len)
       else (st.stack <- arr.(Int64.to_int idx) :: rest; Ok ())
     | _ -> Error "VM type error or stack underflow on OP_INDEX_GET")
  | Ir.OP_INDEX_SET ->
    (match st.stack with
     | v :: VInt idx :: VArray arr :: rest ->
       let len = Array.length arr in
       if idx < 0L || idx >= Int64.of_int len then
         Error (Printf.sprintf "Runtime error: index %Ld out of bounds (length %d)" idx len)
       else (arr.(Int64.to_int idx) <- v; st.stack <- v :: rest; Ok ())
     | _ -> Error "VM type error or stack underflow on OP_INDEX_SET")
  | Ir.OP_ARRAY_CONCAT ->
    (match st.stack with
     | VArray b :: VArray a :: rest -> st.stack <- VArray (Array.append a b) :: rest; Ok ()
     | _ -> Error "VM type error on OP_ARRAY_CONCAT")
  | Ir.OP_ARRAY_REPEAT ->
    (match st.stack with
     | VInt count :: VArray a :: rest ->
       let n = Int64.to_int count and len = Array.length a in
       if n <= 0 || len = 0 then (st.stack <- VArray [||] :: rest; Ok ())
       else (st.stack <- VArray (Array.init (len * n) (fun i -> a.(i mod len))) :: rest; Ok ())
     | _ -> Error "VM type error on OP_ARRAY_REPEAT")
  | Ir.OP_TO_STR ->
    (match st.stack with v :: rest -> st.stack <- VString (value_to_string v) :: rest; Ok () | [] -> Error "VM stack underflow on OP_TO_STR")
  | Ir.OP_FMT_SPEC spec ->
    (match st.stack with v :: rest -> st.stack <- VString (format_value spec v) :: rest; Ok () | [] -> Error "VM stack underflow on OP_FMT_SPEC")
  | Ir.OP_CONCAT_STR count ->
    let rec pop_strs n acc s =
      if n <= 0 then Some (acc, s)
      else match s with VString str :: rest -> pop_strs (n - 1) (str :: acc) rest | _ -> None
    in
    (match pop_strs count [] st.stack with
     | Some (parts, rest) -> st.stack <- VString (String.concat "" parts) :: rest; Ok ()
     | None -> Error "VM stack underflow or type error on OP_CONCAT_STR")
  | Ir.OP_INPUT spec ->
    (match exec_input st spec with Ok v -> st.stack <- v :: st.stack; Ok () | Error m -> Error m)
  | Ir.OP_JUMP target -> st.ip <- target; Ok ()
  | Ir.OP_JUMP_IF_FALSE target ->
    (match st.stack with VBool cond :: rest -> st.stack <- rest; if not cond then st.ip <- target; Ok () | _ -> Error "VM type error on OP_JUMP_IF_FALSE")
  | Ir.OP_JUMP_IF_TRUE target ->
    (match st.stack with VBool cond :: rest -> st.stack <- rest; if cond then st.ip <- target; Ok () | _ -> Error "VM type error on OP_JUMP_IF_TRUE")
  | Ir.OP_SET_TEMP id ->
    (match st.stack with v :: rest -> st.stack <- rest; Hashtbl.replace (cur_vars st) (Printf.sprintf "$tmp_%d" id) (ref v); Ok () | [] -> Error "VM stack underflow on OP_SET_TEMP")
  | Ir.OP_GET_TEMP id ->
    (match Hashtbl.find_opt (cur_vars st) (Printf.sprintf "$tmp_%d" id) with Some r -> st.stack <- !r :: st.stack; Ok () | None -> Error "VM undefined temporary slot")
  | Ir.OP_DEF_GLOBAL_I64 name | Ir.OP_DEF_GLOBAL_F32 name | Ir.OP_DEF_GLOBAL_F64 name
  | Ir.OP_DEF_GLOBAL_DEC name | Ir.OP_DEF_GLOBAL_BOOL name | Ir.OP_DEF_GLOBAL_CHAR name
  | Ir.OP_DEF_GLOBAL_STR name | Ir.OP_DEF_GLOBAL_OBJ name ->
    (match st.stack with v :: rest -> st.stack <- rest; Hashtbl.replace (cur_vars st) name (ref v); Ok () | [] -> Error "VM stack underflow on OP_DEF_GLOBAL")
  | Ir.OP_SET_GLOBAL_I64 name | Ir.OP_SET_GLOBAL_F32 name | Ir.OP_SET_GLOBAL_F64 name
  | Ir.OP_SET_GLOBAL_DEC name | Ir.OP_SET_GLOBAL_BOOL name | Ir.OP_SET_GLOBAL_CHAR name
  | Ir.OP_SET_GLOBAL_STR name | Ir.OP_SET_GLOBAL_OBJ name ->
    (match st.stack with v :: rest -> st.stack <- rest; set_var st name v; Ok () | [] -> Error "VM stack underflow on OP_SET_GLOBAL")
  | Ir.OP_GET_GLOBAL_I64 name | Ir.OP_GET_GLOBAL_F32 name | Ir.OP_GET_GLOBAL_F64 name
  | Ir.OP_GET_GLOBAL_DEC name | Ir.OP_GET_GLOBAL_BOOL name | Ir.OP_GET_GLOBAL_CHAR name
  | Ir.OP_GET_GLOBAL_STR name | Ir.OP_GET_GLOBAL_OBJ name ->
    (match find_var st name with Some v -> st.stack <- v :: st.stack; Ok () | None -> Error (Printf.sprintf "VM undefined global '%s'" name))
  | Ir.OP_GET_REF name ->
    (match find_var_ref st name with Some r -> st.stack <- VRef r :: st.stack; Ok () | None -> Error (Printf.sprintf "VM undefined variable for ref '%s'" name))
  | Ir.OP_UNDEF_GLOBAL name -> Hashtbl.remove (cur_vars st) name; Ok ()
  | Ir.OP_MAKE_FUN { entry; fname; params; captures } ->
    let env = Hashtbl.create (List.length captures) in
    List.iter (fun c -> Option.iter (Hashtbl.replace env c) (find_var_ref st c)) captures;
    st.stack <- VFun { entry; fname; pnames = Array.of_list params; bound = Array.make (List.length params) None; env } :: st.stack;
    Ok ()
  | Ir.OP_CALL idxs -> exec_call st idxs
  | Ir.OP_RETURN ->
    (match st.stack, st.frames with
     | v :: _, fr :: rest_frames -> st.frames <- rest_frames; st.stack <- v :: fr.saved_stack; st.ip <- fr.ret_ip; Ok ()
     | _ -> Error "VM stack underflow or 'return' outside of a call on OP_RETURN")
  | Ir.OP_RETURN_VOID ->
    (match st.frames with
     | fr :: rest_frames -> st.frames <- rest_frames; st.stack <- VInt 0L :: fr.saved_stack; st.ip <- fr.ret_ip; Ok ()
     | [] -> Error "'return' outside of a call on OP_RETURN_VOID")
  | Ir.OP_POP ->
    (match st.stack with _ :: rest -> st.stack <- rest; Ok () | [] -> Error "VM stack underflow on OP_POP")
  | Ir.OP_PRINT_I64 ->
    (match st.stack with VInt n :: rest -> st.stack <- rest; emit_output st out_buf (Printf.sprintf "%Ld" n); Ok () | _ -> Error "VM error on OP_PRINT_I64")
  | Ir.OP_PRINT_F32 ->
    (match st.stack with VFloat f :: rest -> st.stack <- rest; emit_output st out_buf (Printf.sprintf "%.6g" f); Ok () | _ -> Error "VM error on OP_PRINT_F32")
  | Ir.OP_PRINT_F64 ->
    (match st.stack with VDouble d :: rest -> st.stack <- rest; emit_output st out_buf (Printf.sprintf "%.15g" d); Ok () | _ -> Error "VM error on OP_PRINT_F64")
  | Ir.OP_PRINT_DEC ->
    (match st.stack with VDecimal m :: rest -> st.stack <- rest; emit_output st out_buf (Decimal.to_string m); Ok () | _ -> Error "VM error on OP_PRINT_DEC")
  | Ir.OP_PRINT_BOOL ->
    (match st.stack with VBool b :: rest -> st.stack <- rest; emit_output st out_buf (if b then "true" else "false"); Ok () | _ -> Error "VM error on OP_PRINT_BOOL")
  | Ir.OP_PRINT_CHAR ->
    (match st.stack with VChar c :: rest -> st.stack <- rest; emit_output st out_buf c; Ok () | _ -> Error "VM error on OP_PRINT_CHAR")
  | Ir.OP_PRINT_STR ->
    (match st.stack with VString s :: rest -> st.stack <- rest; emit_output st out_buf s; Ok () | _ -> Error "VM error on OP_PRINT_STR")
  | Ir.OP_PRINT_OBJ ->
    (match st.stack with ((VTuple _ | VArray _ | VFun _) as v) :: rest -> st.stack <- rest; emit_output st out_buf (value_to_string v); Ok () | _ -> Error "VM error on OP_PRINT_OBJ")
  | Ir.OP_PRINT_SPACE -> emit_output st out_buf " "; Ok ()
  | Ir.OP_PRINT_NEWLINE -> emit_output st out_buf "\n"; Ok ()
  | Ir.OP_ASSERT_EQ ->
    (match st.stack with
     | VString msg :: expected_val :: actual_val :: rest ->
       st.stack <- rest;
       let passed = value_equal actual_val expected_val in
       let expected_s = value_to_string expected_val and actual_s = value_to_string actual_val in
       let res = { passed; span = instr.span; expected = expected_s; actual = actual_s; message = msg } in
       st.assertions_rev <- res :: st.assertions_rev;
       if passed || st.collect_assertions then Ok ()
       else
         let msg_part = if msg = "" then "" else Printf.sprintf " (%s)" msg in
         Error (Printf.sprintf "assertion failed at %s%s: expected %s, got %s"
                  (Span.to_string instr.span) msg_part expected_s actual_s)
     | _ -> Error "VM stack underflow or type error on OP_ASSERT_EQ")
  | Ir.OP_ASSERT_ERR { expected_code; actual_codes; passed } ->
    (match st.stack with
     | VString msg :: rest ->
       st.stack <- rest;
       let actual_s = if actual_codes = [] then "no error" else String.concat ", " actual_codes in
       let res = { passed; span = instr.span; expected = expected_code; actual = actual_s; message = msg } in
       st.assertions_rev <- res :: st.assertions_rev;
       if passed || st.collect_assertions then Ok ()
       else
         let msg_part = if msg = "" then "" else Printf.sprintf " (%s)" msg in
         Error (Printf.sprintf "assert_error failed at %s%s: expected '%s', got %s"
                  (Span.to_string instr.span) msg_part expected_code actual_s)
     | _ -> Error "VM stack underflow or type error on OP_ASSERT_ERR")
  | Ir.OP_HALT -> st.halted <- true; Ok ()
  | _ -> Error "VM internal error: unhandled opcode"

let step ~out_buf (prog : Ir.program) (st : state) : (unit, string) result =
  if st.halted || st.ip >= Array.length prog then begin
    st.halted <- true;
    Ok ()
  end else begin
    let instr = prog.(st.ip) in
    st.ip <- st.ip + 1;
    match Vm_ops_arith.exec_arith_op st instr.op with
    | Some res -> res
    | None ->
      (match Vm_ops_cmp.exec_cmp_op st instr.op with
       | Some res -> res
       | None -> exec_core_op ~out_buf st instr)
  end

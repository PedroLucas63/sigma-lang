open Sigma_common
open Vm_math

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
  | VFun of fun_value
  | VRef of value ref

and fun_value = {
  entry : int;
  fname : string;
  pnames : string array;
  bound : value option array;
  env : (string, value ref) Hashtbl.t;
}

type frame = {
  vars : (string, value ref) Hashtbl.t;
  f_env : (string, value ref) Hashtbl.t;
  self_fn : fun_value option;
  ret_ip : int;
  saved_stack : value list;
}

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

type state = {
  mutable ip : int;
  mutable stack : value list;
  globals : (string, value ref) Hashtbl.t;
  mutable halted : bool;
  mutable collect_assertions : bool;
  mutable assertions_rev : assertion_result list;
  mutable stdin_lines : string list option;
  mutable stream_stdout : bool;
  mutable frames : frame list;
}

let max_call_depth = 100_000

let create_state ?(collect_assertions = false) ?stdin_lines ?(stream_stdout = false) () =
  {
    ip = 0;
    stack = [];
    globals = Hashtbl.create 16;
    halted = false;
    collect_assertions;
    assertions_rev = [];
    stdin_lines;
    stream_stdout;
    frames = [];
  }

let rec value_to_string = function
  | VInt n -> Int64.to_string n
  | VFloat f -> Printf.sprintf "%.6g" f
  | VDouble d -> Printf.sprintf "%.15g" d
  | VDecimal m -> Decimal.to_string m
  | VBool b -> if b then "true" else "false"
  | VChar c -> c
  | VString s -> s
  | VTuple arr -> "(" ^ String.concat ", " (List.map value_to_string (Array.to_list arr)) ^ ")"
  | VArray arr -> "[" ^ String.concat ", " (List.map value_to_string (Array.to_list arr)) ^ "]"
  | VFun _ -> "<function>"
  | VRef r -> Printf.sprintf "&(%s)" (value_to_string !r)

let rec value_equal (a : value) (b : value) : bool =
  match a, b with
  | VInt x, VInt y -> x = y
  | VFloat x, VFloat y -> x = y
  | VDouble x, VDouble y -> x = y
  | VDecimal x, VDecimal y -> Decimal.compare x y = 0
  | VBool x, VBool y -> x = y
  | VChar x, VChar y -> decode_utf8 x = decode_utf8 y
  | VString x, VString y -> String.equal x y
  | VFun x, VFun y -> x == y
  | VTuple xs, VTuple ys | VArray xs, VArray ys ->
    let len = Array.length xs in
    len = Array.length ys && Array.for_all2 value_equal xs ys
  | _ -> false

let cur_vars (st : state) : (string, value ref) Hashtbl.t =
  match st.frames with
  | f :: _ -> f.vars
  | [] -> st.globals

let find_var_ref (st : state) (name : string) : value ref option =
  match st.frames with
  | f :: _ ->
    (match Hashtbl.find_opt f.vars name with
     | Some r -> Some r
     | None ->
       (match Hashtbl.find_opt f.f_env name with
        | Some r -> Some r
        | None -> Hashtbl.find_opt st.globals name))
  | [] -> Hashtbl.find_opt st.globals name

let find_var (st : state) (name : string) : value option =
  match st.frames with
  | f :: _ ->
    (match Hashtbl.find_opt f.vars name with
     | Some r -> Some !r
     | None ->
       (match Hashtbl.find_opt f.f_env name with
        | Some r -> Some !r
        | None ->
          (match f.self_fn with
           | Some self when self.fname = name -> Some (VFun self)
           | _ -> Option.map ( ! ) (Hashtbl.find_opt st.globals name))))
  | [] -> Option.map ( ! ) (Hashtbl.find_opt st.globals name)

let set_var (st : state) (name : string) (v : value) : unit =
  match find_var_ref st name with
  | Some r -> r := v
  | None -> Hashtbl.replace st.globals name (ref v)

let emit_output (st : state) (out_buf : Buffer.t) (s : string) : unit =
  Buffer.add_string out_buf s;
  if st.stream_stdout then begin
    output_string stdout s;
    flush stdout
  end

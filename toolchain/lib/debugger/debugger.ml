open Sigma_ir
open Sigma_interp

let format_globals globals =
  let pairs =
    Hashtbl.fold
      (fun k v acc ->
        if String.starts_with ~prefix:"$" k then acc else (k, v) :: acc)
      globals []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let inner =
    pairs
    |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" k (Vm.value_to_string v))
    |> String.concat ", "
  in
  "{" ^ inner ^ "}"

let format_stack stack =
  let inner = stack |> List.map Vm.value_to_string |> String.concat "; " in
  "[" ^ inner ^ "]"

let trace_execution (prog : Ir.program) : (string, string) result =
  let st = Vm.create_state () in
  let out_buf = Buffer.create 64 in
  let trace_buf = Buffer.create 256 in
  let rec loop step_no =
    if st.halted || st.ip >= Array.length prog then begin
      if Buffer.length out_buf > 0 then begin
        Buffer.add_string trace_buf "--- stdout ---\n";
        Buffer.add_buffer trace_buf out_buf
      end;
      Ok (Buffer.contents trace_buf)
    end else begin
      let cur_ip = st.ip in
      let instr = prog.(cur_ip) in
      let op_str = Format.asprintf "%a" Ir.pp_opcode instr.op in
      match Vm.step ~out_buf prog st with
      | Error e -> Error e
      | Ok () ->
        Buffer.add_string
          trace_buf
          (Printf.sprintf
             "[step %02d] ip=%04d | %-22s | stack=%s | globals=%s\n"
             step_no cur_ip op_str
             (format_stack st.stack)
             (format_globals st.globals));
        loop (step_no + 1)
    end
  in
  loop 1

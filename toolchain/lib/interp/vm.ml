open Sigma_ir

include Vm_state

let step = Vm_exec.step

let run ?stdin_lines ?(stream_stdout = false) (prog : Ir.program) : (string, string) result =
  let st = create_state ?stdin_lines ~stream_stdout () in
  let out_buf = Buffer.create 64 in
  let rec loop () =
    if st.halted then Ok (Buffer.contents out_buf)
    else
      match step ~out_buf prog st with
      | Ok () -> loop ()
      | Error msg -> Error msg
  in
  loop ()

let run_tests ?stdin_lines (prog : Ir.program) : (test_report, string) result =
  let st = create_state ~collect_assertions:true ?stdin_lines () in
  let out_buf = Buffer.create 64 in
  let rec loop () =
    if st.halted then begin
      let assertions = List.rev st.assertions_rev in
      let passed_count = List.length (List.filter (fun a -> a.passed) assertions) in
      let failed_count = List.length (List.filter (fun a -> not a.passed) assertions) in
      Ok { stdout = Buffer.contents out_buf; passed_count; failed_count; assertions }
    end else
      match step ~out_buf prog st with
      | Ok () -> loop ()
      | Error msg -> Error msg
  in
  loop ()

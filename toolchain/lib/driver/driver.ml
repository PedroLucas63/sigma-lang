open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sigma_sema
open Sigma_ir
open Sigma_interp
open Sigma_codegen
open Sigma_linter
open Sigma_debugger

let compile_frontend (src : Source.t) :
    (Typed_ast.program * Ir.program, Diagnostic.t list) result =
  match Lexer.tokenize src with
  | Error errs -> Error errs
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error errs -> Error errs
     | Ok ast ->
       (match Typecheck.check_program ast with
        | Error errs -> Error errs
        | Ok typed_ast ->
          let ir = Lower.lower_program typed_ast in
          Ok (typed_ast, ir)))

let extract_stdin_directives (content : string) : string list option =
  let lines = String.split_on_char '\n' content in
  let prefix_sp = "# @stdin: " in
  let prefix_empty = "# @stdin:" in
  let acc = ref [] in
  List.iter
    (fun raw_line ->
      let len = String.length raw_line in
      let line =
        if len > 0 && raw_line.[len - 1] = '\r' then String.sub raw_line 0 (len - 1)
        else raw_line
      in
      let trimmed = String.trim line in
      if String.starts_with ~prefix:prefix_sp line then
        let plen = String.length prefix_sp in
        acc := String.sub line plen (String.length line - plen) :: !acc
      else if trimmed = prefix_empty then
        acc := "" :: !acc)
    lines;
  match List.rev !acc with
  | [] -> None
  | xs -> Some xs

let run_source ?stdin_lines (src : Source.t) : (string, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (_typed_ast, ir) ->
    let stdin_lines =
      match stdin_lines with
      | Some _ -> stdin_lines
      | None -> extract_stdin_directives src.content
    in
    (match Vm.run ?stdin_lines ir with
     | Ok out -> Ok out
     | Error msg ->
       Error [ Diagnostic.error ~code:"E0900" ~span:Span.dummy msg ])

let build_source ~output_path (src : Source.t) : (unit, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (typed_ast, _ir) ->
    (match Codegen_c.compile_executable ~output_path typed_ast with
     | Ok () -> Ok ()
     | Error msg ->
       Error [ Diagnostic.error ~code:"E0901" ~span:Span.dummy msg ])

let emit_c_source ?output_path (src : Source.t) : (string, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (typed_ast, _ir) ->
    let c_code = Codegen_c.emit_c typed_ast in
    (match output_path with
     | None -> Ok c_code
     | Some path ->
       (match Codegen_c.write_c_file ~output_path:path typed_ast with
        | Ok () -> Ok c_code
        | Error msg ->
          Error [ Diagnostic.error ~code:"E0901" ~span:Span.dummy msg ]))

let lint_source (src : Source.t) : (Diagnostic.t list, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (typed_ast, _ir) -> Ok (Linter.lint_program typed_ast)

let debug_source (src : Source.t) : (string, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (_typed_ast, ir) ->
    (match Debugger.trace_execution ir with
     | Ok trace -> Ok trace
     | Error msg ->
       Error [ Diagnostic.error ~code:"E0902" ~span:Span.dummy msg ])

type test_mode =
  | ModeVm
  | ModeNative
  | ModeParity

type file_test_result = {
  filename : string;
  passed_count : int;
  failed_count : int;
  assertions : Vm.assertion_result list;
  native_ok : bool option;
  error_msg : string option;
}

let run_native_test_binary ?stdin_lines (typed_ast : Typed_ast.program) : (int * string, string) result =
  let bin_path = Filename.temp_file "sigma_test_native_" ".out" in
  let out_path = Filename.temp_file "sigma_test_out_" ".txt" in
  let in_path_opt =
    match stdin_lines with
    | None -> None
    | Some lines ->
      let p = Filename.temp_file "sigma_test_in_" ".txt" in
      let oc = open_out p in
      List.iter (fun l -> output_string oc l; output_char oc '\n') lines;
      close_out oc;
      Some p
  in
  let cleanup () =
    (try if Sys.file_exists bin_path then Sys.remove bin_path with _ -> ());
    (try if Sys.file_exists out_path then Sys.remove out_path with _ -> ());
    (match in_path_opt with
     | Some p -> (try if Sys.file_exists p then Sys.remove p with _ -> ())
     | None -> ())
  in
  match Codegen_c.compile_executable ~test_mode:true ~output_path:bin_path typed_ast with
  | Error msg ->
    cleanup ();
    Error msg
  | Ok () ->
    let cmd =
      match in_path_opt with
      | None ->
        Printf.sprintf "%s > %s 2>&1" (Filename.quote bin_path) (Filename.quote out_path)
      | Some in_p ->
        Printf.sprintf "%s < %s > %s 2>&1"
          (Filename.quote bin_path) (Filename.quote in_p) (Filename.quote out_path)
    in
    let exit_code = Sys.command cmd in
    let captured =
      try
        let ic = open_in out_path in
        let len = in_channel_length ic in
        let s = really_input_string ic len in
        close_in ic;
        s
      with _ -> ""
    in
    cleanup ();
    Ok (exit_code, captured)

let test_source ?(mode = ModeVm) (src : Source.t) :
    (file_test_result, Diagnostic.t list) result =
  match compile_frontend src with
  | Error errs -> Error errs
  | Ok (typed_ast, ir) ->
    let stdin_lines = extract_stdin_directives src.content in
    (match Vm.run_tests ?stdin_lines ir with
     | Error msg ->
       Error [ Diagnostic.error ~code:"E0900" ~span:Span.dummy msg ]
     | Ok vm_rep ->
       (match mode with
        | ModeVm ->
          Ok
            {
              filename = src.filename;
              passed_count = vm_rep.passed_count;
              failed_count = vm_rep.failed_count;
              assertions = vm_rep.assertions;
              native_ok = None;
              error_msg = None;
            }
        | ModeNative | ModeParity ->
          (match run_native_test_binary ?stdin_lines typed_ast with
           | Error msg ->
             Error [ Diagnostic.error ~code:"E0901" ~span:Span.dummy msg ]
           | Ok (exit_code, native_out) ->
             let expected_exit = if vm_rep.failed_count = 0 then 0 else 1 in
             let parity_ok =
               exit_code = expected_exit
               && (vm_rep.failed_count > 0 || String.equal native_out vm_rep.stdout)
             in
             let failed_count =
               if parity_ok then vm_rep.failed_count else vm_rep.failed_count + 1
             in
             let error_msg =
               if parity_ok then None
               else
                 Some
                   (Printf.sprintf
                      "Native C99 parity mismatch (exit=%d, expected=%d):\nNative output:\n%s\nVM output:\n%s"
                      exit_code expected_exit native_out vm_rep.stdout)
             in
             Ok
               {
                 filename = src.filename;
                 passed_count = vm_rep.passed_count;
                 failed_count;
                 assertions = vm_rep.assertions;
                 native_ok = Some (exit_code = 0 && parity_ok);
                 error_msg;
               })))

let render_test_suite ~(mode : test_mode) (results : file_test_result list) : string * bool =
  let buf = Buffer.create 256 in
  let mode_label =
    match mode with
    | ModeVm -> "VM Interpreter"
    | ModeNative -> "Native C99 (-O3)"
    | ModeParity -> "VM + Native C99 Parity"
  in
  Buffer.add_string buf (Printf.sprintf "=== Sigma Test Runner [%s] ===\n" mode_label);
  let total_files = List.length results in
  let total_passed = ref 0 in
  let total_failed = ref 0 in
  let all_ok = ref true in
  List.iter
    (fun (r : file_test_result) ->
      total_passed := !total_passed + r.passed_count;
      total_failed := !total_failed + r.failed_count;
      let file_ok = r.failed_count = 0 && Option.is_none r.error_msg in
      if not file_ok then all_ok := false;
      let status_tag = if file_ok then "[PASS]" else "[FAIL]" in
      let parity_badge =
        match r.native_ok with
        | None -> ""
        | Some true -> " [VM == C99 OK]"
        | Some false -> " [C99 FAIL]"
      in
      Buffer.add_string
        buf
        (Printf.sprintf "%s %s (%d passed, %d failed)%s\n"
           status_tag r.filename r.passed_count r.failed_count parity_badge);
      List.iter
        (fun (a : Vm.assertion_result) ->
          if not a.passed then begin
            let msg_part = if a.message = "" then "" else Printf.sprintf " \"%s\" —" a.message in
            Buffer.add_string
              buf
              (Printf.sprintf "  ✗ %s:%s expected %s, got %s\n"
                 (Span.to_string a.span) msg_part a.expected a.actual)
          end)
        r.assertions;
      Option.iter
        (fun msg -> Buffer.add_string buf (Printf.sprintf "  ✗ %s\n" msg))
        r.error_msg)
    results;
  Buffer.add_string
    buf
    (Printf.sprintf
       "----------------------------------------\nSummary: %d file(s) | %d assertion(s) passed | %d failed\n"
       total_files !total_passed !total_failed);
  (Buffer.contents buf, !all_ok)

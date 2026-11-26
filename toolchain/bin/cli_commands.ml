open Sigma_common
open Sigma_diagnostics
open Sigma_driver
open Sigma_pkg

let print_diagnostics src diags =
  List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) diags

let load_or_exit path =
  match Source.load path with
  | Ok src -> src
  | Error msg ->
    Printf.eprintf "error: failed to read '%s': %s\n" path msg;
    exit 1

let usage () =
  prerr_endline "Sigma Toolchain CLI (sigma)";
  prerr_endline "Usage:";
  prerr_endline "  sigma run [file.sgm | <script>]           Run entry/file in VM or execute a sigma.toml script";
  prerr_endline "  sigma build [file.sgm] [--lib] [-o <out>] Compile to native executable or shared library";
  prerr_endline "  sigma emit-c [file.sgm] [-o <out.c> | --stdout]  Transpile entry/file to standalone C99 source (.c)";
  prerr_endline "  sigma transpile [file.sgm] [-o <out.c> | --stdout] Alias for 'sigma emit-c'";
  prerr_endline "  sigma test [path] [--native | --parity]   Execute Sigma test files (default: VM interpreter)";
  prerr_endline "  sigma check [file.sgm]                    Typecheck entry/file without running";
  prerr_endline "  sigma lint [file.sgm]                     Run static linter rules on entry/file";
  prerr_endline "  sigma debug [file.sgm]                    Run step-by-step VM debugger trace";
  prerr_endline "  sigma pkg init [name] [--lib] [--native] [-y] Initialize project (sigma.toml, src/, tests/)";
  prerr_endline "  sigma pkg pack [-o <out.sgmpkg>]          Build and pack library into .sgmpkg archive";
  prerr_endline "  sigma pkg run <script>                    Execute a script from [scripts] in sigma.toml";
  exit 1

let resolve_entry_or_exit () : Manifest.t * string =
  match Manifest.load () with
  | Ok m -> (m, m.entry)
  | Error msg ->
    Printf.eprintf "error: %s\n" msg;
    exit 1

let run_manifest_script ?(extra_args = []) (script_name : string) (m : Manifest.t) : int =
  match Manifest.resolve_script script_name m with
  | None ->
    Printf.eprintf "error: script '%s' not found in [scripts] of sigma.toml\n" script_name;
    exit 1
  | Some (Manifest.ScriptEntry { file; fn_name }) ->
    let src = load_or_exit file in
    (match Driver.run_source_with_exit ~stream_stdout:true ~args:(file :: extra_args) ~entry_name:fn_name src with
     | Ok (_, code) -> exit code
     | Error errs -> print_diagnostics src errs; exit 1)
  | Some (Manifest.ScriptCmd cmd_raw) ->
    let self_bin = Filename.quote Sys.argv.(0) in
    let base_cmd =
      if String.equal cmd_raw "sigma" then self_bin
      else if String.starts_with ~prefix:"sigma " cmd_raw then
        self_bin ^ String.sub cmd_raw 5 (String.length cmd_raw - 5)
      else cmd_raw
    in
    let cmd =
      if extra_args = [] then base_cmd
      else
        let quoted_args = String.concat " " (List.map Filename.quote extra_args) in
        if String.starts_with ~prefix:"sigma run" cmd_raw then
          base_cmd ^ " -- " ^ quoted_args
        else
          base_cmd ^ " " ^ quoted_args
    in
    Printf.printf "> Running script '%s': %s\n%!" script_name cmd_raw;
    exit (Sys.command cmd)

let rec collect_sgm_files (path : string) : string list =
  if not (Sys.file_exists path) then []
  else if Sys.is_directory path then
    Sys.readdir path
    |> Array.to_list
    |> List.sort String.compare
    |> List.concat_map (fun e -> collect_sgm_files (Filename.concat path e))
  else if Filename.check_suffix path ".sgm" then [ path ]
  else []

let prompt_line label default_val =
  Printf.printf "%s [%s]: %!" label default_val;
  try
    let line = String.trim (input_line stdin) in
    if line = "" then default_val else line
  with End_of_file ->
    print_newline ();
    default_val

let run_pkg_init ~name_opt ~yes ~is_lib ~is_native =
  let default_name =
    match name_opt with Some n -> n | None -> Filename.basename (Sys.getcwd ())
  in
  let kind = if is_lib then Manifest.KindLib else Manifest.KindApp in
  let native =
    if is_native then
      { Manifest.sources = [ "native/lib.c" ]; libs = []; cflags = []; cxxflags = []; ldflags = [] }
    else
      { Manifest.sources = []; libs = []; cflags = []; cxxflags = []; ldflags = [] }
  in
  let m =
    if yes then Manifest.default ~kind ~native ~name:default_name ()
    else
      let name = prompt_line "Project name" default_name in
      let version = prompt_line "Version" "0.1.0" in
      let description = prompt_line "Description" "" in
      let authors_raw = prompt_line "Authors (comma-separated)" "" in
      let authors =
        if authors_raw = "" then []
        else String.split_on_char ',' authors_raw |> List.map String.trim |> List.filter (fun s -> s <> "")
      in
      let def_entry = if is_lib then "src/lib.sgm" else "src/main.sgm" in
      let entry = prompt_line "Base compilation file (entry)" def_entry in
      let d = Filename.dirname entry in
      let src_dir = if d = "." then "src" else d in
      let test_dir = prompt_line "Tests directory" "tests" in
      Manifest.default ~version ~kind ~description ~authors ~src_dir ~entry ~test_dir ~native ~name ()
  in
  let root_dir =
    match name_opt with
    | Some dir when dir <> "." && not (Sys.file_exists "sigma.toml") && not (String.equal dir (Filename.basename (Sys.getcwd ()))) -> dir
    | _ -> "."
  in
  match Manifest.scaffold_project ~root_dir m with
  | Error err ->
    Printf.eprintf "error: failed to initialize project: %s\n" err;
    exit 1
  | Ok created ->
    Printf.printf "Initialized Sigma project '%s' (v%s):\n" m.name m.version;
    List.iter (fun p -> Printf.printf "  + %s\n" p) created

let run_pkg_pack ~output_opt =
  let m, entry = resolve_entry_or_exit () in
  let ext = Pkg_pack.shared_lib_ext () in
  let stage_dir = Filename.temp_file "sigma_pack_build_" "" in
  (if Sys.file_exists stage_dir then try Sys.remove stage_dir with _ -> ());
  (try Sys.mkdir stage_dir 0o755 with _ -> ());
  let lib_so = Filename.concat stage_dir ("lib" ^ m.name ^ ext) in
  let src = load_or_exit entry in
  let so, h, sgm =
    match Driver.build_lib_source ~output_path:lib_so src with
    | Ok res -> res
    | Error diags -> print_diagnostics src diags; exit 1
  in
  let out_pkg =
    match output_opt with
    | Some o -> o
    | None -> Filename.concat "dist" (Printf.sprintf "%s-%s.sgmpkg" m.name m.version)
  in
  let res = Pkg_pack.pack_archive ~manifest_path:"sigma.toml" ~artifacts:[ so; h; sgm ] ~output_path:out_pkg in
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote stage_dir)));
  match res with
  | Error err ->
    Printf.eprintf "error: %s\n" err;
    exit 1
  | Ok p -> Printf.printf "Packed Sigma library package: %s\n" p

let run_test_command rest_args =
  let mode = ref Driver.ModeVm in
  let paths = ref [] in
  List.iter
    (function
      | "--native" -> mode := Driver.ModeNative
      | "--parity" -> mode := Driver.ModeParity
      | "--vm" -> mode := Driver.ModeVm
      | p when String.starts_with ~prefix:"-" p ->
        Printf.eprintf "error: unknown flag '%s' for 'sigma test'\n" p;
        usage ()
      | p -> paths := p :: !paths)
    rest_args;
  let target_paths =
    match List.rev !paths with
    | _ :: _ as ps -> ps
    | [] -> (match Manifest.load () with Ok m -> [ m.test_dir ] | Error _ -> [ "tests" ])
  in
  let files = List.concat_map collect_sgm_files target_paths in
  if files = [] then begin
    Printf.eprintf "error: no test files (*.sgm) found in %s\n" (String.concat ", " target_paths);
    exit 1
  end;
  let results = ref [] and had_compile_error = ref false in
  List.iter
    (fun file ->
      let src = load_or_exit file in
      match Driver.test_source ~mode:!mode src with
      | Ok res -> results := res :: !results
      | Error errs ->
        had_compile_error := true;
        Printf.eprintf "[FAIL] %s (compilation error)\n" file;
        print_diagnostics src errs)
    files;
  let report, suite_ok = Driver.render_test_suite ~mode:!mode (List.rev !results) in
  print_string report;
  if !had_compile_error || not suite_ok then exit 1

let run_emit_c_command rest_args =
  let input_file = ref None and output_path = ref None and to_stdout = ref false and entry_name = ref None in
  let rec parse_args = function
    | [] -> ()
    | "--stdout" :: tl | "-" :: tl -> to_stdout := true; parse_args tl
    | "-o" :: out :: tl -> output_path := Some out; parse_args tl
    | "--entry" :: fn :: tl -> entry_name := Some fn; parse_args tl
    | arg :: _ when String.starts_with ~prefix:"-" arg ->
      Printf.eprintf "error: unknown flag '%s' for 'sigma emit-c'\n" arg;
      usage ()
    | file :: tl -> input_file := Some file; parse_args tl
  in
  parse_args rest_args;
  let target_file =
    match !input_file with Some f -> f | None -> snd (resolve_entry_or_exit ())
  in
  let src = load_or_exit target_file in
  if !to_stdout then
    match Driver.emit_c_source ?entry_name:!entry_name src with
    | Ok c_code -> print_string c_code
    | Error errs -> print_diagnostics src errs; exit 1
  else
    let out_c =
      match !output_path with
      | Some out -> out
      | None -> (try Filename.remove_extension target_file with Invalid_argument _ -> target_file) ^ ".c"
    in
    match Driver.emit_c_source ~output_path:out_c ?entry_name:!entry_name src with
    | Ok _ -> Printf.printf "Generated C99 source: %s\n" out_c
    | Error errs -> print_diagnostics src errs; exit 1

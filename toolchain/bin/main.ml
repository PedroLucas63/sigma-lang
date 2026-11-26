open Sigma_driver
open Sigma_pkg
open Cli_commands

let run_with_source target f =
  let src = load_or_exit target in
  match f src with
  | Ok v -> v
  | Error errs -> print_diagnostics src errs; exit 1

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | "run" :: rest ->
    let rec parse_run entry_opt target_opt = function
      | [] -> (entry_opt, target_opt, [])
      | "--" :: tl -> (entry_opt, target_opt, tl)
      | "--entry" :: fn :: tl -> parse_run (Some fn) target_opt tl
      | arg :: tl ->
        (match target_opt with
         | None -> parse_run entry_opt (Some arg) tl
         | Some _ -> (entry_opt, target_opt, arg :: tl))
    in
    let entry_opt, target_opt, prog_args = parse_run None None rest in
    let run_file ?entry_name target prog_args =
      let _, code =
        run_with_source target (Driver.run_source_with_exit ~stream_stdout:true ~args:(target :: prog_args) ?entry_name)
      in
      if code <> 0 then exit code
    in
    (match target_opt, entry_opt with
     | None, _ ->
       let _m, entry = resolve_entry_or_exit () in
       run_file ?entry_name:entry_opt entry prog_args
     | Some target, Some fn_name ->
       run_file ~entry_name:fn_name target prog_args
     | Some target, None ->
       if Sys.file_exists target || Filename.check_suffix target ".sgm" then
         run_file target prog_args
       else
         (match Manifest.load () with
          | Ok m when Option.is_some (Manifest.resolve_script target m) ->
            ignore (run_manifest_script ~extra_args:prog_args target m)
          | _ -> run_file target prog_args))
  | "build" :: rest when List.mem "--emit-c" rest ->
    run_emit_c_command (List.filter (fun a -> a <> "--emit-c") rest)
  | "build" :: rest ->
    let input_file = ref None and output_path = ref None and entry_name = ref None and as_lib = ref false in
    let rec parse_build = function
      | [] -> ()
      | "--lib" :: tl -> as_lib := true; parse_build tl
      | "-o" :: out :: tl -> output_path := Some out; parse_build tl
      | "--entry" :: fn :: tl -> entry_name := Some fn; parse_build tl
      | arg :: _ when String.starts_with ~prefix:"-" arg ->
        Printf.eprintf "error: unknown flag '%s' for 'sigma build'\n" arg;
        usage ()
      | file :: tl -> input_file := Some file; parse_build tl
    in
    parse_build rest;
    let is_lib_manifest =
      match !input_file, Manifest.load () with
      | None, Ok m -> m.kind = Manifest.KindLib
      | _ -> false
    in
    let build_as_lib = !as_lib || is_lib_manifest in
    let target, out =
      match !input_file, !output_path with
      | None, None ->
        let m, entry = resolve_entry_or_exit () in
        let default_out =
          if build_as_lib then Filename.concat "dist" ("lib" ^ m.name ^ Pkg_pack.shared_lib_ext ())
          else m.name
        in
        (entry, default_out)
      | None, Some o ->
        let _m, entry = resolve_entry_or_exit () in
        (entry, o)
      | Some f, None ->
        let stem = Filename.remove_extension f in
        let default_out =
          if build_as_lib then
            Filename.concat (Filename.dirname f) ("lib" ^ Filename.basename stem ^ Pkg_pack.shared_lib_ext ())
          else stem
        in
        (f, default_out)
      | Some f, Some o -> (f, o)
    in
    if build_as_lib then begin
      let so, h, sgm = run_with_source target (Driver.build_lib_source ~output_path:out) in
      Printf.printf "Built native library:\n  + %s\n  + %s\n  + %s\n" so h sgm
    end else begin
      run_with_source target (Driver.build_source ?entry_name:!entry_name ~output_path:out);
      Printf.printf "Built native executable: %s\n" out
    end
  | ("emit-c" | "transpile") :: rest -> run_emit_c_command rest
  | "test" :: rest -> run_test_command rest
  | [ "check" ] ->
    let _m, entry = resolve_entry_or_exit () in
    ignore (run_with_source entry Driver.compile_frontend);
    Printf.printf "OK: %s\n" entry
  | [ "check"; file ] ->
    ignore (run_with_source file Driver.compile_frontend);
    Printf.printf "OK: %s\n" file
  | [ "lint" ] ->
    let _m, entry = resolve_entry_or_exit () in
    let src = load_or_exit entry in
    print_diagnostics src (run_with_source entry Driver.lint_source)
  | [ "lint"; file ] ->
    let src = load_or_exit file in
    print_diagnostics src (run_with_source file Driver.lint_source)
  | [ "debug" ] ->
    let _m, entry = resolve_entry_or_exit () in
    print_string (run_with_source entry Driver.debug_source)
  | [ "debug"; file ] ->
    print_string (run_with_source file Driver.debug_source)
  | "pkg" :: "init" :: rest ->
    let name_opt = ref None and yes = ref false and is_lib = ref false and is_native = ref false in
    List.iter
      (function
        | "-y" | "--yes" -> yes := true
        | "--lib" -> is_lib := true
        | "--app" -> is_lib := false
        | "--native" -> is_native := true
        | arg when String.starts_with ~prefix:"-" arg ->
          Printf.eprintf "error: unknown flag '%s' for 'sigma pkg init'\n" arg;
          usage ()
        | n -> name_opt := Some n)
      rest;
    run_pkg_init ~name_opt:!name_opt ~yes:!yes ~is_lib:!is_lib ~is_native:!is_native
  | [ "pkg"; "pack" ] -> run_pkg_pack ~output_opt:None
  | [ "pkg"; "pack"; "-o"; out ] -> run_pkg_pack ~output_opt:(Some out)
  | "pkg" :: "run" :: script_name :: extra ->
    let extra_args = match extra with "--" :: tl -> tl | _ -> extra in
    let m, _ = resolve_entry_or_exit () in
    ignore (run_manifest_script ~extra_args script_name m)
  | _ -> usage ()

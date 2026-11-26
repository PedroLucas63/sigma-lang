let shared_lib_ext () : string =
  if Sys.win32 then ".dll"
  else
    let uname =
      try
        let ic = Unix.open_process_in "uname -s 2>/dev/null" in
        let s = try String.trim (input_line ic) with End_of_file -> "Linux" in
        ignore (Unix.close_process_in ic);
        s
      with _ -> "Linux"
    in
    if uname = "Darwin" then ".dylib" else ".so"

let rec mkdir_p (dir : string) : unit =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let copy_file src dst =
  mkdir_p (Filename.dirname dst);
  let ic = open_in_bin src in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  let oc = open_out_bin dst in
  output_string oc data;
  close_out oc

let cache_root () : string =
  match Sys.getenv_opt "SIGMA_CACHE_DIR" with
  | Some d when d <> "" -> Filename.concat d "pkg_cache"
  | _ ->
    let base =
      match Sys.getenv_opt "XDG_CACHE_HOME" with
      | Some d when d <> "" -> d
      | _ ->
        (match Sys.getenv_opt "HOME" with
         | Some h when h <> "" -> Filename.concat h ".cache"
         | _ -> Filename.get_temp_dir_name ())
    in
    Filename.concat (Filename.concat base "sigma") "pkg_cache"

let pack_archive ~manifest_path ~artifacts ~output_path : (string, string) result =
  let stage = Filename.temp_file "sigma_pack_" "" in
  (if Sys.file_exists stage then try Sys.remove stage with _ -> ());
  mkdir_p stage;
  let cleanup () =
    ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote stage)))
  in
  try
    copy_file manifest_path (Filename.concat stage "sigma.toml");
    List.iter
      (fun art ->
        if Sys.file_exists art then
          copy_file art (Filename.concat stage (Filename.basename art)))
      artifacts;
    mkdir_p (Filename.dirname output_path);
    let cmd =
      Printf.sprintf "tar -czf %s -C %s . 2>&1"
        (Filename.quote output_path) (Filename.quote stage)
    in
    let code = Sys.command cmd in
    cleanup ();
    if code = 0 then Ok output_path
    else Error (Printf.sprintf "failed to create .sgmpkg archive (tar exit %d)" code)
  with exn ->
    cleanup ();
    Error (Printexc.to_string exn)

let unpack_to_cache (pkg_path : string) : (string * Manifest.t, string) result =
  if not (Sys.file_exists pkg_path) then
    Error (Printf.sprintf "package '%s' not found" pkg_path)
  else
    let mtime =
      try Printf.sprintf "%.3f" (Unix.stat pkg_path).Unix.st_mtime with _ -> "0"
    in
    let hash = Digest.to_hex (Digest.string (pkg_path ^ "|" ^ mtime)) in
    let dest = Filename.concat (cache_root ()) hash in
    let toml_path = Filename.concat dest "sigma.toml" in
    let extract_ok =
      if Sys.file_exists toml_path then Ok ()
      else begin
        mkdir_p dest;
        let cmd =
          Printf.sprintf "tar -xzf %s -C %s 2>&1"
            (Filename.quote pkg_path) (Filename.quote dest)
        in
        if Sys.command cmd = 0 then Ok ()
        else Error (Printf.sprintf "failed to extract .sgmpkg '%s'" pkg_path)
      end
    in
    match extract_ok with
    | Error e -> Error e
    | Ok () ->
      (match Manifest.load ~path:toml_path () with
       | Error e -> Error e
       | Ok m -> Ok (dest, m))

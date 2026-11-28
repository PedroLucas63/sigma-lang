let rec mkdir_p (dir : string) : unit =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let write_file_if_missing (path : string) (content : string) : bool =
  mkdir_p (Filename.dirname path);
  if Sys.file_exists path then false
  else begin
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    true
  end

let scaffold ~root_dir ~name ~version ~kind_is_lib ~src_dir ~entry ~test_dir ~native_sources ~toml_str
    : (string list, string) result =
  try
    mkdir_p root_dir;
    let join p = if root_dir = "." then p else Filename.concat root_dir p in
    let toml_path = join "sigma.toml" in
    let oc = open_out toml_path in
    output_string oc toml_str;
    close_out oc;
    let created = ref [ toml_path ] in
    mkdir_p (join src_dir);
    mkdir_p (join test_dir);
    let entry_path = join entry in
    let default_entry =
      if kind_is_lib then
        if native_sources <> [] then
          Printf.sprintf
            "# Biblioteca %s (%s)\nextern \"C\" from %S fun native_add(a: int, b: int): int;\n\npublic fun somar(a: int, b: int): int {\n  return native_add(a, b);\n}\n"
            name version (List.hd native_sources)
        else
          Printf.sprintf
            "# Biblioteca %s (%s)\npublic fun somar(a: int, b: int): int {\n  return a + b;\n}\n"
            name version
      else
        Printf.sprintf
          "# Projeto %s (%s)\nconst PROJETO := %S;\nprintln(\"Bem-vindo ao\", PROJETO);\n"
          name version name
    in
    if write_file_if_missing entry_path default_entry then created := entry_path :: !created;
    List.iter
      (fun src ->
        let p = join src in
        let c_code = "#include <stdint.h>\n\nint64_t native_add(int64_t a, int64_t b) {\n  return a + b;\n}\n" in
        if write_file_if_missing p c_code then created := p :: !created)
      native_sources;
    let test_name = if kind_is_lib then "lib_test.sgm" else "main_test.sgm" in
    let test_path = join (Filename.concat test_dir test_name) in
    let default_test =
      if kind_is_lib then
        Printf.sprintf
          "# Suíte de testes — %s\nfrom %S import somar;\nassert(somar(10, 3), 13, \"Soma da biblioteca\");\n"
          name entry
      else
        Printf.sprintf
          "# Suíte de testes inicial — %s\nassert(10 + 3, 13, \"Soma de inteiros\");\nassert(0.1m + 0.2m, 0.3m, \"Precisão exata decimal 128-bit\");\nassert_error(\"E0106\", \"const X : int;\", \"Constante sem inicializador deve falhar\");\n"
          name
    in
    if write_file_if_missing test_path default_test then created := test_path :: !created;
    Ok (List.rev !created)
  with exn -> Error (Printexc.to_string exn)

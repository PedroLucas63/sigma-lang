type t = {
  name : string;
  version : string;
  description : string;
  authors : string list;
  src_dir : string;
  entry : string;
  test_dir : string;
  scripts : (string * string) list;
}

let default_scripts =
  [
    ("start", "sigma run");
    ("build", "sigma build");
    ("test", "sigma test");
    ("test-parity", "sigma test --parity");
    ("lint", "sigma lint");
  ]

let default
    ?(version = "0.1.0")
    ?(description = "")
    ?(authors = [])
    ?(src_dir = "src")
    ?(entry = "src/main.sgm")
    ?(test_dir = "tests")
    ?(scripts = default_scripts)
    ~name
    ()
  =
  { name; version; description; authors; src_dir; entry; test_dir; scripts }

let toml_quote (s : string) : string =
  let buf = Buffer.create (String.length s + 4) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let to_toml (m : t) : string =
  let authors_str =
    m.authors |> List.map toml_quote |> String.concat ", "
  in
  let scripts_lines =
    m.scripts
    |> List.map (fun (k, v) -> Printf.sprintf "%s = %s" k (toml_quote v))
    |> String.concat "\n"
  in
  Printf.sprintf
    "[package]\nname = %s\nversion = %s\ndescription = %s\nauthors = [%s]\n\n[build]\nsrc_dir = %s\nentry = %s\ntest_dir = %s\n\n[scripts]\n%s\n"
    (toml_quote m.name)
    (toml_quote m.version)
    (toml_quote m.description)
    authors_str
    (toml_quote m.src_dir)
    (toml_quote m.entry)
    (toml_quote m.test_dir)
    scripts_lines

let unquote (s : string) : string =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    let inner = String.sub s 1 (len - 2) in
    let buf = Buffer.create (String.length inner) in
    let i = ref 0 in
    let ilen = String.length inner in
    while !i < ilen do
      if inner.[!i] = '\\' && !i + 1 < ilen then begin
        (match inner.[!i + 1] with
         | '"' -> Buffer.add_char buf '"'
         | '\\' -> Buffer.add_char buf '\\'
         | 'n' -> Buffer.add_char buf '\n'
         | 'r' -> Buffer.add_char buf '\r'
         | 't' -> Buffer.add_char buf '\t'
         | c ->
           Buffer.add_char buf '\\';
           Buffer.add_char buf c);
        i := !i + 2
      end else begin
        Buffer.add_char buf inner.[!i];
        incr i
      end
    done;
    Buffer.contents buf
  else
    s

let parse_string_array (s : string) : string list =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '[' && s.[len - 1] = ']' then
    let inner = String.trim (String.sub s 1 (len - 2)) in
    if inner = "" then []
    else
      String.split_on_char ',' inner
      |> List.map unquote
      |> List.filter (fun x -> x <> "")
  else
    []

let strip_comment (line : string) : string =
  let in_quotes = ref false in
  let len = String.length line in
  let idx = ref len in
  let i = ref 0 in
  while !i < len do
    let c = line.[!i] in
    if c = '"' then in_quotes := not !in_quotes
    else if c = '#' && not !in_quotes then begin
      idx := !i;
      i := len
    end;
    incr i
  done;
  String.trim (String.sub line 0 !idx)

let parse_toml (content : string) : (t, string) result =
  let lines = String.split_on_char '\n' content in
  let section = ref "package" in
  let name = ref "sigma_app" in
  let version = ref "0.1.0" in
  let description = ref "" in
  let authors = ref [] in
  let src_dir = ref "src" in
  let entry = ref "src/main.sgm" in
  let test_dir = ref "tests" in
  let scripts_rev = ref [] in
  let err = ref None in
  List.iteri
    (fun idx raw_line ->
      if Option.is_none !err then begin
        let line = strip_comment raw_line in
        if line <> "" then begin
          let len = String.length line in
          if line.[0] = '[' && line.[len - 1] = ']' then
            section := String.trim (String.sub line 1 (len - 2))
          else
            match String.index_opt line '=' with
            | None ->
              err := Some (Printf.sprintf "sigma.toml line %d: expected 'key = value'" (idx + 1))
            | Some eq_idx ->
              let key = String.trim (String.sub line 0 eq_idx) in
              let raw_val = String.trim (String.sub line (eq_idx + 1) (len - eq_idx - 1)) in
              (match !section, key with
               | "package", "name" -> name := unquote raw_val
               | "package", "version" -> version := unquote raw_val
               | "package", "description" -> description := unquote raw_val
               | "package", "authors" -> authors := parse_string_array raw_val
               | ("package" | "build"), "src_dir" -> src_dir := unquote raw_val
               | ("package" | "build"), "entry" -> entry := unquote raw_val
               | ("package" | "build"), "test_dir" -> test_dir := unquote raw_val
               | "scripts", k -> scripts_rev := (k, unquote raw_val) :: !scripts_rev
               | _ -> ())
        end
      end)
    lines;
  match !err with
  | Some msg -> Error msg
  | None ->
    Ok
      {
        name = !name;
        version = !version;
        description = !description;
        authors = !authors;
        src_dir = !src_dir;
        entry = !entry;
        test_dir = !test_dir;
        scripts = List.rev !scripts_rev;
      }

let load ?(path = "sigma.toml") () : (t, string) result =
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "manifest file '%s' not found in current directory" path)
  else
    try
      let ic = open_in path in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic;
      parse_toml content
    with exn -> Error (Printexc.to_string exn)

let find_script (script_name : string) (m : t) : string option =
  List.assoc_opt script_name m.scripts

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

let scaffold_project ?(root_dir = ".") (m : t) : (string list, string) result =
  try
    mkdir_p root_dir;
    let join p = if root_dir = "." then p else Filename.concat root_dir p in
    let toml_path = join "sigma.toml" in
    let oc = open_out toml_path in
    output_string oc (to_toml m);
    close_out oc;
    let created = ref [ toml_path ] in
    mkdir_p (join m.src_dir);
    mkdir_p (join m.test_dir);
    let entry_path = join m.entry in
    let default_main =
      Printf.sprintf
        "# Projeto %s (%s)\nconst PROJETO := %S;\nprintln(\"Bem-vindo ao\", PROJETO);\n"
        m.name m.version m.name
    in
    if write_file_if_missing entry_path default_main then
      created := entry_path :: !created;
    let test_path = join (Filename.concat m.test_dir "main_test.sgm") in
    let default_test =
      Printf.sprintf
        "# Suíte de testes inicial — %s\nassert(10 + 3, 13, \"Soma de inteiros\");\nassert(0.1m + 0.2m, 0.3m, \"Precisão exata decimal 128-bit\");\nassert_error(\"E0106\", \"const X : int;\", \"Constante sem inicializador deve falhar\");\n"
        m.name
    in
    if write_file_if_missing test_path default_test then
      created := test_path :: !created;
    Ok (List.rev !created)
  with exn -> Error (Printexc.to_string exn)

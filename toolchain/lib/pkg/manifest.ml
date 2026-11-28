type project_kind =
  | KindApp
  | KindLib

type native_config = {
  sources : string list;
  libs : string list;
  cflags : string list;
  cxxflags : string list;
  ldflags : string list;
}

type dep_spec =
  | DepSemver of string
  | DepPath of string
  | DepUrl of {
      url : string;
      version : string option;
    }
  | DepGit of {
      git : string;
      tag : string option;
      branch : string option;
      rev : string option;
    }

type t = {
  name : string;
  version : string;
  kind : project_kind;
  description : string;
  authors : string list;
  src_dir : string;
  entry : string;
  test_dir : string;
  scripts : (string * string) list;
  native : native_config;
  dependencies : (string * dep_spec) list;
  mirrors : (string * string * int) list;
}

let default_native = { sources = []; libs = []; cflags = []; cxxflags = []; ldflags = [] }

let default
    ?(version = "0.1.0")
    ?(kind = KindApp)
    ?(description = "")
    ?(authors = [])
    ?(src_dir = "src")
    ?entry
    ?(test_dir = "tests")
    ?scripts
    ?(native = default_native)
    ?(dependencies = [])
    ?(mirrors = [])
    ~name
    ()
  =
  let entry =
    match entry with
    | Some e -> e
    | None -> if kind = KindLib then "src/lib.sgm" else "src/main.sgm"
  in
  let scripts =
    match scripts with
    | Some s -> s
    | None -> if kind = KindLib then Pkg_scripts.default_scripts_lib else Pkg_scripts.default_scripts_app
  in
  { name; version; kind; description; authors; src_dir; entry; test_dir; scripts; native; dependencies; mirrors }

open Pkg_toml

let to_toml (m : t) : string =
  let authors_str =
    m.authors |> List.map toml_quote |> String.concat ", "
  in
  let scripts_lines =
    m.scripts
    |> List.map (fun (k, v) -> Printf.sprintf "%s = %s" k (toml_quote v))
    |> String.concat "\n"
  in
  let kind_str = match m.kind with KindApp -> "app" | KindLib -> "lib" in
  let native_sec =
    if m.native.sources = [] && m.native.libs = [] && m.native.cflags = [] && m.native.cxxflags = [] && m.native.ldflags = []
    then ""
    else
      let arr xs = "[" ^ (xs |> List.map toml_quote |> String.concat ", ") ^ "]" in
      Printf.sprintf "\n[native]\nsources = %s\nlibs = %s\n"
        (arr m.native.sources) (arr m.native.libs)
  in
  let deps_sec =
    if m.dependencies = [] then ""
    else
      let lines =
        List.map
          (fun (k, spec) ->
            match spec with
            | DepSemver s -> Printf.sprintf "%s = %s" k (toml_quote s)
            | DepPath p -> Printf.sprintf "%s = { path = %s }" k (toml_quote p)
            | DepUrl { url; version = None } -> Printf.sprintf "%s = { url = %s }" k (toml_quote url)
            | DepUrl { url; version = Some v } ->
              Printf.sprintf "%s = { url = %s, version = %s }" k (toml_quote url) (toml_quote v)
            | DepGit { git; tag; branch; rev } ->
              let parts =
                [ Printf.sprintf "git = %s" (toml_quote git) ]
                @ (match tag with Some t -> [ Printf.sprintf "tag = %s" (toml_quote t) ] | None -> [])
                @ (match branch with Some b -> [ Printf.sprintf "branch = %s" (toml_quote b) ] | None -> [])
                @ (match rev with Some r -> [ Printf.sprintf "rev = %s" (toml_quote r) ] | None -> [])
              in
              Printf.sprintf "%s = { %s }" k (String.concat ", " parts))
          m.dependencies
      in
      "\n[dependencies]\n" ^ String.concat "\n" lines ^ "\n"
  in
  let mirrors_sec =
    m.mirrors
    |> List.map (fun (n, u, p) ->
           Printf.sprintf "\n[[mirrors]]\nname = %s\nurl = %s\npriority = %d\n"
             (toml_quote n) (toml_quote u) p)
    |> String.concat ""
  in
  Printf.sprintf
    "[package]\nname = %s\nversion = %s\nkind = %s\ndescription = %s\nauthors = [%s]\n\n[build]\nsrc_dir = %s\nentry = %s\ntest_dir = %s\n\n[scripts]\n%s\n%s%s%s"
    (toml_quote m.name)
    (toml_quote m.version)
    (toml_quote kind_str)
    (toml_quote m.description)
    authors_str
    (toml_quote m.src_dir)
    (toml_quote m.entry)
    (toml_quote m.test_dir)
    scripts_lines
    native_sec
    deps_sec
    mirrors_sec

let parse_toml (content : string) : (t, string) result =
  let lines = String.split_on_char '\n' content in
  let section = ref "package" in
  let name = ref "sigma_app" in
  let version = ref "0.1.0" in
  let kind = ref KindApp in
  let description = ref "" in
  let authors = ref [] in
  let src_dir = ref "src" in
  let entry = ref "src/main.sgm" in
  let test_dir = ref "tests" in
  let scripts_rev = ref [] in
  let nat_sources = ref [] and nat_libs = ref [] in
  let nat_cflags = ref [] and nat_cxxflags = ref [] and nat_ldflags = ref [] in
  let deps_rev = ref [] in
  let mirrors_rev = ref [] in
  let m_name = ref "" and m_url = ref "" and m_prio = ref 10 and in_mirror = ref false in
  let flush_mirror () =
    if !in_mirror && !m_name <> "" && !m_url <> "" then
      mirrors_rev := (!m_name, !m_url, !m_prio) :: !mirrors_rev
  in
  let err = ref None in
  List.iteri
    (fun idx raw_line ->
      if Option.is_none !err then begin
        let line = strip_comment raw_line in
        if line <> "" then begin
          let len = String.length line in
          if line = "[[mirrors]]" then begin
            flush_mirror ();
            in_mirror := true;
            m_name := ""; m_url := ""; m_prio := 10;
            section := "mirrors"
          end else if line.[0] = '[' && line.[len - 1] = ']' then begin
            flush_mirror ();
            in_mirror := false;
            section := String.trim (String.sub line 1 (len - 2))
          end else
            match String.index_opt line '=' with
            | None ->
              err := Some (Printf.sprintf "sigma.toml line %d: expected 'key = value'" (idx + 1))
            | Some eq_idx ->
              let key = String.trim (String.sub line 0 eq_idx) in
              let raw_val = String.trim (String.sub line (eq_idx + 1) (len - eq_idx - 1)) in
              (match !section, key with
               | "package", "name" -> name := unquote raw_val
               | "package", "version" -> version := unquote raw_val
               | "package", "kind" ->
                 (match unquote raw_val with
                  | "app" -> kind := KindApp
                  | "lib" -> kind := KindLib
                  | other -> err := Some (Printf.sprintf "sigma.toml line %d: invalid package kind '%s' (expected 'app' or 'lib')" (idx + 1) other))
               | "package", "description" -> description := unquote raw_val
               | "package", "authors" -> authors := parse_string_array raw_val
               | ("package" | "build"), "src_dir" -> src_dir := unquote raw_val
               | ("package" | "build"), "entry" -> entry := unquote raw_val
               | ("package" | "build"), "test_dir" -> test_dir := unquote raw_val
               | "scripts", k -> scripts_rev := (k, unquote raw_val) :: !scripts_rev
               | "native", "sources" -> nat_sources := parse_string_array raw_val
               | "native", "libs" -> nat_libs := parse_string_array raw_val
               | "native", "cflags" -> nat_cflags := parse_string_array raw_val
               | "native", "cxxflags" -> nat_cxxflags := parse_string_array raw_val
               | "native", "ldflags" -> nat_ldflags := parse_string_array raw_val
               | "dependencies", k ->
                 let trimmed = String.trim raw_val in
                 if String.length trimmed >= 2 && trimmed.[0] = '{' && trimmed.[String.length trimmed - 1] = '}' then
                   let inner = String.sub trimmed 1 (String.length trimmed - 2) in
                   let kvs =
                     String.split_on_char ',' inner
                     |> List.filter_map (fun part ->
                            match String.index_opt part '=' with
                            | Some i ->
                              Some (String.trim (String.sub part 0 i),
                                    unquote (String.sub part (i + 1) (String.length part - i - 1)))
                            | None -> None)
                   in
                   (match List.assoc_opt "path" kvs, List.assoc_opt "url" kvs, List.assoc_opt "git" kvs with
                    | Some p, _, _ -> deps_rev := (k, DepPath p) :: !deps_rev
                    | None, Some u, _ ->
                      deps_rev := (k, DepUrl { url = u; version = List.assoc_opt "version" kvs }) :: !deps_rev
                    | None, None, Some g ->
                      deps_rev :=
                        (k, DepGit { git = g; tag = List.assoc_opt "tag" kvs;
                                     branch = List.assoc_opt "branch" kvs; rev = List.assoc_opt "rev" kvs })
                        :: !deps_rev
                    | None, None, None ->
                      err := Some (Printf.sprintf "sigma.toml line %d: invalid dependency table for '%s'" (idx + 1) k))
                 else
                   deps_rev := (k, DepSemver (unquote raw_val)) :: !deps_rev
               | "mirrors", "name" -> m_name := unquote raw_val
               | "mirrors", "url" -> m_url := unquote raw_val
               | "mirrors", "priority" ->
                 (match int_of_string_opt (unquote raw_val) with Some p -> m_prio := p | None -> ())
               | _ -> ())
        end
      end)
    lines;
  flush_mirror ();
  match !err with
  | Some msg -> Error msg
  | None ->
    Ok
      {
        name = !name;
        version = !version;
        kind = !kind;
        description = !description;
        authors = !authors;
        src_dir = !src_dir;
        entry = !entry;
        test_dir = !test_dir;
        scripts = List.rev !scripts_rev;
        native =
          {
            sources = !nat_sources;
            libs = !nat_libs;
            cflags = !nat_cflags;
            cxxflags = !nat_cxxflags;
            ldflags = !nat_ldflags;
          };
        dependencies = List.rev !deps_rev;
        mirrors = List.rev !mirrors_rev;
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

type script_target = Pkg_scripts.script_target =
  | ScriptCmd of string
  | ScriptEntry of {
      file : string;
      fn_name : string;
    }

let find_script (script_name : string) (m : t) : string option =
  List.assoc_opt script_name m.scripts

let resolve_script (script_name : string) (m : t) : script_target option =
  Option.map (Pkg_scripts.resolve_script_raw ~entry:m.entry) (find_script script_name m)

let scaffold_project ?(root_dir = ".") (m : t) : (string list, string) result =
  Pkg_scaffold.scaffold ~root_dir ~name:m.name ~version:m.version
    ~kind_is_lib:(m.kind = KindLib) ~src_dir:m.src_dir ~entry:m.entry
    ~test_dir:m.test_dir ~native_sources:m.native.sources ~toml_str:(to_toml m)

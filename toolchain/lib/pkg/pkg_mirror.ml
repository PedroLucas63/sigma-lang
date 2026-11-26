open Pkg_toml

type mirror_entry = {
  name : string;
  url : string;
  priority : int;
}

type index_version = {
  version : string;
  url : string;
  checksum : string;
}

let sigma_home () : string =
  match Sys.getenv_opt "SIGMA_HOME" with
  | Some h when h <> "" -> h
  | _ ->
    let home = match Sys.getenv_opt "HOME" with Some h when h <> "" -> h | _ -> "/tmp" in
    Filename.concat home ".sigma"

let config_path () : string = Filename.concat (sigma_home ()) "config.toml"

let rec mkdir_p (dir : string) : unit =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let parse_mirrors (content : string) : (mirror_entry list, string) result =
  let lines = String.split_on_char '\n' content in
  let entries = ref [] in
  let cur_name = ref "" and cur_url = ref "" and cur_prio = ref 10 in
  let in_blk = ref false in
  let flush () =
    if !in_blk && !cur_name <> "" && !cur_url <> "" then
      entries := { name = !cur_name; url = !cur_url; priority = !cur_prio } :: !entries
  in
  let err = ref None in
  List.iteri
    (fun idx raw ->
      if Option.is_none !err then begin
        let line = strip_comment raw in
        if line <> "" then begin
          if line = "[[mirrors]]" then begin
            flush ();
            in_blk := true;
            cur_name := ""; cur_url := ""; cur_prio := 10
          end else
            match String.index_opt line '=' with
            | None -> err := Some (Printf.sprintf "E0140: config.toml line %d: expected 'key = value'" (idx + 1))
            | Some eq_idx ->
              let k = String.trim (String.sub line 0 eq_idx) in
              let v = String.trim (String.sub line (eq_idx + 1) (String.length line - eq_idx - 1)) in
              (match k with
               | "name" -> cur_name := unquote v
               | "url" -> cur_url := unquote v
               | "priority" ->
                 (match int_of_string_opt (unquote v) with Some p -> cur_prio := p | None -> ())
               | _ -> ())
        end
      end)
    lines;
  flush ();
  match !err with
  | Some e -> Error e
  | None ->
    Ok (List.sort (fun a b -> compare a.priority b.priority) (List.rev !entries))

let load_global_mirrors () : (mirror_entry list, string) result =
  let p = config_path () in
  if not (Sys.file_exists p) then Ok []
  else
    try
      let ic = open_in p in
      let len = in_channel_length ic in
      let s = really_input_string ic len in
      close_in ic;
      parse_mirrors s
    with exn -> Error (Printexc.to_string exn)

let save_global_mirrors (ms : mirror_entry list) : (unit, string) result =
  try
    let p = config_path () in
    mkdir_p (Filename.dirname p);
    let body =
      ms
      |> List.map (fun m ->
             Printf.sprintf "[[mirrors]]\nname = %s\nurl = %s\npriority = %d\n"
               (toml_quote m.name) (toml_quote m.url) m.priority)
      |> String.concat "\n"
    in
    let oc = open_out p in
    output_string oc body;
    close_out oc;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let add_global_mirror ~name ~url ~priority =
  match load_global_mirrors () with
  | Error e -> Error e
  | Ok ms ->
    let ms' =
      ({ name; url; priority } :: List.filter (fun m -> m.name <> name) ms)
      |> List.sort (fun a b -> compare a.priority b.priority)
    in
    save_global_mirrors ms'

let remove_global_mirror name =
  match load_global_mirrors () with
  | Error e -> Error e
  | Ok ms -> save_global_mirrors (List.filter (fun m -> m.name <> name) ms)

let parse_package_index (content : string) : (index_version list, string) result =
  let lines = String.split_on_char '\n' content in
  let entries = ref [] in
  let cur_ver = ref "" and cur_url = ref "" and cur_chk = ref "" in
  let in_blk = ref false in
  let flush () =
    if !in_blk && !cur_ver <> "" && !cur_url <> "" then
      entries := { version = !cur_ver; url = !cur_url; checksum = !cur_chk } :: !entries
  in
  List.iter
    (fun raw ->
      let line = strip_comment raw in
      if line <> "" then begin
        if line = "[[version]]" then begin
          flush ();
          in_blk := true;
          cur_ver := ""; cur_url := ""; cur_chk := ""
        end else
          match String.index_opt line '=' with
          | None -> ()
          | Some eq_idx ->
            let k = String.trim (String.sub line 0 eq_idx) in
            let v = unquote (String.sub line (eq_idx + 1) (String.length line - eq_idx - 1)) in
            (match k with
             | "version" -> cur_ver := v
             | "url" -> cur_url := v
             | "checksum" -> cur_chk := v
             | _ -> ())
      end)
    lines;
  flush ();
  Ok (List.rev !entries)

let fetch_index (mirror_url : string) (pkg_name : string) : (index_version list, string) result =
  let base =
    if String.ends_with ~suffix:"/" mirror_url then mirror_url else mirror_url ^ "/"
  in
  let idx_url = Printf.sprintf "%spackages/%s/index.toml" base pkg_name in
  if String.starts_with ~prefix:"file://" idx_url then
    let local_p = String.sub idx_url 7 (String.length idx_url - 7) in
    if not (Sys.file_exists local_p) then Error "not found"
    else
      let ic = open_in local_p in
      let len = in_channel_length ic in
      let s = really_input_string ic len in
      close_in ic;
      parse_package_index s
  else
    let tmp = Filename.temp_file "sigma_idx_" ".toml" in
    let cmd = Printf.sprintf "curl -fsSL --max-time 15 %s -o %s 2>/dev/null" (Filename.quote idx_url) (Filename.quote tmp) in
    if Sys.command cmd <> 0 then begin
      (if Sys.file_exists tmp then try Sys.remove tmp with _ -> ());
      Error "fetch failed"
    end else begin
      let ic = open_in tmp in
      let len = in_channel_length ic in
      let s = really_input_string ic len in
      close_in ic;
      (try Sys.remove tmp with _ -> ());
      parse_package_index s
    end

let resolve_from_mirrors ~(mirrors : mirror_entry list) ~(name : string) ~(range : Pkg_semver.range)
    : (index_version, string) result =
  let sorted = List.sort (fun a b -> compare a.priority b.priority) mirrors in
  let rec try_mirrors = function
    | [] ->
      Error (Printf.sprintf "E0140: package '%s' not found in any configured mirror" name)
    | (m : mirror_entry) :: rest ->
      (match fetch_index m.url name with
       | Error _ -> try_mirrors rest
       | Ok ivs ->
         let vs =
           List.filter_map
             (fun iv -> match Pkg_semver.parse_version iv.version with Ok v -> Some (v, iv) | Error _ -> None)
             ivs
         in
         (match Pkg_semver.best_matching (List.map fst vs) range with
          | None -> try_mirrors rest
          | Some bv ->
            let _, chosen = List.find (fun (v, _) -> Pkg_semver.compare v bv = 0) vs in
            Ok chosen))
  in
  try_mirrors sorted

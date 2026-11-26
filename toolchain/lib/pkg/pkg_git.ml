let is_git_url (s : string) : bool =
  String.starts_with ~prefix:"git+" s
  || String.starts_with ~prefix:"ssh://" s
  || ((String.starts_with ~prefix:"https://" s || String.starts_with ~prefix:"http://" s || String.starts_with ~prefix:"file://" s)
      && (String.ends_with ~suffix:".git" s || String.contains s '#'))

let parse_git_spec (raw : string) : string * string option =
  let clean =
    if String.starts_with ~prefix:"git+" raw then
      String.sub raw 4 (String.length raw - 4)
    else raw
  in
  match String.index_opt clean '#' with
  | None -> (clean, None)
  | Some idx ->
    let url = String.sub clean 0 idx in
    let ref_s = String.sub clean (idx + 1) (String.length clean - idx - 1) in
    (url, if ref_s = "" then None else Some ref_s)

let git_cache_dir () : string =
  let base =
    match Sys.getenv_opt "SIGMA_HOME" with
    | Some h when h <> "" -> h
    | _ ->
      let home = match Sys.getenv_opt "HOME" with Some h when h <> "" -> h | _ -> "/tmp" in
      Filename.concat home ".sigma"
  in
  Filename.concat base "git_cache"

let rec mkdir_p (dir : string) : unit =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let clone_to_cache ~url ~tag ~branch ~rev : (string, string) result =
  let ref_key =
    match tag, branch, rev with
    | Some t, _, _ -> "tag:" ^ t
    | None, Some b, _ -> "branch:" ^ b
    | None, None, Some r -> "rev:" ^ r
    | None, None, None -> "HEAD"
  in
  let hash = Digest.to_hex (Digest.string (url ^ "|" ^ ref_key)) in
  let dest = Filename.concat (git_cache_dir ()) hash in
  let toml = Filename.concat dest "sigma.toml" in
  if Sys.file_exists toml then Ok dest
  else begin
    mkdir_p (git_cache_dir ());
    (if Sys.file_exists dest then ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dest))));
    let ref_flag =
      match tag, branch with
      | Some t, _ -> Printf.sprintf "--branch %s --depth 1" (Filename.quote t)
      | None, Some b -> Printf.sprintf "--branch %s --depth 1" (Filename.quote b)
      | None, None -> if Option.is_some rev then "" else "--depth 1"
    in
    let cmd = Printf.sprintf "git clone %s %s %s 2>/dev/null" ref_flag (Filename.quote url) (Filename.quote dest) in
    if Sys.command cmd <> 0 then
      Error (Printf.sprintf "E0139: failed to clone git repository '%s'" url)
    else
      let checkout_ok =
        match rev with
        | None -> Ok ()
        | Some r ->
          let co = Printf.sprintf "git -C %s checkout %s 2>/dev/null" (Filename.quote dest) (Filename.quote r) in
          if Sys.command co = 0 then Ok ()
          else Error (Printf.sprintf "E0139: failed to checkout revision '%s' in '%s'" r url)
      in
      match checkout_ok with
      | Error e ->
        ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dest)));
        Error e
      | Ok () ->
        if not (Sys.file_exists toml) then
          Error (Printf.sprintf "E0139: repository '%s' does not contain sigma.toml" url)
        else Ok dest
  end

let rec mkdir_p (dir : string) : unit =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let store_dir () : string =
  match Sys.getenv_opt "SIGMA_HOME" with
  | Some h when h <> "" -> Filename.concat h "store"
  | _ ->
    let home = match Sys.getenv_opt "HOME" with Some h when h <> "" -> h | _ -> "/tmp" in
    Filename.concat (Filename.concat home ".sigma") "store"

let copy_dir_contents src dst =
  (if Sys.file_exists dst then ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dst))));
  mkdir_p dst;
  let cmd =
    Printf.sprintf "tar --exclude=.git -C %s -cf - . | tar -C %s -xf - 2>&1"
      (Filename.quote src) (Filename.quote dst)
  in
  if Sys.command cmd = 0 then Ok ()
  else Error (Printf.sprintf "failed to copy '%s' to '%s'" src dst)

let link_to_modules ~root_dir ~name ~store_pkg_dir =
  let mods_dir = Filename.concat root_dir ".sigma_modules" in
  mkdir_p mods_dir;
  let link_path = Filename.concat mods_dir name in
  (if Sys.file_exists link_path then ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote link_path))));
  try
    Unix.symlink store_pkg_dir link_path;
    Ok ()
  with _ -> copy_dir_contents store_pkg_dir link_path

let publish_dir_to_store ?(root_dir = ".") (src_dir : string) : (string * string, string) result =
  let _ = root_dir in
  let toml = Filename.concat src_dir "sigma.toml" in
  match Manifest.load ~path:toml () with
  | Error e -> Error e
  | Ok m ->
    (match Pkg_semver.parse_version m.version with
     | Error e -> Error e
     | Ok _ ->
       let dest = Filename.concat (Filename.concat (store_dir ()) m.name) m.version in
       (match copy_dir_contents src_dir dest with
        | Error e -> Error e
        | Ok () -> Ok (m.name, m.version)))

let available_store_versions (name : string) : (Pkg_semver.version * string) list =
  let pkg_dir = Filename.concat (store_dir ()) name in
  if not (Sys.file_exists pkg_dir && Sys.is_directory pkg_dir) then []
  else
    Sys.readdir pkg_dir
    |> Array.to_list
    |> List.filter_map (fun vstr ->
           match Pkg_semver.parse_version vstr with
           | Ok v -> Some (v, Filename.concat pkg_dir vstr)
           | Error _ -> None)

let fetch_url_to_file (url : string) (dst : string) : bool =
  if String.starts_with ~prefix:"file://" url then
    let local_p = String.sub url 7 (String.length url - 7) in
    if not (Sys.file_exists local_p) then false
    else Sys.command (Printf.sprintf "cp %s %s 2>/dev/null" (Filename.quote local_p) (Filename.quote dst)) = 0
  else
    Sys.command (Printf.sprintf "curl -fsSL --max-time 20 %s -o %s 2>/dev/null" (Filename.quote url) (Filename.quote dst)) = 0

let install_sgmpkg_url ~root_dir ~name ~ver_opt ~url =
  let tmp = Filename.temp_file "sigma_dep_" ".sgmpkg" in
  if not (fetch_url_to_file url tmp) then begin
    (if Sys.file_exists tmp then try Sys.remove tmp with _ -> ());
    Error (Printf.sprintf "E0138: failed to download package '%s' from '%s'" name url)
  end else
    let res =
      match Pkg_pack.unpack_to_cache tmp with
      | Error e -> Error e
      | Ok (cdir, pm) ->
        let ver = Option.value ver_opt ~default:pm.version in
        let dest = Filename.concat (Filename.concat (store_dir ()) name) ver in
        (match copy_dir_contents cdir dest with
         | Error e -> Error e
         | Ok () ->
           (match link_to_modules ~root_dir ~name ~store_pkg_dir:dest with
            | Error e -> Error e
            | Ok () ->
              Ok (ver, pm.entry)))
    in
    (if Sys.file_exists tmp then try Sys.remove tmp with _ -> ());
    res

let resolve_single_dep ~root_dir ~mirrors ~(locked : Pkg_lock.lock_entry option) (name : string) (spec : Manifest.dep_spec)
    : (Pkg_lock.lock_entry, string) result =
  match spec with
  | Manifest.DepPath p ->
    let abs_p = if Filename.is_relative p then Filename.concat root_dir p else p in
    if not (Sys.file_exists abs_p) then
      Error (Printf.sprintf "E0138: local dependency path '%s' not found" p)
    else if Filename.check_suffix abs_p ".sgmpkg" then
      (match Pkg_pack.unpack_to_cache abs_p with
       | Error e -> Error e
       | Ok (cdir, pm) ->
         let dest = Filename.concat (Filename.concat (store_dir ()) name) pm.version in
         (match copy_dir_contents cdir dest with
          | Error e -> Error e
          | Ok () ->
            (match link_to_modules ~root_dir ~name ~store_pkg_dir:dest with
             | Error e -> Error e
             | Ok () ->
               Ok { Pkg_lock.name; version = pm.version; source = "path:" ^ p; checksum = "local"; entry = pm.entry })))
    else
      (match Manifest.load ~path:(Filename.concat abs_p "sigma.toml") () with
       | Error e -> Error ("E0138: " ^ e)
       | Ok pm ->
         let dest = Filename.concat (Filename.concat (store_dir ()) name) pm.version in
         (match copy_dir_contents abs_p dest with
          | Error e -> Error e
          | Ok () ->
            (match link_to_modules ~root_dir ~name ~store_pkg_dir:dest with
             | Error e -> Error e
             | Ok () ->
               Ok { Pkg_lock.name; version = pm.version; source = "path:" ^ p; checksum = "local"; entry = pm.entry })))
  | Manifest.DepUrl { url; version } ->
    (match install_sgmpkg_url ~root_dir ~name ~ver_opt:version ~url with
     | Error e -> Error e
     | Ok (ver, entry) ->
       Ok { Pkg_lock.name; version = ver; source = "url:" ^ url; checksum = Digest.to_hex (Digest.string url); entry })
  | Manifest.DepGit { git; tag; branch; rev } ->
    (match Pkg_git.clone_to_cache ~url:git ~tag ~branch ~rev with
     | Error e -> Error e
     | Ok repo_dir ->
       (match Manifest.load ~path:(Filename.concat repo_dir "sigma.toml") () with
        | Error e -> Error ("E0139: " ^ e)
        | Ok pm ->
          let dest = Filename.concat (Filename.concat (store_dir ()) name) pm.version in
          (match copy_dir_contents repo_dir dest with
           | Error e -> Error e
           | Ok () ->
             (match link_to_modules ~root_dir ~name ~store_pkg_dir:dest with
              | Error e -> Error e
              | Ok () ->
                Ok { Pkg_lock.name; version = pm.version; source = "git:" ^ git; checksum = "git"; entry = pm.entry }))))
  | Manifest.DepSemver range_str ->
    (match Pkg_semver.parse_range range_str with
     | Error e -> Error e
     | Ok range ->
       let avail = available_store_versions name in
       let chosen_opt =
         match locked with
         | Some le ->
           (match Pkg_semver.parse_version le.version with
            | Ok lv when Pkg_semver.satisfies lv range ->
              List.find_opt (fun (v, _) -> Pkg_semver.compare v lv = 0) avail
            | _ -> None)
         | None -> None
       in
       let chosen =
         match chosen_opt with
         | Some x -> Some x
         | None ->
           let vs = List.map fst avail in
           (match Pkg_semver.best_matching vs range with
            | None -> None
            | Some bv -> List.find_opt (fun (v, _) -> Pkg_semver.compare v bv = 0) avail)
       in
       match chosen with
       | Some (v, dir) ->
         let pm_entry =
           match Manifest.load ~path:(Filename.concat dir "sigma.toml") () with
           | Ok pm -> pm.entry
           | Error _ -> "src/lib.sgm"
         in
         (match link_to_modules ~root_dir ~name ~store_pkg_dir:dir with
          | Error e -> Error e
          | Ok () ->
            let vstr = Pkg_semver.to_string v in
            Ok { Pkg_lock.name; version = vstr; source = "store"; checksum = "store:" ^ vstr; entry = pm_entry })
       | None ->
         if mirrors = [] then
           Error (Printf.sprintf "E0138: no version of '%s' matching '%s' found in store" name range_str)
         else
           (match Pkg_mirror.resolve_from_mirrors ~mirrors ~name ~range with
            | Error e -> Error e
            | Ok iv ->
              (match install_sgmpkg_url ~root_dir ~name ~ver_opt:(Some iv.version) ~url:iv.url with
               | Error e -> Error e
               | Ok (ver, entry) ->
                 Ok { Pkg_lock.name; version = ver; source = "mirror:" ^ iv.url; checksum = iv.checksum; entry })))

let install_all ?(root_dir = ".") ~frozen (m : Manifest.t) : (Pkg_lock.t, string) result =
  let lock_path = Filename.concat root_dir "sigma.lock" in
  let global_mirrors = match Pkg_mirror.load_global_mirrors () with Ok ms -> ms | Error _ -> [] in
  let proj_mirrors =
    List.map (fun (name, url, priority) -> { Pkg_mirror.name; url; priority }) m.mirrors
  in
  let mirrors = proj_mirrors @ global_mirrors in
  match Pkg_lock.load ~path:lock_path () with
  | Error e -> Error e
  | Ok existing_lock ->
    if frozen && m.dependencies <> [] && existing_lock = [] then
      Error "E0137: sigma.lock is missing in --frozen mode"
    else
      let rec loop acc = function
        | [] ->
          let final_lock = List.rev acc in
          if frozen then
            let old_names = List.map (fun (e : Pkg_lock.lock_entry) -> (e.name, e.version)) existing_lock in
            let new_names = List.map (fun (e : Pkg_lock.lock_entry) -> (e.name, e.version)) final_lock in
            if old_names <> new_names then Error "E0137: sigma.lock is out of sync with sigma.toml (--frozen)"
            else Ok final_lock
          else
            (match Pkg_lock.save ~path:lock_path final_lock with
             | Error e -> Error e
             | Ok () -> Ok final_lock)
        | (name, spec) :: tl ->
          let locked = List.find_opt (fun (e : Pkg_lock.lock_entry) -> e.name = name) existing_lock in
          (match resolve_single_dep ~root_dir ~mirrors ~locked name spec with
           | Error e -> Error e
           | Ok entry -> loop (entry :: acc) tl)
      in
      loop [] m.dependencies

let add_dependency ?(root_dir = ".") ~target ~version_opt () : (string * string, string) result =
  let toml_path = Filename.concat root_dir "sigma.toml" in
  match Manifest.load ~path:toml_path () with
  | Error e -> Error e
  | Ok m ->
    let name, spec =
      if Pkg_git.is_git_url target then
        let url, tag_opt = Pkg_git.parse_git_spec target in
        let b = Filename.basename url in
        let n = if Filename.check_suffix b ".git" then Filename.chop_suffix b ".git" else b in
        (n, Manifest.DepGit { git = url; tag = tag_opt; branch = None; rev = None })
      else if String.starts_with ~prefix:"http://" target || String.starts_with ~prefix:"https://" target then
        let b = Filename.basename target in
        let n = match String.index_opt b '-' with Some i -> String.sub b 0 i | None -> Filename.remove_extension b in
        (n, Manifest.DepUrl { url = target; version = version_opt })
      else if Sys.file_exists target || String.contains target '/' || Filename.check_suffix target ".sgmpkg" then
        let n =
          if Sys.is_directory target then
            match Manifest.load ~path:(Filename.concat target "sigma.toml") () with
            | Ok pm -> pm.name | Error _ -> Filename.basename target
          else Filename.remove_extension (Filename.basename target)
        in
        (n, Manifest.DepPath target)
      else
        (target, Manifest.DepSemver (Option.value version_opt ~default:"^0.1.0"))
    in
    let deps' = (name, spec) :: List.remove_assoc name m.dependencies in
    let m' = { m with Manifest.dependencies = List.rev deps' } in
    let oc = open_out toml_path in
    output_string oc (Manifest.to_toml m');
    close_out oc;
    (match install_all ~root_dir ~frozen:false m' with
     | Error e -> Error e
     | Ok lock ->
       let ver =
         match List.find_opt (fun (e : Pkg_lock.lock_entry) -> e.name = name) lock with
         | Some e -> e.version | None -> "0.1.0"
       in
       Ok (name, ver))

let remove_dependency ?(root_dir = ".") (name : string) : (unit, string) result =
  let toml_path = Filename.concat root_dir "sigma.toml" in
  match Manifest.load ~path:toml_path () with
  | Error e -> Error e
  | Ok m ->
    let m' = { m with Manifest.dependencies = List.remove_assoc name m.dependencies } in
    let oc = open_out toml_path in
    output_string oc (Manifest.to_toml m');
    close_out oc;
    let mod_path = Filename.concat (Filename.concat root_dir ".sigma_modules") name in
    (if Sys.file_exists mod_path then ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote mod_path))));
    (match install_all ~root_dir ~frozen:false m' with
     | Error e -> Error e
     | Ok _ -> Ok ())

let update_dependencies ?(root_dir = ".") ?pkg_name () : (Pkg_lock.t, string) result =
  let toml_path = Filename.concat root_dir "sigma.toml" in
  match Manifest.load ~path:toml_path () with
  | Error e -> Error e
  | Ok m ->
    let lock_path = Filename.concat root_dir "sigma.lock" in
    let _ =
      match pkg_name with
      | None -> if Sys.file_exists lock_path then (try Sys.remove lock_path with _ -> ())
      | Some target ->
        (match Pkg_lock.load ~path:lock_path () with
         | Ok ls -> ignore (Pkg_lock.save ~path:lock_path (List.filter (fun (e : Pkg_lock.lock_entry) -> e.name <> target) ls))
         | Error _ -> ())
    in
    install_all ~root_dir ~frozen:false m

let list_dependencies ?(root_dir = ".") () : (Pkg_lock.t, string) result =
  Pkg_lock.load ~path:(Filename.concat root_dir "sigma.lock") ()

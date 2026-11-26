open Sigma_common
open Sigma_diagnostics

let is_url (s : string) : bool =
  let rec find i =
    if i + 2 >= String.length s then false
    else if s.[i] = ':' && s.[i + 1] = '/' && s.[i + 2] = '/' then true
    else find (i + 1)
  in
  find 0

let is_supported_http_url (s : string) : bool =
  String.starts_with ~prefix:"http://" s || String.starts_with ~prefix:"https://" s

let normalize_github_url (url : string) : string =
  let prefix = "https://github.com/" in
  if not (String.starts_with ~prefix url) then url
  else
    let rest = String.sub url (String.length prefix) (String.length url - String.length prefix) in
    match String.split_on_char '/' rest with
    | user :: repo :: "blob" :: branch :: path_parts when path_parts <> [] ->
      Printf.sprintf "https://raw.githubusercontent.com/%s/%s/%s/%s"
        user repo branch (String.concat "/" path_parts)
    | _ -> url

let resolve_relative_url (base_url : string) (rel : string) : string =
  if is_url rel then rel
  else
    let clean_rel =
      if String.starts_with ~prefix:"./" rel then String.sub rel 2 (String.length rel - 2)
      else rel
    in
    match String.rindex_opt base_url '/' with
    | Some idx -> String.sub base_url 0 (idx + 1) ^ clean_rel
    | None -> clean_rel

let rec mkdir_p (dir : string) : unit =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let cache_dir () : string =
  match Sys.getenv_opt "SIGMA_CACHE_DIR" with
  | Some d when d <> "" -> d
  | _ ->
    let base =
      match Sys.getenv_opt "XDG_CACHE_HOME" with
      | Some d when d <> "" -> d
      | _ ->
        (match Sys.getenv_opt "HOME" with
         | Some h when h <> "" -> Filename.concat h ".cache"
         | _ -> Filename.get_temp_dir_name ())
    in
    Filename.concat (Filename.concat base "sigma") "url_modules"

let cache_path_for (norm_url : string) : string =
  let hash = Digest.to_hex (Digest.string norm_url) in
  Filename.concat (cache_dir ()) (hash ^ ".sgm")

let fetch_url ~span (raw_url : string) : (string * Source.t, Diagnostic.t) result =
  if not (is_supported_http_url raw_url) then
    Error
      (Diagnostic.error
         ~hint:"only 'http://' and 'https://' URLs are supported for remote imports"
         ~code:"E0134" ~span
         (Printf.sprintf "unsupported URL scheme in import '%s'" raw_url))
  else
    let norm_url = normalize_github_url raw_url in
    let cdir = cache_dir () in
    mkdir_p cdir;
    let cpath = cache_path_for norm_url in
    let refresh = Sys.getenv_opt "SIGMA_REFRESH" = Some "1" in
    let offline = Sys.getenv_opt "SIGMA_OFFLINE" = Some "1" in
    let load_cached () =
      match Source.load cpath with
      | Ok src -> Ok (norm_url, { src with Source.filename = norm_url })
      | Error msg ->
        Error
          (Diagnostic.error ~code:"E0129" ~span
             (Printf.sprintf "failed to read cached remote module '%s': %s" norm_url msg))
    in
    if Sys.file_exists cpath && not refresh then load_cached ()
    else if offline then
      Error
        (Diagnostic.error
           ~hint:"disable SIGMA_OFFLINE or populate the cache before running offline"
           ~code:"E0129" ~span
           (Printf.sprintf "remote module '%s' is not in local cache (offline mode)" norm_url))
    else
      let tmp = Filename.temp_file "sigma_url_" ".sgm" in
      let cmd =
        Printf.sprintf "curl -fsSL --max-time 15 %s -o %s 2>/dev/null"
          (Filename.quote norm_url) (Filename.quote tmp)
      in
      let code = Sys.command cmd in
      if code <> 0 then begin
        (if Sys.file_exists tmp then try Sys.remove tmp with _ -> ());
        Error
          (Diagnostic.error
             ~hint:"check the URL, network connectivity, or HTTP status"
             ~code:"E0129" ~span
             (Printf.sprintf "failed to fetch remote module '%s' (curl exit %d)" norm_url code))
      end else begin
        (try
           let ic = open_in_bin tmp in
           let len = in_channel_length ic in
           let data = really_input_string ic len in
           close_in ic;
           let oc = open_out_bin cpath in
           output_string oc data;
           close_out oc;
           Sys.remove tmp
         with _ -> ());
        load_cached ()
      end

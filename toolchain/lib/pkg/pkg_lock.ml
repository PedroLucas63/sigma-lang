type lock_entry = {
  name : string;
  version : string;
  source : string;
  checksum : string;
  entry : string;
}

type t = lock_entry list

let to_string (entries : t) : string =
  entries
  |> List.map (fun e ->
         Printf.sprintf
           "[[package]]\nname = %S\nversion = %S\nsource = %S\nchecksum = %S\nentry = %S\n"
           e.name e.version e.source e.checksum e.entry)
  |> String.concat "\n"

let unquote s =
  let s = String.trim s in
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s

let parse (content : string) : (t, string) result =
  let lines = String.split_on_char '\n' content in
  let entries = ref [] in
  let cur_name = ref "" and cur_ver = ref "" and cur_src = ref "" and cur_chk = ref "" and cur_ent = ref "src/lib.sgm" in
  let in_pkg = ref false in
  let flush () =
    if !in_pkg && !cur_name <> "" then
      entries :=
        { name = !cur_name; version = !cur_ver; source = !cur_src; checksum = !cur_chk; entry = !cur_ent }
        :: !entries
  in
  let err = ref None in
  List.iteri
    (fun idx raw ->
      if Option.is_none !err then begin
        let line = String.trim raw in
        if line <> "" && line.[0] <> '#' then begin
          if line = "[[package]]" then begin
            flush ();
            in_pkg := true;
            cur_name := ""; cur_ver := ""; cur_src := ""; cur_chk := ""; cur_ent := "src/lib.sgm"
          end else
            match String.index_opt line '=' with
            | None -> err := Some (Printf.sprintf "E0137: sigma.lock line %d: invalid syntax" (idx + 1))
            | Some eq_idx ->
              let k = String.trim (String.sub line 0 eq_idx) in
              let v = unquote (String.sub line (eq_idx + 1) (String.length line - eq_idx - 1)) in
              (match k with
               | "name" -> cur_name := v
               | "version" -> cur_ver := v
               | "source" -> cur_src := v
               | "checksum" -> cur_chk := v
               | "entry" -> cur_ent := v
               | _ -> ())
        end
      end)
    lines;
  flush ();
  match !err with Some e -> Error e | None -> Ok (List.rev !entries)

let load ?(path = "sigma.lock") () : (t, string) result =
  if not (Sys.file_exists path) then Ok []
  else
    try
      let ic = open_in path in
      let len = in_channel_length ic in
      let s = really_input_string ic len in
      close_in ic;
      parse s
    with exn -> Error (Printexc.to_string exn)

let save ?(path = "sigma.lock") (entries : t) : (unit, string) result =
  try
    let oc = open_out path in
    output_string oc (to_string entries);
    close_out oc;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

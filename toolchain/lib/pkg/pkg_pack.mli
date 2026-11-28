val shared_lib_ext : unit -> string

val pack_archive :
  manifest_path:string ->
  artifacts:string list ->
  output_path:string ->
  (string, string) result

val unpack_to_cache : string -> (string * Manifest.t, string) result

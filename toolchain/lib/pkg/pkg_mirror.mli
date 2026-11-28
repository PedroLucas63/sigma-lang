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

val config_path : unit -> string
val load_global_mirrors : unit -> (mirror_entry list, string) result
val save_global_mirrors : mirror_entry list -> (unit, string) result
val add_global_mirror : name:string -> url:string -> priority:int -> (unit, string) result
val remove_global_mirror : string -> (unit, string) result
val parse_package_index : string -> (index_version list, string) result
val resolve_from_mirrors :
  mirrors:mirror_entry list ->
  name:string ->
  range:Pkg_semver.range ->
  (index_version, string) result

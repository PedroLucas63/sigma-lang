val store_dir : unit -> string

val publish_dir_to_store : ?root_dir:string -> string -> (string * string, string) result

val install_all : ?root_dir:string -> frozen:bool -> Manifest.t -> (Pkg_lock.t, string) result

val add_dependency :
  ?root_dir:string ->
  target:string ->
  version_opt:string option ->
  unit ->
  (string * string, string) result

val remove_dependency : ?root_dir:string -> string -> (unit, string) result

val update_dependencies : ?root_dir:string -> ?pkg_name:string -> unit -> (Pkg_lock.t, string) result

val list_dependencies : ?root_dir:string -> unit -> (Pkg_lock.t, string) result

val is_git_url : string -> bool
val parse_git_spec : string -> string * string option
val clone_to_cache :
  url:string ->
  tag:string option ->
  branch:string option ->
  rev:string option ->
  (string, string) result

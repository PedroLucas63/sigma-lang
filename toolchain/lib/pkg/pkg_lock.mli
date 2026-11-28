type lock_entry = {
  name : string;
  version : string;
  source : string;
  checksum : string;
  entry : string;
}

type t = lock_entry list

val to_string : t -> string
val parse : string -> (t, string) result
val load : ?path:string -> unit -> (t, string) result
val save : ?path:string -> t -> (unit, string) result

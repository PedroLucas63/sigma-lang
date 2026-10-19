(** Gerenciador de pacotes e manifesto [sigma.toml]. *)

type t = {
  name : string;
  version : string;
  entry : string;
}

val default : name:string -> t
val to_toml : t -> string

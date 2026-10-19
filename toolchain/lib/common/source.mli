(** Representação em memória de um arquivo-fonte [.sgm]. *)

type t = {
  filename : string;
  content : string;
  lines : string array;
}

val of_string : filename:string -> content:string -> t
val load : string -> (t, string) result
val get_line : t -> int -> string option

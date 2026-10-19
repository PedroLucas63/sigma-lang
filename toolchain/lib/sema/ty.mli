(** Sistema de tipos estáticos da linguagem Sigma. *)

type t =
  | Int
  | Bool

val equal : t -> t -> bool
val to_string : t -> string
val of_string : string -> t option
val default_int_value : int64
val default_bool_value : bool

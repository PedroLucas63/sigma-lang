(** Sistema de tipos estáticos da linguagem Sigma. *)

open Sigma_common

type t =
  | Int
  | Float
  | Double
  | Decimal
  | Bool
  | Char
  | String

val equal : t -> t -> bool
val to_string : t -> string
val of_string : string -> t option
val default_int_value : int64
val default_float_value : float
val default_decimal_value : Decimal.t
val default_bool_value : bool
val default_char_value : string
val default_string_value : string

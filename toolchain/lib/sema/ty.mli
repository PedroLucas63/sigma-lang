(** Sistema de tipos estáticos da linguagem Sigma. *)

open Sigma_common

type param_mode =
  | ParamVal
  | ParamRef
  | ParamOut

type fun_param = {
  pty : t;
  pmode : param_mode;
}

and t =
  | Int
  | Float
  | Double
  | Decimal
  | Bool
  | Char
  | String
  | Tuple of t list
  | Array of t * int
  | Fun of fun_param list * t  (** Função/procedimento: parâmetros restantes e retorno ([Void] para [act]). *)
  | Void  (** Retorno de [act]; só aparece como retorno de [Fun] ou como tipo de chamada de [act]. *)

val equal : t -> t -> bool
val to_string : t -> string
val of_string : string -> t option
val default_int_value : int64
val default_float_value : float
val default_decimal_value : Decimal.t
val default_bool_value : bool
val default_char_value : string
val default_string_value : string

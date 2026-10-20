(** Representação exata de ponto flutuante em Base 10 de 128 bits ([decimal]).
    Projetado para computação financeira e estatística de alta precisão sem erros
    de arredondamento binário (suporta até 38 dígitos decimais significativos exatos). *)

type t = {
  neg : bool;
  scale : int;     (** Número de casas decimais após a vírgula (>= 0): valor = (-1)^neg * coeff * 10^(-scale) *)
  hi : int64;      (** 64 bits superiores (unsigned) da mantissa inteira de 128 bits *)
  lo : int64;      (** 64 bits inferiores (unsigned) da mantissa inteira de 128 bits *)
}

val zero : t
val is_zero : t -> bool
val negate : t -> t
val equal : t -> t -> bool
val compare : t -> t -> int

val of_int64 : int64 -> t
val to_float : t -> float

(** Operações aritméticas em Base 10 de 128 bits. *)
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> (t, string) result
val div_trunc_i64 : t -> t -> (int64, string) result
val rem : t -> t -> (t, string) result
val pow : t -> t -> t

(** Converte uma string numérica limpa (sem '_' ou sufixo 'm') para [t] em base 10 exata.
    Retorna [None] se o coeficiente exceder 128 bits (~38 dígitos significativos) ou a escala exceder 100 casas. *)
val of_string_opt : string -> t option

val to_string : t -> string
val pp : Format.formatter -> t -> unit

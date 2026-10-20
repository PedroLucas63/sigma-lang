(** Tokens léxicos da linguagem Sigma [.sgm]. *)

open Sigma_common

type kind =
  | KW_VAR
  | IDENT of string
  | INT of int64
  | FLOAT of float
  | DOUBLE of float
  | DECIMAL of Decimal.t
  | BOOL of bool
  | COLON
  | EQUAL
  | COLON_EQUAL
  | MINUS
  | SEMI
  | EOF

type t = {
  kind : kind;
  span : Span.t;
}

val pp_kind : Format.formatter -> kind -> unit
val kind_to_string : kind -> string

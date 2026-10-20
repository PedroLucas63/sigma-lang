(** Tokens léxicos da linguagem Sigma [.sgm]. *)

open Sigma_common

type kind =
  | KW_VAR
  | KW_AND
  | KW_OR
  | KW_XOR
  | KW_NOT
  | IDENT of string
  | INT of int64
  | FLOAT of float
  | DOUBLE of float
  | DECIMAL of Decimal.t
  | BOOL of bool
  | CHAR of string
  | STRING of string
  | INTERP_STRING of interp_segment list
  | COLON
  | EQUAL
  | COLON_EQUAL
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | SLASH_SLASH
  | CARET
  | PERCENT
  | LSHIFT
  | RSHIFT
  | EQ_EQ
  | BANG_EQ
  | LT
  | GT
  | LT_EQ
  | GT_EQ
  | LPAREN
  | RPAREN
  | SEMI
  | EOF

and interp_segment =
  | SegLit of string
  | SegTokens of t list * Span.t

and t = {
  kind : kind;
  span : Span.t;
}

val pp_kind : Format.formatter -> kind -> unit
val kind_to_string : kind -> string

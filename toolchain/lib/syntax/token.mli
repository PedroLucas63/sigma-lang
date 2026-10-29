(** Tokens léxicos da linguagem Sigma [.sgm]. *)

open Sigma_common

type kind =
  | KW_VAR
  | KW_CONST
  | KW_IF
  | KW_ELSE
  | KW_WHILE
  | KW_FOR
  | KW_FOREACH
  | KW_IN
  | KW_BREAK
  | KW_CONTINUE
  | KW_INPUT
  | KW_PROMPT
  | KW_PRINT
  | KW_PRINTLN
  | KW_ASSERT
  | KW_ASSERT_ERROR
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
  | PLUS_EQ
  | MINUS_EQ
  | STAR_EQ
  | SLASH_EQ
  | SLASH_SLASH_EQ
  | PERCENT_EQ
  | CARET_EQ
  | KW_AND_EQ
  | KW_OR_EQ
  | KW_XOR_EQ
  | LSHIFT_EQ
  | RSHIFT_EQ
  | PLUS_PLUS
  | MINUS_MINUS
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
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | DOT_DOT
  | COMMA
  | SEMI
  | EOF

and interp_segment =
  | SegLit of string
  | SegTokens of t list * (string * Span.t) option * Span.t

and t = {
  kind : kind;
  span : Span.t;
}

val pp_kind : Format.formatter -> kind -> unit
val kind_to_string : kind -> string

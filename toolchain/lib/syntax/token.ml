open Sigma_common

type kind =
  | KW_VAR
  | IDENT of string
  | INT of int64
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

let kind_to_string = function
  | KW_VAR -> "'var'"
  | IDENT s -> Printf.sprintf "identifier '%s'" s
  | INT n -> Printf.sprintf "integer '%Ld'" n
  | BOOL b -> Printf.sprintf "boolean '%B'" b
  | COLON -> "':'"
  | EQUAL -> "'='"
  | COLON_EQUAL -> "':='"
  | MINUS -> "'-'"
  | SEMI -> "';'"
  | EOF -> "end of file"

let pp_kind fmt k = Format.fprintf fmt "%s" (kind_to_string k)

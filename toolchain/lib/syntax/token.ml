open Sigma_common

type kind =
  | KW_VAR
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

let kind_to_string = function
  | KW_VAR -> "'var'"
  | IDENT s -> Printf.sprintf "identifier '%s'" s
  | INT n -> Printf.sprintf "integer '%Ld'" n
  | FLOAT f -> Printf.sprintf "float '%.6g'" f
  | DOUBLE d -> Printf.sprintf "double '%.15g'" d
  | DECIMAL m -> Printf.sprintf "decimal '%s'" (Decimal.to_string m)
  | BOOL b -> Printf.sprintf "boolean '%B'" b
  | CHAR c -> Printf.sprintf "char '%s'" c
  | STRING s -> Printf.sprintf "string %S" s
  | INTERP_STRING _ -> "interpolated string"
  | COLON -> "':'"
  | EQUAL -> "'='"
  | COLON_EQUAL -> "':='"
  | PLUS -> "'+'"
  | MINUS -> "'-'"
  | STAR -> "'*'"
  | SLASH -> "'/'"
  | SLASH_SLASH -> "'//'"
  | CARET -> "'^'"
  | PERCENT -> "'%'"
  | LPAREN -> "'('"
  | RPAREN -> "')'"
  | SEMI -> "';'"
  | EOF -> "end of file"

let pp_kind fmt k = Format.fprintf fmt "%s" (kind_to_string k)

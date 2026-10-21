open Sigma_common

type kind =
  | KW_VAR
  | KW_IF
  | KW_ELSE
  | KW_PRINT
  | KW_PRINTLN
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
  | COMMA
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
  | KW_IF -> "'if'"
  | KW_ELSE -> "'else'"
  | KW_PRINT -> "'print'"
  | KW_PRINTLN -> "'println'"
  | KW_AND -> "'and'"
  | KW_OR -> "'or'"
  | KW_XOR -> "'xor'"
  | KW_NOT -> "'not'"
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
  | PLUS_EQ -> "'+='"
  | MINUS_EQ -> "'-='"
  | STAR_EQ -> "'*='"
  | SLASH_EQ -> "'/='"
  | SLASH_SLASH_EQ -> "'//='"
  | PERCENT_EQ -> "'%='"
  | CARET_EQ -> "'^='"
  | KW_AND_EQ -> "'and='"
  | KW_OR_EQ -> "'or='"
  | KW_XOR_EQ -> "'xor='"
  | LSHIFT_EQ -> "'<<='"
  | RSHIFT_EQ -> "'>>='"
  | PLUS_PLUS -> "'++'"
  | MINUS_MINUS -> "'--'"
  | PLUS -> "'+'"
  | MINUS -> "'-'"
  | STAR -> "'*'"
  | SLASH -> "'/'"
  | SLASH_SLASH -> "'//'"
  | CARET -> "'^'"
  | PERCENT -> "'%'"
  | LSHIFT -> "'<<'"
  | RSHIFT -> "'>>'"
  | EQ_EQ -> "'=='"
  | BANG_EQ -> "'!='"
  | LT -> "'<'"
  | GT -> "'>'"
  | LT_EQ -> "'<='"
  | GT_EQ -> "'>='"
  | LPAREN -> "'('"
  | RPAREN -> "')'"
  | LBRACE -> "'{'"
  | RBRACE -> "'}'"
  | COMMA -> "','"
  | SEMI -> "';'"
  | EOF -> "end of file"

let pp_kind fmt k = Format.fprintf fmt "%s" (kind_to_string k)

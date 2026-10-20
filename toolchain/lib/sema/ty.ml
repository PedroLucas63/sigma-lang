open Sigma_common

type t =
  | Int
  | Float
  | Double
  | Decimal
  | Bool
  | Char
  | String

let equal a b =
  match a, b with
  | Int, Int
  | Float, Float
  | Double, Double
  | Decimal, Decimal
  | Bool, Bool
  | Char, Char
  | String, String -> true
  | (Int | Float | Double | Decimal | Bool | Char | String), _ -> false

let to_string = function
  | Int -> "int"
  | Float -> "float"
  | Double -> "double"
  | Decimal -> "decimal"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"

let of_string = function
  | "int" -> Some Int
  | "float" -> Some Float
  | "double" -> Some Double
  | "decimal" -> Some Decimal
  | "bool" -> Some Bool
  | "char" -> Some Char
  | "string" -> Some String
  | _ -> None

let default_int_value = 0L
let default_float_value = 0.0
let default_decimal_value = Decimal.zero
let default_bool_value = false
let default_char_value = ""
let default_string_value = ""

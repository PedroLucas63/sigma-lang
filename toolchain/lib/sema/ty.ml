type t =
  | Int
  | Float
  | Double
  | Decimal
  | Bool

let equal a b =
  match a, b with
  | Int, Int | Float, Float | Double, Double | Decimal, Decimal | Bool, Bool -> true
  | (Int | Float | Double | Decimal | Bool), _ -> false

let to_string = function
  | Int -> "int"
  | Float -> "float"
  | Double -> "double"
  | Decimal -> "decimal"
  | Bool -> "bool"

let of_string = function
  | "int" -> Some Int
  | "float" -> Some Float
  | "double" -> Some Double
  | "decimal" -> Some Decimal
  | "bool" -> Some Bool
  | _ -> None

let default_int_value = 0L
let default_float_value = 0.0
let default_bool_value = false

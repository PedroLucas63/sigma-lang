type t =
  | Int
  | Bool

let equal a b =
  match a, b with
  | Int, Int -> true
  | Bool, Bool -> true
  | (Int | Bool), _ -> false

let to_string = function
  | Int -> "int"
  | Bool -> "bool"

let of_string = function
  | "int" -> Some Int
  | "bool" -> Some Bool
  | _ -> None

let default_int_value = 0L
let default_bool_value = false

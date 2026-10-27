open Sigma_common

type t =
  | Int
  | Float
  | Double
  | Decimal
  | Bool
  | Char
  | String
  | Tuple of t list
  | Array of t * int

let rec equal a b =
  match a, b with
  | Int, Int
  | Float, Float
  | Double, Double
  | Decimal, Decimal
  | Bool, Bool
  | Char, Char
  | String, String -> true
  | Tuple xs, Tuple ys ->
    List.length xs = List.length ys && List.for_all2 equal xs ys
  | Array (ta, na), Array (tb, nb) ->
    na = nb && equal ta tb
  | (Int | Float | Double | Decimal | Bool | Char | String | Tuple _ | Array _), _ -> false

let rec to_string = function
  | Int -> "int"
  | Float -> "float"
  | Double -> "double"
  | Decimal -> "decimal"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Tuple tys ->
    "(" ^ String.concat ", " (List.map to_string tys) ^ ")"
  | Array _ as arr_ty ->
    let rec collect_dims acc = function
      | Array (inner, n) ->
        collect_dims (Printf.sprintf "[%d]" n :: acc) inner
      | base -> (to_string base, List.rev acc)
    in
    let base_s, dims = collect_dims [] arr_ty in
    base_s ^ String.concat "" dims

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

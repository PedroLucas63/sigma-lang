type t =
  | Int

let equal a b =
  match a, b with
  | Int, Int -> true

let to_string = function
  | Int -> "int"

let of_string = function
  | "int" -> Some Int
  | _ -> None

let default_int_value = 0L

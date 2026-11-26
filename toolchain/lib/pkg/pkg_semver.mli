type version = {
  major : int;
  minor : int;
  patch : int;
}

type op =
  | Eq
  | Ge
  | Le
  | Gt
  | Lt
  | Caret
  | Tilde

type constraint_part = op * version
type range = constraint_part list

val parse_version : string -> (version, string) result
val to_string : version -> string
val compare : version -> version -> int
val parse_range : string -> (range, string) result
val satisfies : version -> range -> bool
val best_matching : version list -> range -> version option

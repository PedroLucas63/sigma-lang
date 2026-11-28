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

let to_string v = Printf.sprintf "%d.%d.%d" v.major v.minor v.patch

let compare a b =
  match Stdlib.compare a.major b.major with
  | 0 ->
    (match Stdlib.compare a.minor b.minor with
     | 0 -> Stdlib.compare a.patch b.patch
     | c -> c)
  | c -> c

let parse_version (s : string) : (version, string) result =
  let s = String.trim s in
  let core =
    match String.index_opt s '-' with
    | Some idx -> String.sub s 0 idx
    | None -> s
  in
  match String.split_on_char '.' core with
  | [ a; b; c ] ->
    (match int_of_string_opt a, int_of_string_opt b, int_of_string_opt c with
     | Some major, Some minor, Some patch when major >= 0 && minor >= 0 && patch >= 0 ->
       Ok { major; minor; patch }
     | _ -> Error (Printf.sprintf "E0137: invalid SemVer version '%s'" s))
  | _ -> Error (Printf.sprintf "E0137: invalid SemVer version '%s'" s)

let parse_part (raw : string) : (constraint_part, string) result =
  let s = String.trim raw in
  if s = "" || s = "*" then Ok (Ge, { major = 0; minor = 0; patch = 0 })
  else
    let op, rest =
      if String.starts_with ~prefix:">=" s then (Ge, String.sub s 2 (String.length s - 2))
      else if String.starts_with ~prefix:"<=" s then (Le, String.sub s 2 (String.length s - 2))
      else if String.starts_with ~prefix:">" s then (Gt, String.sub s 1 (String.length s - 1))
      else if String.starts_with ~prefix:"<" s then (Lt, String.sub s 1 (String.length s - 1))
      else if String.starts_with ~prefix:"=" s then (Eq, String.sub s 1 (String.length s - 1))
      else if String.starts_with ~prefix:"^" s then (Caret, String.sub s 1 (String.length s - 1))
      else if String.starts_with ~prefix:"~" s then (Tilde, String.sub s 1 (String.length s - 1))
      else (Eq, s)
    in
    match parse_version rest with
    | Ok v -> Ok (op, v)
    | Error e -> Error e

let parse_range (s : string) : (range, string) result =
  let parts = String.split_on_char ',' (String.trim s) in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | p :: tl ->
      (match parse_part p with
       | Error e -> Error e
       | Ok c -> loop (c :: acc) tl)
  in
  loop [] parts

let satisfies_part (v : version) ((op, target) : constraint_part) : bool =
  match op with
  | Eq -> compare v target = 0
  | Ge -> compare v target >= 0
  | Le -> compare v target <= 0
  | Gt -> compare v target > 0
  | Lt -> compare v target < 0
  | Caret ->
    if compare v target < 0 then false
    else if target.major > 0 then
      compare v { major = target.major + 1; minor = 0; patch = 0 } < 0
    else if target.minor > 0 then
      compare v { major = 0; minor = target.minor + 1; patch = 0 } < 0
    else
      compare v { major = 0; minor = 0; patch = target.patch + 1 } < 0
  | Tilde ->
    compare v target >= 0
    && compare v { major = target.major; minor = target.minor + 1; patch = 0 } < 0

let satisfies (v : version) (r : range) : bool =
  List.for_all (satisfies_part v) r

let best_matching (versions : version list) (r : range) : version option =
  versions
  |> List.filter (fun v -> satisfies v r)
  |> List.sort (fun a b -> compare b a)
  |> function x :: _ -> Some x | [] -> None

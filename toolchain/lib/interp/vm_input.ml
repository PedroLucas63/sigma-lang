open Sigma_common
open Sigma_sema
open Sigma_ir
open Vm_math
open Vm_state
open Vm_format

let strip_cr (s : string) : string =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\r' then String.sub s 0 (len - 1) else s

let read_next_line (st : state) : (string, string) result =
  match st.stdin_lines with
  | Some (line :: rest) ->
    st.stdin_lines <- Some rest;
    Ok (strip_cr line)
  | Some [] -> Error "Runtime error: unexpected end of input (EOF) while reading from stdin"
  | None ->
    (try
       flush stdout;
       Ok (strip_cr (input_line stdin))
     with End_of_file ->
       Error "Runtime error: unexpected end of input (EOF) while reading from stdin")

let is_strict_base10_int (s : string) : bool =
  let len = String.length s in
  if len = 0 then false
  else
    let start = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
    if start >= len then false
    else
      let rec check i =
        if i >= len then true
        else if s.[i] >= '0' && s.[i] <= '9' then check (i + 1)
        else false
      in
      check start

let parse_prim_value (ty : Ty.t) (raw : string) : (value, string) result =
  match ty with
  | Ty.Int ->
    let s = String.trim raw in
    if is_strict_base10_int s then
      match Int64.of_string_opt s with
      | Some n -> Ok (VInt n)
      | None -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'int'" raw)
    else Error (Printf.sprintf "Runtime error: failed to parse %S as 'int'" raw)
  | Ty.Float ->
    let s = String.trim raw in
    if s = "" then Error (Printf.sprintf "Runtime error: failed to parse %S as 'float'" raw)
    else
      (match Float.of_string_opt s with
       | Some f when not (Float.is_nan f || Float.is_infinite f) -> Ok (VFloat (round_f32 f))
       | _ -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'float'" raw))
  | Ty.Double ->
    let s = String.trim raw in
    if s = "" then Error (Printf.sprintf "Runtime error: failed to parse %S as 'double'" raw)
    else
      (match Float.of_string_opt s with
       | Some d when not (Float.is_nan d || Float.is_infinite d) -> Ok (VDouble d)
       | _ -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'double'" raw))
  | Ty.Decimal ->
    let s = String.trim raw in
    let s_clean =
      if String.length s > 1 && s.[0] = '+' then String.sub s 1 (String.length s - 1) else s
    in
    (match Decimal.of_string_opt s_clean with
     | Some m -> Ok (VDecimal m)
     | None -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'decimal'" raw))
  | Ty.Bool ->
    (match String.trim raw with
     | "true" -> Ok (VBool true)
     | "false" -> Ok (VBool false)
     | _ -> Error (Printf.sprintf "Runtime error: failed to parse %S as 'bool' (expected 'true' or 'false')" raw))
  | Ty.Char ->
    if utf8_code_point_len raw = 1 then Ok (VChar raw)
    else
      let trimmed = String.trim raw in
      if utf8_code_point_len trimmed = 1 then Ok (VChar trimmed)
      else Error (Printf.sprintf "Runtime error: failed to parse %S as 'char' (expected a single character)" raw)
  | Ty.String -> Ok (VString raw)
  | Ty.Tuple _ | Ty.Array _ | Ty.DynArray _ | Ty.User _ | Ty.Fun _ | Ty.Void ->
    Error "Runtime error: composite types are not supported in primitive input parsing"

let split_whitespace_tokens (s : string) : string list =
  let len = String.length s in
  let is_ws c = c = ' ' || c = '\t' in
  let rec skip_ws i = if i < len && is_ws s.[i] then skip_ws (i + 1) else i in
  let rec take_word i = if i < len && not (is_ws s.[i]) then take_word (i + 1) else i in
  let rec loop i acc =
    let start = skip_ws i in
    if start >= len then List.rev acc
    else
      let stop = take_word start in
      loop stop (String.sub s start (stop - start) :: acc)
  in
  loop 0 []

let find_substring_from (haystack : string) (start_pos : int) (needle : string) : int option =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  if nlen = 0 then Some start_pos
  else if start_pos + nlen > hlen then None
  else
    let rec loop i =
      if i + nlen > hlen then None
      else if String.sub haystack i nlen = needle then Some i
      else loop (i + 1)
    in
    loop start_pos

let exec_input (st : state) (spec : Ir.input_spec) : (value, string) result =
  match read_next_line st with
  | Error msg -> Error msg
  | Ok line ->
    (match spec with
     | Ir.IrInputLine -> Ok (VString line)
     | Ir.IrInputTypes [ single_ty ] ->
       let raw = if single_ty = Ty.String then String.trim line else line in
       parse_prim_value single_ty raw
     | Ir.IrInputTypes tys ->
       let toks = split_whitespace_tokens line in
       let expected_n = List.length tys in
       let actual_n = List.length toks in
       if actual_n <> expected_n then
         Error
           (Printf.sprintf "Runtime error: input expected %d whitespace-separated values, found %d in %S"
              expected_n actual_n line)
       else
         let rec parse_all acc ty_l tok_l =
           match ty_l, tok_l with
           | [], [] -> Ok (VTuple (Array.of_list (List.rev acc)))
           | ty :: rest_ty, tok :: rest_tok ->
             (match parse_prim_value ty tok with
              | Error msg -> Error msg
              | Ok v -> parse_all (v :: acc) rest_ty rest_tok)
           | _ -> Error "Runtime error: internal input token mismatch"
         in
         parse_all [] tys toks
     | Ir.IrInputMask parts ->
       let hlen = String.length line in
       let rec match_parts pos acc = function
         | [] ->
           if pos <> hlen then
             Error
               (Printf.sprintf "Runtime error: unexpected trailing input %S after matching mask in %S"
                  (String.sub line pos (hlen - pos)) line)
           else
             let vals = List.rev acc in
             (match vals with [ single ] -> Ok single | _ -> Ok (VTuple (Array.of_list vals)))
         | Ir.IrMaskLit lit :: rest ->
           let llen = String.length lit in
           if pos + llen <= hlen && String.sub line pos llen = lit then match_parts (pos + llen) acc rest
           else
             Error
               (Printf.sprintf "Runtime error: input %S does not match mask delimiter %S at position %d"
                  line lit pos)
         | Ir.IrMaskType ty :: rest ->
           (match rest with
            | [] ->
              let sub = String.sub line pos (hlen - pos) in
              (match parse_prim_value ty sub with
               | Error msg -> Error msg
               | Ok v -> match_parts hlen (v :: acc) [])
            | Ir.IrMaskLit next_lit :: _ ->
              (match find_substring_from line pos next_lit with
               | None ->
                 Error (Printf.sprintf "Runtime error: input %S is missing mask delimiter %S" line next_lit)
               | Some found_idx ->
                 let sub = String.sub line pos (found_idx - pos) in
                 (match parse_prim_value ty sub with
                  | Error msg -> Error msg
                  | Ok v -> match_parts found_idx (v :: acc) rest))
            | Ir.IrMaskType _ :: _ -> Error "Runtime error: invalid adjacent mask placeholders")
       in
       match_parts 0 [] parts)

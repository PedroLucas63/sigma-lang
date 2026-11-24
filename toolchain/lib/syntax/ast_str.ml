open Ast_types

let type_expr_span = function
  | TypeNamed (_, sp) | TypeTuple (_, sp) | TypeArray (_, _, sp) | TypeFun (_, _, sp) -> sp

let param_mode_prefix = function
  | ParamVal -> ""
  | ParamRef -> "ref "
  | ParamOut -> "out "

let rec type_expr_to_string = function
  | TypeNamed (name, _) -> name
  | TypeTuple (tys, _) ->
    "(" ^ String.concat ", " (List.map type_expr_to_string tys) ^ ")"
  | TypeFun (ret, params, _) ->
    let p_strs =
      List.map (fun (m, te) -> param_mode_prefix m ^ type_expr_to_string te) params
    in
    type_expr_to_string ret ^ "(" ^ String.concat ", " p_strs ^ ")"
  | TypeArray _ as arr ->
    let rec collect_dims acc = function
      | TypeArray (inner, Some n, _) ->
        collect_dims (Printf.sprintf "[%d]" n :: acc) inner
      | TypeArray (inner, None, _) ->
        collect_dims ("[]" :: acc) inner
      | base -> (type_expr_to_string base, List.rev acc)
    in
    let base_s, dims = collect_dims [] arr in
    base_s ^ String.concat "" dims

let lvalue_span = function
  | LValIdent (_, sp) | LValIndex (_, _, sp) -> sp

let unop_to_string = function
  | UnPos -> "+"
  | UnNeg -> "-"
  | UnNot -> "not"

let binop_to_string = function
  | BinAdd -> "+"
  | BinSub -> "-"
  | BinMul -> "*"
  | BinDiv -> "/"
  | BinIntDiv -> "//"
  | BinPow -> "^"
  | BinMod -> "%"
  | BinShl -> "<<"
  | BinShr -> ">>"
  | BinEq -> "=="
  | BinNe -> "!="
  | BinLt -> "<"
  | BinGt -> ">"
  | BinLe -> "<="
  | BinGe -> ">="
  | BinAnd -> "and"
  | BinXor -> "xor"
  | BinOr -> "or"

let assign_op_to_string = function
  | AssignEq -> "="
  | AssignAdd -> "+="
  | AssignSub -> "-="
  | AssignMul -> "*="
  | AssignDiv -> "/="
  | AssignIntDiv -> "//="
  | AssignMod -> "%="
  | AssignPow -> "^="
  | AssignAnd -> "and="
  | AssignOr -> "or="
  | AssignXor -> "xor="
  | AssignShl -> "<<="
  | AssignShr -> ">>="

let incdec_op_to_string = function
  | PreInc | PostInc -> "++"
  | PreDec | PostDec -> "--"

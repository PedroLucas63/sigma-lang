open Sigma_common

type unop = UnPos | UnNeg | UnNot

type binop =
  | BinAdd
  | BinSub
  | BinMul
  | BinDiv
  | BinIntDiv
  | BinPow
  | BinMod
  | BinShl
  | BinShr
  | BinEq
  | BinNe
  | BinLt
  | BinGt
  | BinLe
  | BinGe
  | BinAnd
  | BinXor
  | BinOr

type assign_op =
  | AssignEq
  | AssignAdd
  | AssignSub
  | AssignMul
  | AssignDiv
  | AssignIntDiv
  | AssignMod
  | AssignPow
  | AssignAnd
  | AssignOr
  | AssignXor
  | AssignShl
  | AssignShr

type incdec_op =
  | PreInc
  | PreDec
  | PostInc
  | PostDec

type align = AlignLeft | AlignRight | AlignCenter

type format_spec = {
  fill : string;
  align : align option;
  sign_plus : bool;
  zero_pad : bool;
  width : int option;
  precision : int option;
  span : Span.t;
}

type mask_part =
  | MaskLit of string
  | MaskType of string * Span.t

type input_mode =
  | InputLine
  | InputTypes of (string * Span.t) list
  | InputMask of mask_part list

type param_mode =
  | ParamVal
  | ParamRef
  | ParamOut

type type_expr =
  | TypeNamed of string * Span.t
  | TypeGeneric of string * type_expr list * Span.t
  | TypeWildcard of Span.t
  | TypeTuple of type_expr list * Span.t
  | TypeArray of type_expr * int option * Span.t
  | TypeFun of type_expr * (param_mode * type_expr) list * Span.t

type type_param = {
  tp_name : string;
  tp_span : Span.t;
  tp_variadic : bool;
}

type type_constraint =
  | ConstraintTypes of type_expr list
  | ConstraintNumeric
  | ConstraintClass
  | ConstraintStruct
  | ConstraintRecord

type where_clause = {
  wc_param : string;
  wc_param_span : Span.t;
  wc_constraint : type_constraint;
  wc_span : Span.t;
}

type arg_mode =
  | ArgVal
  | ArgRef
  | ArgOut
  | ArgOutVar of {
      var_name : string;
      var_span : Span.t;
      var_type : type_expr option;
    }

type fun_param = {
  param_name : string;
  param_span : Span.t;
  param_mode : param_mode;
  param_type : type_expr;
  param_variadic : bool;
}

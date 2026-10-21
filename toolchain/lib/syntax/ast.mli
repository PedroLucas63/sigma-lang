(** Abstract Syntax Tree (AST) da linguagem Sigma [.sgm]. *)

open Sigma_common

type unop =
  | UnPos
  | UnNeg
  | UnNot

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

type expr_desc =
  | ExprInt of int64
  | ExprFloat of float
  | ExprDouble of float
  | ExprDecimal of Decimal.t
  | ExprBool of bool
  | ExprChar of string
  | ExprString of string
  | ExprInterp of interp_part list
  | ExprIdent of string
  | ExprUnary of unop * expr
  | ExprBinary of binop * expr * expr
  | ExprChainCmp of expr * binop * expr * binop * expr
  | ExprAssign of string * Span.t * assign_op * expr
  | ExprIncDec of incdec_op * string * Span.t

and interp_part =
  | InterpLit of string
  | InterpExpr of expr

and expr = {
  desc : expr_desc;
  span : Span.t;
}

type var_decl = {
  name : string;
  name_span : Span.t;
  ty_annot : (string * Span.t) option;
  init : expr option;
}

type stmt_desc =
  | StmtVarDecl of var_decl
  | StmtBlock of stmt list
  | StmtIf of {
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
  | StmtPrint of {
      newline : bool;
      args : expr list;
    }
  | StmtExpr of expr

and stmt = {
  desc : stmt_desc;
  span : Span.t;
}

type program = stmt list

val unop_to_string : unop -> string
val binop_to_string : binop -> string
val assign_op_to_string : assign_op -> string
val incdec_op_to_string : incdec_op -> string
val pp_program : Format.formatter -> program -> unit

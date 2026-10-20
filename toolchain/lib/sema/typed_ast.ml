open Sigma_common
open Sigma_syntax

type expr_desc =
  | TExprInt of int64
  | TExprFloat of float
  | TExprDouble of float
  | TExprDecimal of Decimal.t
  | TExprBool of bool
  | TExprChar of string
  | TExprString of string
  | TExprInterp of interp_part list
  | TExprIdent of string
  | TExprUnary of Ast.unop * expr
  | TExprBinary of Ast.binop * expr * expr
  | TExprChainCmp of expr * Ast.binop * expr * Ast.binop * expr

and interp_part =
  | TInterpLit of string
  | TInterpExpr of expr

and expr = {
  desc : expr_desc;
  ty : Ty.t;
  span : Span.t;
}

type var_decl = {
  name : string;
  name_span : Span.t;
  ty : Ty.t;
  init : expr;
  was_default_initialized : bool;
}

type stmt_desc =
  | TStmtVarDecl of var_decl
  | TStmtExpr of expr

type stmt = {
  desc : stmt_desc;
  span : Span.t;
}

type program = stmt list

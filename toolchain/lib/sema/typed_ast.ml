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
  | TExprAssign of string * Ast.assign_op * expr
  | TExprIncDec of Ast.incdec_op * string

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
  is_const : bool;
  ty : Ty.t;
  init : expr;
  was_default_initialized : bool;
}

type stmt_desc =
  | TStmtVarDecl of var_decl
  | TStmtBlock of stmt list
  | TStmtIf of {
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
  | TStmtPrint of {
      newline : bool;
      args : expr list;
    }
  | TStmtAssert of {
      actual : expr;
      expected : expr;
      message : expr option;
    }
  | TStmtAssertError of {
      expected_code : string;
      actual_codes : string list;
      passed : bool;
      message : expr option;
    }
  | TStmtExpr of expr

and stmt = {
  desc : stmt_desc;
  span : Span.t;
}

type program = stmt list

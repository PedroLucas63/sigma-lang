open Sigma_common
open Sigma_syntax

type collection_item =
  | TItemExpr of expr
  | TItemSpread of expr

and lvalue =
  | TLValIdent of string
  | TLValIndex of expr * expr

and expr_desc =
  | TExprInt of int64
  | TExprFloat of float
  | TExprDouble of float
  | TExprDecimal of Decimal.t
  | TExprBool of bool
  | TExprChar of string
  | TExprString of string
  | TExprInterp of interp_part list
  | TExprIdent of string
  | TExprTuple of collection_item list
  | TExprArray of collection_item list
  | TExprIndex of expr * expr
  | TExprUnary of Ast.unop * expr
  | TExprBinary of Ast.binop * expr * expr
  | TExprChainCmp of expr * Ast.binop * expr * Ast.binop * expr
  | TExprAssign of lvalue * Ast.assign_op * expr
  | TExprIncDec of Ast.incdec_op * lvalue

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

type destruct_binding = {
  name : string option;
  name_span : Span.t;
  ty : Ty.t;
}

type destruct_decl = {
  bindings : destruct_binding list;
  is_const : bool;
  ty : Ty.t;
  init : expr;
}

type for_init =
  | TForInitVarDecl of var_decl
  | TForInitDestruct of destruct_decl
  | TForInitExpr of expr

type foreach_head =
  | TForeachVar of {
      name : string option;
      name_span : Span.t;
      is_const : bool;
      ty : Ty.t;
    }
  | TForeachDestruct of {
      bindings : destruct_binding list;
      is_const : bool;
      ty : Ty.t;
    }

type stmt_desc =
  | TStmtVarDecl of var_decl
  | TStmtDestruct of destruct_decl
  | TStmtBlock of stmt list
  | TStmtIf of {
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
  | TStmtWhile of {
      loop_id : int;
      label : string option;
      cond : expr;
      body : stmt;
    }
  | TStmtFor of {
      loop_id : int;
      label : string option;
      init : for_init;
      cond : expr;
      step : expr;
      body : stmt;
    }
  | TStmtForeach of {
      loop_id : int;
      label : string option;
      head : foreach_head;
      collection : expr;
      elem_ty : Ty.t;
      len : int;
      body : stmt;
    }
  | TStmtBreak of {
      target_loop_id : int;
      label : string option;
    }
  | TStmtContinue of {
      target_loop_id : int;
      label : string option;
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

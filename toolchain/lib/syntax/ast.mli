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

type type_expr =
  | TypeNamed of string * Span.t
  | TypeTuple of type_expr list * Span.t
  | TypeArray of type_expr * int option * Span.t

type collection_item =
  | ItemExpr of expr
  | ItemSpread of expr * Span.t

and lvalue =
  | LValIdent of string * Span.t
  | LValIndex of expr * expr * Span.t

and expr_desc =
  | ExprInt of int64
  | ExprFloat of float
  | ExprDouble of float
  | ExprDecimal of Decimal.t
  | ExprBool of bool
  | ExprChar of string
  | ExprString of string
  | ExprInterp of interp_part list
  | ExprIdent of string
  | ExprTuple of collection_item list
  | ExprArray of collection_item list
  | ExprIndex of expr * expr
  | ExprUnary of unop * expr
  | ExprBinary of binop * expr * expr
  | ExprChainCmp of expr * binop * expr * binop * expr
  | ExprAssign of lvalue * assign_op * expr
  | ExprIncDec of incdec_op * lvalue

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
  is_const : bool;
  ty_annot : type_expr option;
  init : expr option;
}

type destruct_decl = {
  names : (string * Span.t) list;
  is_const : bool;
  ty_annot : type_expr option;
  init : expr option;
}

type for_init =
  | ForInitVarDecl of var_decl
  | ForInitDestruct of destruct_decl
  | ForInitExpr of expr

type foreach_binding =
  | ForeachVar of string * Span.t
  | ForeachDestruct of (string * Span.t) list

type foreach_head = {
  binding : foreach_binding;
  is_const : bool;
  ty_annot : type_expr option;
}

type stmt_desc =
  | StmtVarDecl of var_decl
  | StmtDestruct of destruct_decl
  | StmtBlock of stmt list
  | StmtIf of {
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
  | StmtWhile of {
      label : (string * Span.t) option;
      cond : expr;
      body : stmt;
    }
  | StmtFor of {
      label : (string * Span.t) option;
      init : for_init;
      cond : expr;
      step : expr;
      body : stmt;
    }
  | StmtForeach of {
      label : (string * Span.t) option;
      head : foreach_head;
      collection : expr;
      body : stmt;
    }
  | StmtBreak of (string * Span.t) option
  | StmtContinue of (string * Span.t) option
  | StmtPrint of {
      newline : bool;
      args : expr list;
    }
  | StmtAssert of {
      actual : expr;
      expected : expr option;
      message : expr option;
    }
  | StmtAssertError of {
      expected_code : string;
      code_span : Span.t;
      snippet : string;
      snippet_span : Span.t;
      message : expr option;
    }
  | StmtExpr of expr

and stmt = {
  desc : stmt_desc;
  span : Span.t;
}

type program = stmt list

val type_expr_span : type_expr -> Span.t
val type_expr_to_string : type_expr -> string
val lvalue_span : lvalue -> Span.t
val unop_to_string : unop -> string
val binop_to_string : binop -> string
val assign_op_to_string : assign_op -> string
val incdec_op_to_string : incdec_op -> string
val pp_program : Format.formatter -> program -> unit

(** Árvore Sintática Tipada (Typed AST) após validação semântica. *)

open Sigma_common

type expr_desc =
  | TExprInt of int64
  | TExprFloat of float
  | TExprDouble of float
  | TExprDecimal of float
  | TExprBool of bool
  | TExprIdent of string

type expr = {
  desc : expr_desc;
  ty : Ty.t;
  span : Span.t;
}

type var_decl = {
  name : string;
  name_span : Span.t;
  ty : Ty.t;
  init : expr;   (** Já resolvido (com zero-initialization explícita quando omitido) *)
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

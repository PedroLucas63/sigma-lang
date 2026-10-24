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
  is_const : bool;
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

let rec pp_expr fmt (e : expr) =
  match e.desc with
  | ExprInt n -> Format.fprintf fmt "%Ld" n
  | ExprFloat f -> Format.fprintf fmt "%.6g" f
  | ExprDouble d -> Format.fprintf fmt "%.15gd" d
  | ExprDecimal m -> Format.fprintf fmt "%sm" (Decimal.to_string m)
  | ExprBool b -> Format.fprintf fmt "%B" b
  | ExprChar c -> Format.fprintf fmt "'%s'" c
  | ExprString s -> Format.fprintf fmt "%S" s
  | ExprInterp parts ->
    Format.fprintf fmt "\"";
    List.iter
      (function
        | InterpLit s -> Format.fprintf fmt "%s" s
        | InterpExpr ie -> Format.fprintf fmt "${%a}" pp_expr ie)
      parts;
    Format.fprintf fmt "\""
  | ExprIdent id -> Format.fprintf fmt "%s" id
  | ExprUnary (op, sub) ->
    Format.fprintf fmt "(%s %a)" (unop_to_string op) pp_expr sub
  | ExprBinary (op, lhs, rhs) ->
    Format.fprintf fmt "(%a %s %a)" pp_expr lhs (binop_to_string op) pp_expr rhs
  | ExprChainCmp (e1, op1, e2, op2, e3) ->
    Format.fprintf fmt "(%a %s %a %s %a)"
      pp_expr e1 (binop_to_string op1)
      pp_expr e2 (binop_to_string op2)
      pp_expr e3
  | ExprAssign (name, _, op, rhs) ->
    Format.fprintf fmt "(%s %s %a)" name (assign_op_to_string op) pp_expr rhs
  | ExprIncDec (PreInc, name, _) ->
    Format.fprintf fmt "(++%s)" name
  | ExprIncDec (PreDec, name, _) ->
    Format.fprintf fmt "(--%s)" name
  | ExprIncDec (PostInc, name, _) ->
    Format.fprintf fmt "(%s++)" name
  | ExprIncDec (PostDec, name, _) ->
    Format.fprintf fmt "(%s--)" name

let rec pp_stmt fmt (s : stmt) =
  match s.desc with
  | StmtVarDecl v ->
    let kw = if v.is_const then "const" else "var" in
    let ty_s =
      match v.ty_annot with
      | None -> ""
      | Some (t, _) -> ": " ^ t
    in
    (match v.init, v.ty_annot with
     | Some init, None ->
       Format.fprintf fmt "%s %s := %a;" kw v.name pp_expr init
     | Some init, Some _ ->
       Format.fprintf fmt "%s %s%s = %a;" kw v.name ty_s pp_expr init
     | None, _ ->
       Format.fprintf fmt "%s %s%s;" kw v.name ty_s)
  | StmtBlock stmts ->
    Format.fprintf fmt "{ %a }"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_stmt)
      stmts
  | StmtIf { cond; then_branch; else_branch = None } ->
    Format.fprintf fmt "if (%a) %a" pp_expr cond pp_stmt then_branch
  | StmtIf { cond; then_branch; else_branch = Some eb } ->
    Format.fprintf fmt "if (%a) %a else %a" pp_expr cond pp_stmt then_branch pp_stmt eb
  | StmtPrint { newline; args } ->
    let fn_name = if newline then "println" else "print" in
    Format.fprintf fmt "%s(%a);" fn_name
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
      args
  | StmtAssert { actual; expected; message } ->
    (match expected, message with
     | None, None -> Format.fprintf fmt "assert(%a);" pp_expr actual
     | Some exp, None -> Format.fprintf fmt "assert(%a, %a);" pp_expr actual pp_expr exp
     | Some exp, Some msg ->
       Format.fprintf fmt "assert(%a, %a, %a);" pp_expr actual pp_expr exp pp_expr msg
     | None, Some msg ->
       Format.fprintf fmt "assert(%a, %a);" pp_expr actual pp_expr msg)
  | StmtAssertError { expected_code; snippet; message; _ } ->
    (match message with
     | None -> Format.fprintf fmt "assert_error(%S, %S);" expected_code snippet
     | Some msg -> Format.fprintf fmt "assert_error(%S, %S, %a);" expected_code snippet pp_expr msg)
  | StmtExpr e ->
    Format.fprintf fmt "%a;" pp_expr e

let pp_program fmt prog =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
    pp_stmt fmt prog

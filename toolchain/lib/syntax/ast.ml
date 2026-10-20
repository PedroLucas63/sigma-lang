open Sigma_common

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
  | StmtExpr of expr

and stmt = {
  desc : stmt_desc;
  span : Span.t;
}

type program = stmt list

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

let pp_stmt fmt (s : stmt) =
  match s.desc with
  | StmtVarDecl v ->
    let ty_s =
      match v.ty_annot with
      | None -> ""
      | Some (t, _) -> ": " ^ t
    in
    (match v.init, v.ty_annot with
     | Some init, None ->
       Format.fprintf fmt "var %s := %a;" v.name pp_expr init
     | Some init, Some _ ->
       Format.fprintf fmt "var %s%s = %a;" v.name ty_s pp_expr init
     | None, _ ->
       Format.fprintf fmt "var %s%s;" v.name ty_s)
  | StmtExpr e ->
    Format.fprintf fmt "%a;" pp_expr e

let pp_program fmt prog =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
    pp_stmt fmt prog

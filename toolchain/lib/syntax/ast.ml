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

type align =
  | AlignLeft
  | AlignRight
  | AlignCenter

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
  | ExprInput of {
      prompt : expr option;
      mode : input_mode;
    }

and interp_part =
  | InterpLit of string
  | InterpExpr of expr * format_spec option

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

let type_expr_span = function
  | TypeNamed (_, sp) | TypeTuple (_, sp) | TypeArray (_, _, sp) -> sp

let rec type_expr_to_string = function
  | TypeNamed (name, _) -> name
  | TypeTuple (tys, _) ->
    "(" ^ String.concat ", " (List.map type_expr_to_string tys) ^ ")"
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

let rec pp_collection_item fmt = function
  | ItemExpr e -> pp_expr fmt e
  | ItemSpread (e, _) -> Format.fprintf fmt "..%a" pp_expr e

and pp_lvalue fmt = function
  | LValIdent (name, _) -> Format.fprintf fmt "%s" name
  | LValIndex (base, idx, _) -> Format.fprintf fmt "%a[%a]" pp_expr base pp_expr idx

and pp_expr fmt (e : expr) =
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
        | InterpExpr (ie, _) -> Format.fprintf fmt "${%a}" pp_expr ie)
      parts;
    Format.fprintf fmt "\""
  | ExprIdent id -> Format.fprintf fmt "%s" id
  | ExprInput { prompt; mode } ->
    let fn = if Option.is_some prompt then "prompt" else "input" in
    let prompt_s =
      match prompt with
      | None -> ""
      | Some pe -> Format.asprintf "%a" pp_expr pe
    in
    let mode_s =
      match mode with
      | InputLine -> ""
      | InputTypes tys -> String.concat ", " (List.map fst tys)
      | InputMask parts ->
        let buf = Buffer.create 16 in
        Buffer.add_char buf '"';
        List.iter
          (function
            | MaskLit s -> Buffer.add_string buf s
            | MaskType (t, _) -> Buffer.add_string buf ("${" ^ t ^ "}"))
          parts;
        Buffer.add_char buf '"';
        Buffer.contents buf
    in
    let args_s =
      if prompt_s <> "" && mode_s <> "" then prompt_s ^ ", " ^ mode_s
      else if prompt_s <> "" then prompt_s
      else mode_s
    in
    Format.fprintf fmt "%s(%s)" fn args_s
  | ExprTuple items ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_collection_item)
      items
  | ExprArray items ->
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_collection_item)
      items
  | ExprIndex (base, idx) ->
    Format.fprintf fmt "%a[%a]" pp_expr base pp_expr idx
  | ExprUnary (op, sub) ->
    Format.fprintf fmt "(%s %a)" (unop_to_string op) pp_expr sub
  | ExprBinary (op, lhs, rhs) ->
    Format.fprintf fmt "(%a %s %a)" pp_expr lhs (binop_to_string op) pp_expr rhs
  | ExprChainCmp (e1, op1, e2, op2, e3) ->
    Format.fprintf fmt "(%a %s %a %s %a)"
      pp_expr e1 (binop_to_string op1)
      pp_expr e2 (binop_to_string op2)
      pp_expr e3
  | ExprAssign (lval, op, rhs) ->
    Format.fprintf fmt "(%a %s %a)" pp_lvalue lval (assign_op_to_string op) pp_expr rhs
  | ExprIncDec (PreInc, lval) ->
    Format.fprintf fmt "(++%a)" pp_lvalue lval
  | ExprIncDec (PreDec, lval) ->
    Format.fprintf fmt "(--%a)" pp_lvalue lval
  | ExprIncDec (PostInc, lval) ->
    Format.fprintf fmt "(%a++)" pp_lvalue lval
  | ExprIncDec (PostDec, lval) ->
    Format.fprintf fmt "(%a--)" pp_lvalue lval

let rec pp_stmt fmt (s : stmt) =
  match s.desc with
  | StmtVarDecl v ->
    let kw = if v.is_const then "const" else "var" in
    let ty_s =
      match v.ty_annot with
      | None -> ""
      | Some te -> ": " ^ type_expr_to_string te
    in
    (match v.init, v.ty_annot with
     | Some init, None ->
       Format.fprintf fmt "%s %s := %a;" kw v.name pp_expr init
     | Some init, Some _ ->
       Format.fprintf fmt "%s %s%s = %a;" kw v.name ty_s pp_expr init
     | None, _ ->
       Format.fprintf fmt "%s %s%s;" kw v.name ty_s)
  | StmtDestruct d ->
    let kw = if d.is_const then "const" else "var" in
    let names_s = String.concat ", " (List.map fst d.names) in
    let ty_s =
      match d.ty_annot with
      | None -> ""
      | Some te -> ": " ^ type_expr_to_string te
    in
    (match d.init, d.ty_annot with
     | Some init, None ->
       Format.fprintf fmt "%s (%s) := %a;" kw names_s pp_expr init
     | Some init, Some _ ->
       Format.fprintf fmt "%s (%s)%s = %a;" kw names_s ty_s pp_expr init
     | None, _ ->
       Format.fprintf fmt "%s (%s)%s;" kw names_s ty_s)
  | StmtBlock stmts ->
    Format.fprintf fmt "{ %a }"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_stmt)
      stmts
  | StmtIf { cond; then_branch; else_branch = None } ->
    Format.fprintf fmt "if (%a) %a" pp_expr cond pp_stmt then_branch
  | StmtIf { cond; then_branch; else_branch = Some eb } ->
    Format.fprintf fmt "if (%a) %a else %a" pp_expr cond pp_stmt then_branch pp_stmt eb
  | StmtWhile { label; cond; body } ->
    let lbl =
      match label with
      | None -> ""
      | Some (l, _) -> l ^ ": "
    in
    Format.fprintf fmt "%swhile (%a) %a" lbl pp_expr cond pp_stmt body
  | StmtFor { label; init; cond; step; body } ->
    let lbl =
      match label with
      | None -> ""
      | Some (l, _) -> l ^ ": "
    in
    let pp_for_init fmt = function
      | ForInitVarDecl v ->
        pp_stmt fmt { desc = StmtVarDecl v; span = s.span }
      | ForInitDestruct d ->
        pp_stmt fmt { desc = StmtDestruct d; span = s.span }
      | ForInitExpr e ->
        Format.fprintf fmt "%a;" pp_expr e
    in
    Format.fprintf fmt "%sfor (%a %a; %a) %a" lbl pp_for_init init pp_expr cond pp_expr step pp_stmt body
  | StmtForeach { label; head; collection; body } ->
    let lbl =
      match label with
      | None -> ""
      | Some (l, _) -> l ^ ": "
    in
    let kw = if head.is_const then "const" else "var" in
    let bind_s =
      match head.binding with
      | ForeachVar (name, _) -> name
      | ForeachDestruct names -> "(" ^ String.concat ", " (List.map fst names) ^ ")"
    in
    let ty_s =
      match head.ty_annot with
      | None -> ""
      | Some te -> " : " ^ type_expr_to_string te
    in
    Format.fprintf fmt "%sforeach (%s %s%s in %a) %a" lbl kw bind_s ty_s pp_expr collection pp_stmt body
  | StmtBreak None ->
    Format.fprintf fmt "break;"
  | StmtBreak (Some (l, _)) ->
    Format.fprintf fmt "break %s;" l
  | StmtContinue None ->
    Format.fprintf fmt "continue;"
  | StmtContinue (Some (l, _)) ->
    Format.fprintf fmt "continue %s;" l
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

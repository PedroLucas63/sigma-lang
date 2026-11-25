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

type param_mode =
  | ParamVal
  | ParamRef
  | ParamOut

type type_expr =
  | TypeNamed of string * Span.t
  | TypeTuple of type_expr list * Span.t
  | TypeArray of type_expr * int option * Span.t
  | TypeFun of type_expr * (param_mode * type_expr) list * Span.t

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
}

type collection_item =
  | ItemExpr of expr
  | ItemSpread of expr * Span.t

and lvalue =
  | LValIdent of string * Span.t
  | LValIndex of expr * expr * Span.t
  | LValField of expr * string * Span.t

and multi_target =
  | TargetDiscard of Span.t
  | TargetLVal of lvalue

and lambda_expr = {
  lam_is_act : bool;
  lam_params : fun_param list;
  lam_ret : type_expr option;
  lam_body : stmt list;
}

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
  | ExprThis
  | ExprTuple of collection_item list
  | ExprArray of collection_item list
  | ExprIndex of expr * expr
  | ExprField of expr * string
  | ExprStructInit of string * call_arg list
  | ExprNew of {
      new_name : string;
      new_is_ctor : bool;
      new_args : call_arg list;
    }
  | ExprCall of expr * call_arg list
  | ExprLambda of lambda_expr
  | ExprUnary of unop * expr
  | ExprBinary of binop * expr * expr
  | ExprChainCmp of expr * binop * expr * binop * expr
  | ExprAssign of lvalue * assign_op * expr
  | ExprIncDec of incdec_op * lvalue
  | ExprInput of {
      prompt : expr option;
      mode : input_mode;
    }

and call_arg = {
  arg_name : (string * Span.t) option;
  arg_mode : arg_mode;
  arg_value : expr;
}

and interp_part =
  | InterpLit of string
  | InterpExpr of expr * format_spec option

and expr = {
  desc : expr_desc;
  span : Span.t;
}

and var_decl = {
  name : string;
  name_span : Span.t;
  is_const : bool;
  ty_annot : type_expr option;
  init : expr option;
}

and destruct_decl = {
  names : (string * Span.t) list;
  is_const : bool;
  ty_annot : type_expr option;
  init : expr option;
}

and for_init =
  | ForInitVarDecl of var_decl
  | ForInitDestruct of destruct_decl
  | ForInitExpr of expr

and foreach_binding =
  | ForeachVar of string * Span.t
  | ForeachDestruct of (string * Span.t) list

and foreach_head = {
  binding : foreach_binding;
  is_const : bool;
  ty_annot : type_expr option;
}

and stmt_desc =
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
  | StmtFunDecl of fun_decl
  | StmtTypeDecl of type_decl
  | StmtMultiAssign of {
      ma_targets : multi_target list;
      ma_rhs : expr;
    }
  | StmtReturn of expr option
  | StmtExpr of expr

and stmt = {
  desc : stmt_desc;
  span : Span.t;
}

and user_type_kind = KindClass | KindStruct | KindRecord
and visibility = VisPublic | VisProtected | VisPrivate | VisInternal
and accessor_kind = AccGet | AccSet | AccInit
and accessor_spec = { acc_kind : accessor_kind; acc_vis : visibility option; acc_span : Span.t }
and field_decl = {
  field_vis : visibility option;
  field_is_positional : bool;
  field_name : string;
  field_span : Span.t;
  field_type : type_expr;
  field_accessors : accessor_spec list option;
  field_default : expr option;
}
and method_decl = {
  md_vis : visibility option;
  md_is_static : bool;
  md_fun : fun_decl;
  md_span : Span.t;
}
and type_decl = {
  td_vis : visibility option;
  td_kind : user_type_kind;
  td_name : string;
  td_name_span : Span.t;
  td_fields : field_decl list;
  td_methods : method_decl list;
}

and fun_decl = {
  fd_name : string;
  fd_name_span : Span.t;
  fd_params : fun_param list;
  fd_ret : type_expr option;
  fd_body : stmt list;
}

type program = stmt list

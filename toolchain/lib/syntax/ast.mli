(** Abstract Syntax Tree (AST) da linguagem Sigma [.sgm]. *)

open Sigma_common
include module type of Ast_base

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
  | ExprNew of { new_name : string; new_type_args : type_expr list; new_is_ctor : bool; new_args : call_arg list; new_inits : call_arg list option }
  | ExprTypeApp of expr * type_expr list * Span.t
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
  arg_spread : bool;
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

and import_item = { imp_name : string; imp_name_span : Span.t; imp_alias : (string * Span.t) option }
and import_spec =
  | ImportModule of { mod_path : string; mod_is_path : bool; mod_span : Span.t; mod_alias : (string * Span.t) option }
  | ImportFrom of { mod_path : string; mod_is_path : bool; mod_span : Span.t; mod_items : import_item list }
and extern_item = {
  ext_is_act : bool;
  ext_name : string;
  ext_name_span : Span.t;
  ext_params : fun_param list;
  ext_ret : type_expr option;
  ext_symbol : string option;
  ext_span : Span.t;
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
  | StmtImport of import_spec
  | StmtExtern of {
      ext_lang : string;
      ext_lang_span : Span.t;
      ext_from : string option;
      ext_from_span : Span.t option;
      ext_items : extern_item list;
    }
  | StmtVis of visibility * stmt

and stmt = {
  desc : stmt_desc;
  span : Span.t;
}

and user_type_kind = KindClass | KindStruct | KindRecord
and visibility = VisPublic | VisProtected | VisPrivate | VisInternal
and accessor_kind = AccGet | AccSet | AccInit
and accessor_body = AccAuto | AccExpr of expr | AccBlock of stmt list
and accessor_spec = { acc_kind : accessor_kind; acc_vis : visibility option; acc_body : accessor_body; acc_span : Span.t }
and field_decl = {
  field_vis : visibility option;
  field_is_positional : bool;
  field_name : string;
  field_span : Span.t;
  field_type : type_expr;
  field_accessors : accessor_spec list option;
  field_default : expr option;
}
and ctor_param = { cp_name : string; cp_span : Span.t; cp_type : type_expr; cp_default : expr option }
and ctor_decl = {
  cd_vis : visibility option; cd_name : string; cd_name_span : Span.t;
  cd_params : ctor_param list; cd_chain : call_arg list option; cd_body : stmt list; cd_span : Span.t;
}
and method_decl = { md_vis : visibility option; md_is_static : bool; md_fun : fun_decl; md_span : Span.t }
and type_decl = {
  td_vis : visibility option; td_kind : user_type_kind; td_name : string; td_name_span : Span.t;
  td_type_params : type_param list; td_where : where_clause list;
  td_ctor_params : ctor_param list option; td_fields : field_decl list;
  td_ctors : ctor_decl list; td_methods : method_decl list;
}

and fun_decl = {
  fd_name : string;
  fd_name_span : Span.t;
  fd_type_params : type_param list;
  fd_params : fun_param list;
  fd_ret : type_expr option;
  fd_where : where_clause list;
  fd_body : stmt list;
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

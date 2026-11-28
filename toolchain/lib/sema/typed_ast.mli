(** Árvore Sintática Tipada (Typed AST) após validação semântica. *)

open Sigma_common
open Sigma_syntax

type typed_param = {
  tp_name : string;
  tp_mode : Ty.param_mode;
  tp_ty : Ty.t;
}

type collection_item =
  | TItemExpr of expr
  | TItemSpread of expr

and mask_part =
  | TMaskLit of string
  | TMaskType of Ty.t

and input_mode =
  | TInputLine
  | TInputTypes of Ty.t list
  | TInputMask of mask_part list

and lvalue =
  | TLValIdent of string
  | TLValIndex of expr * expr
  | TLValField of expr * string * int

and multi_target =
  | TTargetDiscard
  | TTargetLVal of lvalue * Ty.t

and typed_call_arg = {
  tca_idx : int;
  tca_expr : expr;
  tca_mode : Ty.param_mode;
  tca_is_out_var : bool;
  tca_is_this : bool;
}

and intrinsic_kind =
  | IntrPrintString
  | IntrPrintInt
  | IntrPrintFloat
  | IntrPrintDouble
  | IntrPrintDecimal
  | IntrPrintBool
  | IntrPrintChar
  | IntrPrintNewline
  | IntrReadLine
  | IntrReadBlock
  | IntrParseInt
  | IntrParseFloat
  | IntrParseDouble
  | IntrParseDecimal
  | IntrParseBool
  | IntrParseChar
  | IntrTryParseInt
  | IntrTryParseFloat
  | IntrTryParseDouble
  | IntrTryParseDecimal
  | IntrTryParseBool
  | IntrTryParseChar

and expr_desc =
  | TExprInt of int64
  | TExprFloat of float
  | TExprDouble of float
  | TExprDecimal of Decimal.t
  | TExprBool of bool
  | TExprChar of string
  | TExprString of string
  | TExprNull
  | TExprInterp of interp_part list
  | TExprIdent of string
  | TExprTuple of collection_item list
  | TExprArray of collection_item list
  | TExprIndex of expr * expr
  | TExprField of expr * string * int
  | TExprStructInit of {
      tsi_name : string;
      tsi_kind : Ty.user_type_kind;
      tsi_args : expr list;
    }
  | TExprNewWithInits of {
      nwi_ctor : expr;
      nwi_inits : (int * expr) list;
    }
  | TExprCall of {
      callee : expr;
      args : typed_call_arg list;  (** argumentos em ordem de avaliação *)
      full : bool;  (** [true] quando todos os parâmetros ficam ligados e a função é executada *)
    }
  | TExprLambda of fun_decl
  | TExprUnary of Ast.unop * expr
  | TExprBinary of Ast.binop * expr * expr
  | TExprChainCmp of expr * Ast.binop * expr * Ast.binop * expr
  | TExprAssign of lvalue * Ast.assign_op * expr
  | TExprIncDec of Ast.incdec_op * lvalue
  | TExprInput of {
      prompt : expr option;
      mode : input_mode;
    }
  | TExprIntrinsic of intrinsic_kind * typed_call_arg list

and interp_part =
  | TInterpLit of string
  | TInterpExpr of expr * Ast.format_spec option

and expr = {
  desc : expr_desc;
  ty : Ty.t;
  span : Span.t;
}

and var_decl = {
  name : string;
  name_span : Span.t;
  is_const : bool;
  ty : Ty.t;
  init : expr;   (** Já resolvido (com zero-initialization explícita quando omitido) *)
  was_default_initialized : bool;
}

and destruct_binding = {
  name : string option;
  name_span : Span.t;
  ty : Ty.t;
}

and destruct_decl = {
  bindings : destruct_binding list;
  is_const : bool;
  ty : Ty.t;
  init : expr;
}

and for_init =
  | TForInitVarDecl of var_decl
  | TForInitDestruct of destruct_decl
  | TForInitExpr of expr

and foreach_head =
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

and stmt_desc =
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
      len : int option;
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
  | TStmtFunDecl of fun_decl
  | TStmtTypeDecl of user_type_decl
  | TStmtMultiAssign of {
      tma_targets : multi_target list;
      tma_rhs : expr;
    }
  | TStmtReturn of expr option
  | TStmtExpr of expr
  | TStmtExtern of extern_decl list

and extern_decl = {
  te_name : string;
  te_name_span : Span.t;
  te_lang : string;
  te_from : string option;
  te_symbol : string;
  te_params : typed_param list;
  te_ret : Ty.t;
}

and user_field_decl = {
  tuf_name : string;
  tuf_ty : Ty.t;
  tuf_default : expr option;
}

and user_type_decl = {
  tud_kind : Ty.user_type_kind;
  tud_name : string;
  tud_span : Span.t;
  tud_fields : user_field_decl list;
  tud_methods : fun_decl list;
}

and stmt = {
  desc : stmt_desc;
  span : Span.t;
}

and fun_decl = {
  tf_name : string;
  tf_name_span : Span.t;
  tf_params : typed_param list;
  tf_ret : Ty.t;  (** [Ty.Void] para [act] *)
  tf_captures : (string * Ty.t) list;  (** Variáveis livres capturadas de escopos léxicos externos *)
  tf_body : stmt list;
}

type program = stmt list

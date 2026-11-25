open Sigma_common
open Sigma_diagnostics
open Sigma_syntax

type const_kind =
  | NotConst
  | DeepConst
  | ShallowConst

type symbol_info = {
  ty : Ty.t;
  decl_span : Span.t;
  is_const : bool;
  const_kind : const_kind;
  is_ref_param : bool;
  const_int : int64 option;
}

type env = (string, symbol_info) Hashtbl.t

type user_field = {
  uf_name : string;
  uf_idx : int;
  uf_ty : Ty.t;
  uf_vis : Ast.visibility;
  uf_get : Ast.visibility option;
  uf_set : Ast.visibility option;
  uf_init : Ast.visibility option;
  uf_default : Typed_ast.expr option;
}

type user_method = {
  um_name : string;
  um_mangled : string;
  um_vis : Ast.visibility;
  um_is_static : bool;
  um_params : Ty.fun_param list;
  um_param_names : string list;
  um_ret : Ty.t;
  um_mutates_this : bool;
}

type user_type_info = {
  ut_kind : Ty.user_type_kind;
  ut_vis : Ast.visibility;
  ut_name : string;
  ut_span : Span.t;
  ut_fields : user_field list;
  ut_methods : user_method list;
}

let current_type_ctx : (string * Ty.user_type_kind * bool) option ref = ref None

let vis_to_string = function
  | Ast.VisPublic -> "public"
  | Ast.VisProtected -> "protected"
  | Ast.VisPrivate -> "private"
  | Ast.VisInternal -> "internal"

let is_vis_accessible = function
  | Ast.VisPublic | Ast.VisInternal -> true
  | Ast.VisProtected | Ast.VisPrivate -> false

let is_vis_accessible_in_type (target_type : string) (vis : Ast.visibility) : bool =
  match vis with
  | Ast.VisPublic | Ast.VisInternal -> true
  | Ast.VisProtected | Ast.VisPrivate ->
    (match !current_type_ctx with
     | Some (cur_t, _, _) when cur_t = target_type -> true
     | _ -> false)

let user_types : (string, user_type_info) Hashtbl.t = Hashtbl.create 16

type fun_ret_ctx =
  | RetNone
  | RetExplicit of Ty.t
  | RetInferred of Ty.t option ref

type active_loop = {
  loop_id : int;
  label : (string * Span.t) option;
}

type hooks = {
  check_expr : env -> Ast.expr -> (Typed_ast.expr, Diagnostic.t) result;
  check_expr_any : env -> Ast.expr -> (Typed_ast.expr, Diagnostic.t) result;
  check_call :
    env ->
    Ast.expr ->
    Typed_ast.expr ->
    Ty.fun_param list ->
    Ty.t ->
    Ast.expr ->
    Ast.call_arg list ->
    (Typed_ast.expr, Diagnostic.t) result;
  check_collection_items :
    env -> Ast.collection_item list -> ((Typed_ast.collection_item * Ty.t list) list, Diagnostic.t) result;
  check_index_access :
    env -> span:Span.t -> Ast.expr -> Ast.expr -> (Typed_ast.expr * Typed_ast.expr * Ty.t, Diagnostic.t) result;
  check_lvalue :
    env ->
    for_op:string ->
    Ast.lvalue ->
    (Typed_ast.lvalue * Ty.t * string, Diagnostic.t) result;
  check_stmt :
    env ->
    string list ref ->
    active_loop list ->
    int ref ->
    Ast.stmt ->
    (Typed_ast.stmt, Diagnostic.t list) result;
  check_program : Ast.program -> (Typed_ast.program, Diagnostic.t list) result;
}

let current_fun : fun_ret_ctx ref = ref RetNone
let fn_outer : (string, unit) Hashtbl.t option ref = ref None
let root_scope_ref : string list ref option ref = ref None
let current_scope_vars : string list ref option ref = ref None
let next_lambda_id : int ref = ref 0

let fun_param_names : (Span.t, string list) Hashtbl.t = Hashtbl.create 16
let var_param_names : (string, string list) Hashtbl.t = Hashtbl.create 16
let expr_param_names : (Span.t, string list) Hashtbl.t = Hashtbl.create 16

let find_conflict (env : env) (name : string) : symbol_info option =
  match Hashtbl.find_opt env name with
  | None -> None
  | Some prev ->
    (match !fn_outer with
     | Some outer when Hashtbl.mem outer name ->
       Hashtbl.remove outer name;
       None
     | _ -> Some prev)

let check_loop_label (loop_stack : active_loop list) (label : (string * Span.t) option)
    : Diagnostic.t option =
  match label with
  | None -> None
  | Some (lbl, lbl_span) ->
    (match
       List.find_opt
         (fun al ->
           match al.label with
           | Some (prev_lbl, _) -> prev_lbl = lbl
           | None -> false)
         loop_stack
     with
     | None -> None
     | Some { label = Some (_, prev_sp); _ } ->
       Some
         (Diagnostic.error
            ~hint:(Printf.sprintf "previously defined on an enclosing loop at %s" (Span.to_string prev_sp))
            ~code:"E0103"
            ~span:lbl_span
            (Printf.sprintf "loop label '%s' is already defined on an enclosing loop" lbl))
     | Some _ -> None)

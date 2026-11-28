open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let vis_rank = function
  | Ast.VisPublic -> 3
  | Ast.VisInternal -> 2
  | Ast.VisProtected -> 1
  | Ast.VisPrivate -> 0

let resolve_field_accessors ~(kind : Ty.user_type_kind) ~(field_name : string) ~(member_vis : Ast.visibility)
    (accs_opt : Ast.accessor_spec list option)
    : (Ast.visibility option * Ast.visibility option * Ast.visibility option, Diagnostic.t) result =
  match accs_opt with
  | None ->
    (match kind with
     | Ty.KindRecord -> Ok (Some member_vis, None, Some member_vis)
     | Ty.KindClass | Ty.KindStruct -> Ok (Some member_vis, Some member_vis, None))
  | Some accs ->
    let g = ref None in
    let s = ref None in
    let i = ref None in
    let rec loop = function
      | [] -> Ok (!g, !s, !i)
      | (a : Ast.accessor_spec) :: rest ->
        let acc_name =
          match a.acc_kind with Ast.AccGet -> "get" | Ast.AccSet -> "set" | Ast.AccInit -> "init"
        in
        (match a.acc_vis with
         | Some v when vis_rank v > vis_rank member_vis ->
           Error
             (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                (Printf.sprintf "accessor '%s' on '%s' cannot be more accessible ('%s') than the member ('%s')"
                   acc_name field_name (vis_to_string v) (vis_to_string member_vis)))
         | _ ->
           let eff_v = match a.acc_vis with Some v -> v | None -> member_vis in
           (match a.acc_kind with
            | Ast.AccGet ->
              if !g <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "duplicate 'get' accessor on property '%s'" field_name))
              else (g := Some eff_v; loop rest)
            | Ast.AccSet ->
              if kind = Ty.KindRecord then
                Error (Diagnostic.error ~code:"E0125" ~span:a.acc_span
                         ~hint:"use 'init;' instead of 'set;' in immutable records"
                         (Printf.sprintf "immutable record property '%s' cannot declare a 'set' accessor" field_name))
              else if !s <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "duplicate 'set' accessor on property '%s'" field_name))
              else if !i <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "property '%s' cannot declare both 'set' and 'init' accessors" field_name))
              else (s := Some eff_v; loop rest)
            | Ast.AccInit ->
              if !i <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "duplicate 'init' accessor on property '%s'" field_name))
              else if !s <> None then
                Error (Diagnostic.error ~code:"E0126" ~span:a.acc_span
                         (Printf.sprintf "property '%s' cannot declare both 'set' and 'init' accessors" field_name))
              else (i := Some eff_v; loop rest)))
    in
    loop accs

let rec expr_mutates_this (e : Typed_ast.expr) : bool =
  match e.desc with
  | Typed_ast.TExprAssign (lv, _, rhs) -> lval_mutates_this lv || expr_mutates_this rhs
  | Typed_ast.TExprIncDec (_, lv) -> lval_mutates_this lv
  | Typed_ast.TExprCall { callee; args; _ } ->
    expr_mutates_this callee || List.exists (fun (a : Typed_ast.typed_call_arg) -> expr_mutates_this a.tca_expr) args
  | Typed_ast.TExprUnary (_, s) | Typed_ast.TExprField (s, _, _) -> expr_mutates_this s
  | Typed_ast.TExprBinary (_, a, b) | Typed_ast.TExprIndex (a, b) -> expr_mutates_this a || expr_mutates_this b
  | Typed_ast.TExprChainCmp (a, _, b, _, c) -> expr_mutates_this a || expr_mutates_this b || expr_mutates_this c
  | Typed_ast.TExprTuple items | Typed_ast.TExprArray items ->
    List.exists (function Typed_ast.TItemExpr x | Typed_ast.TItemSpread x -> expr_mutates_this x) items
  | Typed_ast.TExprStructInit { tsi_args; _ } -> List.exists expr_mutates_this tsi_args
  | Typed_ast.TExprNewWithInits { nwi_ctor; nwi_inits } ->
    expr_mutates_this nwi_ctor || List.exists (fun (_, e) -> expr_mutates_this e) nwi_inits
  | _ -> false

and lval_mutates_this = function
  | Typed_ast.TLValField (base, _, _) ->
    (match base.desc with Typed_ast.TExprIdent "this" -> true | _ -> lval_mutates_this_expr base)
  | Typed_ast.TLValIndex (base, _) -> lval_mutates_this_expr base
  | Typed_ast.TLValIdent _ -> false

and lval_mutates_this_expr (e : Typed_ast.expr) : bool =
  match e.desc with
  | Typed_ast.TExprIdent "this" -> true
  | Typed_ast.TExprField (s, _, _) | Typed_ast.TExprIndex (s, _) -> lval_mutates_this_expr s
  | _ -> false

let rec stmt_mutates_this (s : Typed_ast.stmt) : bool =
  match s.desc with
  | Typed_ast.TStmtVarDecl v -> expr_mutates_this v.init
  | Typed_ast.TStmtDestruct d -> expr_mutates_this d.init
  | Typed_ast.TStmtBlock ss -> List.exists stmt_mutates_this ss
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    expr_mutates_this cond || stmt_mutates_this then_branch
    || (match else_branch with Some b -> stmt_mutates_this b | None -> false)
  | Typed_ast.TStmtWhile { cond; body; _ } -> expr_mutates_this cond || stmt_mutates_this body
  | Typed_ast.TStmtFor { cond; step; body; _ } ->
    expr_mutates_this cond || expr_mutates_this step || stmt_mutates_this body
  | Typed_ast.TStmtForeach { collection; body; _ } -> expr_mutates_this collection || stmt_mutates_this body
  | Typed_ast.TStmtReturn (Some e) | Typed_ast.TStmtExpr e -> expr_mutates_this e
  | Typed_ast.TStmtMultiAssign { tma_targets; tma_rhs } ->
    expr_mutates_this tma_rhs
    || List.exists
         (function Typed_ast.TTargetLVal (lv, _) -> lval_mutates_this lv | Typed_ast.TTargetDiscard -> false)
         tma_targets
  | _ -> false

let rec value_type_reaches (visited : (string, unit) Hashtbl.t) (target : string) (ty : Ty.t) : bool =
  match ty with
  | Ty.Tuple sub -> List.exists (value_type_reaches visited target) sub
  | Ty.Array (sub, _) | Ty.DynArray sub -> value_type_reaches visited target sub
  | Ty.User (Ty.KindClass, _) -> false
  | Ty.User ((Ty.KindStruct | Ty.KindRecord), name) ->
    if name = target then true
    else if Hashtbl.mem visited name then false
    else begin
      Hashtbl.replace visited name ();
      match Hashtbl.find_opt user_types name with
      | None -> false
      | Some info -> List.exists (fun f -> value_type_reaches visited target f.uf_ty) info.ut_fields
    end
  | _ -> false

let err_missing_new ~span (info : user_type_info) : ('a, Diagnostic.t) result =
  let usage =
    if info.ut_kind = Ty.KindRecord then Printf.sprintf "new %s(...)" info.ut_name
    else Printf.sprintf "new %s { ... }" info.ut_name
  in
  Error
    (Diagnostic.error ~hint:(Printf.sprintf "use '%s' to create an instance" usage) ~code:"E0122" ~span
       (Printf.sprintf "missing 'new' keyword when instantiating '%s'" info.ut_name))

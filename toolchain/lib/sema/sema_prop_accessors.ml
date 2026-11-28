open Sigma_common
open Sigma_syntax

let rec expr_uses_ident (target : string) (e : Ast.expr) : bool =
  match e.desc with
  | Ast.ExprIdent id -> id = target
  | Ast.ExprInterp parts ->
    List.exists (function Ast.InterpLit _ -> false | Ast.InterpExpr (x, _) -> expr_uses_ident target x) parts
  | Ast.ExprTuple items | Ast.ExprArray items ->
    List.exists (function Ast.ItemExpr x | Ast.ItemSpread (x, _) -> expr_uses_ident target x) items
  | Ast.ExprIndex (a, b) | Ast.ExprBinary (_, a, b) -> expr_uses_ident target a || expr_uses_ident target b
  | Ast.ExprField (a, _) | Ast.ExprUnary (_, a) | Ast.ExprTypeApp (a, _, _) -> expr_uses_ident target a
  | Ast.ExprStructInit (_, args) -> List.exists (fun (a : Ast.call_arg) -> expr_uses_ident target a.arg_value) args
  | Ast.ExprNew { new_args; new_inits; _ } ->
    List.exists (fun (a : Ast.call_arg) -> expr_uses_ident target a.arg_value) new_args
    || (match new_inits with Some l -> List.exists (fun (a : Ast.call_arg) -> expr_uses_ident target a.arg_value) l | None -> false)
  | Ast.ExprCall (c, args) ->
    expr_uses_ident target c || List.exists (fun (a : Ast.call_arg) -> expr_uses_ident target a.arg_value) args
  | Ast.ExprChainCmp (a, _, b, _, c) ->
    expr_uses_ident target a || expr_uses_ident target b || expr_uses_ident target c
  | Ast.ExprAssign (lv, _, rhs) -> lval_uses_ident target lv || expr_uses_ident target rhs
  | Ast.ExprIncDec (_, lv) -> lval_uses_ident target lv
  | Ast.ExprLambda _ | Ast.ExprInt _ | Ast.ExprFloat _ | Ast.ExprDouble _ | Ast.ExprDecimal _
  | Ast.ExprBool _ | Ast.ExprChar _ | Ast.ExprString _ | Ast.ExprThis | Ast.ExprInput _ -> false

and lval_uses_ident target = function
  | Ast.LValIdent (id, _) -> id = target
  | Ast.LValIndex (a, b, _) -> expr_uses_ident target a || expr_uses_ident target b
  | Ast.LValField (a, _, _) -> expr_uses_ident target a

let rec stmt_uses_ident (target : string) (s : Ast.stmt) : bool =
  match s.desc with
  | Ast.StmtVarDecl vd ->
    if vd.name = target then false
    else Option.fold ~none:false ~some:(expr_uses_ident target) vd.init
  | Ast.StmtDestruct dd -> Option.fold ~none:false ~some:(expr_uses_ident target) dd.init
  | Ast.StmtBlock ss -> List.exists (stmt_uses_ident target) ss
  | Ast.StmtIf { cond; then_branch; else_branch } ->
    expr_uses_ident target cond || stmt_uses_ident target then_branch
    || Option.fold ~none:false ~some:(stmt_uses_ident target) else_branch
  | Ast.StmtWhile { cond; body; _ } -> expr_uses_ident target cond || stmt_uses_ident target body
  | Ast.StmtFor { cond; step; body; _ } ->
    expr_uses_ident target cond || expr_uses_ident target step || stmt_uses_ident target body
  | Ast.StmtForeach { collection; body; _ } ->
    expr_uses_ident target collection || stmt_uses_ident target body
  | Ast.StmtPrint { args; _ } -> List.exists (expr_uses_ident target) args
  | Ast.StmtMultiAssign { ma_targets; ma_rhs } ->
    expr_uses_ident target ma_rhs
    || List.exists (function Ast.TargetDiscard _ -> false | Ast.TargetLVal lv -> lval_uses_ident target lv) ma_targets
  | Ast.StmtReturn eopt -> Option.fold ~none:false ~some:(expr_uses_ident target) eopt
  | Ast.StmtExpr e -> expr_uses_ident target e
  | _ -> false

let prop_has_slot (accs_opt : Ast.accessor_spec list option) : bool =
  match accs_opt with
  | None -> true
  | Some accs ->
    List.exists
      (fun (a : Ast.accessor_spec) ->
        match a.acc_body with
        | Ast.AccAuto -> true
        | Ast.AccExpr e -> expr_uses_ident "field" e
        | Ast.AccBlock ss -> List.exists (stmt_uses_ident "field") ss)
      accs

let rec rewrite_setter_returns (sp : Span.t) (s : Ast.stmt) : Ast.stmt =
  let desc =
    match s.desc with
    | Ast.StmtReturn None ->
      Ast.StmtReturn (Some { Ast.desc = Ast.ExprIdent "value"; span = sp })
    | Ast.StmtBlock ss -> Ast.StmtBlock (List.map (rewrite_setter_returns sp) ss)
    | Ast.StmtIf { cond; then_branch; else_branch } ->
      Ast.StmtIf
        { cond;
          then_branch = rewrite_setter_returns sp then_branch;
          else_branch = Option.map (rewrite_setter_returns sp) else_branch }
    | Ast.StmtWhile w -> Ast.StmtWhile { w with body = rewrite_setter_returns sp w.body }
    | Ast.StmtFor f -> Ast.StmtFor { f with body = rewrite_setter_returns sp f.body }
    | Ast.StmtForeach fe -> Ast.StmtForeach { fe with body = rewrite_setter_returns sp fe.body }
    | other -> other
  in
  { s with desc }

let prop_get_method_name prop_name = "__sgm_prop_get_" ^ prop_name
let prop_set_method_name prop_name = "__sgm_prop_set_" ^ prop_name
let prop_init_method_name prop_name = "__sgm_prop_init_" ^ prop_name

let build_prop_methods (td : Ast.type_decl) : Ast.method_decl list =
  let acc_methods = ref [] in
  List.iter
    (fun (f : Ast.field_decl) ->
      match f.field_accessors with
      | None -> ()
      | Some accs ->
        List.iter
          (fun (a : Ast.accessor_spec) ->
            match a.acc_body with
            | Ast.AccAuto -> ()
            | Ast.AccExpr e ->
              let mname, params, body =
                match a.acc_kind with
                | Ast.AccGet ->
                  (prop_get_method_name f.field_name, [],
                   [ { Ast.desc = Ast.StmtReturn (Some e); span = a.acc_span } ])
                | Ast.AccSet | Ast.AccInit ->
                  let mn = if a.acc_kind = Ast.AccSet then prop_set_method_name f.field_name else prop_init_method_name f.field_name in
                  let vp = { Ast.param_name = "value"; param_span = a.acc_span; param_mode = Ast.ParamVal; param_type = f.field_type; param_variadic = false } in
                  let stmts =
                    [ { Ast.desc = Ast.StmtExpr e; span = a.acc_span };
                      { Ast.desc = Ast.StmtReturn (Some { Ast.desc = Ast.ExprIdent "value"; span = a.acc_span }); span = a.acc_span } ]
                  in
                  (mn, [ vp ], stmts)
              in
              let fd : Ast.fun_decl =
                { fd_name = mname; fd_name_span = a.acc_span; fd_type_params = [];
                  fd_params = params; fd_ret = Some f.field_type; fd_where = []; fd_body = body }
              in
              acc_methods := { Ast.md_vis = Some Ast.VisPublic; md_is_static = false; md_fun = fd; md_span = a.acc_span } :: !acc_methods
            | Ast.AccBlock ss ->
              let mname, params, body =
                match a.acc_kind with
                | Ast.AccGet -> (prop_get_method_name f.field_name, [], ss)
                | Ast.AccSet | Ast.AccInit ->
                  let mn = if a.acc_kind = Ast.AccSet then prop_set_method_name f.field_name else prop_init_method_name f.field_name in
                  let vp = { Ast.param_name = "value"; param_span = a.acc_span; param_mode = Ast.ParamVal; param_type = f.field_type; param_variadic = false } in
                  let ss' = List.map (rewrite_setter_returns a.acc_span) ss in
                  let ret_val = { Ast.desc = Ast.StmtReturn (Some { Ast.desc = Ast.ExprIdent "value"; span = a.acc_span }); span = a.acc_span } in
                  (mn, [ vp ], ss' @ [ ret_val ])
              in
              let fd : Ast.fun_decl =
                { fd_name = mname; fd_name_span = a.acc_span; fd_type_params = [];
                  fd_params = params; fd_ret = Some f.field_type; fd_where = []; fd_body = body }
              in
              acc_methods := { Ast.md_vis = Some Ast.VisPublic; md_is_static = false; md_fun = fd; md_span = a.acc_span } :: !acc_methods)
          accs)
    td.td_fields;
  List.rev !acc_methods

let prop_name_of_method_name (mname : string) : string option =
  if String.starts_with ~prefix:"__sgm_prop_get_" mname then
    Some (String.sub mname 15 (String.length mname - 15))
  else if String.starts_with ~prefix:"__sgm_prop_set_" mname then
    Some (String.sub mname 15 (String.length mname - 15))
  else if String.starts_with ~prefix:"__sgm_prop_init_" mname then
    Some (String.sub mname 16 (String.length mname - 16))
  else None

let custom_accessor_names ~(type_name : string) (prop_name : string)
    (accs_opt : Ast.accessor_spec list option) : string option * string option * string option =
  match accs_opt with
  | None -> (None, None, None)
  | Some accs ->
    let find_custom kind mname =
      if List.exists (fun (a : Ast.accessor_spec) -> a.acc_kind = kind && a.acc_body <> Ast.AccAuto) accs then
        Some (Printf.sprintf "__sgm_m_%s_%s" type_name mname)
      else None
    in
    (find_custom Ast.AccGet (prop_get_method_name prop_name),
     find_custom Ast.AccSet (prop_set_method_name prop_name),
     find_custom Ast.AccInit (prop_init_method_name prop_name))

let make_prop_call (sp : Span.t) (fn_name : string) (fn_ty : Ty.t) (ret_ty : Ty.t)
    (this_expr : Typed_ast.expr) (extra_args : Typed_ast.expr list) : Typed_ast.expr =
  let callee = { Typed_ast.desc = Typed_ast.TExprIdent fn_name; ty = fn_ty; span = sp } in
  let this_arg =
    { Typed_ast.tca_idx = 0; tca_mode = Ty.ParamVal; tca_is_out_var = false;
      tca_is_this = true; tca_expr = this_expr }
  in
  let rest =
    List.mapi
      (fun i e ->
        { Typed_ast.tca_idx = i + 1; tca_mode = Ty.ParamVal; tca_is_out_var = false;
          tca_is_this = false; tca_expr = e })
      extra_args
  in
  { Typed_ast.desc = Typed_ast.TExprCall { callee; args = this_arg :: rest; full = true };
    ty = ret_ty; span = sp }

let find_custom_prop_field (tbase : Typed_ast.expr) (field : string) : (Sema_env.user_type_info * Sema_env.user_field) option =
  match tbase.ty with
  | Ty.User (_, tname) ->
    let is_self_backing =
      match !Sema_env.in_prop_accessor, tbase.desc with
      | Some (at, ap, _, _), Typed_ast.TExprIdent "this" -> at = tname && ap = field
      | _ -> false
    in
    if is_self_backing then None
    else
      (match Hashtbl.find_opt Sema_env.user_types tname with
       | Some info ->
         (match List.find_opt (fun f -> f.Sema_env.uf_name = field) info.ut_fields with
          | Some f when f.uf_custom_get <> None || f.uf_custom_set <> None -> Some (info, f)
          | _ -> None)
       | None -> None)
  | _ -> None

let make_prop_read (sp : Span.t) (tbase : Typed_ast.expr) (f : Sema_env.user_field) : Typed_ast.expr =
  match f.uf_custom_get with
  | Some get_m ->
    let fn_ty = Ty.Fun ([ { Ty.pty = tbase.ty; pmode = Ty.ParamVal } ], f.uf_ty) in
    make_prop_call sp get_m fn_ty f.uf_ty tbase []
  | None ->
    { Typed_ast.desc = Typed_ast.TExprField (tbase, f.uf_name, f.uf_idx); ty = f.uf_ty; span = sp }

let make_prop_write (sp : Span.t) (tbase : Typed_ast.expr) (f : Sema_env.user_field) (rhs : Typed_ast.expr) : Typed_ast.expr =
  match f.uf_custom_set with
  | Some set_m ->
    let fn_ty = Ty.Fun ([ { Ty.pty = tbase.ty; pmode = Ty.ParamVal }; { Ty.pty = f.uf_ty; pmode = Ty.ParamVal } ], f.uf_ty) in
    make_prop_call sp set_m fn_ty f.uf_ty tbase [ rhs ]
  | None ->
    { Typed_ast.desc = Typed_ast.TExprAssign (Typed_ast.TLValField (tbase, f.uf_name, f.uf_idx), Ast.AssignEq, rhs);
      ty = f.uf_ty; span = sp }

let make_user_field ~type_name (f : Ast.field_decl) ~member_vis ~fty ~uf_get ~uf_set ~uf_init ~uf_has_slot ~idx ~tdef : Sema_env.user_field =
  let uf_custom_get, uf_custom_set, uf_custom_init = custom_accessor_names ~type_name f.field_name f.field_accessors in
  { Sema_env.uf_name = f.field_name; uf_idx = idx; uf_ty = fty; uf_vis = member_vis;
    uf_get; uf_set; uf_init; uf_custom_get; uf_custom_set; uf_custom_init; uf_has_slot; uf_default = tdef }

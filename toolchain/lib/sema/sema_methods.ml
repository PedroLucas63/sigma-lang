open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_types

let check_method_sigs ~(type_name : string) ~(seen_members : (string, unit) Hashtbl.t)
    (methods : Ast.method_decl list) : (user_method list, Diagnostic.t list) result =
  let errs = ref [] in
  let umethods = ref [] in
  List.iter
    (fun (m : Ast.method_decl) ->
      let fname = m.md_fun.fd_name in
      if Hashtbl.mem seen_members fname then
        errs :=
          Diagnostic.error ~code:"E0103" ~span:m.md_fun.fd_name_span
            (Printf.sprintf "duplicate member '%s' in '%s'" fname type_name)
          :: !errs;
      Hashtbl.replace seen_members fname ();
      match resolve_fun_signature m.md_fun with
      | Error es -> errs := List.rev_append es !errs
      | Ok (params, ret) ->
        let vis = match m.md_vis with Some v -> v | None -> Ast.VisPrivate in
        let um_params =
          List.map (fun (tp : Typed_ast.typed_param) -> { Ty.pty = tp.tp_ty; pmode = tp.tp_mode }) params
        in
        let um_param_names = List.map (fun (tp : Typed_ast.typed_param) -> tp.tp_name) params in
        let um =
          {
            um_name = fname;
            um_mangled = Printf.sprintf "__sgm_m_%s_%s" type_name fname;
            um_vis = vis;
            um_is_static = m.md_is_static;
            um_params;
            um_param_names;
            um_ret = ret;
            um_mutates_this = false;
          }
        in
        umethods := um :: !umethods)
    methods;
  match List.rev !errs with
  | [] -> Ok (List.rev !umethods)
  | es -> Error es

let check_method_bodies hooks (env : env) (next_loop_id : int ref) ~(type_name : string)
    ~(kind : Ty.user_type_kind) (methods : Ast.method_decl list) (umethods : user_method list)
    : (user_method list * Typed_ast.fun_decl list, Diagnostic.t list) result =
  let errs = ref [] in
  let typed_funs = ref [] in
  let updated_ums = ref [] in
  List.iter2
    (fun (m : Ast.method_decl) (um : user_method) ->
      match resolve_fun_signature m.md_fun with
      | Error es -> errs := List.rev_append es !errs
      | Ok (params, ret) ->
        let body_env : env = Hashtbl.copy env in
        let outer : (string, unit) Hashtbl.t = Hashtbl.create 16 in
        Hashtbl.iter (fun k _ -> Hashtbl.replace outer k ()) env;
        let saved_fun = !current_fun in
        let saved_outer = !fn_outer in
        let saved_type = !current_type_ctx in
        current_fun := RetExplicit ret;
        fn_outer := Some outer;
        current_type_ctx := Some (type_name, kind, m.md_is_static);
        let self_ty = Ty.User (kind, type_name) in
        let all_params =
          if m.md_is_static then params
          else begin
            Hashtbl.remove outer "this";
            Hashtbl.replace body_env "this"
              {
                ty = self_ty;
                decl_span = m.md_span;
                is_const = true;
                const_kind = (if kind = Ty.KindRecord then DeepConst else NotConst);
                is_ref_param = false;
                const_int = None;
              };
            { Typed_ast.tp_name = "this"; tp_ty = self_ty; tp_mode = Ty.ParamVal } :: params
          end
        in
        List.iter2
          (fun (p : Ast.fun_param) (tp : Typed_ast.typed_param) ->
            Hashtbl.remove outer tp.tp_name;
            let is_const = tp.tp_mode = Ty.ParamVal in
            let const_kind = if is_const then ShallowConst else NotConst in
            Hashtbl.replace body_env tp.tp_name
              { ty = tp.tp_ty; decl_span = p.param_span; is_const; const_kind;
                is_ref_param = tp.tp_mode <> Ty.ParamVal; const_int = None })
          m.md_fun.fd_params params;
        let body_scope = ref (List.map (fun (tp : Typed_ast.typed_param) -> tp.tp_name) all_params) in
        let body_errs = ref [] in
        let tstmts = ref [] in
        List.iter
          (fun s ->
            match hooks.check_stmt body_env body_scope [] next_loop_id s with
            | Ok ts -> tstmts := ts :: !tstmts
            | Error es -> body_errs := List.rev_append es !body_errs)
          m.md_fun.fd_body;
        current_fun := saved_fun;
        fn_outer := saved_outer;
        current_type_ctx := saved_type;
        let tbody = List.rev !tstmts in
        let ret_errs =
          if !body_errs = [] && ret <> Ty.Void && not (List.exists stmt_always_returns tbody) then
            [ Diagnostic.error ~code:"E0110" ~span:m.md_fun.fd_name_span
                (Printf.sprintf "not all code paths return a value in method '%s'" um.um_name) ]
          else []
        in
        let out_errs =
          if !body_errs = [] then Sema_out_check.check_fun m.md_fun.fd_name m.md_fun.fd_name_span params tbody else []
        in
        let all_errs = List.rev !body_errs @ ret_errs @ out_errs in
        if all_errs <> [] then errs := List.rev_append all_errs !errs
        else begin
          let mutates = (not m.md_is_static) && List.exists Sema_accessors.stmt_mutates_this tbody in
          updated_ums := { um with um_mutates_this = mutates } :: !updated_ums;
          typed_funs :=
            {
              Typed_ast.tf_name = um.um_mangled;
              tf_name_span = m.md_fun.fd_name_span;
              tf_params = all_params;
              tf_ret = ret;
              tf_captures = [];
              tf_body = tbody;
            }
            :: !typed_funs
        end)
    methods umethods;
  match List.rev !errs with
  | [] -> Ok (List.rev !updated_ums, List.rev !typed_funs)
  | es -> Error es

let check_static_method_value ~span (info : user_type_info) (member : string)
    : (Typed_ast.expr, Diagnostic.t) result =
  match List.find_opt (fun m -> m.um_name = member) info.ut_methods with
  | None ->
    Error
      (Diagnostic.error ~code:"E0127" ~span
         (Printf.sprintf "type '%s' has no static method named '%s'" info.ut_name member))
  | Some m when not m.um_is_static ->
    Error
      (Diagnostic.error ~code:"E0127" ~span
         (Printf.sprintf "cannot access instance method '%s' on type '%s' without an instance" member info.ut_name))
  | Some m when not (is_vis_accessible_in_type info.ut_name m.um_vis) ->
    Error
      (Diagnostic.error ~code:"E0124" ~span
         (Printf.sprintf "cannot access %s static method '%s' of '%s'" (vis_to_string m.um_vis) member info.ut_name))
  | Some m ->
    Hashtbl.replace expr_param_names span m.um_param_names;
    Ok { Typed_ast.desc = Typed_ast.TExprIdent m.um_mangled; ty = Ty.Fun (m.um_params, m.um_ret); span }

let check_instance_method_value ~span (tbase : Typed_ast.expr) (info : user_type_info) (m : user_method)
    (const_err : Diagnostic.t option) : (Typed_ast.expr, Diagnostic.t) result =
  if m.um_is_static then
    Error
      (Diagnostic.error ~code:"E0127" ~span
         (Printf.sprintf "cannot access static method '%s' on an instance of '%s'; use '%s.%s'"
            m.um_name info.ut_name info.ut_name m.um_name))
  else if not (is_vis_accessible_in_type info.ut_name m.um_vis) then
    Error
      (Diagnostic.error ~code:"E0124" ~span
         (Printf.sprintf "cannot access %s method '%s' of '%s'" (vis_to_string m.um_vis) m.um_name info.ut_name))
  else
    match const_err with
    | Some err -> Error err
    | None ->
      (match tbase.desc with
       | Typed_ast.TExprIdent vname ->
         let pnames = m.um_param_names in
         let params =
           List.map2
             (fun nm (p : Ty.fun_param) -> { Typed_ast.tp_name = nm; tp_ty = p.pty; tp_mode = p.pmode })
             pnames m.um_params
         in
         let this_arg =
           { Typed_ast.tca_idx = 0; tca_mode = Ty.ParamVal; tca_is_out_var = false; tca_is_this = true;
             tca_expr = { Typed_ast.desc = Typed_ast.TExprIdent vname; ty = tbase.ty; span } }
         in
         let fwd_args =
           List.mapi
             (fun i (tp : Typed_ast.typed_param) ->
               { Typed_ast.tca_idx = i + 1; tca_mode = tp.tp_mode; tca_is_out_var = false; tca_is_this = false;
                 tca_expr = { Typed_ast.desc = Typed_ast.TExprIdent tp.tp_name; ty = tp.tp_ty; span } })
             params
         in
         let callee_fn =
           { Typed_ast.desc = Typed_ast.TExprIdent m.um_mangled;
             ty = Ty.Fun ({ Ty.pty = tbase.ty; pmode = Ty.ParamVal } :: m.um_params, m.um_ret); span }
         in
         let call_e =
           { Typed_ast.desc = Typed_ast.TExprCall { callee = callee_fn; args = this_arg :: fwd_args;
                                                    full = true };
             ty = m.um_ret; span }
         in
         let body_stmt =
           if m.um_ret = Ty.Void then { Typed_ast.desc = Typed_ast.TStmtExpr call_e; span }
           else { Typed_ast.desc = Typed_ast.TStmtReturn (Some call_e); span }
         in
         let lam_id = !next_lambda_id in
         incr next_lambda_id;
         let fd : Typed_ast.fun_decl =
           { tf_name = Printf.sprintf "__bound_%d_%s" lam_id m.um_name; tf_name_span = span;
             tf_params = params; tf_ret = m.um_ret; tf_captures = [ (vname, tbase.ty) ]; tf_body = [ body_stmt ] }
         in
         Hashtbl.replace expr_param_names span pnames;
         Ok { Typed_ast.desc = Typed_ast.TExprLambda fd; ty = Ty.Fun (m.um_params, m.um_ret); span }
       | _ ->
         Error
           (Diagnostic.error ~code:"E0127" ~span
              "binding a method as a first-class value requires a named variable receiver (e.g. 'var f := obj.method;')"))

let rec root_var_const_kind (env : env) (e : Typed_ast.expr) : const_kind option =
  match e.desc with
  | Typed_ast.TExprIdent name -> Option.map (fun info -> info.const_kind) (Hashtbl.find_opt env name)
  | Typed_ast.TExprField (sub, _, _) | Typed_ast.TExprIndex (sub, _) -> root_var_const_kind env sub
  | _ -> None

let try_check_method_call hooks (env : env) ~span ~call_expr ~(callee : Ast.expr)
    (base : Ast.expr) (mname : string) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result option =
  let is_static_type =
    match base.desc with
    | Ast.ExprIdent tname when Hashtbl.mem module_aliases tname -> true
    | Ast.ExprIdent tname when not (Hashtbl.mem env tname) -> Hashtbl.mem user_types tname
    | _ -> false
  in
  if is_static_type then None
  else
    match hooks.check_expr env base with
    | Error e -> Some (Error e)
    | Ok tbase ->
      (match tbase.ty with
       | Ty.User (kind, tname) ->
         (match Hashtbl.find_opt user_types tname with
          | None -> None
          | Some info ->
            if List.exists (fun f -> f.uf_name = mname) info.ut_fields then None
            else
              match List.find_opt (fun m -> m.um_name = mname) info.ut_methods with
              | None ->
                Some
                  (Error
                     (Diagnostic.error ~code:"E0119" ~span
                        (Printf.sprintf "type '%s' has no field or method named '%s'" tname mname)))
              | Some m when m.um_is_static ->
                Some
                  (Error
                     (Diagnostic.error ~code:"E0127" ~span
                        (Printf.sprintf "cannot call static method '%s' on an instance of '%s'; use '%s.%s'"
                           mname tname tname mname)))
              | Some m when not (is_vis_accessible_in_type tname m.um_vis) ->
                Some
                  (Error
                     (Diagnostic.error ~code:"E0124" ~span
                        (Printf.sprintf "cannot call %s method '%s' of '%s'" (vis_to_string m.um_vis) mname tname)))
              | Some m ->
                let const_err =
                  if kind = Ty.KindStruct && m.um_mutates_this then
                    match root_var_const_kind env tbase with
                    | Some (DeepConst | ShallowConst) ->
                      Some
                        (Diagnostic.error ~code:"E0107" ~span
                           (Printf.sprintf "cannot call mutating method '%s' on immutable struct '%s'" mname tname))
                    | _ -> None
                  else None
                in
                (match const_err with
                 | Some err -> Some (Error err)
                 | None ->
                   let synth_callee =
                     { Typed_ast.desc = Typed_ast.TExprIdent m.um_mangled;
                       ty = Ty.Fun (m.um_params, m.um_ret); span = callee.span }
                   in
                   Hashtbl.replace expr_param_names callee.span m.um_param_names;
                   Some
                     (match hooks.check_call env call_expr synth_callee m.um_params m.um_ret callee args with
                      | Error e -> Error e
                      | Ok ({ Typed_ast.desc = Typed_ast.TExprCall { callee = c; args = targs; full };
                              ty; span = csp } : Typed_ast.expr) ->
                        let this_arg =
                          { Typed_ast.tca_idx = 0; tca_mode = Ty.ParamVal;
                            tca_is_out_var = false; tca_is_this = true; tca_expr = tbase }
                        in
                        let shifted = List.map (fun a -> { a with Typed_ast.tca_idx = a.Typed_ast.tca_idx + 1 }) targs in
                        Ok { Typed_ast.desc = Typed_ast.TExprCall { callee = c; args = this_arg :: shifted; full };
                             ty; span = csp }
                      | Ok other -> Ok other)))
       | _ -> None)

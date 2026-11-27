open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_types
open Sema_captures

let check_return_stmt hooks (env : env) (s_span : Span.t) (eo : Ast.expr option)
    : (Typed_ast.stmt, Diagnostic.t list) result =
  let ret_err msg hint span = Error [ Diagnostic.error ~hint ~code:"E0111" ~span msg ] in
  match !current_fun with
  | RetNone ->
    ret_err "'return' used outside of a function"
      "'return' can only appear inside the body of a 'fun' or 'act'" s_span
  | RetExplicit ret_ty ->
    (match eo with
     | None ->
       if !in_ctor then
         (match !current_type_ctx with
          | Some (tname, kind, false) ->
            let this_e =
              { Typed_ast.desc = Typed_ast.TExprIdent "this"; ty = Ty.User (kind, tname); span = s_span }
            in
            Ok { Typed_ast.desc = Typed_ast.TStmtReturn (Some this_e); span = s_span }
          | _ -> Ok { Typed_ast.desc = Typed_ast.TStmtReturn None; span = s_span })
       else if ret_ty = Ty.Void then Ok { Typed_ast.desc = Typed_ast.TStmtReturn None; span = s_span }
       else
         ret_err
           (Printf.sprintf "'return' without a value in a function returning '%s'" (Ty.to_string ret_ty))
           "provide a value, e.g. 'return <expr>;'" s_span
     | Some e ->
       if ret_ty = Ty.Void then
         ret_err "an 'act' cannot return a value" "use 'return;' or declare a 'fun' with a return type" e.span
       else
         (match hooks.check_expr env e with
          | Error err -> Error [ err ]
          | Ok te ->
            if Ty.equal te.ty ret_ty then
              Ok { Typed_ast.desc = Typed_ast.TStmtReturn (Some te); span = s_span }
            else
              ret_err
                (Printf.sprintf "type mismatch in 'return': expected '%s', found '%s'"
                   (Ty.to_string ret_ty) (Ty.to_string te.ty))
                "the returned value must have exactly the declared return type" te.span))
  | RetInferred r_ref ->
    (match eo with
     | None ->
       ret_err "'return' without a value in an anonymous 'fun'"
         "provide a value, e.g. 'return <expr>;', or use 'act(...) => ...' for a void procedure" s_span
     | Some e ->
       (match hooks.check_expr env e with
        | Error err -> Error [ err ]
        | Ok te ->
          (match !r_ref with
           | None ->
             r_ref := Some te.ty;
             Ok { Typed_ast.desc = Typed_ast.TStmtReturn (Some te); span = s_span }
           | Some prev_ty ->
             if Ty.equal te.ty prev_ty then
               Ok { Typed_ast.desc = Typed_ast.TStmtReturn (Some te); span = s_span }
             else
               ret_err
                 (Printf.sprintf "type mismatch in 'return': expected '%s', found '%s'"
                    (Ty.to_string prev_ty) (Ty.to_string te.ty))
                 "all 'return' branches in an anonymous 'fun' must return the exact same type" te.span)))

let check_fun_decl hooks (env : env) (scope_vars : string list ref) (next_loop_id : int ref)
    (s_span : Span.t) (fd : Ast.fun_decl) : (Typed_ast.stmt, Diagnostic.t list) result =
  if fd.fd_type_params <> [] then begin
    match Sema_generics.check_duplicate_type_params fd.fd_type_params with
    | Error e -> Error [ e ]
    | Ok () ->
      Hashtbl.replace Sema_generics.generic_funs fd.fd_name fd;
      Ok { Typed_ast.desc = Typed_ast.TStmtBlock []; span = s_span }
  end else
  let conflict_errs =
    match find_conflict env fd.fd_name with
    | None -> []
    | Some prev ->
      [
        Diagnostic.error
          ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
          ~code:"E0103" ~span:fd.fd_name_span
          (Printf.sprintf "'%s' is already declared in this scope" fd.fd_name);
      ]
  in
  match resolve_fun_signature fd with
  | Error es -> Error (conflict_errs @ es)
  | Ok (params, ret) ->
    if conflict_errs <> [] then Error conflict_errs
    else begin
      let fty =
        Ty.Fun
          (List.map (fun (tp : Typed_ast.typed_param) -> { Ty.pty = tp.tp_ty; pmode = tp.tp_mode }) params,
           ret)
      in
      Hashtbl.replace env fd.fd_name
        {
          ty = fty;
          decl_span = fd.fd_name_span;
          is_const = true;
          const_kind = DeepConst;
          is_ref_param = false;
          const_int = None;
        };
      scope_vars := fd.fd_name :: !scope_vars;
      Hashtbl.replace fun_param_names fd.fd_name_span
        (List.map (fun (tp : Typed_ast.typed_param) -> tp.tp_name) params);
      let body_env : env = Hashtbl.copy env in
      let outer : (string, unit) Hashtbl.t = Hashtbl.create 16 in
      Hashtbl.iter (fun k _ -> Hashtbl.replace outer k ()) env;
      let saved_fun = !current_fun in
      let saved_outer = !fn_outer in
      current_fun := RetExplicit ret;
      fn_outer := Some outer;
      List.iter2
        (fun (p : Ast.fun_param) (tp : Typed_ast.typed_param) ->
          Hashtbl.remove outer tp.tp_name;
          let is_const = tp.tp_mode = Ty.ParamVal in
          let const_kind = if is_const then ShallowConst else NotConst in
          let is_ref_param = tp.tp_mode = Ty.ParamRef || tp.tp_mode = Ty.ParamOut in
          Hashtbl.replace body_env tp.tp_name
            { ty = tp.tp_ty; decl_span = p.param_span; is_const; const_kind; is_ref_param; const_int = None })
        fd.fd_params params;
      let body_scope = ref [] in
      let tbody = ref [] in
      let berrs = ref [] in
      List.iter
        (fun sub ->
          match hooks.check_stmt body_env body_scope [] next_loop_id sub with
          | Ok ts -> tbody := ts :: !tbody
          | Error es -> berrs := List.rev_append es !berrs)
        fd.fd_body;
      current_fun := saved_fun;
      fn_outer := saved_outer;
      match List.rev !berrs with
      | _ :: _ as errors -> Error errors
      | [] ->
        let tbody = List.rev !tbody in
        let out_errs = Sema_out_check.check_fun fd.fd_name fd.fd_name_span params tbody in
        if out_errs <> [] then Error out_errs
        else if ret <> Ty.Void && not (List.exists stmt_always_returns tbody) then
          Error
            [
              Diagnostic.error
                ~hint:"add a 'return <expr>;' on every path that reaches the end of the function"
                ~code:"E0111" ~span:fd.fd_name_span
                (Printf.sprintf "function '%s' may reach its end without returning a value" fd.fd_name);
            ]
        else
          match compute_captures env params fd.fd_name tbody with
          | Error es -> Error es
          | Ok captures ->
            Ok
              {
                Typed_ast.desc =
                  Typed_ast.TStmtFunDecl
                    {
                      Typed_ast.tf_name = fd.fd_name;
                      tf_name_span = fd.fd_name_span;
                      tf_params = params;
                      tf_ret = ret;
                      tf_captures = captures;
                      tf_body = tbody;
                    };
                span = s_span;
              }
    end

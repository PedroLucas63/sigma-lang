open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_generics
open Sema_generics_inst

let split_variadic_targs (tps : Ast.type_param list) (tys : Ty.t list) : Ty.t list * Ty.t list =
  let has_var = List.exists (fun (tp : Ast.type_param) -> tp.tp_variadic) tps in
  if has_var then
    let non_var_n = List.length (List.filter (fun (tp : Ast.type_param) -> not tp.tp_variadic) tps) in
    let var_tys = if List.length tys > non_var_n then List.filteri (fun i _ -> i >= non_var_n) tys else [] in
    let fixed = List.filteri (fun i _ -> i < non_var_n) tys in
    (fixed, var_tys)
  else (tys, [])

let infer_arg_types hooks env (args : Ast.call_arg list) : (Ty.t list, Diagnostic.t) result =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (a : Ast.call_arg) :: rest ->
      (match a.arg_mode with
       | Ast.ArgOutVar { var_type = Some te; _ } ->
         (match Sema_types.resolve_type_expr te None with
          | Error e -> Error e
          | Ok ty -> loop (ty :: acc) rest)
       | Ast.ArgOutVar { var_name; var_span; var_type = None } ->
         Error
           (Diagnostic.error ~code:"E0141" ~span:var_span
              (Printf.sprintf "cannot infer type for 'out var %s'; specify 'out var %s: <type>' or explicit type arguments '<...>'"
                 var_name var_name))
       | Ast.ArgVal | Ast.ArgRef | Ast.ArgOut ->
         (match hooks.check_expr env a.arg_value with
          | Error e -> Error e
          | Ok te -> loop (te.ty :: acc) rest))
  in
  loop [] args

let check_generic_fun_call hooks (env : env) (call_expr : Ast.expr) (name : string)
    (explicit_targs : Ast.type_expr list option) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result =
  match Hashtbl.find_opt generic_funs name with
  | None ->
    Error (Diagnostic.error ~code:"E0102" ~span:call_expr.span (Printf.sprintf "generic function '%s' not found" name))
  | Some fd ->
    let targs_res =
      match explicit_targs with
      | Some tes ->
        Result.map (split_variadic_targs fd.fd_type_params) (resolve_type_args_no_wildcard tes)
      | None ->
        (match infer_arg_types hooks env args with
         | Error e -> Error e
         | Ok arg_tys -> infer_call_type_args call_expr.span fd.fd_type_params fd.fd_params arg_tys)
    in
    match targs_res with
    | Error e -> Error e
    | Ok (targs, var_tys) ->
      (match instantiate_fun hooks env call_expr.span name targs var_tys with
       | Error e -> Error e
       | Ok (mangled, Ty.Fun (ptys, ret)) ->
         let callee_e : Ast.expr = { Ast.desc = Ast.ExprIdent mangled; span = call_expr.span } in
         let tcallee : Typed_ast.expr = { Typed_ast.desc = Typed_ast.TExprIdent mangled; ty = Ty.Fun (ptys, ret); span = call_expr.span } in
         hooks.check_call env call_expr tcallee ptys ret callee_e args
       | Ok (_, other) ->
         Error (Diagnostic.error ~code:"E0112" ~span:call_expr.span (Printf.sprintf "cannot call '%s'" (Ty.to_string other))))

let check_generic_method_call hooks (env : env) (call_expr : Ast.expr) (base : Ast.expr)
    (mname : string) (explicit_targs : Ast.type_expr list option) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result option =
  let owner_opt =
    match base.desc with
    | Ast.ExprIdent id when Hashtbl.mem user_types id -> Some id
    | _ ->
      (match hooks.check_expr env base with
       | Ok tbase -> (match tbase.ty with Ty.User (_, owner) -> Some owner | _ -> None)
       | Error _ -> None)
  in
  match owner_opt with
  | None -> None
  | Some owner ->
    (match Hashtbl.find_opt generic_methods (owner, mname) with
     | None -> None
     | Some md ->
       let targs_res =
         match explicit_targs with
         | Some tes ->
           Result.map (split_variadic_targs md.md_fun.fd_type_params) (resolve_type_args_no_wildcard tes)
         | None ->
           (match infer_arg_types hooks env args with
            | Error e -> Error e
            | Ok arg_tys -> infer_call_type_args call_expr.span md.md_fun.fd_type_params md.md_fun.fd_params arg_tys)
       in
       let res =
         match targs_res with
         | Error e -> Error e
         | Ok (targs, var_tys) ->
           (match instantiate_method hooks env call_expr.span owner mname targs var_tys with
            | Error e -> Error e
            | Ok mangled_m ->
              let callee' : Ast.expr = { Ast.desc = Ast.ExprField (base, mangled_m); span = call_expr.span } in
              (match Sema_methods.try_check_method_call hooks env ~span:call_expr.span ~call_expr ~callee:callee' base mangled_m args with
               | Some r -> r
               | None ->
                 (match hooks.check_expr env callee' with
                  | Error e -> Error e
                  | Ok tcallee ->
                    (match tcallee.ty with
                     | Ty.Fun (ptys, ret) -> hooks.check_call env call_expr tcallee ptys ret callee' args
                     | other ->
                       Error (Diagnostic.error ~code:"E0112" ~span:call_expr.span
                                (Printf.sprintf "cannot call a value of type '%s'" (Ty.to_string other)))))))
       in
       Some res)

let check_generic_new hooks (env : env) ~(span : Span.t) ~(is_ctor : bool)
    (name : string) (targs : Ast.type_expr list) (args : Ast.call_arg list) (inits : Ast.call_arg list option)
    : (Typed_ast.expr, Diagnostic.t) result =
  match Hashtbl.find_opt generic_types name with
  | None -> Sema_user_types.check_new_expr hooks env ~span ~is_ctor name args inits
  | Some td ->
    let targs_res =
      if targs <> [] then resolve_type_args_no_wildcard targs
      else infer_new_type_args hooks env span td args
    in
    match targs_res with
    | Error e -> Error e
    | Ok resolved_targs ->
      (match instantiate_type hooks env span name resolved_targs with
       | Error e -> Error e
       | Ok (Ty.User (_, mangled)) ->
         Sema_user_types.check_new_expr hooks env ~span ~is_ctor mangled args inits
       | Ok _ ->
         Error (Diagnostic.error ~code:"E0141" ~span "failed to instantiate generic type"))

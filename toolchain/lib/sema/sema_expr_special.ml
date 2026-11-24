open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_types
open Sema_captures

let check_input_expr hooks (env : env) (e_span : Span.t) (prompt : Ast.expr option) mode
    : (Typed_ast.expr, Diagnostic.t) result =
  let check_prompt p_expr =
    match hooks.check_expr env p_expr with
    | Error err -> Error err
    | Ok tp ->
      if tp.ty = Ty.String then Ok (Some tp)
      else
        Error
          (Diagnostic.error
             ~hint:"the first argument to 'prompt(...)' must be a 'string' expression"
             ~code:"E0104" ~span:tp.span
             (Printf.sprintf "prompt message must be of type 'string', found '%s'" (Ty.to_string tp.ty)))
  in
  let prompt_res =
    match prompt with
    | None -> Ok None
    | Some ({ Ast.desc = Ast.ExprIdent name; span = p_span } as p_expr) ->
      if Hashtbl.find_opt env name = None && Ty.of_string name <> None then
        Error
          (Diagnostic.error
             ~hint:(Printf.sprintf "use 'input(%s)' to read without a prompt, or 'prompt(\"message: \", %s)' to display a prompt first" name name)
             ~code:"E0104" ~span:p_span
             (Printf.sprintf "first argument of 'prompt(...)' must be a prompt message of type 'string', found type name '%s'" name))
      else check_prompt p_expr
    | Some p_expr -> check_prompt p_expr
  in
  match prompt_res with
  | Error err -> Error err
  | Ok tprompt ->
    let mode_res =
      match mode with
      | Ast.InputLine -> Ok (Typed_ast.TInputLine, Ty.String)
      | Ast.InputTypes ty_list ->
        let rec resolve_all acc = function
          | [] ->
            let tys = List.rev acc in
            let res_ty = match tys with [ single ] -> single | _ -> Ty.Tuple tys in
            Ok (Typed_ast.TInputTypes tys, res_ty)
          | (ty_name, sp) :: rest ->
            (match resolve_input_type ty_name sp with
             | Error err -> Error err
             | Ok ty -> resolve_all (ty :: acc) rest)
        in
        resolve_all [] ty_list
      | Ast.InputMask parts ->
        let rec check_mask acc_parts acc_tys prev_was_type = function
          | [] ->
            let tys = List.rev acc_tys in
            let res_ty = match tys with [ single ] -> single | _ -> Ty.Tuple tys in
            Ok (Typed_ast.TInputMask (List.rev acc_parts), res_ty)
          | Ast.MaskLit s :: rest ->
            check_mask (Typed_ast.TMaskLit s :: acc_parts) acc_tys false rest
          | Ast.MaskType (ty_name, sp) :: rest ->
            if prev_was_type then
              Error
                (Diagnostic.error
                   ~hint:"place a delimiter between placeholders, e.g. '\"${int} ${int}\"' or '\"${int}-${int}\"'"
                   ~code:"E0002" ~span:sp
                   "adjacent type placeholders in input mask require a literal separator")
            else
              (match resolve_input_type ty_name sp with
               | Error err -> Error err
               | Ok ty -> check_mask (Typed_ast.TMaskType ty :: acc_parts) (ty :: acc_tys) true rest)
        in
        check_mask [] [] false parts
    in
    (match mode_res with
     | Error err -> Error err
     | Ok (tmode, res_ty) ->
       Ok { Typed_ast.desc = Typed_ast.TExprInput { prompt = tprompt; mode = tmode }; ty = res_ty; span = e_span })

let check_lambda_expr hooks (env : env) (e_span : Span.t) (lam : Ast.lambda_expr)
    : (Typed_ast.expr, Diagnostic.t) result =
  match resolve_fun_params lam.lam_params with
  | Error (err :: _) -> Error err
  | Error [] -> Error (Diagnostic.error ~code:"E0103" ~span:e_span "invalid lambda parameters")
  | Ok params ->
    let ret_ctx_res =
      if lam.lam_is_act then
        match lam.lam_ret with
        | None -> Ok (RetExplicit Ty.Void)
        | Some te ->
          Error
            (Diagnostic.error
               ~hint:"use 'fun(...) : <type> => ...' to declare an anonymous function that returns a value"
               ~code:"E0113" ~span:(Ast.type_expr_span te)
               "an anonymous 'act' cannot declare a return type")
      else
        match lam.lam_ret with
        | None -> Ok (RetInferred (ref None))
        | Some (Ast.TypeNamed ("void", sp)) ->
          Error
            (Diagnostic.error
               ~hint:"use 'act(...) => ...' to declare an anonymous procedure without a return value"
               ~code:"E0113" ~span:sp
               "a 'fun' must declare a value return type, not 'void'")
        | Some te ->
          (match resolve_type_expr te None with
           | Ok ty -> Ok (RetExplicit ty)
           | Error err -> Error err)
    in
    match ret_ctx_res with
    | Error err -> Error err
    | Ok ret_ctx ->
      incr next_lambda_id;
      let lam_name = Printf.sprintf "__lambda_%d" !next_lambda_id in
      let body_env : env = Hashtbl.copy env in
      let outer : (string, unit) Hashtbl.t = Hashtbl.create 16 in
      Hashtbl.iter (fun k _ -> Hashtbl.replace outer k ()) env;
      let saved_fun = !current_fun in
      let saved_outer = !fn_outer in
      current_fun := ret_ctx;
      fn_outer := Some outer;
      List.iter2
        (fun (p : Ast.fun_param) (tp : Typed_ast.typed_param) ->
          Hashtbl.remove outer tp.tp_name;
          let is_const = tp.tp_mode = Ty.ParamVal in
          let const_kind = if is_const then ShallowConst else NotConst in
          let is_ref_param = tp.tp_mode = Ty.ParamRef || tp.tp_mode = Ty.ParamOut in
          Hashtbl.replace body_env tp.tp_name
            { ty = tp.tp_ty; decl_span = p.param_span; is_const; const_kind; is_ref_param; const_int = None })
        lam.lam_params params;
      let body_scope = ref [] in
      let lam_loop_id = ref 0 in
      let tbody = ref [] in
      let berrs = ref [] in
      List.iter
        (fun sub ->
          match hooks.check_stmt body_env body_scope [] lam_loop_id sub with
          | Ok ts -> tbody := ts :: !tbody
          | Error es -> berrs := List.rev_append es !berrs)
        lam.lam_body;
      current_fun := saved_fun;
      fn_outer := saved_outer;
      (match List.rev !berrs with
       | err :: _ -> Error err
       | [] ->
         let tbody = List.rev !tbody in
         let final_ret_res =
           match ret_ctx with
           | RetExplicit r -> Ok r
           | RetInferred r_ref ->
             (match !r_ref with
              | Some r -> Ok r
              | None ->
                Error
                  (Diagnostic.error
                     ~hint:"return a value with '=> <expr>' or 'return <expr>;', or use 'act(...) => ...' for a void procedure"
                     ~code:"E0111" ~span:e_span "anonymous 'fun' must return a value"))
           | RetNone -> Ok Ty.Void
         in
         match final_ret_res with
         | Error err -> Error err
         | Ok ret ->
           (match Sema_out_check.check_fun "anonymous function" e_span params tbody with
            | err :: _ -> Error err
            | [] ->
              if ret <> Ty.Void && not (List.exists stmt_always_returns tbody) then
                Error
                  (Diagnostic.error
                     ~hint:"add a 'return <expr>;' on every path that reaches the end of the anonymous function"
                     ~code:"E0111" ~span:e_span
                     "anonymous 'fun' may reach its end without returning a value")
              else
                match compute_captures env params lam_name tbody with
                | Error (err :: _) -> Error err
                | Error [] -> Error (Diagnostic.error ~code:"E0118" ~span:e_span "invalid closure capture")
                | Ok captures ->
                  Hashtbl.replace expr_param_names e_span
                    (List.map (fun (tp : Typed_ast.typed_param) -> tp.tp_name) params);
                  let fty =
                    Ty.Fun
                      (List.map (fun (tp : Typed_ast.typed_param) -> { Ty.pty = tp.tp_ty; pmode = tp.tp_mode }) params,
                       ret)
                  in
                  Ok
                    {
                      Typed_ast.desc =
                        Typed_ast.TExprLambda
                          {
                            Typed_ast.tf_name = lam_name;
                            tf_name_span = e_span;
                            tf_params = params;
                            tf_ret = ret;
                            tf_captures = captures;
                            tf_body = tbody;
                          };
                      ty = fty;
                      span = e_span;
                    }))

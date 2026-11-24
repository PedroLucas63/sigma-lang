open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_types

let check_arg_mode (expected_pmode : Ty.param_mode) (arg_mode : Ast.arg_mode) param_name span =
  match expected_pmode, arg_mode with
  | Ty.ParamVal, Ast.ArgVal -> Ok ()
  | Ty.ParamVal, Ast.ArgRef ->
    Error (Diagnostic.error ~code:"E0116" ~span
             (Printf.sprintf "parameter '%s' is not marked 'ref'; remove 'ref'" param_name))
  | Ty.ParamVal, (Ast.ArgOut | Ast.ArgOutVar _) ->
    Error (Diagnostic.error ~code:"E0116" ~span
             (Printf.sprintf "parameter '%s' is not marked 'out'; remove 'out'" param_name))
  | Ty.ParamRef, Ast.ArgRef -> Ok ()
  | Ty.ParamRef, Ast.ArgVal ->
    Error (Diagnostic.error ~code:"E0116" ~span
             (Printf.sprintf "parameter '%s' is marked 'ref'; argument must be passed with 'ref'" param_name))
  | Ty.ParamRef, (Ast.ArgOut | Ast.ArgOutVar _) ->
    Error (Diagnostic.error ~code:"E0116" ~span
             (Printf.sprintf "parameter '%s' is marked 'ref', but argument was passed with 'out'" param_name))
  | Ty.ParamOut, (Ast.ArgOut | Ast.ArgOutVar _) -> Ok ()
  | Ty.ParamOut, Ast.ArgVal ->
    Error (Diagnostic.error ~code:"E0116" ~span
             (Printf.sprintf "parameter '%s' is marked 'out'; argument must be passed with 'out'" param_name))
  | Ty.ParamOut, Ast.ArgRef ->
    Error (Diagnostic.error ~code:"E0116" ~span
             (Printf.sprintf "parameter '%s' is marked 'out', but argument was passed with 'ref'" param_name))

let check_call hooks (env : env) (e : Ast.expr) (tcallee : Typed_ast.expr)
    (ptys : Ty.fun_param list) (ret : Ty.t) (callee : Ast.expr) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result =
  let n = List.length ptys in
  let ptys_a = Array.of_list ptys in
  let names_opt =
    match callee.desc with
    | Ast.ExprIdent name ->
      (match Hashtbl.find_opt var_param_names name with
       | Some pnames -> Some pnames
       | None ->
         (match Hashtbl.find_opt env name with
          | Some info -> Hashtbl.find_opt fun_param_names info.decl_span
          | None -> None))
    | _ -> Hashtbl.find_opt expr_param_names callee.span
  in
  let used = Array.make (max n 1) false in
  let next_pos = ref 0 in
  let seen_named = ref false in
  let call_err ~hint span msg = Error (Diagnostic.error ~hint ~code:"E0110" ~span msg) in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (a : Ast.call_arg) :: rest ->
      let idx_res =
        match a.arg_name with
        | None ->
          if !seen_named then
            call_err ~hint:"pass positional arguments first, then named ones, e.g. 'f(1, b=2)'"
              a.arg_value.span "positional argument after a named argument"
          else begin
            let i = !next_pos in
            incr next_pos;
            if i >= n then
              call_err ~hint:(Printf.sprintf "the callee accepts at most %d more argument(s)" n)
                a.arg_value.span
                (Printf.sprintf "too many arguments: expected at most %d, found more" n)
            else Ok i
          end
        | Some (nm, nsp) ->
          seen_named := true;
          (match names_opt with
           | None ->
             call_err
               ~hint:"named arguments are only available when calling a declared 'fun'/'act' directly by its name"
               nsp (Printf.sprintf "cannot use named argument '%s' here" nm)
           | Some names ->
             let rec find_idx i = function
               | [] -> None
               | x :: tl -> if x = nm then Some i else find_idx (i + 1) tl
             in
             (match find_idx 0 names with
              | None ->
                call_err ~hint:(Printf.sprintf "parameters are: %s" (String.concat ", " names))
                  nsp (Printf.sprintf "unknown parameter name '%s'" nm)
              | Some i -> Ok i))
      in
      (match idx_res with
       | Error err -> Error err
       | Ok i ->
         if used.(i) then
           call_err ~hint:"each parameter can be bound only once" a.arg_value.span
             (Printf.sprintf "parameter #%d is bound more than once" (i + 1))
         else begin
           used.(i) <- true;
           let expected_param = ptys_a.(i) in
           let param_name =
             match names_opt with
             | Some names when i < List.length names -> List.nth names i
             | _ -> Printf.sprintf "#%d" (i + 1)
           in
           match check_arg_mode expected_param.pmode a.arg_mode param_name a.arg_value.span with
           | Error err -> Error err
           | Ok () ->
             (match a.arg_mode with
              | Ast.ArgOutVar { var_name; var_span; var_type } ->
                let conflict =
                  match find_conflict env var_name with
                  | Some prev -> Some prev
                  | None ->
                    (match !current_scope_vars with
                     | Some sc when List.mem var_name !sc -> Hashtbl.find_opt env var_name
                     | _ -> None)
                in
                (match conflict with
                 | Some prev ->
                   Error
                     (Diagnostic.error
                        ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
                        ~code:"E0103" ~span:var_span
                        (Printf.sprintf "'%s' is already declared in this scope" var_name))
                 | None ->
                   let type_check =
                     match var_type with
                     | None -> Ok ()
                     | Some te ->
                       (match resolve_type_expr te None with
                        | Error err -> Error err
                        | Ok annot_ty ->
                          if Ty.equal annot_ty expected_param.pty then Ok ()
                          else
                            Error
                              (Diagnostic.error
                                 ~hint:"explicit type annotation on 'out var' must match the parameter type"
                                 ~code:"E0104" ~span:(Ast.type_expr_span te)
                                 (Printf.sprintf "type mismatch for 'out var %s': parameter expects '%s', found '%s'"
                                    var_name (Ty.to_string expected_param.pty) (Ty.to_string annot_ty))))
                   in
                   (match type_check with
                    | Error err -> Error err
                    | Ok () ->
                      Hashtbl.replace env var_name
                        {
                          ty = expected_param.pty;
                          decl_span = var_span;
                          is_const = false;
                          const_kind = NotConst;
                          is_ref_param = false;
                          const_int = None;
                        };
                      (match !current_scope_vars with
                       | Some sc -> sc := var_name :: !sc
                       | None -> ());
                      let targ =
                        { Typed_ast.desc = Typed_ast.TExprIdent var_name; ty = expected_param.pty; span = var_span }
                      in
                      let tca =
                        {
                          Typed_ast.tca_idx = i;
                          tca_expr = targ;
                          tca_mode = expected_param.pmode;
                          tca_is_out_var = true;
                        }
                      in
                      loop (tca :: acc) rest))
              | Ast.ArgRef | Ast.ArgOut ->
                (match a.arg_value.desc with
                 | Ast.ExprIdent vname ->
                   (match Hashtbl.find_opt env vname with
                    | None ->
                      Error (Diagnostic.error ~code:"E0101" ~span:a.arg_value.span
                               (Printf.sprintf "unknown variable '%s'" vname))
                    | Some info when info.is_const ->
                      Error (Diagnostic.error ~code:"E0115" ~span:a.arg_value.span
                               (Printf.sprintf "cannot pass constant '%s' as '%s' argument" vname
                                  (if a.arg_mode = Ast.ArgRef then "ref" else "out")))
                    | Some info ->
                      if not (Ty.equal info.ty expected_param.pty) then
                        Error
                          (Diagnostic.error
                             ~hint:"Sigma is strongly typed: arguments must have exactly the parameter type"
                             ~code:"E0104" ~span:a.arg_value.span
                             (Printf.sprintf "type mismatch in argument #%d: expected '%s', found '%s'"
                                (i + 1) (Ty.to_string expected_param.pty) (Ty.to_string info.ty)))
                      else
                        let targ =
                          { Typed_ast.desc = Typed_ast.TExprIdent vname; ty = info.ty; span = a.arg_value.span }
                        in
                        let tca =
                          {
                            Typed_ast.tca_idx = i;
                            tca_expr = targ;
                            tca_mode = expected_param.pmode;
                            tca_is_out_var = false;
                          }
                        in
                        loop (tca :: acc) rest)
                 | _ ->
                   Error (Diagnostic.error ~code:"E0115" ~span:a.arg_value.span
                            (Printf.sprintf "argument of '%s' must be a variable"
                               (if a.arg_mode = Ast.ArgRef then "ref" else "out"))))
              | Ast.ArgVal ->
                (match hooks.check_expr env a.arg_value with
                 | Error err -> Error err
                 | Ok targ ->
                   if Ty.equal targ.ty expected_param.pty then
                     let tca =
                       {
                         Typed_ast.tca_idx = i;
                         tca_expr = targ;
                         tca_mode = expected_param.pmode;
                         tca_is_out_var = false;
                       }
                     in
                     loop (tca :: acc) rest
                   else
                     Error
                       (Diagnostic.error
                          ~hint:"Sigma is strongly typed: arguments must have exactly the parameter type"
                          ~code:"E0104" ~span:targ.span
                          (Printf.sprintf "type mismatch in argument #%d: expected '%s', found '%s'"
                             (i + 1) (Ty.to_string expected_param.pty) (Ty.to_string targ.ty)))))
         end)
  in
  match loop [] args with
  | Error err -> Error err
  | Ok targs ->
    let remaining = List.filteri (fun i _ -> not used.(i)) ptys in
    let remaining_names =
      match names_opt with
      | Some names when List.length names = n -> Some (List.filteri (fun i _ -> not used.(i)) names)
      | _ -> None
    in
    Option.iter (Hashtbl.replace expr_param_names e.span) remaining_names;
    let full = remaining = [] in
    let ty = if full then ret else Ty.Fun (remaining, ret) in
    Ok { Typed_ast.desc = Typed_ast.TExprCall { callee = tcallee; args = targs; full }; ty; span = e.span }

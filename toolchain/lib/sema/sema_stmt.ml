open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let check_stmt_inner hooks (env : env) (scope_vars : string list ref)
    (loop_stack : active_loop list) (next_loop_id : int ref) (s : Ast.stmt)
    : (Typed_ast.stmt, Diagnostic.t list) result =
  match s.desc with
  | Ast.StmtExpr e ->
    (match hooks.check_expr_any env e with
     | Ok te -> Ok { Typed_ast.desc = Typed_ast.TStmtExpr te; span = s.span }
     | Error err -> Error [ err ])
  | Ast.StmtReturn eo -> Sema_stmt_fun.check_return_stmt hooks env s.span eo
  | Ast.StmtFunDecl fd -> Sema_stmt_fun.check_fun_decl hooks env scope_vars next_loop_id s.span fd
  | Ast.StmtTypeDecl td ->
    let is_top_level =
      match !root_scope_ref with
      | Some r -> r == scope_vars
      | None -> false
    in
    Sema_user_types.check_type_decl hooks env ~is_top_level s.span td
  | Ast.StmtMultiAssign { ma_targets; ma_rhs } ->
    Sema_multi_assign.check_multi_assign hooks env s.span ma_targets ma_rhs
  | Ast.StmtBlock stmts ->
    let block_vars = ref [] in
    let tstmts = ref [] in
    let errs = ref [] in
    List.iter
      (fun sub ->
        match hooks.check_stmt env block_vars loop_stack next_loop_id sub with
        | Ok ts -> tstmts := ts :: !tstmts
        | Error sub_errs -> errs := List.rev_append sub_errs !errs)
      stmts;
    List.iter (Hashtbl.remove env) !block_vars;
    (match List.rev !errs with
     | [] -> Ok { Typed_ast.desc = Typed_ast.TStmtBlock (List.rev !tstmts); span = s.span }
     | errors -> Error errors)
  | Ast.StmtIf { cond; then_branch; else_branch } ->
    let errs = ref [] in
    let tcond_opt =
      match hooks.check_expr env cond with
      | Error err -> errs := err :: !errs; None
      | Ok tc ->
        if tc.ty <> Ty.Bool then begin
          errs :=
            Diagnostic.error
              ~hint:"the condition of an 'if' statement must evaluate to 'bool' (e.g. 'if (x == 10)')"
              ~code:"E0104" ~span:tc.span
              (Printf.sprintf "type mismatch in 'if' condition: expected 'bool', found '%s'"
                 (Ty.to_string tc.ty))
            :: !errs;
          None
        end else Some tc
    in
    let tthen_opt =
      match hooks.check_stmt env scope_vars loop_stack next_loop_id then_branch with
      | Ok ts -> Some ts
      | Error sub_errs -> errs := List.rev_append sub_errs !errs; None
    in
    let telse_opt =
      match else_branch with
      | None -> Some None
      | Some eb ->
        (match hooks.check_stmt env scope_vars loop_stack next_loop_id eb with
         | Ok teb -> Some (Some teb)
         | Error sub_errs -> errs := List.rev_append sub_errs !errs; None)
    in
    (match List.rev !errs, tcond_opt, tthen_opt, telse_opt with
     | [], Some tc, Some tthen, Some telse ->
       Ok { Typed_ast.desc = Typed_ast.TStmtIf { cond = tc; then_branch = tthen; else_branch = telse }; span = s.span }
     | errors, _, _, _ -> Error errors)
  | Ast.StmtWhile { label; cond; body } ->
    Sema_stmt_loops.check_while_stmt hooks env loop_stack next_loop_id s.span label cond body
  | Ast.StmtFor { label; init; cond; step; body } ->
    Sema_stmt_loops.check_for_stmt hooks env loop_stack next_loop_id s.span label init cond step body
  | Ast.StmtForeach { label; head; collection; body } ->
    Sema_stmt_loops.check_foreach_stmt hooks env loop_stack next_loop_id s.span label head collection body
  | Ast.StmtBreak label_opt -> Sema_stmt_loops.check_loop_jump ~is_break:true loop_stack s.span label_opt
  | Ast.StmtContinue label_opt -> Sema_stmt_loops.check_loop_jump ~is_break:false loop_stack s.span label_opt
  | Ast.StmtPrint { newline; args } ->
    let targs = ref [] in
    let errs = ref [] in
    List.iter
      (fun arg ->
        match hooks.check_expr env arg with
        | Ok targ -> targs := targ :: !targs
        | Error err -> errs := err :: !errs)
      args;
    (match List.rev !errs with
     | [] -> Ok { Typed_ast.desc = Typed_ast.TStmtPrint { newline; args = List.rev !targs }; span = s.span }
     | errors -> Error errors)
  | Ast.StmtAssert { actual; expected; message } ->
    let errs = ref [] in
    let tactual_opt =
      match hooks.check_expr env actual with
      | Ok ta -> Some ta
      | Error e -> errs := e :: !errs; None
    in
    let texpected_opt =
      match expected with
      | None -> None
      | Some exp_e ->
        (match hooks.check_expr env exp_e with
         | Ok te -> Some te
         | Error e -> errs := e :: !errs; None)
    in
    let tmsg_opt =
      match message with
      | None -> None
      | Some msg_e ->
        (match hooks.check_expr env msg_e with
         | Error e -> errs := e :: !errs; None
         | Ok tm ->
           if tm.ty = Ty.String then Some tm
           else begin
             errs :=
               Diagnostic.error
                 ~hint:"the optional 3rd argument of 'assert' is a diagnostic description string"
                 ~code:"E0104" ~span:tm.span
                 (Printf.sprintf "type mismatch in 'assert' message: expected 'string', found '%s'"
                    (Ty.to_string tm.ty))
               :: !errs;
             None
           end)
    in
    (match List.rev !errs, tactual_opt, expected, texpected_opt with
     | _ :: _ as errors, _, _, _ -> Error errors
     | [], Some ta, None, _ ->
       if ta.ty = Ty.Bool then
         let ttrue = { Typed_ast.desc = Typed_ast.TExprBool true; ty = Ty.Bool; span = ta.span } in
         Ok { Typed_ast.desc = Typed_ast.TStmtAssert { actual = ta; expected = ttrue; message = tmsg_opt }; span = s.span }
       else
         Error
           [
             Diagnostic.error
               ~hint:"pass an expected value as second argument, e.g. 'assert(actual, expected);'"
               ~code:"E0104" ~span:ta.span
               (Printf.sprintf "single-argument 'assert' expects a 'bool' condition, found '%s'"
                  (Ty.to_string ta.ty));
           ]
     | [], Some ta, Some _, Some te ->
       if Ty.equal ta.ty te.ty then
         Ok { Typed_ast.desc = Typed_ast.TStmtAssert { actual = ta; expected = te; message = tmsg_opt }; span = s.span }
       else
         Error
           [
             Diagnostic.error
               ~hint:"both arguments in 'assert(actual, expected)' must have the exact same type"
               ~code:"E0104" ~span:s.span
               (Printf.sprintf "type mismatch in 'assert': actual value has type '%s', expected value has type '%s'"
                  (Ty.to_string ta.ty) (Ty.to_string te.ty));
           ]
     | _ -> Error [])
  | Ast.StmtAssertError { expected_code; snippet; message; _ } ->
    let errs = ref [] in
    let tmsg_opt =
      match message with
      | None -> None
      | Some msg_e ->
        (match hooks.check_expr env msg_e with
         | Error e -> errs := e :: !errs; None
         | Ok tm ->
           if tm.ty = Ty.String then Some tm
           else begin
             errs :=
               Diagnostic.error
                 ~hint:"the optional 3rd argument of 'assert_error' is a diagnostic description string"
                 ~code:"E0104" ~span:tm.span
                 (Printf.sprintf "type mismatch in 'assert_error' message: expected 'string', found '%s'"
                    (Ty.to_string tm.ty))
               :: !errs;
             None
           end)
    in
    (match List.rev !errs with
     | _ :: _ as errors -> Error errors
     | [] ->
       let snippet_src = Source.of_string ~filename:"<assert_error>" ~content:snippet in
       let diags =
         match Lexer.tokenize snippet_src with
         | Error lex_errs -> lex_errs
         | Ok tokens ->
           (match Parser.parse tokens with
            | Error parse_errs -> parse_errs
            | Ok ast ->
              (match hooks.check_program ast with
               | Error sema_errs -> sema_errs
               | Ok _ -> []))
       in
       let actual_codes = List.map (fun (d : Diagnostic.t) -> d.code) diags in
       let passed = List.exists (fun c -> c = expected_code) actual_codes in
       Ok
         {
           Typed_ast.desc = Typed_ast.TStmtAssertError { expected_code; actual_codes; passed; message = tmsg_opt };
           span = s.span;
         })
  | Ast.StmtDestruct d -> Sema_stmt_decl.check_destruct_decl hooks env scope_vars s.span d
  | Ast.StmtVarDecl v -> Sema_stmt_decl.check_var_decl hooks env scope_vars s.span v

let check_stmt hooks (env : env) (scope_vars : string list ref)
    (loop_stack : active_loop list) (next_loop_id : int ref) (s : Ast.stmt)
    : (Typed_ast.stmt, Diagnostic.t list) result =
  let saved_scope = !current_scope_vars in
  current_scope_vars := Some scope_vars;
  let res = check_stmt_inner hooks env scope_vars loop_stack next_loop_id s in
  current_scope_vars := saved_scope;
  res

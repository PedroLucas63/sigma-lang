open Sigma_common
open Sigma_diagnostics
open Parser_state

let parse_if_stmt hooks st if_tok ~stmt_parser : (Ast.stmt, Diagnostic.t) result =
  let open_tok = cur st in
  match open_tok.kind with
  | Token.LPAREN ->
    bump st;
    (match hooks.parse_expr st with
     | Error e -> Error e
     | Ok cond ->
       let close_tok = cur st in
       (match close_tok.kind with
        | Token.RPAREN ->
          bump st;
          (match stmt_parser st with
           | Error e -> Error e
           | Ok then_branch ->
             if (cur st).kind = Token.KW_ELSE then begin
               bump st;
               match stmt_parser st with
               | Error e -> Error e
               | Ok else_stmt ->
                 Ok
                   {
                     Ast.desc = Ast.StmtIf { cond; then_branch; else_branch = Some else_stmt };
                     span = Span.merge if_tok.Token.span else_stmt.span;
                   }
             end else
               Ok
                 {
                   Ast.desc = Ast.StmtIf { cond; then_branch; else_branch = None };
                   span = Span.merge if_tok.Token.span then_branch.span;
                 })
        | _ ->
          err_at close_tok
            (Printf.sprintf "expected ')' after 'if' condition, found %s"
               (Token.kind_to_string close_tok.kind))))
  | _ ->
    err_at open_tok ~hint:"wrap the condition in parentheses, e.g. 'if (x == 10) ...'"
      (Printf.sprintf "expected '(' after 'if', found %s" (Token.kind_to_string open_tok.kind))

let parse_while_stmt hooks st start_span ~label ~stmt_parser : (Ast.stmt, Diagnostic.t) result =
  let open_tok = cur st in
  match open_tok.kind with
  | Token.LPAREN ->
    bump st;
    (match hooks.parse_expr st with
     | Error e -> Error e
     | Ok cond ->
       let close_tok = cur st in
       (match close_tok.kind with
        | Token.RPAREN ->
          bump st;
          (match stmt_parser st with
           | Error e -> Error e
           | Ok body ->
             Ok { Ast.desc = Ast.StmtWhile { label; cond; body }; span = Span.merge start_span body.span })
        | _ ->
          err_at close_tok
            (Printf.sprintf "expected ')' after 'while' condition, found %s"
               (Token.kind_to_string close_tok.kind))))
  | _ ->
    err_at open_tok ~hint:"wrap the condition in parentheses, e.g. 'while (i < 10) { ... }'"
      (Printf.sprintf "expected '(' after 'while', found %s" (Token.kind_to_string open_tok.kind))

let parse_for_stmt hooks st start_span ~label ~stmt_parser : (Ast.stmt, Diagnostic.t) result =
  let open_tok = cur st in
  match open_tok.kind with
  | Token.LPAREN ->
    bump st;
    let init_res : (Ast.for_init, Diagnostic.t) result =
      let tok = cur st in
      match tok.kind with
      | Token.KW_VAR | Token.KW_CONST ->
        let is_const = tok.kind = Token.KW_CONST in
        bump st;
        (match hooks.parse_decl st tok ~is_const with
         | Error e -> Error e
         | Ok { Ast.desc = Ast.StmtVarDecl v; _ } -> Ok (Ast.ForInitVarDecl v)
         | Ok { Ast.desc = Ast.StmtDestruct d; _ } -> Ok (Ast.ForInitDestruct d)
         | Ok _ -> err_at tok "invalid declaration in 'for' initializer")
      | _ ->
        (match hooks.parse_expr st with
         | Error e -> Error e
         | Ok init_e ->
           let semi_tok = cur st in
           if semi_tok.kind = Token.SEMI then (bump st; Ok (Ast.ForInitExpr init_e))
           else
             err_at ~code:"E0003" semi_tok
               ~hint:"separate the 3 parts of 'for' with ';', e.g. 'for (var i := 0; i < 10; i++)'"
               (Printf.sprintf "expected ';' after 'for' initializer, found %s"
                  (Token.kind_to_string semi_tok.kind)))
    in
    (match init_res with
     | Error e -> Error e
     | Ok init ->
       (match hooks.parse_expr st with
        | Error e -> Error e
        | Ok cond ->
          let semi2_tok = cur st in
          if semi2_tok.kind <> Token.SEMI then
            err_at ~code:"E0003" semi2_tok
              ~hint:"separate condition and step with ';', e.g. 'for (var i := 0; i < 10; i++)'"
              (Printf.sprintf "expected ';' after 'for' condition, found %s"
                 (Token.kind_to_string semi2_tok.kind))
          else begin
            bump st;
            match hooks.parse_expr st with
            | Error e -> Error e
            | Ok step ->
              let close_tok = cur st in
              if close_tok.kind <> Token.RPAREN then
                err_at close_tok
                  (Printf.sprintf "expected ')' after 'for' step expression, found %s"
                     (Token.kind_to_string close_tok.kind))
              else begin
                bump st;
                match stmt_parser st with
                | Error e -> Error e
                | Ok body ->
                  Ok
                    {
                      Ast.desc = Ast.StmtFor { label; init; cond; step; body };
                      span = Span.merge start_span body.span;
                    }
              end
          end))
  | _ ->
    err_at open_tok ~hint:"use 'for (<init>; <cond>; <step>) <stmt>'"
      (Printf.sprintf "expected '(' after 'for', found %s" (Token.kind_to_string open_tok.kind))

let parse_foreach_stmt hooks st start_span ~label ~stmt_parser : (Ast.stmt, Diagnostic.t) result =
  let open_tok = cur st in
  match open_tok.kind with
  | Token.LPAREN ->
    bump st;
    let kw_tok = cur st in
    (match kw_tok.kind with
     | Token.KW_VAR | Token.KW_CONST ->
       let is_const = kw_tok.kind = Token.KW_CONST in
       bump st;
       let bind_tok = cur st in
       let bind_res : (Ast.foreach_binding, Diagnostic.t) result =
         match bind_tok.kind with
         | Token.IDENT name -> bump st; Ok (Ast.ForeachVar (name, bind_tok.span))
         | Token.LPAREN ->
           bump st;
           (match hooks.parse_destruct_names st bind_tok with
            | Error e -> Error e
            | Ok names -> Ok (Ast.ForeachDestruct names))
         | _ ->
           err_at bind_tok ~hint:"use 'foreach (var x in arr)' or 'foreach (var (a, b) in arr)'"
             (Printf.sprintf "expected variable name or destructuring pattern '(a, b)' in 'foreach', found %s"
                (Token.kind_to_string bind_tok.kind))
       in
       (match bind_res with
        | Error e -> Error e
        | Ok binding ->
          let annot_res : (Ast.type_expr option, Diagnostic.t) result =
            if (cur st).kind = Token.COLON then begin
              bump st;
              match Parser_types.parse_type_expr st with
              | Error e -> Error e
              | Ok te -> Ok (Some te)
            end else Ok None
          in
          (match annot_res with
           | Error e -> Error e
           | Ok ty_annot ->
             let in_tok = cur st in
             if in_tok.kind <> Token.KW_IN then
               err_at in_tok ~hint:"use 'in' before the array expression, e.g. 'foreach (var x in arr)'"
                 (Printf.sprintf "expected 'in' in 'foreach' header, found %s"
                    (Token.kind_to_string in_tok.kind))
             else begin
               bump st;
               match hooks.parse_expr st with
               | Error e -> Error e
               | Ok collection ->
                 let close_tok = cur st in
                 if close_tok.kind <> Token.RPAREN then
                   err_at close_tok
                     (Printf.sprintf "expected ')' after 'foreach' collection expression, found %s"
                        (Token.kind_to_string close_tok.kind))
                 else begin
                   bump st;
                   match stmt_parser st with
                   | Error e -> Error e
                   | Ok body ->
                     let head = { Ast.binding; is_const; ty_annot } in
                     Ok
                       {
                         Ast.desc = Ast.StmtForeach { label; head; collection; body };
                         span = Span.merge start_span body.span;
                       }
                 end
             end))
     | _ ->
       err_at kw_tok ~hint:"declare the loop variable with 'var' or 'const', e.g. 'foreach (var x in arr)'"
         (Printf.sprintf "expected 'var' or 'const' in 'foreach' header, found %s"
            (Token.kind_to_string kw_tok.kind)))
  | _ ->
    err_at open_tok ~hint:"use 'foreach (var x in arr) <stmt>'"
      (Printf.sprintf "expected '(' after 'foreach', found %s" (Token.kind_to_string open_tok.kind))

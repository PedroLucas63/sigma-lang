open Sigma_common
open Sigma_diagnostics
open Parser_state

let parse_destruct_names st open_tok : ((string * Span.t) list, Diagnostic.t) result =
  let rec loop acc =
    if (cur st).kind = Token.RPAREN then begin
      bump st;
      Ok (List.rev acc)
    end else
      let tok = cur st in
      match tok.kind with
      | Token.IDENT name ->
        bump st;
        let acc = (name, tok.span) :: acc in
        let next_tok = cur st in
        if next_tok.kind = Token.COMMA then begin
          bump st;
          loop acc
        end else if next_tok.kind = Token.RPAREN then begin
          bump st;
          Ok (List.rev acc)
        end else
          err_at next_tok
            (Printf.sprintf "expected ')' or ',' in destructuring pattern, found %s"
               (Token.kind_to_string next_tok.kind))
      | _ ->
        err_at tok ~hint:"use variable names or '_' inside destructuring, e.g. 'var (a, b, _) := tuple;'"
          (Printf.sprintf "expected variable identifier in destructuring pattern, found %s"
             (Token.kind_to_string tok.kind))
  in
  match loop [] with
  | Error e -> Error e
  | Ok [] -> err_at open_tok "destructuring pattern cannot be empty '()'"
  | Ok names -> Ok names

let parse_decl hooks st kw_tok ~is_const : (Ast.stmt, Diagnostic.t) result =
  let kw_name = if is_const then "const" else "var" in
  let entity_name = if is_const then "constant" else "variable" in
  let name_tok = cur st in
  match name_tok.kind with
  | Token.LPAREN ->
    bump st;
    (match parse_destruct_names st name_tok with
     | Error e -> Error e
     | Ok names ->
       let op_tok = cur st in
       (match op_tok.kind with
        | Token.COLON_EQUAL ->
          bump st;
          (match hooks.parse_expr st with
           | Error e -> Error e
           | Ok init ->
             (match expect_semi st kw_tok.Token.span with
              | Error e -> Error e
              | Ok full_span ->
                let decl = { Ast.names; is_const; ty_annot = None; init = Some init } in
                Ok { Ast.desc = Ast.StmtDestruct decl; span = full_span }))
        | Token.COLON ->
          bump st;
          (match Parser_types.parse_type_expr st with
           | Error e -> Error e
           | Ok ty_expr ->
             let ty_annot = Some ty_expr in
             let ty_str = Ast.type_expr_to_string ty_expr in
             let after_ty = cur st in
             (match after_ty.kind with
              | Token.EQUAL ->
                bump st;
                (match hooks.parse_expr st with
                 | Error e -> Error e
                 | Ok init ->
                   (match expect_semi st kw_tok.Token.span with
                    | Error e -> Error e
                    | Ok full_span ->
                      let decl = { Ast.names; is_const; ty_annot; init = Some init } in
                      Ok { Ast.desc = Ast.StmtDestruct decl; span = full_span }))
              | Token.SEMI ->
                bump st;
                let full_span = Span.merge kw_tok.span after_ty.span in
                if is_const then
                  Error
                    (Diagnostic.error ~code:"E0106" ~span:full_span
                       "constant destructuring declaration requires an initializing expression")
                else
                  let decl = { Ast.names; is_const = false; ty_annot; init = None } in
                  Ok { Ast.desc = Ast.StmtDestruct decl; span = full_span }
              | _ ->
                let expected_msg = if is_const then "expected '=' after" else "expected '=' or ';' after" in
                err_at ~code:"E0003" after_ty
                  (Printf.sprintf "%s type '%s', found %s"
                     expected_msg ty_str (Token.kind_to_string after_ty.kind))))
        | _ ->
          err_at op_tok
            ~hint:(Printf.sprintf "use '%s (a, b) := <tuple>;' or '%s (a, b) : (int, int) = <tuple>;'" kw_name kw_name)
            (Printf.sprintf "expected ':' or ':=' after destructuring pattern, found %s"
               (Token.kind_to_string op_tok.kind))))
  | Token.IDENT name ->
    bump st;
    let op_tok = cur st in
    (match op_tok.kind with
     | Token.COLON_EQUAL ->
       bump st;
       (match hooks.parse_expr st with
        | Error e -> Error e
        | Ok init ->
          (match expect_semi st kw_tok.Token.span with
           | Error e -> Error e
           | Ok full_span ->
             let decl =
               { Ast.name; name_span = name_tok.span; is_const; ty_annot = None; init = Some init }
             in
             Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }))
     | Token.COLON ->
       bump st;
       (match Parser_types.parse_type_expr st with
        | Error e -> Error e
        | Ok ty_expr ->
          let ty_annot = Some ty_expr in
          let ty_name = Ast.type_expr_to_string ty_expr in
          let after_ty = cur st in
          (match after_ty.kind with
           | Token.EQUAL ->
             bump st;
             (match hooks.parse_expr st with
              | Error e -> Error e
              | Ok init ->
                (match expect_semi st kw_tok.Token.span with
                 | Error e -> Error e
                 | Ok full_span ->
                   let decl =
                     { Ast.name; name_span = name_tok.span; is_const; ty_annot; init = Some init }
                   in
                   Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }))
           | Token.SEMI ->
             bump st;
             let full_span = Span.merge kw_tok.span after_ty.span in
             if is_const then
               Error
                 (Diagnostic.error
                    ~hint:(Printf.sprintf "provide a value, e.g. 'const %s : %s = <value>;'" name ty_name)
                    ~code:"E0106"
                    ~span:full_span
                    (Printf.sprintf "constant '%s' requires an initializing expression" name))
             else
               let decl =
                 { Ast.name; name_span = name_tok.span; is_const = false; ty_annot; init = None }
               in
               Ok { Ast.desc = Ast.StmtVarDecl decl; span = full_span }
           | _ ->
             let hint =
               if is_const then Printf.sprintf "use 'const %s : %s = <value>;'" name ty_name
               else Printf.sprintf "use 'var %s : %s = <value>;' or 'var %s : %s;'" name ty_name name ty_name
             in
             let expected_msg = if is_const then "expected '=' after" else "expected '=' or ';' after" in
             err_at ~code:"E0003" ~hint after_ty
               (Printf.sprintf "%s type '%s', found %s"
                  expected_msg ty_name (Token.kind_to_string after_ty.kind))))
     | Token.EQUAL ->
       err_at op_tok
         ~hint:(Printf.sprintf "use '%s %s := <value>;' for type inference or '%s %s : int = <value>;'" kw_name name kw_name name)
         (Printf.sprintf "cannot use '=' without an explicit type annotation in '%s' declaration" kw_name)
     | _ ->
       err_at op_tok
         ~hint:(Printf.sprintf "use '%s %s : <type> = <value>;' or '%s %s := <value>;'" kw_name name kw_name name)
         (Printf.sprintf "expected ':' or ':=' after %s name '%s', found %s"
            entity_name name (Token.kind_to_string op_tok.kind)))
  | _ ->
    err_at name_tok
      (Printf.sprintf "expected %s name after '%s', found %s"
         entity_name kw_name (Token.kind_to_string name_tok.kind))

let parse_fun_params st : (Ast.fun_param list, Diagnostic.t) result =
  let rec params acc =
    let t = cur st in
    if t.kind = Token.RPAREN then begin
      bump st;
      Ok (List.rev acc)
    end else
      let pmode =
        let tok = cur st in
        if tok.kind = Token.KW_REF then (bump st; Ast.ParamRef)
        else if tok.kind = Token.KW_OUT then (bump st; Ast.ParamOut)
        else Ast.ParamVal
      in
      let t = cur st in
      match t.kind with
      | Token.IDENT pname ->
        bump st;
        if (cur st).kind <> Token.COLON then
          err_at (cur st) (Printf.sprintf "expected ':' and a type after parameter '%s'" pname)
        else begin
          bump st;
          match Parser_types.parse_type_expr st with
          | Error e -> Error e
          | Ok te ->
            let param_variadic =
              if (cur st).kind = Token.ELLIPSIS then (bump st; true) else false
            in
            let acc =
              { Ast.param_name = pname; param_span = t.span; param_mode = pmode;
                param_type = te; param_variadic }
              :: acc
            in
            let nt = cur st in
            if nt.kind = Token.COMMA then (bump st; params acc)
            else if nt.kind = Token.RPAREN then (bump st; Ok (List.rev acc))
            else
              err_at nt
                (Printf.sprintf "expected ')' or ',' in parameter list, found %s"
                   (Token.kind_to_string nt.kind))
        end
      | _ -> err_at t (Printf.sprintf "expected parameter name, found %s" (Token.kind_to_string t.kind))
  in
  params []

let parse_fun_decl hooks st kw_tok ~is_act : (Ast.stmt, Diagnostic.t) result =
  let kw_name = if is_act then "act" else "fun" in
  let name_tok = cur st in
  match name_tok.kind with
  | Token.IDENT name ->
    bump st;
    (match Parser_generics.parse_opt_type_params st with
     | Error e -> Error e
     | Ok fd_type_params ->
       let lp = cur st in
       if lp.kind <> Token.LPAREN then
         err_at lp
           (Printf.sprintf "expected '(' after %s name '%s', found %s" kw_name name
              (Token.kind_to_string lp.kind))
       else begin
         bump st;
         match parse_fun_params st with
         | Error e -> Error e
         | Ok ps ->
           let ret_res =
             if is_act then Ok None
             else if (cur st).kind = Token.COLON then begin
               bump st;
               match Parser_types.parse_type_expr st with
               | Ok te -> Ok (Some te)
               | Error e -> Error e
             end else
               err_at (cur st)
                 (Printf.sprintf "expected ':' and a return type after the parameters of 'fun %s'" name)
           in
           (match ret_res with
            | Error e -> Error e
            | Ok ret ->
              (match Parser_generics.parse_opt_where_clauses ~parse_type_expr:Parser_types.parse_type_expr st with
               | Error e -> Error e
               | Ok fd_where ->
                 let ob = cur st in
                 if ob.kind <> Token.LBRACE then
                   err_at ob
                     (Printf.sprintf "expected '{' to start the body of '%s %s', found %s" kw_name name
                        (Token.kind_to_string ob.kind))
                 else begin
                   bump st;
                   match hooks.parse_block_stmt st ob with
                   | Error e -> Error e
                   | Ok { Ast.desc = Ast.StmtBlock body; span } ->
                     Ok
                       {
                         Ast.desc =
                           Ast.StmtFunDecl
                             {
                               Ast.fd_name = name;
                               fd_name_span = name_tok.span;
                               fd_type_params;
                               fd_params = ps;
                               fd_ret = ret;
                               fd_where;
                               fd_body = body;
                             };
                         span = Span.merge kw_tok.Token.span span;
                       }
                   | Ok _ -> err_at ob "internal error: function body must be a block"
                 end))
       end)
  | _ ->
    err_at name_tok
      (Printf.sprintf "expected %s name after '%s', found %s" kw_name kw_name
         (Token.kind_to_string name_tok.kind))

open Sigma_diagnostics
open Parser_state

let rec hooks : hooks =
  {
    parse_expr = (fun st -> Parser_expr.parse_expr hooks st);
    parse_stmt = (fun st -> Parser_stmt.parse_stmt hooks st);
    parse_non_decl_stmt = (fun st -> Parser_stmt.parse_non_decl_stmt hooks st);
    parse_block_stmt = (fun st ob -> Parser_stmt.parse_block_stmt hooks st ob);
    parse_lambda_expr = (fun st kw ~is_act -> Parser_lambda.parse_lambda_expr hooks st kw ~is_act);
    parse_decl = (fun st kw ~is_const -> Parser_decl.parse_decl hooks st kw ~is_const);
    parse_destruct_names = Parser_decl.parse_destruct_names;
    parse_if_stmt = (fun st tok ~stmt_parser -> Parser_loops.parse_if_stmt hooks st tok ~stmt_parser);
    parse_while_stmt =
      (fun st sp ~label ~stmt_parser -> Parser_loops.parse_while_stmt hooks st sp ~label ~stmt_parser);
    parse_for_stmt =
      (fun st sp ~label ~stmt_parser -> Parser_loops.parse_for_stmt hooks st sp ~label ~stmt_parser);
    parse_foreach_stmt =
      (fun st sp ~label ~stmt_parser -> Parser_loops.parse_foreach_stmt hooks st sp ~label ~stmt_parser);
    parse_print_stmt =
      (fun st tok ~newline ~require_semi ->
        Parser_stmt.parse_print_stmt hooks st tok ~newline ~require_semi);
    parse_assert_stmt =
      (fun st tok ~require_semi -> Parser_stmt.parse_assert_stmt hooks st tok ~require_semi);
    parse_assert_error_stmt =
      (fun st tok ~require_semi -> Parser_stmt.parse_assert_error_stmt hooks st tok ~require_semi);
    parse_break_or_continue_stmt = Parser_stmt.parse_break_or_continue_stmt;
  }

let parse (token_list : Token.t list) : (Ast.program, Diagnostic.t list) result =
  let tokens = Array.of_list token_list in
  let st = { tokens; idx = 0 } in
  let stmts = ref [] in
  let errors = ref [] in
  while not (is_eof st) do
    match hooks.parse_stmt st with
    | Ok s -> stmts := s :: !stmts
    | Error err ->
      errors := err :: !errors;
      sync_to_semi st
  done;
  match List.rev !errors with
  | [] -> Ok (List.rev !stmts)
  | errs -> Error errs

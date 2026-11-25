open Sigma_common
open Sigma_sema
open Lower_ctx

let rec hooks : hooks =
  {
    lower_expr = (fun b e -> Lower_expr.lower_expr hooks b e);
    lower_stmt = (fun b loops s -> Lower_stmt.lower_stmt hooks b loops s);
  }

let lower_program (prog : Typed_ast.program) : Ir.program =
  let b = create_builder () in
  List.iter (hooks.lower_stmt b []) prog;
  (match Typecheck.detect_entry_point prog with
   | None -> ()
   | Some ep ->
     let fn_ty =
       match ep with
       | Typecheck.EntryActNoArgs -> Ty.Fun ([], Ty.Void)
       | Typecheck.EntryActArgs ->
         Ty.Fun ([ { Ty.pty = Ty.DynArray Ty.String; pmode = Ty.ParamVal } ], Ty.Void)
       | Typecheck.EntryFunIntNoArgs -> Ty.Fun ([], Ty.Int)
       | Typecheck.EntryFunIntArgs ->
         Ty.Fun ([ { Ty.pty = Ty.DynArray Ty.String; pmode = Ty.ParamVal } ], Ty.Int)
     in
     ignore (emit b (get_global_opcode fn_ty "main") Span.dummy);
     (match ep with
      | Typecheck.EntryActNoArgs ->
        ignore (emit b (Ir.OP_CALL []) Span.dummy);
        ignore (emit b Ir.OP_POP Span.dummy)
      | Typecheck.EntryActArgs ->
        ignore (emit b Ir.OP_GET_ARGS Span.dummy);
        ignore (emit b (Ir.OP_CALL [ 0 ]) Span.dummy);
        ignore (emit b Ir.OP_POP Span.dummy)
      | Typecheck.EntryFunIntNoArgs ->
        ignore (emit b (Ir.OP_CALL []) Span.dummy);
        ignore (emit b Ir.OP_SET_EXIT_CODE Span.dummy)
      | Typecheck.EntryFunIntArgs ->
        ignore (emit b Ir.OP_GET_ARGS Span.dummy);
        ignore (emit b (Ir.OP_CALL [ 0 ]) Span.dummy);
        ignore (emit b Ir.OP_SET_EXIT_CODE Span.dummy)));
  ignore (emit b Ir.OP_HALT Span.dummy);
  Array.sub b.code 0 b.len

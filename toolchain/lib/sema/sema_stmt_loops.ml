open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_types

let check_loop_cond hooks env ~loop_kw ~example cond errs =
  match hooks.check_expr env cond with
  | Error err -> errs := err :: !errs; None
  | Ok tc ->
    if tc.ty <> Ty.Bool then begin
      errs :=
        Diagnostic.error
          ~hint:(Printf.sprintf "the condition of a '%s' loop must evaluate to 'bool' (e.g. '%s')" loop_kw example)
          ~code:"E0104" ~span:tc.span
          (Printf.sprintf "type mismatch in '%s' condition: expected 'bool', found '%s'"
             loop_kw (Ty.to_string tc.ty))
        :: !errs;
      None
    end else Some tc

let check_while_stmt hooks (env : env) (loop_stack : active_loop list) (next_loop_id : int ref)
    (s_span : Span.t) label cond body : (Typed_ast.stmt, Diagnostic.t list) result =
  incr next_loop_id;
  let loop_id = !next_loop_id in
  let errs = ref [] in
  Option.iter (fun e -> errs := e :: !errs) (check_loop_label loop_stack label);
  let tcond_opt = check_loop_cond hooks env ~loop_kw:"while" ~example:"while (i < 10)" cond errs in
  let while_scope = ref [] in
  let new_loop_stack = { loop_id; label } :: loop_stack in
  let tbody_opt =
    match hooks.check_stmt env while_scope new_loop_stack next_loop_id body with
    | Ok tb -> Some tb
    | Error sub_errs -> errs := List.rev_append sub_errs !errs; None
  in
  List.iter (Hashtbl.remove env) !while_scope;
  match List.rev !errs, tcond_opt, tbody_opt with
  | [], Some tc, Some tb ->
    Ok
      {
        Typed_ast.desc =
          Typed_ast.TStmtWhile { loop_id; label = Option.map fst label; cond = tc; body = tb };
        span = s_span;
      }
  | errors, _, _ -> Error errors

let check_for_stmt hooks (env : env) (loop_stack : active_loop list) (next_loop_id : int ref)
    (s_span : Span.t) label init cond step body : (Typed_ast.stmt, Diagnostic.t list) result =
  incr next_loop_id;
  let loop_id = !next_loop_id in
  let errs = ref [] in
  Option.iter (fun e -> errs := e :: !errs) (check_loop_label loop_stack label);
  let for_scope = ref [] in
  let tinit_opt =
    match init with
    | Ast.ForInitVarDecl v ->
      (match hooks.check_stmt env for_scope loop_stack next_loop_id { Ast.desc = Ast.StmtVarDecl v; span = s_span } with
       | Ok { Typed_ast.desc = Typed_ast.TStmtVarDecl tv; _ } -> Some (Typed_ast.TForInitVarDecl tv)
       | Ok _ -> None
       | Error sub_errs -> errs := List.rev_append sub_errs !errs; None)
    | Ast.ForInitDestruct d ->
      (match hooks.check_stmt env for_scope loop_stack next_loop_id { Ast.desc = Ast.StmtDestruct d; span = s_span } with
       | Ok { Typed_ast.desc = Typed_ast.TStmtDestruct td; _ } -> Some (Typed_ast.TForInitDestruct td)
       | Ok _ -> None
       | Error sub_errs -> errs := List.rev_append sub_errs !errs; None)
    | Ast.ForInitExpr e ->
      (match hooks.check_expr env e with
       | Ok te -> Some (Typed_ast.TForInitExpr te)
       | Error err -> errs := err :: !errs; None)
  in
  let tcond_opt =
    check_loop_cond hooks env ~loop_kw:"for" ~example:"for (var i := 0; i < 10; i++)" cond errs
  in
  let tstep_opt =
    match hooks.check_expr env step with
    | Ok ts -> Some ts
    | Error err -> errs := err :: !errs; None
  in
  let new_loop_stack = { loop_id; label } :: loop_stack in
  let tbody_opt =
    match hooks.check_stmt env for_scope new_loop_stack next_loop_id body with
    | Ok tb -> Some tb
    | Error sub_errs -> errs := List.rev_append sub_errs !errs; None
  in
  List.iter (Hashtbl.remove env) !for_scope;
  match List.rev !errs, tinit_opt, tcond_opt, tstep_opt, tbody_opt with
  | [], Some ti, Some tc, Some ts, Some tb ->
    Ok
      {
        Typed_ast.desc =
          Typed_ast.TStmtFor
            { loop_id; label = Option.map fst label; init = ti; cond = tc; step = ts; body = tb };
        span = s_span;
      }
  | errors, _, _, _, _ -> Error errors

let check_foreach_stmt hooks (env : env) (loop_stack : active_loop list) (next_loop_id : int ref)
    (s_span : Span.t) label (head : Ast.foreach_head) collection body
    : (Typed_ast.stmt, Diagnostic.t list) result =
  incr next_loop_id;
  let loop_id = !next_loop_id in
  let errs = ref [] in
  Option.iter (fun e -> errs := e :: !errs) (check_loop_label loop_stack label);
  let tcoll_opt =
    match hooks.check_expr env collection with
    | Error err -> errs := err :: !errs; None
    | Ok tc -> Some tc
  in
  let array_info_opt =
    match tcoll_opt with
    | None -> None
    | Some tc ->
      (match tc.ty with
       | Ty.Array (elem_ty, len) -> Some (tc, elem_ty, Some len)
       | Ty.DynArray elem_ty -> Some (tc, elem_ty, None)
       | _ ->
         errs :=
           Diagnostic.error
             ~hint:"'foreach' iterates over static arrays 'T[N]', e.g. 'foreach (var x in [10, 20, 30])'"
             ~code:"E0104" ~span:tc.span
             (Printf.sprintf "type mismatch in 'foreach' collection: expected an array 'T[N]', found '%s'"
                (Ty.to_string tc.ty))
           :: !errs;
         None)
  in
  let foreach_scope = ref [] in
  let thead_opt =
    match array_info_opt with
    | None -> None
    | Some (_, elem_ty, _) ->
      let annot_ok =
        match head.ty_annot with
        | None -> true
        | Some annot ->
          (match resolve_type_expr annot (Some elem_ty) with
           | Error err -> errs := err :: !errs; false
           | Ok annot_ty ->
             if Ty.equal annot_ty elem_ty then true
             else begin
               errs :=
                 Diagnostic.error ~code:"E0104" ~span:(Ast.type_expr_span annot)
                   (Printf.sprintf "type mismatch in 'foreach' binding: expected '%s', found array element of type '%s'"
                      (Ty.to_string annot_ty) (Ty.to_string elem_ty))
                 :: !errs;
               false
             end)
      in
      if not annot_ok then None
      else
        (match head.binding with
         | Ast.ForeachVar (name, name_span) ->
           if name = "_" then
             Some (Typed_ast.TForeachVar { name = None; name_span; is_const = head.is_const; ty = elem_ty })
           else begin
             (match find_conflict env name with
              | None -> ()
              | Some prev ->
                let entity = if prev.is_const then "constant" else "variable" in
                errs :=
                  Diagnostic.error
                    ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
                    ~code:"E0103" ~span:name_span
                    (Printf.sprintf "%s '%s' is already declared in this scope" entity name)
                  :: !errs);
             if !errs = [] then begin
               Hashtbl.replace env name
                 {
                   ty = elem_ty;
                   decl_span = name_span;
                   is_const = head.is_const;
                   const_kind = (if head.is_const then DeepConst else NotConst);
                   is_ref_param = false;
                   const_int = None;
                 };
               foreach_scope := name :: !foreach_scope;
               Some (Typed_ast.TForeachVar { name = Some name; name_span; is_const = head.is_const; ty = elem_ty })
             end else None
           end
         | Ast.ForeachDestruct names ->
           errs := List.rev_append (Sema_stmt_decl.check_destruct_names env names) !errs;
           let sub_tys_opt =
             match elem_ty with
             | Ty.Tuple tys -> Some tys
             | Ty.Array (sub_ty, n) -> Some (List.init n (fun _ -> sub_ty))
             | _ -> None
           in
           (match sub_tys_opt with
            | None ->
              errs :=
                Diagnostic.error
                  ~hint:"destructuring in 'foreach' requires an array of tuples or nested arrays, e.g. '[(10, 20), (30, 40)]'"
                  ~code:"E0104" ~span:s_span
                  (Printf.sprintf "cannot destructure 'foreach' element of type '%s' (expected a tuple or array)"
                     (Ty.to_string elem_ty))
                :: !errs;
              None
            | Some sub_tys ->
              let pat_len = List.length names in
              let val_len = List.length sub_tys in
              if pat_len <> val_len then begin
                errs :=
                  Diagnostic.error
                    ~hint:(Printf.sprintf "provide %d binding(s) in the pattern (use '_' to ignore elements)" val_len)
                    ~code:"E0104" ~span:s_span
                    (Printf.sprintf "destructuring arity mismatch in 'foreach': pattern has %d elements, but element type '%s' has %d elements"
                       pat_len (Ty.to_string elem_ty) val_len)
                  :: !errs;
                None
              end else if !errs = [] then begin
                let bindings =
                  List.map2
                    (fun (name, name_span) sub_ty ->
                      if name = "_" then { Typed_ast.name = None; name_span; ty = sub_ty }
                      else begin
                        Hashtbl.replace env name
                          {
                            ty = sub_ty;
                            decl_span = name_span;
                            is_const = head.is_const;
                            const_kind = (if head.is_const then DeepConst else NotConst);
                            is_ref_param = false;
                            const_int = None;
                          };
                        foreach_scope := name :: !foreach_scope;
                        { Typed_ast.name = Some name; name_span; ty = sub_ty }
                      end)
                    names sub_tys
                in
                Some (Typed_ast.TForeachDestruct { bindings; is_const = head.is_const; ty = elem_ty })
              end else None))
  in
  let new_loop_stack = { loop_id; label } :: loop_stack in
  let tbody_opt =
    match hooks.check_stmt env foreach_scope new_loop_stack next_loop_id body with
    | Ok tb -> Some tb
    | Error sub_errs -> errs := List.rev_append sub_errs !errs; None
  in
  List.iter (Hashtbl.remove env) !foreach_scope;
  match List.rev !errs, array_info_opt, thead_opt, tbody_opt with
  | [], Some (tcoll, elem_ty, len), Some thead, Some tbody ->
    Ok
      {
        Typed_ast.desc =
          Typed_ast.TStmtForeach
            { loop_id; label = Option.map fst label; head = thead; collection = tcoll; elem_ty; len; body = tbody };
        span = s_span;
      }
  | errors, _, _, _ -> Error errors

let check_loop_jump ~is_break (loop_stack : active_loop list) (s_span : Span.t) label_opt
    : (Typed_ast.stmt, Diagnostic.t list) result =
  let kw = if is_break then "break" else "continue" in
  let make_desc target_loop_id label =
    if is_break then Typed_ast.TStmtBreak { target_loop_id; label }
    else Typed_ast.TStmtContinue { target_loop_id; label }
  in
  match label_opt with
  | None ->
    (match loop_stack with
     | [] ->
       Error
         [
           Diagnostic.error
             ~hint:(Printf.sprintf "use '%s;' inside a 'while', 'for', or 'foreach' loop" kw)
             ~code:"E0109" ~span:s_span (Printf.sprintf "'%s' cannot be used outside of a loop" kw);
         ]
     | top :: _ -> Ok { Typed_ast.desc = make_desc top.loop_id None; span = s_span })
  | Some (lbl, lbl_span) ->
    if loop_stack = [] then
      Error
        [
          Diagnostic.error
            ~hint:(Printf.sprintf "use '%s <label>;' inside an enclosing labeled loop" kw)
            ~code:"E0109" ~span:s_span (Printf.sprintf "'%s %s' cannot be used outside of a loop" kw lbl);
        ]
    else
      (match
         List.find_opt
           (fun al -> match al.label with Some (l, _) -> l = lbl | None -> false)
           loop_stack
       with
       | Some target -> Ok { Typed_ast.desc = make_desc target.loop_id (Some lbl); span = s_span }
       | None ->
         Error
           [
             Diagnostic.error
               ~hint:(Printf.sprintf "annotate an enclosing loop with '%s: for (...) { ... }'" lbl)
               ~code:"E0109" ~span:lbl_span
               (Printf.sprintf "unknown or inactive loop label '%s' in '%s %s'" lbl kw lbl);
           ])

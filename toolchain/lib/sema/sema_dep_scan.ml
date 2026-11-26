let stmt_defined_names (s : Typed_ast.stmt) : string list =
  match s.desc with
  | Typed_ast.TStmtVarDecl v -> [ v.name ]
  | Typed_ast.TStmtDestruct d -> List.filter_map (fun (b : Typed_ast.destruct_binding) -> b.name) d.bindings
  | Typed_ast.TStmtFunDecl fd -> [ fd.tf_name ]
  | Typed_ast.TStmtExtern exts -> List.map (fun (e : Typed_ast.extern_decl) -> e.te_name) exts
  | Typed_ast.TStmtTypeDecl tud -> [ tud.tud_name ]
  | _ -> []

let add_dep deps name =
  if not (List.mem name !deps) then deps := name :: !deps

let rec collect_ty_deps deps (top_types : (string, unit) Hashtbl.t) (ty : Ty.t) =
  match ty with
  | Ty.User (_, tname) -> if Hashtbl.mem top_types tname then add_dep deps tname
  | Ty.Tuple ts -> List.iter (collect_ty_deps deps top_types) ts
  | Ty.Array (t, _) | Ty.DynArray t -> collect_ty_deps deps top_types t
  | Ty.Fun (ps, r) ->
    List.iter (fun (p : Ty.fun_param) -> collect_ty_deps deps top_types p.pty) ps;
    collect_ty_deps deps top_types r
  | _ -> ()

let rec rename_expr prefix top_vals top_types locals deps (e : Typed_ast.expr) : Typed_ast.expr =
  collect_ty_deps deps top_types e.ty;
  let re = rename_expr prefix top_vals top_types locals deps in
  let desc =
    match e.desc with
    | Typed_ast.TExprIdent nm ->
      if (not (Hashtbl.mem locals nm)) && Hashtbl.mem top_vals nm then begin
        add_dep deps nm;
        Typed_ast.TExprIdent (prefix ^ nm)
      end else e.desc
    | Typed_ast.TExprInterp parts ->
      Typed_ast.TExprInterp
        (List.map (function Typed_ast.TInterpLit _ as p -> p | Typed_ast.TInterpExpr (x, sp) -> Typed_ast.TInterpExpr (re x, sp)) parts)
    | Typed_ast.TExprInput { prompt; mode } -> Typed_ast.TExprInput { prompt = Option.map re prompt; mode }
    | Typed_ast.TExprTuple items ->
      Typed_ast.TExprTuple (List.map (function Typed_ast.TItemExpr x -> Typed_ast.TItemExpr (re x) | Typed_ast.TItemSpread x -> Typed_ast.TItemSpread (re x)) items)
    | Typed_ast.TExprArray items ->
      Typed_ast.TExprArray (List.map (function Typed_ast.TItemExpr x -> Typed_ast.TItemExpr (re x) | Typed_ast.TItemSpread x -> Typed_ast.TItemSpread (re x)) items)
    | Typed_ast.TExprIndex (a, i) -> Typed_ast.TExprIndex (re a, re i)
    | Typed_ast.TExprField (b, f, idx) -> Typed_ast.TExprField (re b, f, idx)
    | Typed_ast.TExprStructInit si ->
      if Hashtbl.mem top_types si.tsi_name then add_dep deps si.tsi_name;
      Typed_ast.TExprStructInit { si with tsi_args = List.map re si.tsi_args }
    | Typed_ast.TExprNewWithInits { nwi_ctor; nwi_inits } ->
      Typed_ast.TExprNewWithInits { nwi_ctor = re nwi_ctor; nwi_inits = List.map (fun (i, x) -> (i, re x)) nwi_inits }
    | Typed_ast.TExprCall c ->
      Typed_ast.TExprCall { c with callee = re c.callee; args = List.map (fun (a : Typed_ast.typed_call_arg) -> { a with Typed_ast.tca_expr = re a.tca_expr }) c.args }
    | Typed_ast.TExprUnary (op, x) -> Typed_ast.TExprUnary (op, re x)
    | Typed_ast.TExprBinary (op, a, b) -> Typed_ast.TExprBinary (op, re a, re b)
    | Typed_ast.TExprChainCmp (a, o1, b, o2, c) -> Typed_ast.TExprChainCmp (re a, o1, re b, o2, re c)
    | Typed_ast.TExprAssign (lv, op, rhs) -> Typed_ast.TExprAssign (rename_lval prefix top_vals top_types locals deps lv, op, re rhs)
    | Typed_ast.TExprIncDec (op, lv) -> Typed_ast.TExprIncDec (op, rename_lval prefix top_vals top_types locals deps lv)
    | Typed_ast.TExprLambda fd -> Typed_ast.TExprLambda (rename_fun prefix top_vals top_types locals deps fd)
    | _ -> e.desc
  in
  { e with desc }

and rename_lval prefix top_vals top_types locals deps = function
  | Typed_ast.TLValIdent nm ->
    if (not (Hashtbl.mem locals nm)) && Hashtbl.mem top_vals nm then begin
      add_dep deps nm;
      Typed_ast.TLValIdent (prefix ^ nm)
    end else Typed_ast.TLValIdent nm
  | Typed_ast.TLValIndex (a, i) ->
    Typed_ast.TLValIndex (rename_expr prefix top_vals top_types locals deps a, rename_expr prefix top_vals top_types locals deps i)
  | Typed_ast.TLValField (b, f, idx) ->
    Typed_ast.TLValField (rename_expr prefix top_vals top_types locals deps b, f, idx)

and rename_fun prefix top_vals top_types outer_locals deps (fd : Typed_ast.fun_decl) : Typed_ast.fun_decl =
  let locals = Hashtbl.copy outer_locals in
  List.iter (fun (tp : Typed_ast.typed_param) -> Hashtbl.replace locals tp.tp_name ()) fd.tf_params;
  let tbody = List.map (rename_stmt_inner prefix top_vals top_types locals deps) fd.tf_body in
  let tcaps =
    List.map (fun (cn, cty) ->
      if (not (Hashtbl.mem outer_locals cn)) && Hashtbl.mem top_vals cn then (add_dep deps cn; (prefix ^ cn, cty))
      else (cn, cty)) fd.tf_captures
  in
  { fd with tf_params = fd.tf_params; tf_captures = tcaps; tf_body = tbody }

and rename_stmt_inner prefix top_vals top_types locals deps (s : Typed_ast.stmt) : Typed_ast.stmt =
  let re = rename_expr prefix top_vals top_types locals deps in
  let rs = rename_stmt_inner prefix top_vals top_types locals deps in
  let desc =
    match s.desc with
    | Typed_ast.TStmtVarDecl v ->
      let init = re v.init in
      Hashtbl.replace locals v.name ();
      Typed_ast.TStmtVarDecl { v with init }
    | Typed_ast.TStmtDestruct d ->
      let init = re d.init in
      List.iter (fun (b : Typed_ast.destruct_binding) -> Option.iter (fun n -> Hashtbl.replace locals n ()) b.name) d.bindings;
      Typed_ast.TStmtDestruct { d with init }
    | Typed_ast.TStmtBlock ss ->
      let bl = Hashtbl.copy locals in
      Typed_ast.TStmtBlock (List.map (rename_stmt_inner prefix top_vals top_types bl deps) ss)
    | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
      Typed_ast.TStmtIf { cond = re cond; then_branch = rs then_branch; else_branch = Option.map rs else_branch }
    | Typed_ast.TStmtWhile w -> Typed_ast.TStmtWhile { w with cond = re w.cond; body = rs w.body }
    | Typed_ast.TStmtFor f ->
      let bl = Hashtbl.copy locals in
      let init =
        match f.init with
        | Typed_ast.TForInitVarDecl v ->
          let i = rename_expr prefix top_vals top_types bl deps v.init in
          Hashtbl.replace bl v.name ();
          Typed_ast.TForInitVarDecl { v with init = i }
        | Typed_ast.TForInitDestruct d ->
          let i = rename_expr prefix top_vals top_types bl deps d.init in
          List.iter (fun (b : Typed_ast.destruct_binding) -> Option.iter (fun n -> Hashtbl.replace bl n ()) b.name) d.bindings;
          Typed_ast.TForInitDestruct { d with init = i }
        | Typed_ast.TForInitExpr e -> Typed_ast.TForInitExpr (rename_expr prefix top_vals top_types bl deps e)
      in
      Typed_ast.TStmtFor { f with init; cond = rename_expr prefix top_vals top_types bl deps f.cond;
                           step = rename_expr prefix top_vals top_types bl deps f.step;
                           body = rename_stmt_inner prefix top_vals top_types bl deps f.body }
    | Typed_ast.TStmtForeach fe ->
      let col = re fe.collection in
      let bl = Hashtbl.copy locals in
      (match fe.head with
       | Typed_ast.TForeachVar { name = Some n; _ } -> Hashtbl.replace bl n ()
       | Typed_ast.TForeachDestruct { bindings; _ } ->
         List.iter (fun (b : Typed_ast.destruct_binding) -> Option.iter (fun n -> Hashtbl.replace bl n ()) b.name) bindings
       | _ -> ());
      Typed_ast.TStmtForeach { fe with collection = col; body = rename_stmt_inner prefix top_vals top_types bl deps fe.body }
    | Typed_ast.TStmtPrint p -> Typed_ast.TStmtPrint { p with args = List.map re p.args }
    | Typed_ast.TStmtAssert a ->
      Typed_ast.TStmtAssert { actual = re a.actual; expected = re a.expected; message = Option.map re a.message }
    | Typed_ast.TStmtAssertError ae -> Typed_ast.TStmtAssertError { ae with message = Option.map re ae.message }
    | Typed_ast.TStmtFunDecl fd ->
      Hashtbl.replace locals fd.tf_name ();
      Typed_ast.TStmtFunDecl (rename_fun prefix top_vals top_types locals deps fd)
    | Typed_ast.TStmtMultiAssign ma ->
      let tgts = List.map (function
        | Typed_ast.TTargetDiscard -> Typed_ast.TTargetDiscard
        | Typed_ast.TTargetLVal (lv, ty) -> Typed_ast.TTargetLVal (rename_lval prefix top_vals top_types locals deps lv, ty))
        ma.tma_targets
      in
      Typed_ast.TStmtMultiAssign { tma_targets = tgts; tma_rhs = re ma.tma_rhs }
    | Typed_ast.TStmtReturn eo -> Typed_ast.TStmtReturn (Option.map re eo)
    | Typed_ast.TStmtExpr e -> Typed_ast.TStmtExpr (re e)
    | _ -> s.desc
  in
  { s with desc }

let rename_toplevel_stmt prefix top_vals top_types (s : Typed_ast.stmt) : Typed_ast.stmt * string list =
  let deps = ref [] in
  let locals = Hashtbl.create 8 in
  let desc =
    match s.desc with
    | Typed_ast.TStmtVarDecl v ->
      Typed_ast.TStmtVarDecl { v with name = prefix ^ v.name; init = rename_expr prefix top_vals top_types locals deps v.init }
    | Typed_ast.TStmtDestruct d ->
      let bs = List.map (fun (b : Typed_ast.destruct_binding) -> { b with name = Option.map (fun n -> prefix ^ n) b.name }) d.bindings in
      Typed_ast.TStmtDestruct { d with bindings = bs; init = rename_expr prefix top_vals top_types locals deps d.init }
    | Typed_ast.TStmtFunDecl fd ->
      let fd' = rename_fun prefix top_vals top_types locals deps fd in
      Typed_ast.TStmtFunDecl { fd' with tf_name = prefix ^ fd.tf_name }
    | Typed_ast.TStmtExtern exts ->
      Typed_ast.TStmtExtern
        (List.map (fun (e : Typed_ast.extern_decl) -> { e with te_name = prefix ^ e.te_name }) exts)
    | Typed_ast.TStmtTypeDecl tud ->
      let fields' = List.map (fun (f : Typed_ast.user_field_decl) ->
        { f with tuf_default = Option.map (rename_expr prefix top_vals top_types locals deps) f.tuf_default }) tud.tud_fields
      in
      let methods' = List.map (rename_fun prefix top_vals top_types locals deps) tud.tud_methods in
      Typed_ast.TStmtTypeDecl { tud with tud_fields = fields'; tud_methods = methods' }
    | _ -> s.desc
  in
  ({ s with desc }, !deps)

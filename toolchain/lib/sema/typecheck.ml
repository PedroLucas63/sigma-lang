open Sigma_common
open Sigma_diagnostics
open Sigma_syntax

type symbol_info = {
  ty : Ty.t;
  decl_span : Span.t;
}

type env = (string, symbol_info) Hashtbl.t

let check_expr (env : env) (e : Ast.expr) : (Typed_ast.expr, Diagnostic.t) result =
  match e.desc with
  | Ast.ExprInt n ->
    Ok { Typed_ast.desc = Typed_ast.TExprInt n; ty = Ty.Int; span = e.span }
  | Ast.ExprBool b ->
    Ok { Typed_ast.desc = Typed_ast.TExprBool b; ty = Ty.Bool; span = e.span }
  | Ast.ExprIdent name ->
    (match Hashtbl.find_opt env name with
     | Some info ->
       Ok { Typed_ast.desc = Typed_ast.TExprIdent name; ty = info.ty; span = e.span }
     | None ->
       Error
         (Diagnostic.error
            ~hint:(Printf.sprintf "declare '%s' first using 'var %s := ...;'" name name)
            ~code:"E0102"
            ~span:e.span
            (Printf.sprintf "use of undeclared variable '%s'" name)))

let resolve_type_annot (ty_name, ty_span) : (Ty.t, Diagnostic.t) result =
  match Ty.of_string ty_name with
  | Some ty -> Ok ty
  | None ->
    let hint =
      if ty_name = "float" then
        "supported types right now: 'int', 'bool' ('float' will be added in a future slice)"
      else
        "supported types: 'int', 'bool'"
    in
    Error
      (Diagnostic.error
         ~hint
         ~code:"E0101"
         ~span:ty_span
         (Printf.sprintf "unknown or unsupported type '%s'" ty_name))

let default_expr_for_type (ty : Ty.t) (span : Span.t) : Typed_ast.expr =
  match ty with
  | Ty.Int ->
    { Typed_ast.desc = Typed_ast.TExprInt Ty.default_int_value; ty = Ty.Int; span }
  | Ty.Bool ->
    { Typed_ast.desc = Typed_ast.TExprBool Ty.default_bool_value; ty = Ty.Bool; span }

let check_stmt (env : env) (s : Ast.stmt) : (Typed_ast.stmt, Diagnostic.t list) result =
  match s.desc with
  | Ast.StmtExpr e ->
    (match check_expr env e with
     | Ok te -> Ok { Typed_ast.desc = Typed_ast.TStmtExpr te; span = s.span }
     | Error err -> Error [ err ])
  | Ast.StmtVarDecl v ->
    let errs = ref [] in
    (match Hashtbl.find_opt env v.name with
     | None -> ()
     | Some prev ->
       errs :=
         Diagnostic.error
           ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
           ~code:"E0103"
           ~span:v.name_span
           (Printf.sprintf "variable '%s' is already declared in this scope" v.name)
         :: !errs);
    let resolved_annot =
      match v.ty_annot with
      | None -> None
      | Some annot ->
        (match resolve_type_annot annot with
         | Ok ty -> Some ty
         | Error err ->
           errs := err :: !errs;
           None)
    in
    let resolved_init =
      match v.init with
      | None -> None
      | Some init_expr ->
        (match check_expr env init_expr with
         | Ok te -> Some te
         | Error err ->
           errs := err :: !errs;
           None)
    in
    (match List.rev !errs with
     | _ :: _ as errors -> Error errors
     | [] ->
       (match resolved_annot, resolved_init with
        | Some annot_ty, Some init_te when not (Ty.equal annot_ty init_te.ty) ->
          Error
            [
              Diagnostic.error
                ~hint:
                  (Printf.sprintf
                     "Sigma is strongly typed: cannot assign value of type '%s' to variable '%s' of type '%s'"
                     (Ty.to_string init_te.ty)
                     v.name
                     (Ty.to_string annot_ty))
                ~code:"E0104"
                ~span:init_te.span
                (Printf.sprintf "type mismatch: expected '%s', found '%s'"
                   (Ty.to_string annot_ty)
                   (Ty.to_string init_te.ty));
            ]
        | _ ->
          let final_ty, final_init, was_default_initialized =
            match resolved_annot, resolved_init with
            | Some annot_ty, Some init_te ->
              (annot_ty, init_te, false)
            | None, Some init_te ->
              (init_te.ty, init_te, false)
            | Some annot_ty, None ->
              (annot_ty, default_expr_for_type annot_ty v.name_span, true)
            | None, None ->
              (Ty.Int, default_expr_for_type Ty.Int v.name_span, true)
          in
          Hashtbl.replace env v.name { ty = final_ty; decl_span = v.name_span };
          let tdecl =
            {
              Typed_ast.name = v.name;
              name_span = v.name_span;
              ty = final_ty;
              init = final_init;
              was_default_initialized;
            }
          in
          Ok { Typed_ast.desc = Typed_ast.TStmtVarDecl tdecl; span = s.span }))

let check_program (prog : Ast.program) : (Typed_ast.program, Diagnostic.t list) result =
  let env : env = Hashtbl.create 16 in
  let tstmts = ref [] in
  let all_errors = ref [] in
  List.iter
    (fun stmt ->
      match check_stmt env stmt with
      | Ok ts -> tstmts := ts :: !tstmts
      | Error errs -> all_errors := List.rev_append errs !all_errors)
    prog;
  match List.rev !all_errors with
  | [] -> Ok (List.rev !tstmts)
  | errs -> Error errs

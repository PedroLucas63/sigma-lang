open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_ops
open Sema_types

let check_destruct_names (env : env) (names : (string * Span.t) list) : Diagnostic.t list =
  let errs = ref [] in
  let seen : (string, Span.t) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun (name, name_span) ->
      if name <> "_" then begin
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
        (match Hashtbl.find_opt seen name with
         | None -> Hashtbl.replace seen name name_span
         | Some prev_sp ->
           errs :=
             Diagnostic.error
               ~hint:(Printf.sprintf "first bound in this pattern at %s" (Span.to_string prev_sp))
               ~code:"E0103" ~span:name_span
               (Printf.sprintf "duplicate variable '%s' in destructuring pattern" name)
             :: !errs)
      end)
    names;
  List.rev !errs

let check_destruct_decl hooks (env : env) (scope_vars : string list ref) (s_span : Span.t)
    (d : Ast.destruct_decl) : (Typed_ast.stmt, Diagnostic.t list) result =
  let errs = ref (List.rev (check_destruct_names env d.names)) in
  if d.is_const && Option.is_none d.init then
    errs :=
      Diagnostic.error ~code:"E0106" ~span:s_span
        "constant destructuring declaration requires an initializing expression"
      :: !errs;
  let resolved_init =
    match d.init with
    | None -> None
    | Some init_expr ->
      (match hooks.check_expr env init_expr with
       | Ok te -> Some te
       | Error err -> errs := err :: !errs; None)
  in
  let init_ty_opt = Option.map (fun (te : Typed_ast.expr) -> te.ty) resolved_init in
  let resolved_annot =
    match d.ty_annot with
    | None -> None
    | Some annot ->
      (match resolve_type_expr annot init_ty_opt with
       | Ok ty -> Some ty
       | Error err -> errs := err :: !errs; None)
  in
  match List.rev !errs with
  | _ :: _ as errors -> Error errors
  | [] ->
    (match resolved_annot, resolved_init with
     | Some annot_ty, Some init_te when not (Ty.equal annot_ty init_te.ty) ->
       Error
         [
           Diagnostic.error ~code:"E0104" ~span:init_te.span
             (Printf.sprintf "type mismatch in destructuring: expected '%s', found '%s'"
                (Ty.to_string annot_ty) (Ty.to_string init_te.ty));
         ]
     | _ ->
       let final_ty, final_init =
         match resolved_annot, resolved_init with
         | Some annot_ty, Some init_te -> (annot_ty, init_te)
         | None, Some init_te -> (init_te.ty, init_te)
         | Some annot_ty, None -> (annot_ty, default_expr_for_type annot_ty s_span)
         | None, None -> (Ty.Int, default_expr_for_type Ty.Int s_span)
       in
       let pat_len = List.length d.names in
       (match Sema_multi_assign.resolve_destruct_tys ~span:final_init.span final_ty pat_len with
        | Error err -> Error [ err ]
        | Ok elem_tys ->
          begin
            let bindings =
              List.map2
                (fun (name, name_span) elem_ty ->
                  if name = "_" then { Typed_ast.name = None; name_span; ty = elem_ty }
                  else begin
                    Hashtbl.replace env name
                      {
                        ty = elem_ty;
                        decl_span = name_span;
                        is_const = d.is_const;
                        const_kind = (if d.is_const then DeepConst else NotConst);
                        is_ref_param = false;
                        const_int = None;
                      };
                    scope_vars := name :: !scope_vars;
                    { Typed_ast.name = Some name; name_span; ty = elem_ty }
                  end)
                d.names elem_tys
            in
            Ok
              {
                Typed_ast.desc =
                  Typed_ast.TStmtDestruct
                    { bindings; is_const = d.is_const; ty = final_ty; init = final_init };
                span = s_span;
              }
          end))

let check_var_decl hooks (env : env) (scope_vars : string list ref) (s_span : Span.t)
    (v : Ast.var_decl) : (Typed_ast.stmt, Diagnostic.t list) result =
  let errs = ref [] in
  (match find_conflict env v.name with
   | None -> ()
   | Some prev ->
     let entity = if prev.is_const then "constant" else "variable" in
     errs :=
       Diagnostic.error
         ~hint:(Printf.sprintf "previously declared at %s" (Span.to_string prev.decl_span))
         ~code:"E0103" ~span:v.name_span
         (Printf.sprintf "%s '%s' is already declared in this scope" entity v.name)
       :: !errs);
  (match v.ty_annot with
   | Some annot when Option.is_none v.init ->
     (match resolve_type_expr annot None with
      | Ok ty when ty_contains_fun ty ->
        errs :=
          Diagnostic.error
            ~hint:"initialize it with a function, e.g. 'var f : int(int) = my_fun;'"
            ~code:"E0106" ~span:v.name_span
            (Printf.sprintf "variable '%s' of function type requires an initializing expression" v.name)
          :: !errs
      | _ -> ())
   | _ -> ());
  if v.is_const && Option.is_none v.init then
    errs :=
      Diagnostic.error
        ~hint:(Printf.sprintf "provide a value, e.g. 'const %s := <value>;'" v.name)
        ~code:"E0106" ~span:v.name_span
        (Printf.sprintf "constant '%s' requires an initializing expression" v.name)
      :: !errs;
  let resolved_init =
    match v.init with
    | None -> None
    | Some init_expr ->
      (match hooks.check_expr env init_expr with
       | Ok te -> Some te
       | Error err -> errs := err :: !errs; None)
  in
  let init_ty_opt = Option.map (fun (te : Typed_ast.expr) -> te.ty) resolved_init in
  let resolved_annot =
    match v.ty_annot with
    | None -> None
    | Some annot ->
      (match resolve_type_expr annot init_ty_opt with
       | Ok ty -> Some ty
       | Error err -> errs := err :: !errs; None)
  in
  match List.rev !errs with
  | _ :: _ as errors -> Error errors
  | [] ->
    (match resolved_annot, resolved_init with
     | Some annot_ty, Some init_te when not (Ty.equal annot_ty init_te.ty) ->
       let entity = if v.is_const then "constant" else "variable" in
       Error
         [
           Diagnostic.error
             ~hint:
               (Printf.sprintf
                  "Sigma is strongly typed: cannot assign value of type '%s' to %s '%s' of type '%s'"
                  (Ty.to_string init_te.ty) entity v.name (Ty.to_string annot_ty))
             ~code:"E0104" ~span:init_te.span
             (Printf.sprintf "type mismatch: expected '%s', found '%s'"
                (Ty.to_string annot_ty) (Ty.to_string init_te.ty));
         ]
     | _ ->
       let def_res =
         match resolved_annot, resolved_init with
         | Some annot_ty, Some init_te -> Ok (annot_ty, init_te, false)
         | None, Some init_te -> Ok (init_te.ty, init_te, false)
         | Some (Ty.User (_, tname) as annot_ty), None ->
           (match Sema_ctors.check_default_user_var hooks env ~span:v.name_span tname with
            | Error err -> Error [ err ]
            | Ok def_e -> Ok (annot_ty, def_e, true))
         | Some annot_ty, None -> Ok (annot_ty, default_expr_for_type annot_ty v.name_span, true)
         | None, None -> Ok (Ty.Int, default_expr_for_type Ty.Int v.name_span, true)
       in
       (match def_res with
        | Error es -> Error es
        | Ok (final_ty, final_init, was_default_initialized) ->
          let const_int =
            if v.is_const && final_ty = Ty.Int then eval_const_int env final_init else None
          in
          Hashtbl.replace env v.name
            {
              ty = final_ty;
              decl_span = v.name_span;
              is_const = v.is_const;
              const_kind = (if v.is_const then DeepConst else NotConst);
              is_ref_param = false;
              const_int;
            };
          (match v.init with
           | Some { Ast.desc = Ast.ExprIdent orig_name; _ } ->
             (match Hashtbl.find_opt var_param_names orig_name with
              | Some pnames -> Hashtbl.replace var_param_names v.name pnames
              | None ->
                (match Hashtbl.find_opt env orig_name with
                 | Some info ->
                   (match Hashtbl.find_opt fun_param_names info.decl_span with
                    | Some pnames -> Hashtbl.replace var_param_names v.name pnames
                    | None -> ())
                 | None -> ()))
           | Some ie ->
             (match Hashtbl.find_opt expr_param_names ie.span with
              | Some pnames -> Hashtbl.replace var_param_names v.name pnames
              | None -> ())
           | None -> ());
          scope_vars := v.name :: !scope_vars;
          let tdecl =
            {
              Typed_ast.name = v.name;
              name_span = v.name_span;
              is_const = v.is_const;
              ty = final_ty;
              init = final_init;
              was_default_initialized;
            }
          in
          Ok { Typed_ast.desc = Typed_ast.TStmtVarDecl tdecl; span = s_span }))

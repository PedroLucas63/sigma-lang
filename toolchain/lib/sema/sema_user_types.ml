open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let ty_kind_of_ast = function
  | Ast.KindClass -> Ty.KindClass
  | Ast.KindStruct -> Ty.KindStruct
  | Ast.KindRecord -> Ty.KindRecord

let rec value_type_reaches (visited : (string, unit) Hashtbl.t) (target : string) (ty : Ty.t) : bool =
  match ty with
  | Ty.Tuple sub -> List.exists (value_type_reaches visited target) sub
  | Ty.Array (sub, _) -> value_type_reaches visited target sub
  | Ty.User (Ty.KindClass, _) -> false
  | Ty.User ((Ty.KindStruct | Ty.KindRecord), name) ->
    if name = target then true
    else if Hashtbl.mem visited name then false
    else begin
      Hashtbl.replace visited name ();
      match Hashtbl.find_opt user_types name with
      | None -> false
      | Some info -> List.exists (fun f -> value_type_reaches visited target f.uf_ty) info.ut_fields
    end
  | _ -> false

let check_type_decl hooks (env : env) ~(is_top_level : bool) (span : Span.t) (td : Ast.type_decl)
    : (Typed_ast.stmt, Diagnostic.t list) result =
  if not is_top_level then
    Error
      [
        Diagnostic.error
          ~hint:"move 'class', 'struct', or 'record' declarations to the top level of the file"
          ~code:"E0002" ~span:td.td_name_span
          (Printf.sprintf "type '%s' can only be declared at top level" td.td_name);
      ]
  else if Hashtbl.mem user_types td.td_name || Hashtbl.mem env td.td_name then
    Error
      [
        Diagnostic.error
          ~code:"E0103" ~span:td.td_name_span
          (Printf.sprintf "name '%s' is already defined in this scope" td.td_name);
      ]
  else begin
    let kind = ty_kind_of_ast td.td_kind in
    Hashtbl.replace user_types td.td_name
      { ut_kind = kind; ut_name = td.td_name; ut_span = td.td_name_span; ut_fields = [] };
    let seen = Hashtbl.create 8 in
    let errs = ref [] in
    let tfields = ref [] in
    let ufields = ref [] in
    List.iteri
      (fun idx (f : Ast.field_decl) ->
        if Hashtbl.mem seen f.field_name then
          errs :=
            Diagnostic.error ~code:"E0103" ~span:f.field_span
              (Printf.sprintf "duplicate field '%s' in '%s'" f.field_name td.td_name)
            :: !errs;
        Hashtbl.replace seen f.field_name ();
        match Sema_types.resolve_type_expr f.field_type None with
        | Error err -> errs := err :: !errs
        | Ok fty ->
          if (kind = Ty.KindStruct || kind = Ty.KindRecord)
             && value_type_reaches (Hashtbl.create 8) td.td_name fty
          then
            errs :=
              Diagnostic.error
                ~hint:"use 'class' instead of 'struct'/'record' for recursive self-referential types"
                ~code:"E0120" ~span:f.field_span
                (Printf.sprintf "recursive value type '%s' cannot contain itself by value" td.td_name)
              :: !errs;
          let def_opt =
            match f.field_default with
            | None -> Some None
            | Some de ->
              (match hooks.check_expr env de with
               | Error err -> errs := err :: !errs; None
               | Ok tde ->
                 if not (Ty.equal tde.ty fty) then begin
                   errs :=
                     Diagnostic.error ~code:"E0104" ~span:tde.span
                       (Printf.sprintf "type mismatch in default value for field '%s': expected '%s', found '%s'"
                          f.field_name (Ty.to_string fty) (Ty.to_string tde.ty))
                     :: !errs;
                   None
                 end else Some (Some tde))
          in
          (match def_opt with
           | Some tdef ->
             tfields := { Typed_ast.tuf_name = f.field_name; tuf_ty = fty; tuf_default = tdef } :: !tfields;
             ufields := { uf_name = f.field_name; uf_idx = idx; uf_ty = fty; uf_default = tdef } :: !ufields
           | None -> ()))
      td.td_fields;
    match List.rev !errs with
    | [] ->
      let rev_ufields = List.rev !ufields in
      Hashtbl.replace user_types td.td_name
        { ut_kind = kind; ut_name = td.td_name; ut_span = td.td_name_span; ut_fields = rev_ufields };
      let tud : Typed_ast.user_type_decl =
        { tud_kind = kind; tud_name = td.td_name; tud_span = td.td_name_span; tud_fields = List.rev !tfields }
      in
      Ok { Typed_ast.desc = Typed_ast.TStmtTypeDecl tud; span }
    | es ->
      Hashtbl.remove user_types td.td_name;
      Error es
  end

let resolve_init_args hooks (env : env) ~span (info : user_type_info) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result =
  let n_fields = List.length info.ut_fields in
  let slots : Typed_ast.expr option array = Array.make n_fields None in
  let fields_arr = Array.of_list info.ut_fields in
  let seen_named = ref false in
  let pos_idx = ref 0 in
  let rec process = function
    | [] -> Ok ()
    | (a : Ast.call_arg) :: rest ->
      if a.arg_mode <> Ast.ArgVal then
        Error
          (Diagnostic.error ~code:"E0104" ~span:a.arg_value.span
             "field initializers cannot use 'ref' or 'out'")
      else
        let target_idx_res =
          match a.arg_name with
          | None ->
            if !seen_named then
              Error
                (Diagnostic.error ~code:"E0002" ~span:a.arg_value.span
                   "positional field argument cannot follow a named argument")
            else if !pos_idx >= n_fields then
              Error
                (Diagnostic.error ~code:"E0119" ~span:a.arg_value.span
                   (Printf.sprintf "too many field arguments for '%s' (expected at most %d)"
                      info.ut_name n_fields))
            else begin
              let idx = !pos_idx in
              incr pos_idx;
              Ok idx
            end
          | Some (fname, fspan) ->
            seen_named := true;
            (match List.find_opt (fun f -> f.uf_name = fname) info.ut_fields with
             | None ->
               Error
                 (Diagnostic.error ~code:"E0119" ~span:fspan
                    (Printf.sprintf "type '%s' has no field named '%s'" info.ut_name fname))
             | Some f ->
               if Option.is_some slots.(f.uf_idx) then
                 Error
                   (Diagnostic.error ~code:"E0103" ~span:fspan
                      (Printf.sprintf "field '%s' specified more than once" fname))
               else Ok f.uf_idx)
        in
        (match target_idx_res with
         | Error e -> Error e
         | Ok idx ->
           (match hooks.check_expr env a.arg_value with
            | Error e -> Error e
            | Ok targ ->
              let expected_ty = fields_arr.(idx).uf_ty in
              if not (Ty.equal targ.ty expected_ty) then
                Error
                  (Diagnostic.error ~code:"E0104" ~span:targ.span
                     (Printf.sprintf "type mismatch for field '%s' of '%s': expected '%s', found '%s'"
                        fields_arr.(idx).uf_name info.ut_name (Ty.to_string expected_ty) (Ty.to_string targ.ty)))
              else begin
                slots.(idx) <- Some targ;
                process rest
              end))
  in
  match process args with
  | Error e -> Error e
  | Ok () ->
    let final_args =
      List.init n_fields (fun i ->
        match slots.(i) with
        | Some e -> e
        | None ->
          (match fields_arr.(i).uf_default with
           | Some d -> d
           | None -> Sema_types.default_expr_for_type fields_arr.(i).uf_ty span))
    in
    Ok
      {
        Typed_ast.desc =
          Typed_ast.TExprStructInit
            { tsi_name = info.ut_name; tsi_kind = info.ut_kind; tsi_args = final_args };
        ty = Ty.User (info.ut_kind, info.ut_name);
        span;
      }

let check_struct_init hooks (env : env) ~span (type_name : string) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result =
  match Hashtbl.find_opt user_types type_name with
  | None ->
    Error
      (Diagnostic.error ~code:"E0101" ~span
         (Printf.sprintf "unknown type '%s'" type_name))
  | Some info ->
    if info.ut_kind = Ty.KindRecord then
      Error
        (Diagnostic.error
           ~hint:(Printf.sprintf "instantiate records with parentheses, e.g. '%s(...)'" type_name)
           ~code:"E0121" ~span
           (Printf.sprintf "record '%s' must be instantiated with parentheses, not '{ ... }'" type_name))
    else resolve_init_args hooks env ~span info args

let check_record_ctor hooks (env : env) ~span (type_name : string) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result option =
  if Hashtbl.mem env type_name then None
  else
    match Hashtbl.find_opt user_types type_name with
    | None -> None
    | Some info ->
      if info.ut_kind = Ty.KindRecord then Some (resolve_init_args hooks env ~span info args)
      else
        Some
          (Error
             (Diagnostic.error
                ~hint:(Printf.sprintf "use '%s { ... }' to initialize fields" type_name)
                ~code:"E0121" ~span
                (Printf.sprintf "type '%s' has no constructor; use '%s { ... }' to initialize fields"
                   type_name type_name)))

let check_field_access hooks (env : env) ~span (base : Ast.expr) (field : string)
    : (Typed_ast.expr, Diagnostic.t) result =
  match hooks.check_expr env base with
  | Error e -> Error e
  | Ok tbase ->
    (match tbase.ty with
     | Ty.User (_, tname) ->
       (match Hashtbl.find_opt user_types tname with
        | Some info ->
          (match List.find_opt (fun f -> f.uf_name = field) info.ut_fields with
           | Some f ->
             Ok { Typed_ast.desc = Typed_ast.TExprField (tbase, field, f.uf_idx); ty = f.uf_ty; span }
           | None ->
             Error
               (Diagnostic.error ~code:"E0119" ~span
                  (Printf.sprintf "type '%s' has no field named '%s'" tname field)))
        | None ->
          Error
            (Diagnostic.error ~code:"E0101" ~span:tbase.span
               (Printf.sprintf "unknown type '%s'" tname)))
     | other ->
       Error
         (Diagnostic.error ~code:"E0119" ~span
            (Printf.sprintf "cannot access field '%s' on value of type '%s'" field (Ty.to_string other))))

let rec root_var_const_kind (env : env) (e : Typed_ast.expr) : const_kind option =
  match e.desc with
  | Typed_ast.TExprIdent name ->
    Option.map (fun info -> info.const_kind) (Hashtbl.find_opt env name)
  | Typed_ast.TExprField (sub, _, _) | Typed_ast.TExprIndex (sub, _) ->
    root_var_const_kind env sub
  | _ -> None

let check_field_lvalue hooks (env : env) ~span ~for_op:_ (base : Ast.expr) (field : string)
    : (Typed_ast.lvalue * Ty.t * string, Diagnostic.t) result =
  match hooks.check_expr env base with
  | Error e -> Error e
  | Ok tbase ->
    (match tbase.ty with
     | Ty.User (kind, tname) ->
       (match Hashtbl.find_opt user_types tname with
        | None ->
          Error (Diagnostic.error ~code:"E0101" ~span:tbase.span (Printf.sprintf "unknown type '%s'" tname))
        | Some info ->
          (match List.find_opt (fun f -> f.uf_name = field) info.ut_fields with
           | None ->
             Error
               (Diagnostic.error ~code:"E0119" ~span
                  (Printf.sprintf "type '%s' has no field named '%s'" tname field))
           | Some f ->
             if kind = Ty.KindRecord then
               Error
                 (Diagnostic.error ~code:"E0107" ~span
                    (Printf.sprintf "cannot mutate field '%s' of immutable record '%s'" field tname))
             else if kind = Ty.KindStruct then
               (match root_var_const_kind env tbase with
                | Some (DeepConst | ShallowConst) ->
                  Error
                    (Diagnostic.error ~code:"E0107" ~span
                       (Printf.sprintf "cannot mutate field '%s' of immutable struct '%s'" field tname))
                | _ -> Ok (Typed_ast.TLValField (tbase, field, f.uf_idx), f.uf_ty, "struct field"))
             else Ok (Typed_ast.TLValField (tbase, field, f.uf_idx), f.uf_ty, "class field")))
     | other ->
       Error
         (Diagnostic.error ~code:"E0119" ~span
            (Printf.sprintf "cannot assign to field '%s' of non-object type '%s'" field (Ty.to_string other))))

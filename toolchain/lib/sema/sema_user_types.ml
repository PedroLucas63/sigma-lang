open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env

let ty_kind_of_ast = function Ast.KindClass -> Ty.KindClass | Ast.KindStruct -> Ty.KindStruct | Ast.KindRecord -> Ty.KindRecord

let check_type_decl hooks (env : env) ~(is_top_level : bool) (span : Span.t) (td : Ast.type_decl)
    : (Typed_ast.stmt, Diagnostic.t list) result =
  if not is_top_level then
    Error [ Diagnostic.error ~hint:"move 'class', 'struct', or 'record' declarations to the top level of the file"
              ~code:"E0002" ~span:td.td_name_span (Printf.sprintf "type '%s' can only be declared at top level" td.td_name) ]
  else if Hashtbl.mem user_types td.td_name || Hashtbl.mem env td.td_name then
    Error [ Diagnostic.error ~code:"E0103" ~span:td.td_name_span
              (Printf.sprintf "name '%s' is already defined in this scope" td.td_name) ]
  else begin
    let kind = ty_kind_of_ast td.td_kind in
    let td_vis = match td.td_vis with Some v -> v | None -> Ast.VisPublic in
    Hashtbl.replace user_types td.td_name
      { ut_kind = kind; ut_vis = td_vis; ut_name = td.td_name; ut_span = td.td_name_span;
        ut_fields = []; ut_ctors = []; ut_methods = [] };
    let primary_ctor_res =
      match td.td_ctor_params with
      | None -> Ok None
      | Some cps ->
        (match Sema_ctors.resolve_ctor_params hooks env cps with
         | Error es -> Error es
         | Ok uc_params ->
           Ok (Some { uc_mangled = Printf.sprintf "__sgm_ctor_%s_0" td.td_name;
                       uc_vis = td_vis; uc_params; uc_is_primary = true }))
    in
    match primary_ctor_res with
    | Error es -> Hashtbl.remove user_types td.td_name; Error es
    | Ok primary_ctor_opt ->
      let field_init_env : env = Hashtbl.copy env in
      (match primary_ctor_opt with
       | None -> ()
       | Some pc ->
         List.iter
           (fun p ->
             Hashtbl.replace field_init_env p.ucp_name
               { ty = p.ucp_ty; decl_span = td.td_name_span; is_const = true;
                 const_kind = ShallowConst; is_ref_param = false; const_int = None })
           pc.uc_params);
      let seen = Hashtbl.create 8 in
      let errs = ref [] in
      let tfields = ref [] in
      let ufields = ref [] in
      List.iteri
        (fun idx (f : Ast.field_decl) ->
          if Hashtbl.mem seen f.field_name then
            errs := Diagnostic.error ~code:"E0103" ~span:f.field_span
                      (Printf.sprintf "duplicate field '%s' in '%s'" f.field_name td.td_name) :: !errs;
          Hashtbl.replace seen f.field_name ();
          let member_vis =
            match f.field_vis with
            | Some v -> v
            | None -> if f.field_is_positional && kind = Ty.KindRecord then Ast.VisPublic else Ast.VisPrivate
          in
          match Sema_accessors.resolve_field_accessors ~kind ~field_name:f.field_name ~member_vis f.field_accessors with
          | Error err -> errs := err :: !errs
          | Ok (uf_get, uf_set, uf_init) ->
            (match Sema_types.resolve_type_expr f.field_type None with
             | Error err -> errs := err :: !errs
             | Ok fty ->
               if (kind = Ty.KindStruct || kind = Ty.KindRecord)
                  && Sema_accessors.value_type_reaches (Hashtbl.create 8) td.td_name fty
               then
                 errs := Diagnostic.error
                           ~hint:"use 'class' instead of 'struct'/'record' for recursive self-referential types"
                           ~code:"E0120" ~span:f.field_span
                           (Printf.sprintf "recursive value type '%s' cannot contain itself by value" td.td_name) :: !errs;
               let def_opt =
                 match f.field_default with
                 | None -> Some None
                 | Some de ->
                   (match hooks.check_expr field_init_env de with
                    | Error err -> errs := err :: !errs; None
                    | Ok tde ->
                      if not (Ty.equal tde.ty fty) then begin
                        errs := Diagnostic.error ~code:"E0104" ~span:tde.span
                                  (Printf.sprintf "type mismatch in default value for field '%s': expected '%s', found '%s'"
                                     f.field_name (Ty.to_string fty) (Ty.to_string tde.ty)) :: !errs;
                        None
                      end else Some (Some tde))
               in
               (match def_opt with
                | Some tdef ->
                  tfields := { Typed_ast.tuf_name = f.field_name; tuf_ty = fty; tuf_default = tdef } :: !tfields;
                  ufields := { uf_name = f.field_name; uf_idx = idx; uf_ty = fty;
                               uf_vis = member_vis; uf_get; uf_set; uf_init; uf_default = tdef } :: !ufields
                | None -> ())))
        td.td_fields;
      match List.rev !errs with
      | _ :: _ as es -> Hashtbl.remove user_types td.td_name; Error es
      | [] ->
        let rev_ufields = List.rev !ufields in
        let rev_tfields = List.rev !tfields in
        let start_idx = if Option.is_some primary_ctor_opt then 1 else 0 in
        (match Sema_ctors.check_general_ctor_sigs hooks env ~type_name:td.td_name ~start_idx td.td_ctors,
               Sema_methods.check_method_sigs ~type_name:td.td_name ~seen_members:seen td.td_methods with
         | Error ces, _ -> Hashtbl.remove user_types td.td_name; Error ces
         | _, Error mes -> Hashtbl.remove user_types td.td_name; Error mes
         | Ok gen_ctors, Ok umethods ->
           let all_ctors =
             (match primary_ctor_opt with Some pc -> [ pc ] | None -> []) @ gen_ctors
           in
           let info_mid =
             { ut_kind = kind; ut_vis = td_vis; ut_name = td.td_name; ut_span = td.td_name_span;
               ut_fields = rev_ufields; ut_ctors = all_ctors; ut_methods = umethods }
           in
           Hashtbl.replace user_types td.td_name info_mid;
           let loop_ref = ref 10000 in
           let primary_fun_list =
             match primary_ctor_opt with
             | Some pc -> [ Sema_ctors.build_primary_ctor_fun ~type_name:td.td_name ~kind ~span pc rev_tfields ]
             | None -> []
           in
           (match Sema_ctors.check_general_ctor_bodies hooks env loop_ref ~info:info_mid td.td_ctors gen_ctors,
                  Sema_methods.check_method_bodies hooks env loop_ref ~type_name:td.td_name ~kind
                    td.td_methods umethods with
            | Error cbes, _ -> Hashtbl.remove user_types td.td_name; Error cbes
            | _, Error bes -> Hashtbl.remove user_types td.td_name; Error bes
            | Ok gen_ctor_funs, Ok (final_ums, typed_methods) ->
              Hashtbl.replace user_types td.td_name { info_mid with ut_methods = final_ums };
              let tud : Typed_ast.user_type_decl =
                { tud_kind = kind; tud_name = td.td_name; tud_span = td.td_name_span;
                  tud_fields = rev_tfields; tud_methods = primary_fun_list @ gen_ctor_funs @ typed_methods }
              in
              Ok { Typed_ast.desc = Typed_ast.TStmtTypeDecl tud; span }))
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
           let uf = fields_arr.(idx) in
           let init_check =
             match uf.uf_init, uf.uf_set with
             | None, None ->
               Error
                 (Diagnostic.error ~code:"E0125" ~span:a.arg_value.span
                    (Printf.sprintf "property '%s' of '%s' is read-only and cannot be initialized in 'new'"
                       uf.uf_name info.ut_name))
             | Some v, _ | None, Some v ->
               if is_vis_accessible_in_type info.ut_name v then Ok ()
               else
                 Error
                   (Diagnostic.error ~code:"E0124" ~span:a.arg_value.span
                      (Printf.sprintf "cannot initialize %s property '%s' of '%s'"
                         (vis_to_string v) uf.uf_name info.ut_name))
           in
           (match init_check with
            | Error e -> Error e
            | Ok () ->
              (match hooks.check_expr env a.arg_value with
               | Error e -> Error e
               | Ok targ ->
                 let expected_ty = uf.uf_ty in
                 if not (Ty.equal targ.ty expected_ty) then
                   Error
                     (Diagnostic.error ~code:"E0104" ~span:targ.span
                        (Printf.sprintf "type mismatch for field '%s' of '%s': expected '%s', found '%s'"
                           uf.uf_name info.ut_name (Ty.to_string expected_ty) (Ty.to_string targ.ty)))
                 else begin
                   slots.(idx) <- Some targ;
                   process rest
                 end)))
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

let check_new_expr hooks (env : env) ~span ~(is_ctor : bool) (type_name : string)
    (args : Ast.call_arg list) (inits_opt : Ast.call_arg list option) : (Typed_ast.expr, Diagnostic.t) result =
  match Hashtbl.find_opt user_types type_name with
  | None ->
    Error
      (Diagnostic.error ~code:"E0101" ~span
         (Printf.sprintf "unknown type '%s'" type_name))
  | Some info ->
    if info.ut_ctors <> [] then begin
      if not is_ctor then
        Error
          (Diagnostic.error
             ~hint:(Printf.sprintf "use 'new %s(...)' to invoke a constructor" type_name)
             ~code:"E0121" ~span
             (Printf.sprintf "type '%s' has constructor(s); use 'new %s(...)'" type_name type_name))
      else
        match Sema_ctors.resolve_ctor_call hooks env ~span info args with
        | Error e -> Error e
        | Ok ctor_e ->
          (match inits_opt with
           | None -> Ok ctor_e
           | Some inits -> Sema_ctors.check_new_inits hooks env ~span info ctor_e inits)
    end else if is_ctor && info.ut_kind <> Ty.KindRecord then
      Error
        (Diagnostic.error
           ~hint:(Printf.sprintf "use 'new %s { ... }' to initialize fields" type_name)
           ~code:"E0121" ~span
           (Printf.sprintf "type '%s' has no constructor; use 'new %s { ... }' to initialize fields"
              type_name type_name))
    else if (not is_ctor) && info.ut_kind = Ty.KindRecord then
      Error
        (Diagnostic.error
           ~hint:(Printf.sprintf "instantiate records with parentheses, e.g. 'new %s(...)'" type_name)
           ~code:"E0121" ~span
           (Printf.sprintf "record '%s' must be instantiated with parentheses, not '{ ... }'" type_name))
    else
      match resolve_init_args hooks env ~span info args with
      | Error e -> Error e
      | Ok ctor_e ->
        (match inits_opt with
         | None -> Ok ctor_e
         | Some inits -> Sema_ctors.check_new_inits hooks env ~span info ctor_e inits)

let check_struct_init _hooks _env ~span (type_name : string) (_args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result =
  match Hashtbl.find_opt user_types type_name with
  | None -> Error (Diagnostic.error ~code:"E0101" ~span (Printf.sprintf "unknown type '%s'" type_name))
  | Some info -> Sema_accessors.err_missing_new ~span info

let check_record_ctor _hooks (env : env) ~span (type_name : string) (_args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result option =
  if Hashtbl.mem env type_name then None
  else
    match Hashtbl.find_opt user_types type_name with
    | None -> None
    | Some info -> Some (Sema_accessors.err_missing_new ~span info)

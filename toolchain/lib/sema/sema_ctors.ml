open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_types

let resolve_ctor_params (hooks : hooks) (env : env) (cps : Ast.ctor_param list)
    : (user_ctor_param list, Diagnostic.t list) result =
  let seen = Hashtbl.create 8 in
  let errs = ref [] in
  let out = ref [] in
  List.iter
    (fun (cp : Ast.ctor_param) ->
      if Hashtbl.mem seen cp.cp_name then
        errs := Diagnostic.error ~code:"E0112" ~span:cp.cp_span
                  (Printf.sprintf "duplicate constructor parameter '%s'" cp.cp_name) :: !errs;
      Hashtbl.replace seen cp.cp_name ();
      match resolve_type_expr cp.cp_type None with
      | Error err -> errs := err :: !errs
      | Ok ty ->
        if ty = Ty.Void then
          errs := Diagnostic.error ~code:"E0113" ~span:cp.cp_span "constructor parameter cannot have type 'void'" :: !errs
        else
          match cp.cp_default with
          | None -> out := { ucp_name = cp.cp_name; ucp_ty = ty; ucp_default = None } :: !out
          | Some de ->
            (match hooks.check_expr env de with
             | Error err -> errs := err :: !errs
             | Ok tde ->
               if not (Ty.equal tde.ty ty) then
                 errs := Diagnostic.error ~code:"E0104" ~span:tde.span
                           (Printf.sprintf "default value type mismatch for parameter '%s': expected '%s', found '%s'"
                              cp.cp_name (Ty.to_string ty) (Ty.to_string tde.ty)) :: !errs
               else out := { ucp_name = cp.cp_name; ucp_ty = ty; ucp_default = Some tde } :: !out))
    cps;
  match List.rev !errs with [] -> Ok (List.rev !out) | es -> Error es

let build_primary_ctor_fun ~type_name ~kind ~span (uc : user_ctor)
    (tfields : Typed_ast.user_field_decl list) : Typed_ast.fun_decl =
  let ret_ty = Ty.User (kind, type_name) in
  let field_args =
    List.map
      (fun (f : Typed_ast.user_field_decl) ->
        match f.tuf_default with Some d -> d | None -> default_expr_for_type f.tuf_ty span)
      tfields
  in
  let init_e =
    { Typed_ast.desc = Typed_ast.TExprStructInit { tsi_name = type_name; tsi_kind = kind; tsi_args = field_args };
      ty = ret_ty; span }
  in
  let params =
    List.map (fun p -> { Typed_ast.tp_name = p.ucp_name; tp_ty = p.ucp_ty; tp_mode = Ty.ParamVal }) uc.uc_params
  in
  { Typed_ast.tf_name = uc.uc_mangled; tf_name_span = span; tf_params = params; tf_ret = ret_ty;
    tf_captures = []; tf_body = [ { Typed_ast.desc = Typed_ast.TStmtReturn (Some init_e); span } ] }

let try_match_ctor hooks (env : env) ~span (info : user_type_info) (uc : user_ctor) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result =
  let n_params = List.length uc.uc_params in
  let params_arr = Array.of_list uc.uc_params in
  let slots : Typed_ast.expr option array = Array.make n_params None in
  let seen_named = ref false in
  let pos_idx = ref 0 in
  let rec process = function
    | [] -> Ok ()
    | (a : Ast.call_arg) :: rest ->
      if a.arg_mode <> Ast.ArgVal then
        Error (Diagnostic.error ~code:"E0104" ~span:a.arg_value.span "constructor arguments cannot use 'ref' or 'out'")
      else
        let idx_res =
          match a.arg_name with
          | None ->
            if !seen_named then
              Error (Diagnostic.error ~code:"E0002" ~span:a.arg_value.span "positional argument cannot follow a named argument")
            else if !pos_idx >= n_params then
              Error (Diagnostic.error ~code:"E0114" ~span:a.arg_value.span
                       (Printf.sprintf "too many arguments for constructor of '%s'" info.ut_name))
            else let i = !pos_idx in (incr pos_idx; Ok i)
          | Some (nm, nsp) ->
            seen_named := true;
            let rec find i =
              if i >= n_params then
                Error (Diagnostic.error ~code:"E0114" ~span:nsp
                         (Printf.sprintf "constructor of '%s' has no parameter named '%s'" info.ut_name nm))
              else if params_arr.(i).ucp_name = nm then
                if Option.is_some slots.(i) then
                  Error (Diagnostic.error ~code:"E0112" ~span:nsp (Printf.sprintf "duplicate argument '%s'" nm))
                else Ok i
              else find (i + 1)
            in
            find 0
        in
        (match idx_res with
         | Error e -> Error e
         | Ok idx ->
           (match hooks.check_expr env a.arg_value with
            | Error e -> Error e
            | Ok targ ->
              if not (Ty.equal targ.ty params_arr.(idx).ucp_ty) then
                Error (Diagnostic.error ~code:"E0104" ~span:targ.span
                         (Printf.sprintf "type mismatch for parameter '%s': expected '%s', found '%s'"
                            params_arr.(idx).ucp_name (Ty.to_string params_arr.(idx).ucp_ty) (Ty.to_string targ.ty)))
              else (slots.(idx) <- Some targ; process rest)))
  in
  match process args with
  | Error e -> Error e
  | Ok () ->
    let rec fill i acc =
      if i >= n_params then Ok (List.rev acc)
      else
        match slots.(i) with
        | Some e -> fill (i + 1) ({ Typed_ast.tca_idx = i; tca_expr = e; tca_mode = Ty.ParamVal;
                                     tca_is_out_var = false; tca_is_this = false } :: acc)
        | None ->
          (match params_arr.(i).ucp_default with
           | Some d -> fill (i + 1) ({ Typed_ast.tca_idx = i; tca_expr = d; tca_mode = Ty.ParamVal;
                                        tca_is_out_var = false; tca_is_this = false } :: acc)
           | None ->
             Error (Diagnostic.error ~code:"E0114" ~span
                      (Printf.sprintf "missing required argument '%s' for constructor of '%s'"
                         params_arr.(i).ucp_name info.ut_name)))
    in
    (match fill 0 [] with
     | Error e -> Error e
     | Ok targs ->
       if not (is_vis_accessible_in_type info.ut_name uc.uc_vis) then
         Error (Diagnostic.error ~code:"E0124" ~span
                  (Printf.sprintf "constructor of '%s' is %s and inaccessible here"
                     info.ut_name (vis_to_string uc.uc_vis)))
       else
         let fn_ty =
           Ty.Fun (List.map (fun p -> { Ty.pty = p.ucp_ty; pmode = Ty.ParamVal }) uc.uc_params,
                   Ty.User (info.ut_kind, info.ut_name))
         in
         let callee = { Typed_ast.desc = Typed_ast.TExprIdent uc.uc_mangled; ty = fn_ty; span } in
         Ok { Typed_ast.desc = Typed_ast.TExprCall { callee; args = targs; full = true };
              ty = Ty.User (info.ut_kind, info.ut_name); span })

let resolve_ctor_call hooks (env : env) ~span (info : user_type_info) (args : Ast.call_arg list)
    : (Typed_ast.expr, Diagnostic.t) result =
  let rec try_all first_err = function
    | [] ->
      (match first_err with
       | Some e -> Error e
       | None -> Error (Diagnostic.error ~code:"E0121" ~span (Printf.sprintf "no matching constructor for '%s'" info.ut_name)))
    | uc :: rest ->
      (match try_match_ctor hooks env ~span info uc args with
       | Ok _ as ok -> ok
       | Error e -> try_all (match first_err with None -> Some e | some -> some) rest)
  in
  try_all None info.ut_ctors

let check_new_inits hooks (env : env) ~span (info : user_type_info) (ctor_e : Typed_ast.expr)
    (inits : Ast.call_arg list) : (Typed_ast.expr, Diagnostic.t) result =
  let seen = Hashtbl.create 8 in
  let rec loop acc = function
    | [] -> Ok { Typed_ast.desc = Typed_ast.TExprNewWithInits { nwi_ctor = ctor_e; nwi_inits = List.rev acc };
                 ty = ctor_e.ty; span }
    | (a : Ast.call_arg) :: rest ->
      if a.arg_mode <> Ast.ArgVal then
        Error (Diagnostic.error ~code:"E0104" ~span:a.arg_value.span "object initializer cannot use 'ref' or 'out'")
      else
        match a.arg_name with
        | None ->
          Error (Diagnostic.error ~code:"E0002" ~span:a.arg_value.span
                   "object initializer after constructor arguments requires named 'field = value' assignments")
        | Some (fname, fspan) ->
          if Hashtbl.mem seen fname then
            Error (Diagnostic.error ~code:"E0103" ~span:fspan (Printf.sprintf "field '%s' initialized more than once" fname))
          else begin
            Hashtbl.replace seen fname ();
            match List.find_opt (fun f -> f.uf_name = fname) info.ut_fields with
            | None ->
              Error (Diagnostic.error ~code:"E0119" ~span:fspan
                       (Printf.sprintf "type '%s' has no field named '%s'" info.ut_name fname))
            | Some f ->
              let acc_check =
                match f.uf_init, f.uf_set with
                | None, None ->
                  Error (Diagnostic.error ~code:"E0125" ~span:fspan
                           (Printf.sprintf "property '%s' of '%s' is read-only" fname info.ut_name))
                | Some v, _ | None, Some v ->
                  if is_vis_accessible_in_type info.ut_name v then Ok ()
                  else Error (Diagnostic.error ~code:"E0124" ~span:fspan
                                (Printf.sprintf "cannot initialize %s property '%s' of '%s'"
                                   (vis_to_string v) fname info.ut_name))
              in
              (match acc_check with
               | Error e -> Error e
               | Ok () ->
                 match hooks.check_expr env a.arg_value with
                 | Error e -> Error e
                 | Ok tv ->
                   if not (Ty.equal tv.ty f.uf_ty) then
                     Error (Diagnostic.error ~code:"E0104" ~span:tv.span
                              (Printf.sprintf "type mismatch for '%s': expected '%s', found '%s'"
                                 fname (Ty.to_string f.uf_ty) (Ty.to_string tv.ty)))
                   else loop ((f.uf_idx, tv) :: acc) rest)
          end
  in
  loop [] inits

let check_general_ctor_sigs hooks (env : env) ~(type_name : string) ~(start_idx : int)
    (ctors : Ast.ctor_decl list) : (user_ctor list, Diagnostic.t list) result =
  let errs = ref [] in
  let out = ref [] in
  List.iteri
    (fun i (cd : Ast.ctor_decl) ->
      if cd.cd_name <> type_name then
        errs := Diagnostic.error ~code:"E0002" ~span:cd.cd_name_span
                  (Printf.sprintf "constructor name '%s' must match enclosing type '%s'" cd.cd_name type_name) :: !errs
      else
        match resolve_ctor_params hooks env cd.cd_params with
        | Error es -> errs := List.rev_append es !errs
        | Ok uc_params ->
          let uc_vis = match cd.cd_vis with Some v -> v | None -> Ast.VisPrivate in
          out := { uc_mangled = Printf.sprintf "__sgm_ctor_%s_%d" type_name (start_idx + i);
                   uc_vis; uc_params; uc_is_primary = false } :: !out)
    ctors;
  match List.rev !errs with [] -> Ok (List.rev !out) | es -> Error es

let check_general_ctor_bodies hooks (env : env) (next_loop_id : int ref) ~(info : user_type_info)
    (ctors : Ast.ctor_decl list) (ucs : user_ctor list) : (Typed_ast.fun_decl list, Diagnostic.t list) result =
  let errs = ref [] in
  let funs = ref [] in
  let self_ty = Ty.User (info.ut_kind, info.ut_name) in
  List.iter2
    (fun (cd : Ast.ctor_decl) (uc : user_ctor) ->
      let body_env : env = Hashtbl.copy env in
      let outer : (string, unit) Hashtbl.t = Hashtbl.create 16 in
      Hashtbl.iter (fun k _ -> Hashtbl.replace outer k ()) env;
      let saved_fun = !current_fun and saved_outer = !fn_outer
      and saved_type = !current_type_ctx and saved_ctor = !in_ctor in
      current_fun := RetExplicit Ty.Void;
      fn_outer := Some outer;
      current_type_ctx := Some (info.ut_name, info.ut_kind, false);
      in_ctor := true;
      List.iter
        (fun p ->
          Hashtbl.remove outer p.ucp_name;
          Hashtbl.replace body_env p.ucp_name
            { ty = p.ucp_ty; decl_span = cd.cd_name_span; is_const = true;
              const_kind = ShallowConst; is_ref_param = false; const_int = None })
        uc.uc_params;
      let init_res =
        match cd.cd_chain with
        | Some cargs -> resolve_ctor_call hooks body_env ~span:cd.cd_name_span info cargs
        | None ->
          let fargs =
            List.map (fun f -> match f.uf_default with Some d -> d | None -> default_expr_for_type f.uf_ty cd.cd_span)
              info.ut_fields
          in
          Ok { Typed_ast.desc = Typed_ast.TExprStructInit
                   { tsi_name = info.ut_name; tsi_kind = info.ut_kind; tsi_args = fargs };
               ty = self_ty; span = cd.cd_span }
      in
      (match init_res with
       | Error e ->
         current_fun := saved_fun; fn_outer := saved_outer; current_type_ctx := saved_type; in_ctor := saved_ctor;
         errs := e :: !errs
       | Ok this_init ->
         Hashtbl.remove outer "this";
         Hashtbl.replace body_env "this"
           { ty = self_ty; decl_span = cd.cd_span; is_const = true;
             const_kind = NotConst; is_ref_param = false; const_int = None };
         let this_decl =
           { Typed_ast.desc = Typed_ast.TStmtVarDecl
                 { name = "this"; name_span = cd.cd_span; is_const = false; ty = self_ty;
                   init = this_init; was_default_initialized = false }; span = cd.cd_span }
         in
         let body_scope = ref ("this" :: List.map (fun p -> p.ucp_name) uc.uc_params) in
         let berrs = ref [] and tstmts = ref [] in
         List.iter (fun s -> match hooks.check_stmt body_env body_scope [] next_loop_id s with
            | Ok ts -> tstmts := ts :: !tstmts | Error es -> berrs := List.rev_append es !berrs) cd.cd_body;
         current_fun := saved_fun; fn_outer := saved_outer; current_type_ctx := saved_type; in_ctor := saved_ctor;
         match List.rev !berrs with
         | _ :: _ as es -> errs := List.rev_append es !errs
         | [] ->
           let ret_this =
             { Typed_ast.desc = Typed_ast.TStmtReturn
                   (Some { Typed_ast.desc = Typed_ast.TExprIdent "this"; ty = self_ty; span = cd.cd_span }); span = cd.cd_span }
           in
           let tparams = List.map (fun p -> { Typed_ast.tp_name = p.ucp_name; tp_ty = p.ucp_ty; tp_mode = Ty.ParamVal }) uc.uc_params in
           funs := { Typed_ast.tf_name = uc.uc_mangled; tf_name_span = cd.cd_name_span; tf_params = tparams;
                     tf_ret = self_ty; tf_captures = []; tf_body = (this_decl :: List.rev !tstmts) @ [ ret_this ] } :: !funs))
    ctors ucs;
  match List.rev !errs with [] -> Ok (List.rev !funs) | es -> Error es

let check_default_user_var hooks (env : env) ~span (tname : string) : (Typed_ast.expr, Diagnostic.t) result =
  match Hashtbl.find_opt user_types tname with
  | None -> Ok (default_expr_for_type (Ty.User (Ty.KindClass, tname)) span)
  | Some info when info.ut_ctors = [] -> Ok (default_expr_for_type (Ty.User (info.ut_kind, tname)) span)
  | Some info ->
    match resolve_ctor_call hooks env ~span info [] with
    | Ok e -> Ok e
    | Error _ -> Error (Diagnostic.error ~code:"E0121" ~span
                          (Printf.sprintf "type '%s' requires constructor arguments; initialize with 'new %s(...)'" tname tname))

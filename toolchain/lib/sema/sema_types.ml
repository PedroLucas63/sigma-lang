open Sigma_common
open Sigma_diagnostics
open Sigma_syntax

let resolve_input_type (ty_name : string) (sp : Span.t) : (Ty.t, Diagnostic.t) result =
  match Ty.of_string ty_name with
  | Some ty -> Ok ty
  | None ->
    Error
      (Diagnostic.error
         ~hint:"supported input types are 'int', 'float', 'double', 'decimal', 'bool', 'char', and 'string'"
         ~code:"E0101" ~span:sp
         (Printf.sprintf "unknown or unsupported input type '%s'" ty_name))

let rec resolve_type_expr ?(allow_unsized = false) (te : Ast.type_expr) (init_ty_opt : Ty.t option)
    : (Ty.t, Diagnostic.t) result =
  match te with
  | Ast.TypeNamed (ty_name, ty_span) ->
    (match Ty.of_string ty_name with
     | Some ty -> Ok ty
     | None ->
       (match Hashtbl.find_opt Sema_env.user_types ty_name with
        | Some info -> Ok (Ty.User (info.ut_kind, info.ut_name))
        | None when ty_name = "void" ->
          Error
            (Diagnostic.error
               ~hint:"'void' only appears as the return type of a function type, e.g. 'void(int)' for an 'act'"
               ~code:"E0113" ~span:ty_span
               "'void' cannot be used as the type of a value")
        | None ->
          Error
            (Diagnostic.error
               ~hint:"supported types: 'int', 'float', 'double', 'decimal', 'bool', 'char', 'string', user types, tuples '(T1, T2)', or arrays 'T[N]'"
               ~code:"E0101" ~span:ty_span
               (Printf.sprintf "unknown or unsupported type '%s'" ty_name))))
  | Ast.TypeWildcard sp ->
    Error (Diagnostic.error ~code:"E0143" ~span:sp "wildcard '?' can only be used inside generic type arguments, e.g. 'Caixa<?>'")
  | Ast.TypeGeneric (ty_name, arg_tes, ty_span) ->
    (match Hashtbl.find_opt Sema_generics.generic_types ty_name with
     | None ->
       Error (Diagnostic.error ~code:"E0101" ~span:ty_span (Printf.sprintf "unknown generic type '%s'" ty_name))
     | Some td ->
       if List.exists (function Ast.TypeWildcard _ -> true | _ -> false) arg_tes then
         let kind = match td.td_kind with Ast.KindClass -> Ty.KindClass | Ast.KindStruct -> Ty.KindStruct | Ast.KindRecord -> Ty.KindRecord in
         Ok (Ty.WildcardUser (kind, ty_name))
       else
         let rec res_args acc = function
           | [] -> Ok (List.rev acc)
           | a :: rest ->
             (match resolve_type_expr a None with
              | Error e -> Error e
              | Ok ty -> res_args (ty :: acc) rest)
         in
         (match res_args [] arg_tes with
          | Error e -> Error e
          | Ok arg_tys -> !Sema_generics.instantiate_type_hook ty_span ty_name arg_tys))
  | Ast.TypeFun (ret_te, param_tes, _) ->
    let ret_res =
      match ret_te with
      | Ast.TypeNamed ("void", _) -> Ok Ty.Void
      | _ -> resolve_type_expr ret_te None
    in
    (match ret_res with
     | Error err -> Error err
     | Ok ret_ty ->
       let rec params acc = function
         | [] -> Ok (List.rev acc)
         | (pmode, pte) :: rest ->
           let ty_pmode =
             match pmode with
             | Ast.ParamVal -> Ty.ParamVal
             | Ast.ParamRef -> Ty.ParamRef
             | Ast.ParamOut -> Ty.ParamOut
           in
           (match resolve_type_expr pte None with
            | Error err -> Error err
            | Ok pty -> params ({ Ty.pty; pmode = ty_pmode } :: acc) rest)
       in
       (match params [] param_tes with
        | Error err -> Error err
        | Ok ptys -> Ok (Ty.Fun (ptys, ret_ty))))
  | Ast.TypeTuple (sub_tes, _) ->
    let init_subs =
      match init_ty_opt with
      | Some (Ty.Tuple tys) when List.length tys = List.length sub_tes -> List.map Option.some tys
      | _ -> List.map (fun _ -> None) sub_tes
    in
    let rec loop acc = function
      | [], [] -> Ok (Ty.Tuple (List.rev acc))
      | sub_te :: r1, sub_init :: r2 ->
        (match resolve_type_expr sub_te sub_init with
         | Error err -> Error err
         | Ok sub_ty -> loop (sub_ty :: acc) (r1, r2))
      | _ -> Ok (Ty.Tuple (List.rev acc))
    in
    loop [] (sub_tes, init_subs)
  | Ast.TypeArray (elem_te, size_opt, sp) ->
    let init_elem_opt, init_len_opt =
      match init_ty_opt with
      | Some (Ty.Array (ie_ty, ilen)) -> (Some ie_ty, Some ilen)
      | _ -> (None, None)
    in
    (match resolve_type_expr elem_te init_elem_opt with
     | Error err -> Error err
     | Ok elem_ty ->
       (match size_opt with
        | Some n ->
          if n <= 0 then
            Error
              (Diagnostic.error ~hint:"specify a positive array size >= 1, e.g. 'int[3]'"
                 ~code:"E0108" ~span:sp
                 (Printf.sprintf "array size must be greater than 0, found %d" n))
          else Ok (Ty.Array (elem_ty, n))
        | None ->
          (match init_len_opt, init_ty_opt with
           | Some ilen, _ -> Ok (Ty.Array (elem_ty, ilen))
           | None, None when allow_unsized -> Ok (Ty.DynArray elem_ty)
           | None, None ->
             Error
               (Diagnostic.error
                  ~hint:(Printf.sprintf "provide an initializer like 'var a : %s = [...];' or specify a fixed size like '%s[3]'"
                           (Ast.type_expr_to_string te) (Ty.to_string elem_ty))
                  ~code:"E0106" ~span:sp
                  (Printf.sprintf "array type '%s' with omitted size requires an initializing expression to infer its length"
                     (Ast.type_expr_to_string te)))
           | None, Some bad_init_ty ->
             Error
               (Diagnostic.error ~code:"E0104" ~span:sp
                  (Printf.sprintf "type mismatch: expected array type '%s', found '%s'"
                     (Ast.type_expr_to_string te) (Ty.to_string bad_init_ty))))))

let rec default_expr_for_type ?(visited = []) (ty : Ty.t) (span : Span.t) : Typed_ast.expr =
  match ty with
  | Ty.Int -> { Typed_ast.desc = Typed_ast.TExprInt Ty.default_int_value; ty = Ty.Int; span }
  | Ty.Float -> { Typed_ast.desc = Typed_ast.TExprFloat Ty.default_float_value; ty = Ty.Float; span }
  | Ty.Double -> { Typed_ast.desc = Typed_ast.TExprDouble Ty.default_float_value; ty = Ty.Double; span }
  | Ty.Decimal -> { Typed_ast.desc = Typed_ast.TExprDecimal Ty.default_decimal_value; ty = Ty.Decimal; span }
  | Ty.Bool -> { Typed_ast.desc = Typed_ast.TExprBool Ty.default_bool_value; ty = Ty.Bool; span }
  | Ty.Char -> { Typed_ast.desc = Typed_ast.TExprChar Ty.default_char_value; ty = Ty.Char; span }
  | Ty.String -> { Typed_ast.desc = Typed_ast.TExprString Ty.default_string_value; ty = Ty.String; span }
  | Ty.Tuple tys ->
    let items = List.map (fun sub_ty -> Typed_ast.TItemExpr (default_expr_for_type ~visited sub_ty span)) tys in
    { Typed_ast.desc = Typed_ast.TExprTuple items; ty; span }
  | Ty.Array (elem_ty, n) ->
    let items = List.init n (fun _ -> Typed_ast.TItemExpr (default_expr_for_type ~visited elem_ty span)) in
    { Typed_ast.desc = Typed_ast.TExprArray items; ty; span }
  | Ty.DynArray _ ->
    { Typed_ast.desc = Typed_ast.TExprArray []; ty; span }
  | Ty.User (kind, name) ->
    if List.mem name visited then
      { Typed_ast.desc = Typed_ast.TExprNull; ty; span }
    else
      let visited' = name :: visited in
      let args =
        match Hashtbl.find_opt Sema_env.user_types name with
        | None -> []
        | Some info ->
          List.map
            (fun (f : Sema_env.user_field) ->
              match f.uf_default with
              | Some d -> d
              | None -> default_expr_for_type ~visited:visited' f.uf_ty span)
            info.ut_fields
      in
      { Typed_ast.desc = Typed_ast.TExprStructInit { tsi_name = name; tsi_kind = kind; tsi_args = args }; ty; span }
  | Ty.Fun _ | Ty.Void | Ty.WildcardUser _ ->
    { Typed_ast.desc = Typed_ast.TExprInt 0L; ty = Ty.Int; span }

let rec stmt_always_returns (s : Typed_ast.stmt) : bool =
  match s.desc with
  | Typed_ast.TStmtReturn _ -> true
  | Typed_ast.TStmtBlock ss -> List.exists stmt_always_returns ss
  | Typed_ast.TStmtIf { then_branch; else_branch = Some eb; _ } ->
    stmt_always_returns then_branch && stmt_always_returns eb
  | _ -> false

let resolve_fun_params (raw_params : Ast.fun_param list)
    : (Typed_ast.typed_param list, Diagnostic.t list) result =
  let errs = ref [] in
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let params =
    List.filter_map
      (fun (p : Ast.fun_param) ->
        let pname = p.param_name in
        if Hashtbl.mem seen pname then
          errs := Diagnostic.error ~code:"E0103" ~span:p.param_span
                    (Printf.sprintf "duplicate parameter '%s'" pname) :: !errs;
        Hashtbl.replace seen pname ();
        let pmode =
          match p.param_mode with
          | Ast.ParamVal -> Ty.ParamVal
          | Ast.ParamRef -> Ty.ParamRef
          | Ast.ParamOut -> Ty.ParamOut
        in
        match resolve_type_expr ~allow_unsized:true p.param_type None with
        | Ok ty -> Some { Typed_ast.tp_name = pname; tp_mode = pmode; tp_ty = ty }
        | Error err -> errs := err :: !errs; None)
      raw_params
  in
  match List.rev !errs with
  | [] -> Ok params
  | es -> Error es

let resolve_fun_signature (fd : Ast.fun_decl)
    : (Typed_ast.typed_param list * Ty.t, Diagnostic.t list) result =
  let param_errs, params =
    match resolve_fun_params fd.fd_params with
    | Ok ps -> ([], ps)
    | Error es -> (es, [])
  in
  let errs = ref (List.rev param_errs) in
  let ret =
    match fd.fd_ret with
    | None -> Some Ty.Void
    | Some (Ast.TypeNamed ("void", sp)) ->
      errs :=
        Diagnostic.error ~hint:"use 'act' to declare a procedure without a return value"
          ~code:"E0113" ~span:sp "a 'fun' must declare a value return type, not 'void'"
        :: !errs;
      None
    | Some te ->
      (match resolve_type_expr te None with
       | Ok ty -> Some ty
       | Error err -> errs := err :: !errs; None)
  in
  match List.rev !errs, ret with
  | [], Some r -> Ok (params, r)
  | es, _ -> Error es

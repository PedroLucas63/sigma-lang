open Sigma_common
open Sigma_diagnostics
open Sigma_syntax
open Sema_env
open Sema_types

type abi =
  | AbiC
  | AbiCpp

let resolve_abi ~span (lang : string) : (abi, Diagnostic.t) result =
  match lang with
  | "C" -> Ok AbiC
  | "C++" -> Ok AbiCpp
  | _ ->
    Error
      (Diagnostic.error
         ~hint:"supported extern ABIs are currently \"C\" and \"C++\""
         ~code:"E0131" ~span
         (Printf.sprintf "unsupported extern language/ABI '%s'" lang))

let is_ffi_scalar_elem = function
  | Ty.Int | Ty.Float | Ty.Double | Ty.Bool | Ty.Char -> true
  | _ -> false

let is_ffi_param_ty = function
  | Ty.Int | Ty.Float | Ty.Double | Ty.Bool | Ty.Char | Ty.String -> true
  | Ty.Array (elem, _) | Ty.DynArray elem -> is_ffi_scalar_elem elem
  | _ -> false

let is_ffi_ret_ty = function
  | Ty.Void | Ty.Int | Ty.Float | Ty.Double | Ty.Bool | Ty.Char | Ty.String -> true
  | _ -> false

let resolve_from_path ~(from_file : string) (from_opt : string option) : string option =
  match from_opt with
  | None -> None
  | Some p ->
    let from_dir =
      if from_file = "" || from_file = "<dummy>" then Sys.getcwd () else Filename.dirname from_file
    in
    let cand = if Filename.is_relative p then Filename.concat from_dir p else p in
    if Sys.file_exists cand then Some (Sema_modules.normalize_path cand)
    else Some p

let check_extern_stmt (env : env) (scope_vars : string list ref) (s_span : Span.t)
    ~ext_lang ~ext_lang_span ~ext_from ~ext_items
    : (Typed_ast.stmt, Diagnostic.t list) result =
  match resolve_abi ~span:ext_lang_span ext_lang with
  | Error e -> Error [ e ]
  | Ok _ ->
    let is_top_level =
      match !root_scope_ref with Some r -> r == scope_vars | None -> false
    in
    if not is_top_level then
      Error
        [ Diagnostic.error ~code:"E0002" ~span:s_span
            "'extern' declarations are only allowed at top level" ]
    else
      let resolved_from = resolve_from_path ~from_file:s_span.Span.file ext_from in
      let errs = ref [] in
      let decls = ref [] in
      List.iter
        (fun (it : Ast.extern_item) ->
          if Hashtbl.mem env it.ext_name && List.mem it.ext_name !scope_vars then
            errs :=
              Diagnostic.error ~code:"E0103" ~span:it.ext_name_span
                (Printf.sprintf "symbol '%s' is already declared in this scope" it.ext_name)
              :: !errs
          else begin
            let pnames_seen = Hashtbl.create 8 in
            let tparams_rev = ref [] in
            let fun_params_rev = ref [] in
            List.iter
              (fun (p : Ast.fun_param) ->
                if Hashtbl.mem pnames_seen p.param_name then
                  errs :=
                    Diagnostic.error ~code:"E0112" ~span:p.param_span
                      (Printf.sprintf "duplicate parameter '%s' in extern '%s'" p.param_name it.ext_name)
                    :: !errs
                else begin
                  Hashtbl.replace pnames_seen p.param_name ();
                  if p.param_mode <> Ast.ParamVal then
                    errs :=
                      Diagnostic.error ~code:"E0132" ~span:p.param_span
                        (Printf.sprintf "'ref' and 'out' parameters are not supported in extern '%s'" it.ext_name)
                      :: !errs
                  else
                    match resolve_type_expr ~allow_unsized:true p.param_type None with
                    | Error e -> errs := e :: !errs
                    | Ok pty ->
                      if not (is_ffi_param_ty pty) then
                        errs :=
                          Diagnostic.error ~code:"E0132" ~span:p.param_span
                            (Printf.sprintf "type '%s' is not supported in extern FFI parameters"
                               (Ty.to_string pty))
                          :: !errs
                      else begin
                        tparams_rev :=
                          {
                            Typed_ast.tp_name = p.param_name;
                            tp_mode = Ty.ParamVal;
                            tp_ty = pty;
                          }
                          :: !tparams_rev;
                        fun_params_rev :=
                          { Ty.pty; pmode = Ty.ParamVal } :: !fun_params_rev
                      end
                end)
              it.ext_params;
            let ret_ty_opt =
              match it.ext_ret with
              | None -> Some Ty.Void
              | Some rt ->
                (match resolve_type_expr rt None with
                 | Error e -> errs := e :: !errs; None
                 | Ok rty ->
                   if not (is_ffi_ret_ty rty) then begin
                     errs :=
                       Diagnostic.error ~code:"E0132" ~span:it.ext_span
                         (Printf.sprintf "return type '%s' is not supported in extern FFI"
                            (Ty.to_string rty))
                       :: !errs;
                     None
                   end else Some rty)
            in
            match ret_ty_opt with
            | None -> ()
            | Some ret_ty ->
              let tparams = List.rev !tparams_rev in
              let fparams = List.rev !fun_params_rev in
              let fn_ty = Ty.Fun (fparams, ret_ty) in
              let pnames = List.map (fun (tp : Typed_ast.typed_param) -> tp.tp_name) tparams in
              Hashtbl.replace env it.ext_name
                {
                  ty = fn_ty;
                  decl_span = it.ext_name_span;
                  is_const = true;
                  const_kind = DeepConst;
                  is_ref_param = false;
                  const_int = None;
                };
              scope_vars := it.ext_name :: !scope_vars;
              Hashtbl.replace fun_param_names it.ext_name_span pnames;
              Hashtbl.replace var_param_names it.ext_name pnames;
              let sym = Option.value it.ext_symbol ~default:it.ext_name in
              decls :=
                {
                  Typed_ast.te_name = it.ext_name;
                  te_name_span = it.ext_name_span;
                  te_lang = ext_lang;
                  te_from = resolved_from;
                  te_symbol = sym;
                  te_params = tparams;
                  te_ret = ret_ty;
                }
                :: !decls
          end)
        ext_items;
      match List.rev !errs with
      | _ :: _ as es -> Error es
      | [] -> Ok { Typed_ast.desc = Typed_ast.TStmtExtern (List.rev !decls); span = s_span }

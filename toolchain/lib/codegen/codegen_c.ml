open Sigma_sema
open Codegen_c_ctx
open Codegen_c_collect

let rec hooks : hooks =
  {
    emit_expr = (fun e -> Codegen_c_expr.emit_expr hooks e);
    emit_stmt = (fun buf indent s -> Codegen_c_stmt.emit_stmt hooks buf indent s);
  }

let emit_fun_def (buf : Buffer.t) (fd : Typed_ast.fun_decl) : unit =
  let c_fn_name =
    match Hashtbl.find_opt fun_c_names fd.tf_name_span with Some n -> n | None -> "sgm_fn_" ^ fd.tf_name
  in
  Buffer.add_string buf
    (Printf.sprintf "static sgm_val_t %s(sgm_val_t *_sgm_args, sgm_fn_t *_sgm_self) {\n  (void)_sgm_args;\n  (void)_sgm_self;\n" c_fn_name);
  Hashtbl.clear current_cells;
  Hashtbl.clear current_boxed;
  Hashtbl.replace current_cells fd.tf_name "(_sgm_self->orig)";
  List.iter (fun name -> Hashtbl.replace current_boxed name ()) (compute_boxed_vars_in_fun fd);
  List.iter
    (fun (name, ty) ->
      Buffer.add_string buf (Printf.sprintf "  %s %s = %s;\n" (c_type_of_ty ty) (mangle_ident name) (emit_default_c_val ty)))
    (List.concat_map collect_out_vars_stmt fd.tf_body);
  List.iteri
    (fun i (cap_name, cap_ty) ->
      Buffer.add_string buf
        (Printf.sprintf "  sgm_val_t *sgm_cell_%s = ((sgm_val_t**)_sgm_self->env)[%d];\n" cap_name i);
      Hashtbl.replace current_cells cap_name (Printf.sprintf "(sgm_cell_%s->u.%s)" cap_name (c_val_union_field cap_ty)))
    fd.tf_captures;
  List.iteri
    (fun i (tp : Typed_ast.typed_param) ->
      match tp.tp_mode with
      | Ty.ParamRef | Ty.ParamOut ->
        let c_type = c_type_of_ty tp.tp_ty in
        let ref_var = Printf.sprintf "_sgm_ref_%s" (mangle_ident tp.tp_name) in
        Buffer.add_string buf (Printf.sprintf "  %s *%s = (%s*)_sgm_args[%d].u.ref_ptr;\n" c_type ref_var c_type i);
        Hashtbl.replace current_cells tp.tp_name (Printf.sprintf "(*%s)" ref_var)
      | Ty.ParamVal ->
        if Hashtbl.mem current_boxed tp.tp_name then begin
          Buffer.add_string buf (Printf.sprintf "  sgm_val_t *sgm_cell_%s = sgm_cell_make(_sgm_args[%d]);\n" tp.tp_name i);
          Hashtbl.replace current_cells tp.tp_name (Printf.sprintf "(sgm_cell_%s->u.%s)" tp.tp_name (c_val_union_field tp.tp_ty))
        end else
          Buffer.add_string buf
            (Printf.sprintf "  %s %s = _sgm_args[%d].u.%s;\n"
               (c_type_of_ty tp.tp_ty) (mangle_ident tp.tp_name) i (c_val_union_field tp.tp_ty)))
    fd.tf_params;
  List.iter (hooks.emit_stmt buf "  ") fd.tf_body;
  Buffer.add_string buf "  return sgm_val_i64(0);\n}\n\n"

let rec emit_toplevel_stmt (buf : Buffer.t) (indent : string) (s : Typed_ast.stmt) : unit =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    let c_init = emit_copy_if_struct v.init (hooks.emit_expr v.init) in
    if Hashtbl.mem current_boxed v.name then begin
      Buffer.add_string buf
        (Printf.sprintf "%ssgm_val_t *sgm_cell_%s = sgm_cell_make(%s);\n"
           indent v.name (emit_box_val v.ty c_init));
      Hashtbl.replace current_cells v.name
        (Printf.sprintf "(sgm_cell_%s->u.%s)" v.name (c_val_union_field v.ty))
    end else
      Buffer.add_string buf (Printf.sprintf "%s%s = %s;\n" indent (mangle_ident v.name) c_init)
  | Typed_ast.TStmtDestruct d ->
    incr chain_counter;
    let tmp_obj = Printf.sprintf "_sgm_destr_%d" !chain_counter in
    let pat_len = List.length d.bindings in
    Buffer.add_string buf (Printf.sprintf "%ssgm_obj_t *%s = %s;\n%s(void)%s;\n" indent tmp_obj (hooks.emit_expr d.init) indent tmp_obj);
    List.iteri
      (fun i (bind : Typed_ast.destruct_binding) ->
        Option.iter
          (fun name ->
            let raw, _ =
              Codegen_c_user_types.destruct_slot_c ~tmp_obj ~rhs_ty:d.ty ~pat_len ~elem_ty:bind.ty i
            in
            Buffer.add_string buf (Printf.sprintf "%s%s = %s;\n" indent (mangle_ident name) raw))
          bind.name)
      d.bindings
  | Typed_ast.TStmtFunDecl fd ->
    let c_fn_name =
      match Hashtbl.find_opt fun_c_names fd.tf_name_span with Some n -> n | None -> "sgm_fn_" ^ fd.tf_name
    in
    let n_caps = List.length fd.tf_captures in
    if n_caps = 0 then
      Buffer.add_string buf
        (Printf.sprintf "%s%s = sgm_fn_make(%s, INT64_C(%d), NULL);\n"
           indent (mangle_ident fd.tf_name) c_fn_name (List.length fd.tf_params))
    else begin
      incr chain_counter;
      let env_var = Printf.sprintf "_sgm_env_%d" !chain_counter in
      Buffer.add_string buf (Printf.sprintf "%ssgm_val_t **%s = (sgm_val_t**)malloc(%d * sizeof(sgm_val_t*));\n" indent env_var n_caps);
      List.iteri (fun j (cap_name, _) -> Buffer.add_string buf (Printf.sprintf "%s%s[%d] = sgm_cell_%s;\n" indent env_var j cap_name)) fd.tf_captures;
      Buffer.add_string buf
        (Printf.sprintf "%s%s = sgm_fn_make(%s, INT64_C(%d), %s);\n"
           indent (mangle_ident fd.tf_name) c_fn_name (List.length fd.tf_params) env_var)
    end
  | Typed_ast.TStmtTypeDecl tud ->
    List.iter
      (fun fd -> emit_toplevel_stmt buf indent { Typed_ast.desc = Typed_ast.TStmtFunDecl fd; span = s.span })
      tud.tud_methods
  | Typed_ast.TStmtExtern exts ->
    List.iter
      (fun (ext : Typed_ast.extern_decl) ->
        let wrap_name = "sgm_ext_" ^ mangle_ident ext.te_name in
        Buffer.add_string buf
          (Printf.sprintf "%s%s = sgm_fn_make(%s, INT64_C(%d), NULL);\n"
             indent (mangle_ident ext.te_name) wrap_name (List.length ext.te_params)))
      exts
  | _ -> hooks.emit_stmt buf indent s

let emit_c ?(test_mode = false) ?entry_name (prog : Typed_ast.program) : string =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "/* Generated by Sigma Compiler (C99 High-Performance Backend) */\n";
  Buffer.add_string buf "#include <stdint.h>\n#include <stdbool.h>\n#include <inttypes.h>\n#include <stdio.h>\n\n";
  Buffer.add_string buf C_rt_decimal.c_runtime_decimal;
  Buffer.add_string buf C_rt_math_str.c_runtime_math_str;
  Buffer.add_string buf C_rt_obj.c_runtime_obj;
  Buffer.add_string buf C_rt_fmt.c_runtime_fmt;
  Buffer.add_string buf C_rt_io.c_runtime_io;
  Buffer.add_string buf "\n__attribute__((unused)) static int sgm_test_passed = 0;\n";
  Buffer.add_string buf "__attribute__((unused)) static int sgm_test_failed = 0;\n";
  Buffer.add_string buf (Printf.sprintf "__attribute__((unused)) static bool sgm_test_mode = %s;\n\n" (if test_mode then "true" else "false"));
  let all_funs = List.concat_map collect_all_funs prog in
  let all_exts = Codegen_c_ffi.collect_externs prog in
  Codegen_c_ffi.emit_extern_decls buf all_exts;
  Hashtbl.clear fun_c_names;
  let fn_counter = ref 0 in
  List.iter
    (fun (fd : Typed_ast.fun_decl) ->
      incr fn_counter;
      Hashtbl.replace fun_c_names fd.tf_name_span (Printf.sprintf "sgm_fn_%s_%d" fd.tf_name !fn_counter))
    all_funs;
  if all_funs <> [] || all_exts <> [] then begin
    List.iter
      (fun (fd : Typed_ast.fun_decl) ->
        let c_name = Hashtbl.find fun_c_names fd.tf_name_span in
        Buffer.add_string buf (Printf.sprintf "static sgm_val_t %s(sgm_val_t *_sgm_args, sgm_fn_t *_sgm_self);\n" c_name))
      all_funs;
    Buffer.add_string buf "\n";
    List.iter
      (fun (name, ty) -> Buffer.add_string buf (Printf.sprintf "static %s %s;\n" (c_type_of_ty ty) (mangle_ident name)))
      (toplevel_decls prog);
    Buffer.add_string buf "\n";
    List.iter (emit_fun_def buf) all_funs
  end;
  Buffer.add_string buf "int main(int argc, char **argv) {\n  (void)argc;\n  (void)argv;\n";
  Hashtbl.clear current_cells;
  Hashtbl.clear current_boxed;
  List.iter (fun name -> Hashtbl.replace current_boxed name ()) (List.concat_map collect_inner_captures prog);
  List.iter (if all_funs <> [] || all_exts <> [] then emit_toplevel_stmt buf "  " else hooks.emit_stmt buf "  ") prog;
  (match Typecheck.detect_entry_point ?entry_name prog with
   | Ok None | Error _ -> Buffer.add_string buf "  return (sgm_test_failed > 0) ? 1 : 0;\n}\n"
   | Ok (Some (fn_name, ep)) ->
     let main_var = mangle_ident fn_name in
     (match ep with
      | Typecheck.EntryActNoArgs ->
        Buffer.add_string buf (Printf.sprintf "  (void)sgm_fn_invoke(%s);\n" main_var);
        Buffer.add_string buf "  return (sgm_test_failed > 0) ? 1 : 0;\n}\n"
      | Typecheck.EntryFunIntNoArgs ->
        Buffer.add_string buf (Printf.sprintf "  sgm_val_t _sgm_entry_ret = sgm_fn_invoke(%s);\n" main_var);
        Buffer.add_string buf "  return (sgm_test_failed > 0) ? 1 : (int)_sgm_entry_ret.u.i64;\n}\n"
      | Typecheck.EntryActArgs | Typecheck.EntryFunIntArgs ->
        Buffer.add_string buf "  sgm_obj_t *_sgm_cli_args = sgm_obj_alloc(false, (int64_t)argc);\n";
        Buffer.add_string buf "  for (int i = 0; i < argc; i++) {\n";
        Buffer.add_string buf "    *sgm_obj_slot(_sgm_cli_args, (int64_t)i) = sgm_val_str(argv[i]);\n";
        Buffer.add_string buf "  }\n";
        Buffer.add_string buf "  sgm_val_t _sgm_entry_argv[1] = { sgm_val_obj(_sgm_cli_args) };\n";
        Buffer.add_string buf "  int64_t _sgm_entry_idxs[1] = { 0 };\n";
        Buffer.add_string buf
          (Printf.sprintf "  sgm_val_t _sgm_entry_ret = sgm_fn_invoke(sgm_fn_bind(%s, 1, _sgm_entry_idxs, _sgm_entry_argv));\n"
             main_var);
        if ep = Typecheck.EntryFunIntArgs then
          Buffer.add_string buf "  return (sgm_test_failed > 0) ? 1 : (int)_sgm_entry_ret.u.i64;\n}\n"
        else begin
          Buffer.add_string buf "  (void)_sgm_entry_ret;\n";
          Buffer.add_string buf "  return (sgm_test_failed > 0) ? 1 : 0;\n}\n"
        end));
  Buffer.contents buf

let write_c_file ?(test_mode = false) ?entry_name ~output_path (prog : Typed_ast.program) : (unit, string) result =
  let c_source = emit_c ~test_mode ?entry_name prog in
  try
    let oc = open_out_bin output_path in
    output_string oc c_source;
    close_out oc;
    Ok ()
  with Sys_error msg -> Error msg

let compile_executable ?(test_mode = false) ?entry_name ~output_path (prog : Typed_ast.program) : (unit, string) result =
  let c_source = emit_c ~test_mode ?entry_name prog in
  let exts = Codegen_c_ffi.collect_externs prog in
  match Codegen_c_ffi.prepare_link_inputs exts with
  | Error msg -> Error msg
  | Ok (link_items, temp_objs, _) ->
    let tmp_c = Filename.temp_file "sigma_build_" ".c" in
    let cleanup () =
      if Sys.file_exists tmp_c then (try Sys.remove tmp_c with _ -> ());
      List.iter (fun o -> if Sys.file_exists o then try Sys.remove o with _ -> ()) temp_objs
    in
    try
      let oc = open_out_bin tmp_c in
      output_string oc c_source;
      close_out oc;
      let extra_link = if link_items = [] then "" else " " ^ String.concat " " link_items in
      let cmd =
        Printf.sprintf "gcc -std=c99 -O3 -Wall -Wextra -Wno-unused-variable %s%s -o %s -lm 2>&1"
          (Filename.quote tmp_c) extra_link (Filename.quote output_path)
      in
      let code = Sys.command cmd in
      cleanup ();
      if code = 0 then Ok () else Error (Printf.sprintf "C backend compilation failed (exit code %d)" code)
    with Sys_error msg -> cleanup (); Error msg

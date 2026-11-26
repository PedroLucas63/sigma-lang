open Sigma_sema
open Codegen_c_ctx
open Codegen_c_ffi

let is_ffi_elem = function
  | Ty.Int | Ty.Float | Ty.Double | Ty.Bool | Ty.Char -> true
  | _ -> false

let is_ffi_param = function
  | Ty.Int | Ty.Float | Ty.Double | Ty.Bool | Ty.Char | Ty.String -> true
  | Ty.Array (e, _) | Ty.DynArray e -> is_ffi_elem e
  | _ -> false

let is_ffi_ret = function
  | Ty.Void | Ty.Int | Ty.Float | Ty.Double | Ty.Bool | Ty.Char | Ty.String -> true
  | _ -> false

let is_exportable_fun (fd : Typed_ast.fun_decl) : bool =
  (not (String.starts_with ~prefix:"__m" fd.tf_name))
  && fd.tf_captures = []
  && List.for_all (fun (tp : Typed_ast.typed_param) -> tp.tp_mode = Ty.ParamVal && is_ffi_param tp.tp_ty) fd.tf_params
  && is_ffi_ret fd.tf_ret

let is_lib_allowed_stmt (s : Typed_ast.stmt) : bool =
  match s.desc with
  | Typed_ast.TStmtFunDecl _
  | Typed_ast.TStmtTypeDecl _
  | Typed_ast.TStmtVarDecl _
  | Typed_ast.TStmtDestruct _
  | Typed_ast.TStmtExtern _
  | Typed_ast.TStmtBlock [] -> true
  | _ -> false

let sigma_ty_str = function
  | Ty.Int -> "int"
  | Ty.Float -> "float"
  | Ty.Double -> "double"
  | Ty.Bool -> "bool"
  | Ty.Char -> "char"
  | Ty.String -> "string"
  | Ty.Void -> "void"
  | Ty.Array (e, _) | Ty.DynArray e ->
    (match e with
     | Ty.Int -> "int[]"
     | Ty.Float -> "float[]"
     | Ty.Double -> "double[]"
     | Ty.Bool -> "bool[]"
     | Ty.Char -> "char[]"
     | _ -> "int[]")
  | _ -> "int"

let emit_export_wrappers (buf : Buffer.t) (exports : Typed_ast.fun_decl list) : unit =
  if exports <> [] then begin
    Buffer.add_string buf "\n/* Exported C ABI Symbols */\n";
    List.iter
      (fun (fd : Typed_ast.fun_decl) ->
        let params_c =
          if fd.tf_params = [] then "void"
          else
            String.concat ", "
              (List.mapi (fun i (tp : Typed_ast.typed_param) -> Printf.sprintf "%s _p%d" (c_ffi_type tp.tp_ty) i) fd.tf_params)
        in
        let ret_c = c_ffi_type fd.tf_ret in
        Buffer.add_string buf
          (Printf.sprintf "__attribute__((visibility(\"default\"))) %s %s(%s) {\n" ret_c fd.tf_name params_c);
        let n = List.length fd.tf_params in
        if n > 0 then begin
          Buffer.add_string buf (Printf.sprintf "  sgm_val_t _args[%d];\n" n);
          List.iteri
            (fun i (tp : Typed_ast.typed_param) ->
              let v =
                match tp.tp_ty with
                | Ty.Int -> Printf.sprintf "sgm_val_i64(_p%d)" i
                | Ty.Float -> Printf.sprintf "sgm_val_f32(_p%d)" i
                | Ty.Double -> Printf.sprintf "sgm_val_f64(_p%d)" i
                | Ty.Bool -> Printf.sprintf "sgm_val_bool(_p%d)" i
                | Ty.Char -> Printf.sprintf "sgm_val_char(sgm_ffi_cp_to_utf8(_p%d))" i
                | Ty.String -> Printf.sprintf "sgm_val_str(_p%d ? _p%d : \"\")" i i
                | _ -> "sgm_val_i64(0)"
              in
              Buffer.add_string buf (Printf.sprintf "  _args[%d] = %s;\n" i v))
            fd.tf_params;
          Buffer.add_string buf
            (Printf.sprintf "  sgm_val_t _ret = %s->fp(_args, %s);\n" (mangle_ident fd.tf_name) (mangle_ident fd.tf_name))
        end else
          Buffer.add_string buf
            (Printf.sprintf "  sgm_val_t _ret = %s->fp(NULL, %s);\n" (mangle_ident fd.tf_name) (mangle_ident fd.tf_name));
        (match fd.tf_ret with
         | Ty.Void -> Buffer.add_string buf "  (void)_ret;\n}\n\n"
         | Ty.Int -> Buffer.add_string buf "  return _ret.u.i64;\n}\n\n"
         | Ty.Float -> Buffer.add_string buf "  return _ret.u.f32;\n}\n\n"
         | Ty.Double -> Buffer.add_string buf "  return _ret.u.f64;\n}\n\n"
         | Ty.Bool -> Buffer.add_string buf "  return _ret.u.b;\n}\n\n"
         | Ty.Char -> Buffer.add_string buf "  return sgm_ffi_utf8_to_cp(_ret.u.str);\n}\n\n"
         | Ty.String -> Buffer.add_string buf "  return _ret.u.str;\n}\n\n"
         | _ -> Buffer.add_string buf "  return 0;\n}\n\n"))
      exports
  end

let emit_c_header (exports : Typed_ast.fun_decl list) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "#ifndef SIGMA_LIB_H\n#define SIGMA_LIB_H\n\n#include <stdint.h>\n#include <stdbool.h>\n\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n";
  List.iter
    (fun (fd : Typed_ast.fun_decl) ->
      let ps =
        if fd.tf_params = [] then "void"
        else String.concat ", " (List.map (fun (tp : Typed_ast.typed_param) -> Printf.sprintf "%s %s" (c_ffi_type tp.tp_ty) tp.tp_name) fd.tf_params)
      in
      Buffer.add_string buf (Printf.sprintf "%s %s(%s);\n" (c_ffi_type fd.tf_ret) fd.tf_name ps))
    exports;
  Buffer.add_string buf "\n#ifdef __cplusplus\n}\n#endif\n\n#endif\n";
  Buffer.contents buf

let emit_sgm_stub ~(lib_ref : string) (exports : Typed_ast.fun_decl list) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf "public extern \"C\" from %S {\n" lib_ref);
  List.iter
    (fun (fd : Typed_ast.fun_decl) ->
      let ps =
        String.concat ", "
          (List.map (fun (tp : Typed_ast.typed_param) -> Printf.sprintf "%s: %s" tp.tp_name (sigma_ty_str tp.tp_ty)) fd.tf_params)
      in
      if fd.tf_ret = Ty.Void then
        Buffer.add_string buf (Printf.sprintf "  act %s(%s);\n" fd.tf_name ps)
      else
        Buffer.add_string buf (Printf.sprintf "  fun %s(%s): %s;\n" fd.tf_name ps (sigma_ty_str fd.tf_ret)))
    exports;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let compile_library ~emit_c_lib ~output_path (prog : Typed_ast.program) : (string * string * string, string) result =
  if not (List.for_all is_lib_allowed_stmt prog) then
    Error "E0135: library build (--lib) cannot contain top-level executable statements"
  else
    let exports =
      List.filter_map
        (fun (s : Typed_ast.stmt) ->
          match s.desc with Typed_ast.TStmtFunDecl fd when is_exportable_fun fd -> Some fd | _ -> None)
        prog
    in
    let exts = collect_externs prog in
    match prepare_link_inputs exts with
    | Error msg -> Error msg
    | Ok (link_items, temp_objs, _) ->
      let base_c = emit_c_lib prog in
      let buf = Buffer.create (String.length base_c + 1024) in
      Buffer.add_string buf base_c;
      if exts = [] then emit_ffi_helpers buf;
      emit_export_wrappers buf exports;
      let tmp_c = Filename.temp_file "sigma_lib_" ".c" in
      let cleanup () =
        if Sys.file_exists tmp_c then (try Sys.remove tmp_c with _ -> ());
        List.iter (fun o -> if Sys.file_exists o then try Sys.remove o with _ -> ()) temp_objs
      in
      try
        let dir = Filename.dirname output_path in
        (if dir <> "." && dir <> "" && not (Sys.file_exists dir) then try Sys.mkdir dir 0o755 with _ -> ());
        let oc = open_out_bin tmp_c in
        output_string oc (Buffer.contents buf);
        close_out oc;
        let extra_link = if link_items = [] then "" else " " ^ String.concat " " link_items in
        let cmd =
          Printf.sprintf "gcc -std=c99 -shared -fPIC -O3 -Wall -Wextra -Wno-unused-variable %s%s -o %s -lm 2>&1"
            (Filename.quote tmp_c) extra_link (Filename.quote output_path)
        in
        let code = Sys.command cmd in
        cleanup ();
        if code <> 0 then Error (Printf.sprintf "E0135: C library compilation failed (exit code %d)" code)
        else begin
          let stem =
            try Filename.chop_extension output_path with Invalid_argument _ -> output_path
          in
          let h_path = stem ^ ".h" in
          let sgm_path = stem ^ ".sgm" in
          let oc_h = open_out_bin h_path in
          output_string oc_h (emit_c_header exports);
          close_out oc_h;
          let oc_s = open_out_bin sgm_path in
          output_string oc_s (emit_sgm_stub ~lib_ref:(Filename.basename output_path) exports);
          close_out oc_s;
          Ok (output_path, h_path, sgm_path)
        end
      with Sys_error msg -> cleanup (); Error msg

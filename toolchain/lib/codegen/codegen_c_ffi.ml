open Sigma_sema
open Codegen_c_ctx

let collect_externs (prog : Typed_ast.program) : Typed_ast.extern_decl list =
  List.concat_map
    (fun (s : Typed_ast.stmt) ->
      match s.desc with Typed_ast.TStmtExtern exts -> exts | _ -> [])
    prog

let c_ffi_type = function
  | Ty.Int -> "int64_t"
  | Ty.Float -> "float"
  | Ty.Double -> "double"
  | Ty.Bool -> "bool"
  | Ty.Char -> "uint32_t"
  | Ty.String -> "const char *"
  | Ty.Void -> "void"
  | Ty.Array (e, _) | Ty.DynArray e ->
    (match e with
     | Ty.Int -> "int64_t *"
     | Ty.Float -> "float *"
     | Ty.Double -> "double *"
     | Ty.Bool -> "uint8_t *"
     | Ty.Char -> "uint32_t *"
     | _ -> "void *")
  | _ -> "void *"

let c_elem_type = function
  | Ty.Int -> "int64_t"
  | Ty.Float -> "float"
  | Ty.Double -> "double"
  | Ty.Bool -> "uint8_t"
  | Ty.Char -> "uint32_t"
  | _ -> "int64_t"

let emit_ffi_helpers (buf : Buffer.t) : unit =
  Buffer.add_string buf
    {|__attribute__((unused)) static uint32_t sgm_ffi_utf8_to_cp(const char *s) {
  if (!s || !s[0]) return 0;
  unsigned char c0 = (unsigned char)s[0];
  if (c0 < 0x80) return (uint32_t)c0;
  if ((c0 & 0xE0) == 0xC0 && s[1]) return ((uint32_t)(c0 & 0x1F) << 6) | ((uint32_t)s[1] & 0x3F);
  if ((c0 & 0xF0) == 0xE0 && s[1] && s[2]) return ((uint32_t)(c0 & 0x0F) << 12) | (((uint32_t)s[1] & 0x3F) << 6) | ((uint32_t)s[2] & 0x3F);
  if ((c0 & 0xF8) == 0xF0 && s[1] && s[2] && s[3]) return ((uint32_t)(c0 & 0x07) << 18) | (((uint32_t)s[1] & 0x3F) << 12) | (((uint32_t)s[2] & 0x3F) << 6) | ((uint32_t)s[3] & 0x3F);
  return (uint32_t)c0;
}
__attribute__((unused)) static const char *sgm_ffi_cp_to_utf8(uint32_t cp) {
  char *out = (char *)malloc(5);
  if (cp < 0x80) { out[0] = (char)cp; out[1] = '\0'; }
  else if (cp < 0x800) { out[0] = (char)(0xC0 | (cp >> 6)); out[1] = (char)(0x80 | (cp & 0x3F)); out[2] = '\0'; }
  else if (cp < 0x10000) { out[0] = (char)(0xE0 | (cp >> 12)); out[1] = (char)(0x80 | ((cp >> 6) & 0x3F)); out[2] = (char)(0x80 | (cp & 0x3F)); out[3] = '\0'; }
  else { out[0] = (char)(0xF0 | (cp >> 18)); out[1] = (char)(0x80 | ((cp >> 12) & 0x3F)); out[2] = (char)(0x80 | ((cp >> 6) & 0x3F)); out[3] = (char)(0x80 | (cp & 0x3F)); out[4] = '\0'; }
  return out;
}
|}

let emit_extern_decls (buf : Buffer.t) (exts : Typed_ast.extern_decl list) : unit =
  if exts <> [] then begin
    emit_ffi_helpers buf;
    let seen_protos = Hashtbl.create 16 in
    List.iter
      (fun (ext : Typed_ast.extern_decl) ->
        if not (Hashtbl.mem seen_protos ext.te_symbol) then begin
          Hashtbl.replace seen_protos ext.te_symbol ();
          let params_c =
            if ext.te_params = [] then "void"
            else
              String.concat ", "
                (List.map (fun (tp : Typed_ast.typed_param) -> c_ffi_type tp.tp_ty) ext.te_params)
          in
          Buffer.add_string buf
            (Printf.sprintf "extern %s %s(%s);\n" (c_ffi_type ext.te_ret) ext.te_symbol params_c)
        end;
        let wrap_name = "sgm_ext_" ^ mangle_ident ext.te_name in
        Buffer.add_string buf
          (Printf.sprintf "static sgm_val_t %s(sgm_val_t *_sgm_args, sgm_fn_t *_sgm_self) {\n  (void)_sgm_args;\n  (void)_sgm_self;\n"
             wrap_name);
        let call_args = ref [] in
        let cleanups = ref [] in
        List.iteri
          (fun i (tp : Typed_ast.typed_param) ->
            match tp.tp_ty with
            | Ty.Int -> call_args := Printf.sprintf "_sgm_args[%d].u.i64" i :: !call_args
            | Ty.Float -> call_args := Printf.sprintf "_sgm_args[%d].u.f32" i :: !call_args
            | Ty.Double -> call_args := Printf.sprintf "_sgm_args[%d].u.f64" i :: !call_args
            | Ty.Bool -> call_args := Printf.sprintf "_sgm_args[%d].u.b" i :: !call_args
            | Ty.Char -> call_args := Printf.sprintf "sgm_ffi_utf8_to_cp(_sgm_args[%d].u.str)" i :: !call_args
            | Ty.String -> call_args := Printf.sprintf "_sgm_args[%d].u.str" i :: !call_args
            | Ty.Array (elem, _) | Ty.DynArray elem ->
              let ety = c_elem_type elem in
              let obj_v = Printf.sprintf "_arr_%d" i in
              let buf_v = Printf.sprintf "_buf_%d" i in
              Buffer.add_string buf
                (Printf.sprintf "  sgm_obj_t *%s = _sgm_args[%d].u.obj;\n  %s *%s = (%s *)malloc((size_t)(%s->len > 0 ? %s->len : 1) * sizeof(%s));\n"
                   obj_v i ety buf_v ety obj_v obj_v ety);
              let read_expr =
                match elem with
                | Ty.Char -> Printf.sprintf "sgm_ffi_utf8_to_cp(%s->items[_k].u.str)" obj_v
                | Ty.Bool -> Printf.sprintf "(uint8_t)(%s->items[_k].u.b ? 1 : 0)" obj_v
                | _ -> Printf.sprintf "%s->items[_k].u.%s" obj_v (c_val_union_field elem)
              in
              Buffer.add_string buf
                (Printf.sprintf "  for (int64_t _k = 0; _k < %s->len; _k++) %s[_k] = %s;\n" obj_v buf_v read_expr);
              call_args := buf_v :: !call_args;
              let write_stmt =
                match elem with
                | Ty.Char -> Printf.sprintf "%s->items[_k] = sgm_val_char(sgm_ffi_cp_to_utf8(%s[_k]));" obj_v buf_v
                | Ty.Bool -> Printf.sprintf "%s->items[_k] = sgm_val_bool(%s[_k] != 0);" obj_v buf_v
                | _ ->
                  let ctor =
                    match elem with
                    | Ty.Int -> "sgm_val_i64"
                    | Ty.Float -> "sgm_val_f32"
                    | Ty.Double -> "sgm_val_f64"
                    | _ -> "sgm_val_i64"
                  in
                  Printf.sprintf "%s->items[_k] = %s(%s[_k]);" obj_v ctor buf_v
              in
              cleanups :=
                Printf.sprintf "  for (int64_t _k = 0; _k < %s->len; _k++) { %s }\n  free(%s);\n"
                  obj_v write_stmt buf_v
                :: !cleanups
            | _ -> call_args := "0" :: !call_args)
          ext.te_params;
        let args_str = String.concat ", " (List.rev !call_args) in
        if ext.te_ret = Ty.Void then begin
          Buffer.add_string buf (Printf.sprintf "  %s(%s);\n" ext.te_symbol args_str);
          List.iter (Buffer.add_string buf) (List.rev !cleanups);
          Buffer.add_string buf "  return sgm_val_i64(0);\n}\n\n"
        end else begin
          Buffer.add_string buf
            (Printf.sprintf "  %s _res = %s(%s);\n" (c_ffi_type ext.te_ret) ext.te_symbol args_str);
          List.iter (Buffer.add_string buf) (List.rev !cleanups);
          let boxed =
            match ext.te_ret with
            | Ty.Int -> "sgm_val_i64(_res)"
            | Ty.Float -> "sgm_val_f32(_res)"
            | Ty.Double -> "sgm_val_f64(_res)"
            | Ty.Bool -> "sgm_val_bool(_res)"
            | Ty.Char -> "sgm_val_char(sgm_ffi_cp_to_utf8(_res))"
            | Ty.String -> "sgm_val_str(_res ? _res : \"\")"
            | _ -> "sgm_val_i64(0)"
          in
          Buffer.add_string buf (Printf.sprintf "  return %s;\n}\n\n" boxed)
        end)
      exts
  end

let is_cpp p =
  Filename.check_suffix p ".cpp" || Filename.check_suffix p ".cc" || Filename.check_suffix p ".cxx"

let is_c p = Filename.check_suffix p ".c"

let is_lib_file p =
  Filename.check_suffix p ".so" || Filename.check_suffix p ".dylib" || Filename.check_suffix p ".dll"
  || Filename.check_suffix p ".a" || String.contains p '/'

let prepare_link_inputs (exts : Typed_ast.extern_decl list)
    : (string list * string list * string list, string) result =
  let seen = Hashtbl.create 16 in
  let inputs = ref [] in
  let add_input s =
    if not (Hashtbl.mem seen s) then begin
      Hashtbl.replace seen s ();
      inputs := s :: !inputs
    end
  in
  List.iter (fun (ext : Typed_ast.extern_decl) -> Option.iter add_input ext.te_from) exts;
  let extra_cflags = ref [] in
  let extra_cxxflags = ref [] in
  let extra_ldflags = ref [] in
  (match Sigma_pkg.Manifest.load () with
   | Ok m ->
     List.iter add_input m.native.sources;
     List.iter add_input m.native.libs;
     extra_cflags := m.native.cflags;
     extra_cxxflags := m.native.cxxflags;
     extra_ldflags := m.native.ldflags
   | Error _ -> ());
  let temp_objs = ref [] in
  let link_items = ref [] in
  let need_cxx = ref false in
  let err = ref None in
  let cflags_s = String.concat " " (List.map Filename.quote !extra_cflags) in
  let cxxflags_s = String.concat " " (List.map Filename.quote !extra_cxxflags) in
  List.iter
    (fun src ->
      if Option.is_none !err then begin
        if is_cpp src then begin
          if not (Sys.file_exists src) then
            err := Some (Printf.sprintf "E0133: native C++ source '%s' not found" src)
          else begin
            need_cxx := true;
            let obj = Filename.temp_file "sigma_cpp_" ".o" in
            temp_objs := obj :: !temp_objs;
            let cmd =
              Printf.sprintf "g++ -std=c++17 -O3 %s -c %s -o %s 2>&1"
                cxxflags_s (Filename.quote src) (Filename.quote obj)
            in
            if Sys.command cmd <> 0 then
              err := Some (Printf.sprintf "E0133: failed to compile C++ source '%s'" src)
            else link_items := Filename.quote obj :: !link_items
          end
        end else if is_c src then begin
          if not (Sys.file_exists src) then
            err := Some (Printf.sprintf "E0133: native C source '%s' not found" src)
          else begin
            let obj = Filename.temp_file "sigma_c_" ".o" in
            temp_objs := obj :: !temp_objs;
            let cmd =
              Printf.sprintf "gcc -std=c99 -O3 %s -c %s -o %s 2>&1"
                cflags_s (Filename.quote src) (Filename.quote obj)
            in
            if Sys.command cmd <> 0 then
              err := Some (Printf.sprintf "E0133: failed to compile C source '%s'" src)
            else link_items := Filename.quote obj :: !link_items
          end
        end else if is_lib_file src then begin
          if not (Sys.file_exists src) then
            err := Some (Printf.sprintf "E0133: native library '%s' not found" src)
          else begin
            let dir = Filename.dirname src in
            link_items := Printf.sprintf "%s -Wl,-rpath,%s" (Filename.quote src) (Filename.quote dir) :: !link_items
          end
        end else
          link_items := Printf.sprintf "-l%s" (Filename.quote src) :: !link_items
      end)
    (List.rev !inputs);
  if !need_cxx then link_items := "-lstdc++" :: !link_items;
  List.iter (fun f -> link_items := Filename.quote f :: !link_items) !extra_ldflags;
  match !err with
  | Some msg ->
    List.iter (fun o -> if Sys.file_exists o then try Sys.remove o with _ -> ()) !temp_objs;
    Error msg
  | None -> Ok (List.rev !link_items, !temp_objs, !extra_cflags)

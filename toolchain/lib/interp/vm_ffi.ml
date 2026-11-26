open Sigma_sema
open Vm_state

type ffi_arg =
  | FfiInt of int64
  | FfiFloat of float
  | FfiDouble of float
  | FfiBool of bool
  | FfiChar of int32
  | FfiString of string
  | FfiBytes of bytes

external invoke_so : string -> string -> ffi_arg array -> int -> Obj.t = "sigma_ffi_invoke_so"

let compiled_cache : (string, string) Hashtbl.t = Hashtbl.create 16

let utf8_to_codepoint (s : string) : int32 =
  if String.length s = 0 then 0l
  else Int32.of_int (Uchar.to_int (String.get_utf_8_uchar s 0 |> Uchar.utf_decode_uchar))

let codepoint_to_utf8 (cp : int32) : string =
  let buf = Buffer.create 4 in
  let u = if Uchar.is_valid (Int32.to_int cp) then Uchar.of_int (Int32.to_int cp) else Uchar.rep in
  Buffer.add_utf_8_uchar buf u;
  Buffer.contents buf

let c_type_str = function
  | Ty.Int -> "int64_t"
  | Ty.Float -> "float"
  | Ty.Double -> "double"
  | Ty.Bool -> "uint8_t"
  | Ty.Char -> "uint32_t"
  | Ty.String -> "const char*"
  | Ty.Void -> "void"
  | Ty.Array (e, _) | Ty.DynArray e ->
    (match e with
     | Ty.Int -> "int64_t*"
     | Ty.Float -> "float*"
     | Ty.Double -> "double*"
     | Ty.Bool -> "uint8_t*"
     | Ty.Char -> "uint32_t*"
     | _ -> "void*")
  | _ -> "void*"

let ffi_field_name = function
  | Ty.Int -> "i64"
  | Ty.Float -> "f32"
  | Ty.Double -> "f64"
  | Ty.Bool -> "b"
  | Ty.Char -> "ch"
  | Ty.String -> "str"
  | Ty.Array _ | Ty.DynArray _ -> "ptr"
  | _ -> "ptr"

let is_c_or_cpp_source p =
  Filename.check_suffix p ".c"
  || Filename.check_suffix p ".cpp"
  || Filename.check_suffix p ".cc"
  || Filename.check_suffix p ".cxx"

let is_cpp_source p =
  Filename.check_suffix p ".cpp"
  || Filename.check_suffix p ".cc"
  || Filename.check_suffix p ".cxx"

let is_shared_lib_path p =
  Filename.check_suffix p ".so"
  || Filename.check_suffix p ".dylib"
  || Filename.check_suffix p ".dll"
  || String.contains p '/'

let ret_tag_of_ty = function
  | Ty.Void -> 0
  | Ty.Int -> 1
  | Ty.Float -> 2
  | Ty.Double -> 3
  | Ty.Bool -> 4
  | Ty.Char -> 5
  | Ty.String -> 6
  | _ -> 0

let ensure_wrapper_so ~ext_lang ~ext_from ~ext_symbol ~ext_params ~ext_ret : (string, string) result =
  let mtime_str =
    match ext_from with
    | Some p when Sys.file_exists p ->
      (try Printf.sprintf "%.3f" (Unix.stat p).Unix.st_mtime with _ -> "0")
    | _ -> "0"
  in
  let sig_key =
    Printf.sprintf "%s|%s|%s|%s|%s|%s"
      ext_lang (Option.value ext_from ~default:"") ext_symbol
      (String.concat "," (List.map Ty.to_string ext_params))
      (Ty.to_string ext_ret) mtime_str
  in
  let hash = Digest.to_hex (Digest.string sig_key) in
  match Hashtbl.find_opt compiled_cache hash with
  | Some so_path when Sys.file_exists so_path -> Ok so_path
  | _ ->
    let cache_dir = Filename.concat (Filename.get_temp_dir_name ()) "sigma_ffi_cache" in
    (if not (Sys.file_exists cache_dir) then try Sys.mkdir cache_dir 0o755 with _ -> ());
    let so_path = Filename.concat cache_dir (Printf.sprintf "sgm_ffi_%s.so" hash) in
    if Sys.file_exists so_path then begin
      Hashtbl.replace compiled_cache hash so_path;
      Ok so_path
    end else begin
      let wrap_c = Filename.concat cache_dir (Printf.sprintf "sgm_wrap_%s.c" hash) in
      let params_c =
        if ext_params = [] then "void"
        else String.concat ", " (List.mapi (fun i ty -> Printf.sprintf "%s p%d" (c_type_str ty) i) ext_params)
      in
      let args_c =
        String.concat ", "
          (List.mapi (fun i ty -> Printf.sprintf "(%s)args[%d].%s" (c_type_str ty) i (ffi_field_name ty)) ext_params)
      in
      let call_stmt =
        if ext_ret = Ty.Void then Printf.sprintf "  %s(%s);\n" ext_symbol args_c
        else Printf.sprintf "  ret->%s = %s(%s);\n" (ffi_field_name ext_ret) ext_symbol args_c
      in
      let wrap_src =
        Printf.sprintf
          "#include <stdint.h>\n#include <stddef.h>\ntypedef struct { int64_t i64; float f32; double f64; uint8_t b; uint32_t ch; const char *str; void *ptr; } sgm_ffi_val_t;\n#ifdef __cplusplus\nextern \"C\" {\n#endif\nextern %s %s(%s);\nvoid sigma_ffi_entry(const sgm_ffi_val_t *args, sgm_ffi_val_t *ret) {\n  (void)args;\n  (void)ret;\n%s}\n#ifdef __cplusplus\n}\n#endif\n"
          (c_type_str ext_ret) ext_symbol params_c call_stmt
      in
      let oc = open_out_bin wrap_c in
      output_string oc wrap_src;
      close_out oc;
      let link_res =
        match ext_from with
        | None ->
          Ok (Printf.sprintf "gcc -shared -fPIC -O2 %s -o %s -lm 2>&1"
                (Filename.quote wrap_c) (Filename.quote so_path))
        | Some p when is_c_or_cpp_source p ->
          if not (Sys.file_exists p) then
            Error (Printf.sprintf "E0133: native source file '%s' not found" p)
          else
            let src_hash = Digest.to_hex (Digest.string (p ^ "|" ^ mtime_str)) in
            let lib_so = Filename.concat cache_dir (Printf.sprintf "sgm_lib_%s.so" src_hash) in
            let build_lib_ok =
              if Sys.file_exists lib_so then Ok ()
              else if is_cpp_source p || ext_lang = "C++" then
                let cc_cmd = Printf.sprintf "g++ -std=c++17 -shared -fPIC -O2 %s -o %s -lm 2>&1"
                               (Filename.quote p) (Filename.quote lib_so) in
                if Sys.command cc_cmd = 0 then Ok ()
                else Error (Printf.sprintf "E0133: failed to compile C++ library '%s'" p)
              else
                let cc_cmd = Printf.sprintf "gcc -std=c99 -shared -fPIC -O2 %s -o %s -lm 2>&1"
                               (Filename.quote p) (Filename.quote lib_so) in
                if Sys.command cc_cmd = 0 then Ok ()
                else Error (Printf.sprintf "E0133: failed to compile C library '%s'" p)
            in
            (match build_lib_ok with
             | Error e -> Error e
             | Ok () ->
               Ok (Printf.sprintf "gcc -shared -fPIC -O2 %s %s -Wl,-rpath,%s -o %s -lm 2>&1"
                     (Filename.quote wrap_c) (Filename.quote lib_so) (Filename.quote cache_dir) (Filename.quote so_path)))
        | Some p when is_shared_lib_path p ->
          if not (Sys.file_exists p) then
            Error (Printf.sprintf "E0133: native library '%s' not found" p)
          else
            let dir = Filename.dirname p in
            Ok (Printf.sprintf "gcc -shared -fPIC -O2 %s %s -Wl,-rpath,%s -o %s -lm 2>&1"
                  (Filename.quote wrap_c) (Filename.quote p) (Filename.quote dir) (Filename.quote so_path))
        | Some lib_name ->
          Ok (Printf.sprintf "gcc -shared -fPIC -O2 %s -l%s -o %s -lm 2>&1"
                (Filename.quote wrap_c) (Filename.quote lib_name) (Filename.quote so_path))
      in
      match link_res with
      | Error e -> Error e
      | Ok cmd ->
        let code = Sys.command cmd in
        (try Sys.remove wrap_c with _ -> ());
        if code <> 0 then
          Error (Printf.sprintf "E0133: failed to build native FFI wrapper for '%s'" ext_symbol)
        else begin
          Hashtbl.replace compiled_cache hash so_path;
          Ok so_path
        end
    end

let pack_array elem_ty (arr : value array) : bytes =
  let n = Array.length arr in
  match elem_ty with
  | Ty.Int ->
    let b = Bytes.create (max 1 (n * 8)) in
    Array.iteri (fun i v -> match v with VInt x -> Bytes.set_int64_ne b (i * 8) x | _ -> ()) arr;
    b
  | Ty.Float ->
    let b = Bytes.create (max 1 (n * 4)) in
    Array.iteri (fun i v -> match v with VFloat x -> Bytes.set_int32_ne b (i * 4) (Int32.bits_of_float x) | _ -> ()) arr;
    b
  | Ty.Double ->
    let b = Bytes.create (max 1 (n * 8)) in
    Array.iteri (fun i v -> match v with VDouble x -> Bytes.set_int64_ne b (i * 8) (Int64.bits_of_float x) | _ -> ()) arr;
    b
  | Ty.Bool ->
    let b = Bytes.create (max 1 n) in
    Array.iteri (fun i v -> match v with VBool x -> Bytes.set_uint8 b i (if x then 1 else 0) | _ -> ()) arr;
    b
  | Ty.Char ->
    let b = Bytes.create (max 1 (n * 4)) in
    Array.iteri (fun i v -> match v with VChar s -> Bytes.set_int32_ne b (i * 4) (utf8_to_codepoint s) | _ -> ()) arr;
    b
  | _ -> Bytes.create 1

let unpack_array elem_ty (b : bytes) (arr : value array) : unit =
  let n = Array.length arr in
  match elem_ty with
  | Ty.Int -> for i = 0 to n - 1 do arr.(i) <- VInt (Bytes.get_int64_ne b (i * 8)) done
  | Ty.Float -> for i = 0 to n - 1 do arr.(i) <- VFloat (Int32.float_of_bits (Bytes.get_int32_ne b (i * 4))) done
  | Ty.Double -> for i = 0 to n - 1 do arr.(i) <- VDouble (Int64.float_of_bits (Bytes.get_int64_ne b (i * 8))) done
  | Ty.Bool -> for i = 0 to n - 1 do arr.(i) <- VBool (Bytes.get_uint8 b i <> 0) done
  | Ty.Char -> for i = 0 to n - 1 do arr.(i) <- VChar (codepoint_to_utf8 (Bytes.get_int32_ne b (i * 4))) done
  | _ -> ()

let exec_call_extern (st : state) ~ext_lang ~ext_from ~ext_symbol ~ext_params ~ext_ret : (unit, string) result =
  let n = List.length ext_params in
  let raw_vals = Array.make n VNull in
  let rec pop_args i =
    if i < 0 then Ok ()
    else
      match st.stack with
      | v :: rest -> st.stack <- rest; raw_vals.(i) <- v; pop_args (i - 1)
      | [] -> Error "VM stack underflow on OP_CALL_EXTERN"
  in
  match pop_args (n - 1) with
  | Error e -> Error e
  | Ok () ->
    (match ensure_wrapper_so ~ext_lang ~ext_from ~ext_symbol ~ext_params ~ext_ret with
     | Error e -> Error e
     | Ok so_path ->
       let mut_arrays = ref [] in
       let ffi_args =
         Array.of_list
           (List.mapi
              (fun i pty ->
                match pty, raw_vals.(i) with
                | Ty.Int, VInt x -> FfiInt x
                | Ty.Float, VFloat x -> FfiFloat x
                | Ty.Double, VDouble x -> FfiDouble x
                | Ty.Bool, VBool b -> FfiBool b
                | Ty.Char, VChar s -> FfiChar (utf8_to_codepoint s)
                | Ty.String, VString s -> FfiString s
                | (Ty.Array (elem, _) | Ty.DynArray elem), VArray arr ->
                  let buf = pack_array elem arr in
                  mut_arrays := (elem, buf, arr) :: !mut_arrays;
                  FfiBytes buf
                | _ -> FfiInt 0L)
              ext_params)
       in
       try
         let raw_ret = invoke_so so_path "sigma_ffi_entry" ffi_args (ret_tag_of_ty ext_ret) in
         List.iter (fun (elem, buf, arr) -> unpack_array elem buf arr) !mut_arrays;
         (match ext_ret with
          | Ty.Void -> ()
          | Ty.Int -> st.stack <- VInt (Obj.obj raw_ret : int64) :: st.stack
          | Ty.Float -> st.stack <- VFloat (Obj.obj raw_ret : float) :: st.stack
          | Ty.Double -> st.stack <- VDouble (Obj.obj raw_ret : float) :: st.stack
          | Ty.Bool -> st.stack <- VBool (Obj.obj raw_ret : bool) :: st.stack
          | Ty.Char -> st.stack <- VChar (codepoint_to_utf8 (Obj.obj raw_ret : int32)) :: st.stack
          | Ty.String -> st.stack <- VString (Obj.obj raw_ret : string) :: st.stack
          | _ -> st.stack <- VNull :: st.stack);
         Ok ()
       with Failure msg ->
         Error (Printf.sprintf "E0133: native FFI call '%s' failed: %s" ext_symbol msg))

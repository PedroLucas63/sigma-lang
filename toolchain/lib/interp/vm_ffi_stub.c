#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <dlfcn.h>
#include <stdint.h>
#include <string.h>

typedef struct {
  int64_t i64;
  float f32;
  double f64;
  uint8_t b;
  uint32_t ch;
  const char *str;
  void *ptr;
} sgm_ffi_val_t;

typedef void (*sgm_ffi_entry_t)(const sgm_ffi_val_t *args, sgm_ffi_val_t *ret);

CAMLprim value sigma_ffi_invoke_so(value v_so_path, value v_sym_name, value v_args, value v_ret_tag) {
  CAMLparam4(v_so_path, v_sym_name, v_args, v_ret_tag);
  CAMLlocal1(v_res);

  const char *so_path = String_val(v_so_path);
  const char *sym_name = String_val(v_sym_name);
  int ret_tag = Int_val(v_ret_tag);

  void *handle = dlopen(so_path, RTLD_LAZY | RTLD_LOCAL);
  if (!handle) {
    const char *err = dlerror();
    caml_failwith(err ? err : "dlopen failed");
  }

  dlerror();
  void *raw_sym = dlsym(handle, sym_name);
  const char *sym_err = dlerror();
  if (sym_err != NULL || !raw_sym) {
    caml_failwith(sym_err ? sym_err : "dlsym failed");
  }

  mlsize_t nargs = Wosize_val(v_args);
  sgm_ffi_val_t stack_args[32];
  sgm_ffi_val_t *args = nargs <= 32 ? stack_args : (sgm_ffi_val_t *)malloc(nargs * sizeof(sgm_ffi_val_t));
  if (!args) caml_failwith("out of memory in FFI");

  for (mlsize_t i = 0; i < nargs; i++) {
    value v_arg = Field(v_args, i);
    int tag = Tag_val(v_arg);
    value f0 = Field(v_arg, 0);
    memset(&args[i], 0, sizeof(sgm_ffi_val_t));
    switch (tag) {
      case 0: args[i].i64 = Int64_val(f0); break;
      case 1: args[i].f32 = (float)Double_val(f0); break;
      case 2: args[i].f64 = Double_val(f0); break;
      case 3: args[i].b = (uint8_t)Bool_val(f0); break;
      case 4: args[i].ch = (uint32_t)Int32_val(f0); break;
      case 5: args[i].str = String_val(f0); break;
      case 6: args[i].ptr = (void *)Bytes_val(f0); break;
      default: break;
    }
  }

  sgm_ffi_val_t ret;
  memset(&ret, 0, sizeof(sgm_ffi_val_t));
  sgm_ffi_entry_t entry = (sgm_ffi_entry_t)raw_sym;
  entry(args, &ret);

  if (nargs > 32) free(args);

  switch (ret_tag) {
    case 0: v_res = Val_int(0); break;
    case 1: v_res = caml_copy_int64(ret.i64); break;
    case 2: v_res = caml_copy_double((double)ret.f32); break;
    case 3: v_res = caml_copy_double(ret.f64); break;
    case 4: v_res = Val_bool(ret.b != 0); break;
    case 5: v_res = caml_copy_int32((int32_t)ret.ch); break;
    case 6: v_res = caml_copy_string(ret.str ? ret.str : ""); break;
    default: v_res = Val_int(0); break;
  }

  CAMLreturn(v_res);
}

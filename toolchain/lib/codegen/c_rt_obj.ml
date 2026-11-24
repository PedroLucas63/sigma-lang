let c_runtime_obj =
  {|typedef enum {
  SGM_TAG_I64,
  SGM_TAG_F32,
  SGM_TAG_F64,
  SGM_TAG_DEC,
  SGM_TAG_BOOL,
  SGM_TAG_CHAR,
  SGM_TAG_STR,
  SGM_TAG_OBJ,
  SGM_TAG_FN,
  SGM_TAG_REF
} sgm_tag_t;

typedef struct sgm_obj sgm_obj_t;
typedef struct sgm_fn sgm_fn_t;

typedef struct {
  sgm_tag_t tag;
  union {
    int64_t i64;
    float f32;
    double f64;
    sgm_decimal_t dec;
    bool b;
    const char *str;
    sgm_obj_t *obj;
    sgm_fn_t *fn;
    void *ref_ptr;
  } u;
} sgm_val_t;

struct sgm_obj {
  bool is_tuple;
  int64_t len;
  sgm_val_t *items;
};

__attribute__((unused)) static sgm_val_t sgm_val_i64(int64_t v) { return (sgm_val_t){ SGM_TAG_I64, { .i64 = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_f32(float v) { return (sgm_val_t){ SGM_TAG_F32, { .f32 = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_f64(double v) { return (sgm_val_t){ SGM_TAG_F64, { .f64 = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_dec(sgm_decimal_t v) { return (sgm_val_t){ SGM_TAG_DEC, { .dec = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_bool(bool v) { return (sgm_val_t){ SGM_TAG_BOOL, { .b = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_char(const char *v) { return (sgm_val_t){ SGM_TAG_CHAR, { .str = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_str(const char *v) { return (sgm_val_t){ SGM_TAG_STR, { .str = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_obj(sgm_obj_t *v) { return (sgm_val_t){ SGM_TAG_OBJ, { .obj = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_ref(void *p) { return (sgm_val_t){ SGM_TAG_REF, { .ref_ptr = p } }; }

struct sgm_fn {
  sgm_val_t (*fp)(sgm_val_t *args, sgm_fn_t *self);
  void *env;
  struct sgm_fn *orig;
  int64_t nparams;
  bool *has;
  sgm_val_t *vals;
};

__attribute__((unused)) static sgm_val_t sgm_val_fn(sgm_fn_t *v) { return (sgm_val_t){ SGM_TAG_FN, { .fn = v } }; }

__attribute__((unused)) static sgm_fn_t* sgm_fn_make(sgm_val_t (*fp)(sgm_val_t *, sgm_fn_t *), int64_t n, void *env) {
  sgm_fn_t *f = (sgm_fn_t*)malloc(sizeof(sgm_fn_t));
  f->fp = fp;
  f->env = env;
  f->orig = f;
  f->nparams = n;
  f->has = (bool*)calloc((size_t)(n > 0 ? n : 1), sizeof(bool));
  f->vals = (sgm_val_t*)calloc((size_t)(n > 0 ? n : 1), sizeof(sgm_val_t));
  return f;
}

__attribute__((unused)) static sgm_fn_t* sgm_fn_bind(sgm_fn_t *f, int64_t n, const int64_t *idxs, const sgm_val_t *argv) {
  sgm_fn_t *g = sgm_fn_make(f->fp, f->nparams, f->env);
  g->orig = f->orig;
  for (int64_t k = 0; k < f->nparams; k++) {
    g->has[k] = f->has[k];
    g->vals[k] = f->vals[k];
  }
  for (int64_t j = 0; j < n; j++) {
    int64_t want = idxs[j];
    int64_t seen = 0;
    for (int64_t k = 0; k < f->nparams; k++) {
      if (!f->has[k]) {
        if (seen == want) {
          g->has[k] = true;
          g->vals[k] = argv[j];
          break;
        }
        seen++;
      }
    }
  }
  return g;
}

__attribute__((unused)) static sgm_val_t sgm_fn_invoke(sgm_fn_t *g) { return g->fp(g->vals, g); }

__attribute__((unused)) static sgm_val_t* sgm_cell_make(sgm_val_t v) {
  sgm_val_t *c = (sgm_val_t*)malloc(sizeof(sgm_val_t));
  *c = v;
  return c;
}

__attribute__((unused)) static const char* sgm_fmt_fn(sgm_fn_t *f) { (void)f; return "<function>"; }

__attribute__((unused)) static sgm_obj_t* sgm_obj_alloc(bool is_tuple, int64_t len) {
  sgm_obj_t *o = (sgm_obj_t*)malloc(sizeof(sgm_obj_t));
  o->is_tuple = is_tuple;
  o->len = len;
  o->items = (len > 0) ? (sgm_val_t*)calloc((size_t)len, sizeof(sgm_val_t)) : NULL;
  return o;
}

__attribute__((unused)) static void sgm_obj_spread(sgm_obj_t *dst, int64_t *k, sgm_obj_t *src) {
  for (int64_t i = 0; i < src->len; i++) {
    dst->items[(*k)++] = src->items[i];
  }
}

__attribute__((unused)) static sgm_val_t* sgm_obj_slot(sgm_obj_t *o, int64_t idx) {
  if (idx < 0 || idx >= o->len) {
    fprintf(stderr, "Runtime error: index %" PRId64 " out of bounds (length %" PRId64 ")\n", idx, o->len);
    exit(1);
  }
  return &o->items[idx];
}

__attribute__((unused)) static sgm_obj_t* sgm_array_concat(sgm_obj_t *a, sgm_obj_t *b) {
  sgm_obj_t *out = sgm_obj_alloc(false, a->len + b->len);
  for (int64_t i = 0; i < a->len; i++) out->items[i] = a->items[i];
  for (int64_t j = 0; j < b->len; j++) out->items[a->len + j] = b->items[j];
  return out;
}

__attribute__((unused)) static sgm_obj_t* sgm_array_repeat(sgm_obj_t *a, int64_t n) {
  if (n <= 0 || a->len <= 0) return sgm_obj_alloc(false, 0);
  int64_t total = a->len * n;
  sgm_obj_t *out = sgm_obj_alloc(false, total);
  for (int64_t i = 0; i < total; i++) {
    out->items[i] = a->items[i % a->len];
  }
  return out;
}

static bool sgm_obj_eq(sgm_obj_t *a, sgm_obj_t *b);

__attribute__((unused)) static bool sgm_val_eq(sgm_val_t a, sgm_val_t b) {
  if (a.tag != b.tag) return false;
  switch (a.tag) {
    case SGM_TAG_I64: return a.u.i64 == b.u.i64;
    case SGM_TAG_F32: return a.u.f32 == b.u.f32;
    case SGM_TAG_F64: return a.u.f64 == b.u.f64;
    case SGM_TAG_DEC: return sgm_dec_cmp(a.u.dec, b.u.dec) == 0;
    case SGM_TAG_BOOL: return a.u.b == b.u.b;
    case SGM_TAG_CHAR: return sgm_decode_utf8(a.u.str) == sgm_decode_utf8(b.u.str);
    case SGM_TAG_STR: return strcmp(a.u.str, b.u.str) == 0;
    case SGM_TAG_OBJ: return sgm_obj_eq(a.u.obj, b.u.obj);
    case SGM_TAG_FN: return a.u.fn == b.u.fn;
    case SGM_TAG_REF: return a.u.ref_ptr == b.u.ref_ptr;
  }
  return false;
}

__attribute__((unused)) static bool sgm_obj_eq(sgm_obj_t *a, sgm_obj_t *b) {
  if (a == b) return true;
  if (!a || !b) return false;
  if (a->is_tuple != b->is_tuple || a->len != b->len) return false;
  for (int64_t i = 0; i < a->len; i++) {
    if (!sgm_val_eq(a->items[i], b->items[i])) return false;
  }
  return true;
}

static const char* sgm_fmt_obj(sgm_obj_t *o);

__attribute__((unused)) static const char* sgm_fmt_val(sgm_val_t v) {
  switch (v.tag) {
    case SGM_TAG_I64: return sgm_fmt_i64(v.u.i64);
    case SGM_TAG_F32: return sgm_fmt_f32(v.u.f32);
    case SGM_TAG_F64: return sgm_fmt_f64(v.u.f64);
    case SGM_TAG_DEC: return sgm_fmt_dec(v.u.dec);
    case SGM_TAG_BOOL: return v.u.b ? "true" : "false";
    case SGM_TAG_CHAR:
    case SGM_TAG_STR: return v.u.str;
    case SGM_TAG_OBJ: return sgm_fmt_obj(v.u.obj);
    case SGM_TAG_FN: return sgm_fmt_fn(v.u.fn);
    case SGM_TAG_REF: return "<ref>";
  }
  return "";
}

__attribute__((unused)) static const char* sgm_fmt_obj(sgm_obj_t *o) {
  if (!o) return "null";
  int64_t n = o->len;
  const char **parts = (n > 0) ? (const char**)malloc((size_t)n * sizeof(const char*)) : NULL;
  size_t total = 2;
  for (int64_t i = 0; i < n; i++) {
    parts[i] = sgm_fmt_val(o->items[i]);
    total += strlen(parts[i]);
    if (i > 0) total += 2;
  }
  char *out = (char*)malloc(total + 1);
  size_t pos = 0;
  out[pos++] = o->is_tuple ? '(' : '[';
  for (int64_t i = 0; i < n; i++) {
    if (i > 0) {
      out[pos++] = ',';
      out[pos++] = ' ';
    }
    size_t plen = strlen(parts[i]);
    memcpy(out + pos, parts[i], plen);
    pos += plen;
  }
  out[pos++] = o->is_tuple ? ')' : ']';
  out[pos] = '\0';
  if (parts) free(parts);
  return out;
}
|}

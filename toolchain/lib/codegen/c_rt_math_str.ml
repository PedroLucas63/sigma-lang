let c_runtime_math_str =
  {|__attribute__((unused)) static float sgm_div_f32(float a, float b) {
  if (b == 0.0f) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return a / b;
}

__attribute__((unused)) static double sgm_div_f64(double a, double b) {
  if (b == 0.0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return a / b;
}

__attribute__((unused)) static int64_t sgm_idiv_i64(int64_t a, int64_t b) {
  if (b == 0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return a / b;
}

__attribute__((unused)) static int64_t sgm_idiv_f32(float a, float b) {
  if (b == 0.0f) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return (int64_t)truncf(a / b);
}

__attribute__((unused)) static int64_t sgm_idiv_f64(double a, double b) {
  if (b == 0.0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return (int64_t)trunc(a / b);
}

__attribute__((unused)) static int64_t sgm_mod_i64(int64_t a, int64_t b) {
  if (b == 0) { fprintf(stderr, "Runtime error: Modulo by zero\n"); exit(1); }
  int64_t r = a % b;
  int64_t abs_b = (b < 0) ? -b : b;
  return (r < 0) ? (r + abs_b) : r;
}

__attribute__((unused)) static float sgm_mod_f32(float a, float b) {
  if (b == 0.0f) { fprintf(stderr, "Runtime error: Modulo by zero\n"); exit(1); }
  float abs_b = fabsf(b);
  float r = fmodf(a, abs_b);
  if (r < 0.0f) r += abs_b;
  return (r == 0.0f) ? 0.0f : r;
}

__attribute__((unused)) static double sgm_mod_f64(double a, double b) {
  if (b == 0.0) { fprintf(stderr, "Runtime error: Modulo by zero\n"); exit(1); }
  double abs_b = fabs(b);
  double r = fmod(a, abs_b);
  if (r < 0.0) r += abs_b;
  return (r == 0.0) ? 0.0 : r;
}

__attribute__((unused)) static int64_t sgm_pow_i64(int64_t base, int64_t exp) {
  if (exp < 0) {
    if (base == 1) return 1;
    if (base == -1) return (exp % 2 == 0) ? 1 : -1;
    return 0;
  }
  uint64_t b = (uint64_t)base;
  uint64_t e = (uint64_t)exp;
  uint64_t acc = 1;
  while (e > 0) {
    if (e & 1) acc *= b;
    b *= b;
    e >>= 1;
  }
  return (int64_t)acc;
}

__attribute__((unused)) static int64_t sgm_shl_i64(int64_t a, int64_t b) {
  if (b < 0 || b >= 64) return 0;
  return (int64_t)((uint64_t)a << (int)b);
}

__attribute__((unused)) static int64_t sgm_shr_i64(int64_t a, int64_t b) {
  if (b < 0) return 0;
  if (b >= 64) return (a < 0) ? -1 : 0;
  return a >> (int)b;
}

__attribute__((unused)) static int sgm_decode_utf8(const char *s) {
  if (!s || !s[0]) return 0;
  unsigned char b0 = (unsigned char)s[0];
  if (b0 < 0x80) return b0;
  if (b0 < 0xE0) return ((b0 & 0x1F) << 6) | ((unsigned char)s[1] & 0x3F);
  if (b0 < 0xF0) return ((b0 & 0x0F) << 12) | (((unsigned char)s[1] & 0x3F) << 6) | ((unsigned char)s[2] & 0x3F);
  return ((b0 & 0x07) << 18) | (((unsigned char)s[1] & 0x3F) << 12) | (((unsigned char)s[2] & 0x3F) << 6) | ((unsigned char)s[3] & 0x3F);
}

__attribute__((unused)) static const char* sgm_encode_utf8(int cp) {
  if (cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) {
    fprintf(stderr, "Runtime error: Invalid Unicode code point\n");
    exit(1);
  }
  char *out = (char*)malloc(5);
  if (cp < 0x80) {
    out[0] = (char)cp; out[1] = '\0';
  } else if (cp < 0x800) {
    out[0] = (char)(0xC0 | (cp >> 6));
    out[1] = (char)(0x80 | (cp & 0x3F));
    out[2] = '\0';
  } else if (cp < 0x10000) {
    out[0] = (char)(0xE0 | (cp >> 12));
    out[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[2] = (char)(0x80 | (cp & 0x3F));
    out[3] = '\0';
  } else {
    out[0] = (char)(0xF0 | (cp >> 18));
    out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[3] = (char)(0x80 | (cp & 0x3F));
    out[4] = '\0';
  }
  return out;
}

__attribute__((unused)) static const char* sgm_char_add(const char *c, int64_t n) {
  return sgm_encode_utf8(sgm_decode_utf8(c) + (int)n);
}

__attribute__((unused)) static const char* sgm_char_sub(const char *c, int64_t n) {
  return sgm_encode_utf8(sgm_decode_utf8(c) - (int)n);
}

__attribute__((unused)) static int64_t sgm_char_diff(const char *a, const char *b) {
  return (int64_t)(sgm_decode_utf8(a) - sgm_decode_utf8(b));
}

__attribute__((unused)) static const char* sgm_repeat_str(const char *s, int64_t n) {
  size_t len = strlen(s);
  if (n <= 0 || len == 0) {
    char *empty = (char*)malloc(1);
    empty[0] = '\0';
    return empty;
  }
  size_t count = (size_t)n;
  char *out = (char*)malloc(len * count + 1);
  for (size_t i = 0; i < count; i++) {
    memcpy(out + i * len, s, len);
  }
  out[len * count] = '\0';
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_i64(int64_t v) {
  char *out = (char*)malloc(32);
  snprintf(out, 32, "%" PRId64, v);
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_f32(float v) {
  char *out = (char*)malloc(32);
  snprintf(out, 32, "%.6g", (double)v);
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_f64(double v) {
  char *out = (char*)malloc(48);
  snprintf(out, 48, "%.15g", v);
  return out;
}

__attribute__((unused)) static const char* sgm_concat(int n, const char **parts) {
  size_t total = 0;
  for (int i = 0; i < n; i++) total += strlen(parts[i]);
  char *out = (char*)malloc(total + 1);
  out[0] = '\0';
  for (int i = 0; i < n; i++) strcat(out, parts[i]);
  return out;
}
|}

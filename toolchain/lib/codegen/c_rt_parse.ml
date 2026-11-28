let c_runtime_parse =
  {|__attribute__((unused)) static bool sgm_try_parse_prim(sgm_tag_t tag, const char *raw, sgm_val_t *out) {
  switch (tag) {
    case SGM_TAG_I64: {
      const char *s = sgm_trim_ws(raw);
      size_t slen = strlen(s);
      size_t start = (slen > 0 && (s[0] == '+' || s[0] == '-')) ? 1 : 0;
      if (slen <= start) return false;
      for (size_t i = start; i < slen; i++) {
        if (s[i] < '0' || s[i] > '9') return false;
      }
      bool neg = (s[0] == '-');
      uint64_t acc = 0;
      uint64_t limit = neg ? 9223372036854775808ULL : 9223372036854775807ULL;
      for (size_t i = start; i < slen; i++) {
        uint64_t d = (uint64_t)(s[i] - '0');
        if (acc > (limit - d) / 10ULL) return false;
        acc = acc * 10ULL + d;
      }
      int64_t res = neg
        ? ((acc == 9223372036854775808ULL) ? (-9223372036854775807LL - 1LL) : -(int64_t)acc)
        : (int64_t)acc;
      *out = sgm_val_i64(res);
      return true;
    }
    case SGM_TAG_F32: {
      const char *s = sgm_trim_ws(raw);
      if (s[0] == '\0') return false;
      char *endp = NULL;
      float v = strtof(s, &endp);
      if (!endp || *endp != '\0' || isnan(v) || isinf(v)) return false;
      *out = sgm_val_f32(v);
      return true;
    }
    case SGM_TAG_F64: {
      const char *s = sgm_trim_ws(raw);
      if (s[0] == '\0') return false;
      char *endp = NULL;
      double v = strtod(s, &endp);
      if (!endp || *endp != '\0' || isnan(v) || isinf(v)) return false;
      *out = sgm_val_f64(v);
      return true;
    }
    case SGM_TAG_DEC: {
      const char *s = sgm_trim_ws(raw);
      size_t slen = strlen(s), idx = 0;
      int neg = 0;
      if (idx < slen && (s[idx] == '+' || s[idx] == '-')) { if (s[idx] == '-') neg = 1; idx++; }
      bool seen_dot = false, seen_digit = false;
      int scale = 0;
      sgm_u128 coeff = 0;
      for (; idx < slen; idx++) {
        char c = s[idx];
        if (c == '.') { if (seen_dot) return false; seen_dot = true; }
        else if (c >= '0' && c <= '9') { seen_digit = true; coeff = coeff * 10 + (sgm_u128)(c - '0'); if (seen_dot) scale++; }
        else return false;
      }
      if (!seen_digit) return false;
      *out = sgm_val_dec(sgm_dec_normalize(neg, scale, coeff));
      return true;
    }
    case SGM_TAG_BOOL: {
      const char *s = sgm_trim_ws(raw);
      if (strcmp(s, "true") == 0) { *out = sgm_val_bool(true); return true; }
      if (strcmp(s, "false") == 0) { *out = sgm_val_bool(false); return true; }
      return false;
    }
    case SGM_TAG_CHAR: {
      if (sgm_utf8_cp_len(raw) == 1) {
        char *dup = (char*)malloc(strlen(raw) + 1);
        strcpy(dup, raw);
        *out = sgm_val_char(dup);
        return true;
      }
      const char *trimmed = sgm_trim_ws(raw);
      if (sgm_utf8_cp_len(trimmed) == 1) { *out = sgm_val_char(trimmed); return true; }
      return false;
    }
    default:
      return false;
  }
}

__attribute__((unused)) static bool sgm_try_parse_i64(const char *raw, int64_t *dst) {
  sgm_val_t v; if (!sgm_try_parse_prim(SGM_TAG_I64, raw, &v)) return false; *dst = v.u.i64; return true;
}
__attribute__((unused)) static bool sgm_try_parse_f32(const char *raw, float *dst) {
  sgm_val_t v; if (!sgm_try_parse_prim(SGM_TAG_F32, raw, &v)) return false; *dst = v.u.f32; return true;
}
__attribute__((unused)) static bool sgm_try_parse_f64(const char *raw, double *dst) {
  sgm_val_t v; if (!sgm_try_parse_prim(SGM_TAG_F64, raw, &v)) return false; *dst = v.u.f64; return true;
}
__attribute__((unused)) static bool sgm_try_parse_dec(const char *raw, sgm_decimal_t *dst) {
  sgm_val_t v; if (!sgm_try_parse_prim(SGM_TAG_DEC, raw, &v)) return false; *dst = v.u.dec; return true;
}
__attribute__((unused)) static bool sgm_try_parse_bool(const char *raw, bool *dst) {
  sgm_val_t v; if (!sgm_try_parse_prim(SGM_TAG_BOOL, raw, &v)) return false; *dst = v.u.b; return true;
}
__attribute__((unused)) static bool sgm_try_parse_char(const char *raw, const char **dst) {
  sgm_val_t v; if (!sgm_try_parse_prim(SGM_TAG_CHAR, raw, &v)) return false; *dst = v.u.str; return true;
}
|}

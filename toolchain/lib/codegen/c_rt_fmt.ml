let c_runtime_fmt =
  {|__attribute__((unused)) static int sgm_utf8_cp_len(const char *s) {
  int count = 0;
  size_t i = 0;
  size_t len = strlen(s);
  while (i < len) {
    unsigned char b = (unsigned char)s[i];
    size_t clen = (b < 0x80) ? 1 : ((b & 0xE0) == 0xC0) ? 2 : ((b & 0xF0) == 0xE0) ? 3 : 4;
    i += clen;
    count++;
  }
  return count;
}

__attribute__((unused)) static const char* sgm_utf8_truncate(const char *s, int max_chars) {
  if (max_chars <= 0) {
    char *empty = (char*)malloc(1);
    empty[0] = '\0';
    return empty;
  }
  size_t len = strlen(s);
  size_t i = 0;
  int count = 0;
  while (i < len && count < max_chars) {
    unsigned char b = (unsigned char)s[i];
    size_t clen = (b < 0x80) ? 1 : ((b & 0xE0) == 0xC0) ? 2 : ((b & 0xF0) == 0xE0) ? 3 : 4;
    if (i + clen > len) clen = len - i;
    i += clen;
    count++;
  }
  char *out = (char*)malloc(i + 1);
  memcpy(out, s, i);
  out[i] = '\0';
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_dec_prec(const char *raw, int p) {
  const char *dot = strchr(raw, '.');
  size_t ilen = dot ? (size_t)(dot - raw) : strlen(raw);
  const char *frac = dot ? (dot + 1) : "";
  size_t flen = strlen(frac);
  if ((int)flen <= p) {
    if (p == 0) {
      char *out = (char*)malloc(ilen + 1);
      memcpy(out, raw, ilen);
      out[ilen] = '\0';
      return out;
    }
    size_t total = ilen + 1 + (size_t)p;
    char *out = (char*)malloc(total + 1);
    memcpy(out, raw, ilen);
    out[ilen] = '.';
    if (flen > 0) memcpy(out + ilen + 1, frac, flen);
    for (int k = (int)flen; k < p; k++) out[ilen + 1 + (size_t)k] = '0';
    out[total] = '\0';
    return out;
  }
  size_t clen = ilen + (size_t)p;
  char *comb = (char*)malloc(clen + 2);
  memcpy(comb, raw, ilen);
  if (p > 0) memcpy(comb + ilen, frac, (size_t)p);
  comb[clen] = '\0';
  bool round_up = (frac[p] >= '5');
  if (round_up) {
    int idx = (int)clen - 1;
    bool carry = true;
    while (idx >= 0 && carry) {
      int d = (comb[idx] - '0') + 1;
      if (d == 10) {
        comb[idx] = '0';
        idx--;
      } else {
        comb[idx] = (char)('0' + d);
        carry = false;
      }
    }
    if (carry) {
      memmove(comb + 1, comb, clen + 1);
      comb[0] = '1';
      clen++;
    }
  }
  if (p == 0) return comb;
  char *out = (char*)malloc(clen + 2);
  size_t new_ilen = clen - (size_t)p;
  memcpy(out, comb, new_ilen);
  out[new_ilen] = '.';
  memcpy(out + new_ilen + 1, comb + new_ilen, (size_t)p);
  out[clen + 1] = '\0';
  free(comb);
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_spec(sgm_val_t v, const char *fill, int align, bool sign_plus, bool zero_pad, int width, int prec) {
  const char *sign_str = "";
  const char *digits_str = "";
  switch (v.tag) {
    case SGM_TAG_I64: {
      bool is_neg = (v.u.i64 < 0);
      uint64_t abs_v = (v.u.i64 == (-9223372036854775807LL - 1LL))
        ? 9223372036854775808ULL
        : (uint64_t)(is_neg ? -v.u.i64 : v.u.i64);
      char *buf = (char*)malloc(32);
      snprintf(buf, 32, "%" PRIu64, abs_v);
      digits_str = buf;
      sign_str = is_neg ? "-" : (sign_plus ? "+" : "");
      break;
    }
    case SGM_TAG_F32: {
      bool is_neg = (signbit(v.u.f32) != 0) && !isnan(v.u.f32);
      float abs_f = fabsf(v.u.f32);
      if (prec >= 0) {
        int sz = snprintf(NULL, 0, "%.*f", prec, (double)abs_f);
        char *buf = (char*)malloc((size_t)sz + 1);
        snprintf(buf, (size_t)sz + 1, "%.*f", prec, (double)abs_f);
        digits_str = buf;
      } else {
        digits_str = sgm_fmt_f32(abs_f);
      }
      sign_str = is_neg ? "-" : (sign_plus ? "+" : "");
      break;
    }
    case SGM_TAG_F64: {
      bool is_neg = (signbit(v.u.f64) != 0) && !isnan(v.u.f64);
      double abs_d = fabs(v.u.f64);
      if (prec >= 0) {
        int sz = snprintf(NULL, 0, "%.*f", prec, abs_d);
        char *buf = (char*)malloc((size_t)sz + 1);
        snprintf(buf, (size_t)sz + 1, "%.*f", prec, abs_d);
        digits_str = buf;
      } else {
        digits_str = sgm_fmt_f64(abs_d);
      }
      sign_str = is_neg ? "-" : (sign_plus ? "+" : "");
      break;
    }
    case SGM_TAG_DEC: {
      bool is_neg = (v.u.dec.neg != 0) && (v.u.dec.coeff != 0);
      sgm_decimal_t abs_m = v.u.dec;
      abs_m.neg = 0;
      const char *raw = sgm_fmt_dec(abs_m);
      digits_str = (prec >= 0) ? sgm_fmt_dec_prec(raw, prec) : raw;
      sign_str = is_neg ? "-" : (sign_plus ? "+" : "");
      break;
    }
    case SGM_TAG_STR: {
      digits_str = (prec >= 0) ? sgm_utf8_truncate(v.u.str, prec) : v.u.str;
      break;
    }
    default: {
      digits_str = sgm_fmt_val(v);
      break;
    }
  }
  if (zero_pad && align == 0) {
    int cur_len = (int)strlen(sign_str) + sgm_utf8_cp_len(digits_str);
    if (width > cur_len) {
      const char *zeros = sgm_repeat_str("0", width - cur_len);
      return sgm_concat(3, (const char*[]){ sign_str, zeros, digits_str });
    }
    return sgm_concat(2, (const char*[]){ sign_str, digits_str });
  }
  const char *full_body = (sign_str[0] != '\0')
    ? sgm_concat(2, (const char*[]){ sign_str, digits_str })
    : digits_str;
  int cur_len = sgm_utf8_cp_len(full_body);
  if (width <= 0 || width <= cur_len) return full_body;
  int pad_total = width - cur_len;
  if (align == 1) {
    return sgm_concat(2, (const char*[]){ full_body, sgm_repeat_str(fill, pad_total) });
  }
  if (align == 3) {
    int lp = pad_total / 2;
    int rp = pad_total - lp;
    return sgm_concat(3, (const char*[]){ sgm_repeat_str(fill, lp), full_body, sgm_repeat_str(fill, rp) });
  }
  return sgm_concat(2, (const char*[]){ sgm_repeat_str(fill, pad_total), full_body });
}
|}

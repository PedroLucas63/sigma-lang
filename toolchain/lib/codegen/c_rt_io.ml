let c_runtime_io =
  {|__attribute__((unused)) static const char* sgm_trim_ws(const char *s) {
  size_t len = strlen(s);
  size_t start = 0;
  while (start < len && (s[start] == ' ' || s[start] == '\t')) start++;
  size_t end = len;
  while (end > start && (s[end - 1] == ' ' || s[end - 1] == '\t')) end--;
  char *out = (char*)malloc(end - start + 1);
  memcpy(out, s + start, end - start);
  out[end - start] = '\0';
  return out;
}

__attribute__((unused)) static const char* sgm_read_line(void) {
  fflush(stdout);
  size_t cap = 64;
  size_t len = 0;
  char *buf = (char*)malloc(cap);
  int c;
  bool got_any = false;
  while ((c = fgetc(stdin)) != EOF) {
    got_any = true;
    if (c == '\n') break;
    if (len + 1 >= cap) {
      cap *= 2;
      buf = (char*)realloc(buf, cap);
    }
    buf[len++] = (char)c;
  }
  if (!got_any && c == EOF) {
    free(buf);
    fprintf(stderr, "Runtime error: unexpected end of input (EOF) while reading from stdin\n");
    exit(1);
  }
  if (len > 0 && buf[len - 1] == '\r') len--;
  buf[len] = '\0';
  return buf;
}

__attribute__((unused)) static sgm_val_t sgm_parse_prim(sgm_tag_t tag, const char *raw) {
  switch (tag) {
    case SGM_TAG_I64: {
      const char *s = sgm_trim_ws(raw);
      size_t slen = strlen(s);
      size_t start = (slen > 0 && (s[0] == '+' || s[0] == '-')) ? 1 : 0;
      bool valid = (slen > start);
      for (size_t i = start; i < slen; i++) {
        if (s[i] < '0' || s[i] > '9') { valid = false; break; }
      }
      if (!valid) {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'int'\n", raw);
        exit(1);
      }
      bool neg = (s[0] == '-');
      uint64_t acc = 0;
      uint64_t limit = neg ? 9223372036854775808ULL : 9223372036854775807ULL;
      for (size_t i = start; i < slen; i++) {
        uint64_t d = (uint64_t)(s[i] - '0');
        if (acc > (limit - d) / 10ULL) {
          fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'int'\n", raw);
          exit(1);
        }
        acc = acc * 10ULL + d;
      }
      int64_t res = neg
        ? ((acc == 9223372036854775808ULL) ? (-9223372036854775807LL - 1LL) : -(int64_t)acc)
        : (int64_t)acc;
      return sgm_val_i64(res);
    }
    case SGM_TAG_F32: {
      const char *s = sgm_trim_ws(raw);
      if (s[0] == '\0') {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'float'\n", raw);
        exit(1);
      }
      char *endp = NULL;
      float v = strtof(s, &endp);
      if (!endp || *endp != '\0' || isnan(v) || isinf(v)) {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'float'\n", raw);
        exit(1);
      }
      return sgm_val_f32(v);
    }
    case SGM_TAG_F64: {
      const char *s = sgm_trim_ws(raw);
      if (s[0] == '\0') {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'double'\n", raw);
        exit(1);
      }
      char *endp = NULL;
      double v = strtod(s, &endp);
      if (!endp || *endp != '\0' || isnan(v) || isinf(v)) {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'double'\n", raw);
        exit(1);
      }
      return sgm_val_f64(v);
    }
    case SGM_TAG_DEC: {
      const char *s = sgm_trim_ws(raw);
      size_t slen = strlen(s);
      size_t idx = 0;
      int neg = 0;
      if (idx < slen && (s[idx] == '+' || s[idx] == '-')) {
        if (s[idx] == '-') neg = 1;
        idx++;
      }
      bool seen_dot = false;
      bool seen_digit = false;
      int scale = 0;
      sgm_u128 coeff = 0;
      for (; idx < slen; idx++) {
        char c = s[idx];
        if (c == '.') {
          if (seen_dot) { seen_digit = false; break; }
          seen_dot = true;
        } else if (c >= '0' && c <= '9') {
          seen_digit = true;
          coeff = coeff * 10 + (sgm_u128)(c - '0');
          if (seen_dot) scale++;
        } else {
          seen_digit = false;
          break;
        }
      }
      if (!seen_digit) {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'decimal'\n", raw);
        exit(1);
      }
      return sgm_val_dec(sgm_dec_normalize(neg, scale, coeff));
    }
    case SGM_TAG_BOOL: {
      const char *s = sgm_trim_ws(raw);
      if (strcmp(s, "true") == 0) return sgm_val_bool(true);
      if (strcmp(s, "false") == 0) return sgm_val_bool(false);
      fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'bool' (expected 'true' or 'false')\n", raw);
      exit(1);
    }
    case SGM_TAG_CHAR: {
      if (sgm_utf8_cp_len(raw) == 1) {
        char *dup = (char*)malloc(strlen(raw) + 1);
        strcpy(dup, raw);
        return sgm_val_char(dup);
      }
      const char *trimmed = sgm_trim_ws(raw);
      if (sgm_utf8_cp_len(trimmed) == 1) {
        return sgm_val_char(trimmed);
      }
      fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'char' (expected a single character)\n", raw);
      exit(1);
    }
    case SGM_TAG_STR: {
      char *dup = (char*)malloc(strlen(raw) + 1);
      strcpy(dup, raw);
      return sgm_val_str(dup);
    }
    default:
      fprintf(stderr, "Runtime error: unsupported composite input type\n");
      exit(1);
  }
}

__attribute__((unused)) static sgm_val_t sgm_input_types(int n, const sgm_tag_t *tags) {
  const char *line = sgm_read_line();
  if (n == 1) {
    if (tags[0] == SGM_TAG_STR) {
      return sgm_val_str(sgm_trim_ws(line));
    }
    return sgm_parse_prim(tags[0], line);
  }
  size_t len = strlen(line);
  const char **toks = (const char**)malloc((size_t)(n + 2) * sizeof(const char*));
  int count = 0;
  size_t i = 0;
  while (i < len) {
    while (i < len && (line[i] == ' ' || line[i] == '\t')) i++;
    if (i >= len) break;
    size_t start = i;
    while (i < len && line[i] != ' ' && line[i] != '\t') i++;
    if (count < n + 1) {
      char *tok = (char*)malloc(i - start + 1);
      memcpy(tok, line + start, i - start);
      tok[i - start] = '\0';
      toks[count] = tok;
    }
    count++;
  }
  if (count != n) {
    fprintf(stderr, "Runtime error: input expected %d whitespace-separated values, found %d in \"%s\"\n", n, count, line);
    exit(1);
  }
  sgm_obj_t *tup = sgm_obj_alloc(true, (int64_t)n);
  for (int k = 0; k < n; k++) {
    tup->items[k] = sgm_parse_prim(tags[k], toks[k]);
  }
  free(toks);
  return sgm_val_obj(tup);
}

__attribute__((unused)) static sgm_val_t sgm_input_mask(int n_parts, const int *is_type, const char **lits, const sgm_tag_t *tags) {
  const char *line = sgm_read_line();
  size_t hlen = strlen(line);
  size_t pos = 0;
  int n_vals = 0;
  for (int i = 0; i < n_parts; i++) {
    if (is_type[i]) n_vals++;
  }
  sgm_val_t *vals = (sgm_val_t*)malloc((size_t)n_vals * sizeof(sgm_val_t));
  int v_idx = 0;
  for (int i = 0; i < n_parts; i++) {
    if (!is_type[i]) {
      size_t llen = strlen(lits[i]);
      if (pos + llen > hlen || strncmp(line + pos, lits[i], llen) != 0) {
        fprintf(stderr, "Runtime error: input \"%s\" does not match mask delimiter \"%s\" at position %zu\n", line, lits[i], pos);
        exit(1);
      }
      pos += llen;
    } else {
      if (i + 1 >= n_parts) {
        size_t slen = hlen - pos;
        char *sub = (char*)malloc(slen + 1);
        memcpy(sub, line + pos, slen);
        sub[slen] = '\0';
        vals[v_idx++] = sgm_parse_prim(tags[i], sub);
        free(sub);
        pos = hlen;
      } else {
        const char *next_lit = lits[i + 1];
        const char *found = strstr(line + pos, next_lit);
        if (!found) {
          fprintf(stderr, "Runtime error: input \"%s\" is missing mask delimiter \"%s\"\n", line, next_lit);
          exit(1);
        }
        size_t slen = (size_t)(found - (line + pos));
        char *sub = (char*)malloc(slen + 1);
        memcpy(sub, line + pos, slen);
        sub[slen] = '\0';
        vals[v_idx++] = sgm_parse_prim(tags[i], sub);
        free(sub);
        pos = (size_t)(found - line);
      }
    }
  }
  if (pos != hlen) {
    fprintf(stderr, "Runtime error: unexpected trailing input \"%s\" after matching mask in \"%s\"\n", line + pos, line);
    exit(1);
  }
  if (n_vals == 1) {
    sgm_val_t single = vals[0];
    free(vals);
    return single;
  }
  sgm_obj_t *tup = sgm_obj_alloc(true, (int64_t)n_vals);
  for (int k = 0; k < n_vals; k++) {
    tup->items[k] = vals[k];
  }
  free(vals);
  return sgm_val_obj(tup);
}
|}

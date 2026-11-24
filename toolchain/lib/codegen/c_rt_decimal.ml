let c_runtime_decimal =
  {|#include <stdlib.h>
#include <string.h>
#include <math.h>

__extension__ typedef unsigned __int128 sgm_u128;
typedef struct {
  int neg;
  int scale;
  sgm_u128 coeff;
} sgm_decimal_t;

static const sgm_u128 SGM_U128_MAX = ~(sgm_u128)0;
static const sgm_u128 SGM_U128_MAX_DIV10 = (~(sgm_u128)0) / 10;
static const sgm_u128 SGM_U128_MAX_MUL10_ADD9 = ((~(sgm_u128)0) - 9) / 10;

__attribute__((unused)) static sgm_decimal_t sgm_dec_normalize(int neg, int scale, sgm_u128 coeff) {
  if (coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  while (scale < 0 && coeff <= SGM_U128_MAX_DIV10) {
    coeff *= 10;
    scale++;
  }
  if (scale < 0) scale = 0;
  while (scale > 0 && (coeff % 10) == 0) {
    coeff /= 10;
    scale--;
  }
  if (coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  return (sgm_decimal_t){ neg ? 1 : 0, scale, coeff };
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_neg(sgm_decimal_t d) {
  if (d.coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  d.neg = !d.neg;
  return d;
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_of_i64(int64_t n) {
  if (n == 0) return (sgm_decimal_t){ 0, 0, 0 };
  int neg = n < 0;
  uint64_t abs_v = (n == INT64_MIN) ? (uint64_t)0x8000000000000000ULL : (uint64_t)(neg ? -n : n);
  return sgm_dec_normalize(neg, 0, (sgm_u128)abs_v);
}

__attribute__((unused)) static void sgm_dec_align(sgm_decimal_t a, sgm_decimal_t b, sgm_u128 *ac, sgm_u128 *bc, int *sc) {
  *ac = a.coeff;
  int asc = a.scale;
  *bc = b.coeff;
  int bsc = b.scale;
  while (asc < bsc) {
    if (*ac <= SGM_U128_MAX_DIV10) { *ac *= 10; asc++; }
    else { *bc /= 10; bsc--; }
  }
  while (bsc < asc) {
    if (*bc <= SGM_U128_MAX_DIV10) { *bc *= 10; bsc++; }
    else { *ac /= 10; asc--; }
  }
  *sc = asc;
}

__attribute__((unused)) static int sgm_dec_cmp(sgm_decimal_t a, sgm_decimal_t b) {
  if (a.coeff == 0 && b.coeff == 0) return 0;
  if (a.coeff == 0) return b.neg ? 1 : -1;
  if (b.coeff == 0) return a.neg ? -1 : 1;
  if (a.neg != b.neg) return a.neg ? -1 : 1;
  sgm_u128 ac, bc;
  int sc;
  sgm_dec_align(a, b, &ac, &bc, &sc);
  int c = (ac < bc) ? -1 : ((ac > bc) ? 1 : 0);
  return a.neg ? -c : c;
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_add(sgm_decimal_t a, sgm_decimal_t b) {
  if (a.coeff == 0) return b;
  if (b.coeff == 0) return a;
  sgm_u128 ac, bc;
  int sc;
  sgm_dec_align(a, b, &ac, &bc, &sc);
  if (a.neg == b.neg) {
    return sgm_dec_normalize(a.neg, sc, ac + bc);
  } else {
    if (ac == bc) return (sgm_decimal_t){ 0, 0, 0 };
    else if (ac > bc) return sgm_dec_normalize(a.neg, sc, ac - bc);
    else return sgm_dec_normalize(b.neg, sc, bc - ac);
  }
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_sub(sgm_decimal_t a, sgm_decimal_t b) {
  return sgm_dec_add(a, sgm_dec_neg(b));
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_mul(sgm_decimal_t a, sgm_decimal_t b) {
  if (a.coeff == 0 || b.coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  sgm_u128 ac = a.coeff, bc = b.coeff;
  int asc = a.scale, bsc = b.scale;
  while (bc != 0 && ac > SGM_U128_MAX / bc && (asc > 0 || bsc > 0)) {
    if (asc >= bsc) { ac /= 10; asc--; }
    else { bc /= 10; bsc--; }
  }
  return sgm_dec_normalize(a.neg != b.neg, asc + bsc, ac * bc);
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_div(sgm_decimal_t a, sgm_decimal_t b) {
  if (b.coeff == 0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  if (a.coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  int neg = a.neg != b.neg;
  sgm_u128 q = a.coeff / b.coeff;
  sgm_u128 rem = a.coeff % b.coeff;
  int sc = a.scale - b.scale;
  while (rem != 0 && sc < 28 && q <= SGM_U128_MAX_MUL10_ADD9 && rem <= SGM_U128_MAX_DIV10) {
    sgm_u128 rem10 = rem * 10;
    sgm_u128 d = rem10 / b.coeff;
    rem = rem10 % b.coeff;
    q = q * 10 + d;
    sc++;
  }
  return sgm_dec_normalize(neg, sc, q);
}

__attribute__((unused)) static int64_t sgm_dec_idiv(sgm_decimal_t a, sgm_decimal_t b) {
  if (b.coeff == 0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  if (a.coeff == 0) return 0;
  sgm_u128 ac, bc;
  int sc;
  sgm_dec_align(a, b, &ac, &bc, &sc);
  int64_t q = (int64_t)(ac / bc);
  return (a.neg != b.neg) ? -q : q;
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_rem(sgm_decimal_t a, sgm_decimal_t b) {
  if (b.coeff == 0) { fprintf(stderr, "Runtime error: Modulo by zero\n"); exit(1); }
  if (a.coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  sgm_u128 ac, bc;
  int sc;
  sgm_dec_align(a, b, &ac, &bc, &sc);
  sgm_u128 r_mag = ac % bc;
  if (r_mag == 0) return (sgm_decimal_t){ 0, 0, 0 };
  if (!a.neg) return sgm_dec_normalize(0, sc, r_mag);
  else return sgm_dec_normalize(0, sc, bc - r_mag);
}

__attribute__((unused)) static const char* sgm_fmt_dec(sgm_decimal_t d) {
  char *out = (char*)malloc(80);
  if (d.coeff == 0) {
    strcpy(out, "0");
    return out;
  }
  char digits[64];
  int len = 0;
  sgm_u128 tmp = d.coeff;
  while (tmp > 0) {
    digits[len++] = (char)('0' + (int)(tmp % 10));
    tmp /= 10;
  }
  int pos = 0;
  if (d.neg) out[pos++] = '-';
  if (d.scale <= 0) {
    for (int i = len - 1; i >= 0; i--) out[pos++] = digits[i];
  } else if (len <= d.scale) {
    out[pos++] = '0';
    out[pos++] = '.';
    for (int i = 0; i < d.scale - len; i++) out[pos++] = '0';
    for (int i = len - 1; i >= 0; i--) out[pos++] = digits[i];
  } else {
    for (int i = len - 1; i >= d.scale; i--) out[pos++] = digits[i];
    out[pos++] = '.';
    for (int i = d.scale - 1; i >= 0; i--) out[pos++] = digits[i];
  }
  out[pos] = '\0';
  return out;
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_pow(sgm_decimal_t a, sgm_decimal_t b) {
  if (b.scale == 0 && b.coeff <= 1000) {
    int exp = (int)b.coeff;
    sgm_decimal_t base = a;
    sgm_decimal_t acc = sgm_dec_of_i64(1);
    while (exp > 0) {
      if (exp & 1) acc = sgm_dec_mul(acc, base);
      base = sgm_dec_mul(base, base);
      exp >>= 1;
    }
    if (b.neg) return sgm_dec_div(sgm_dec_of_i64(1), acc);
    return acc;
  }
  double da = strtod(sgm_fmt_dec(a), NULL);
  double db = strtod(sgm_fmt_dec(b), NULL);
  double r = pow(da, db);
  char buf[64];
  snprintf(buf, sizeof(buf), "%.15g", r);
  int neg = (r < 0);
  if (neg) r = -r;
  int64_t scaled = (int64_t)llround(r * 1e12);
  return sgm_dec_normalize(neg, 12, (sgm_u128)(uint64_t)scaled);
}

__attribute__((unused)) static void sgm_print_decimal(sgm_decimal_t d) {
  const char *s = sgm_fmt_dec(d);
  puts(s);
}
|}

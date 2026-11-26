#include <stdint.h>
#include <stdbool.h>
#include <string.h>

static int64_t g_acc = 0;

int64_t c_add(int64_t a, int64_t b) {
  return a + b;
}

int64_t c_sub_impl(int64_t a, int64_t b) {
  return a - b;
}

float c_mul_f32(float a, float b) {
  return a * b;
}

double c_avg_f64(double a, double b) {
  return (a + b) * 0.5;
}

bool c_is_even(int64_t n) {
  return (n % 2) == 0;
}

uint32_t c_next_char(uint32_t cp) {
  return cp + 1;
}

int64_t c_str_len(const char *s) {
  return s ? (int64_t)strlen(s) : 0;
}

const char *c_greet(void) {
  return "hello_from_c";
}

void c_acc_add(int64_t delta) {
  g_acc += delta;
}

int64_t c_acc_get(void) {
  return g_acc;
}

double c_sum_f64_arr(double *xs, int64_t n) {
  double total = 0.0;
  for (int64_t i = 0; i < n; i++) {
    total += xs[i];
  }
  return total;
}

void c_scale_i64_arr(int64_t *xs, int64_t n, int64_t factor) {
  for (int64_t i = 0; i < n; i++) {
    xs[i] *= factor;
  }
}

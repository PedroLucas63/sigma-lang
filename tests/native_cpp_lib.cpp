#include <cstdint>
#include <numeric>
#include <vector>

extern "C" {

int64_t cpp_dot_i64(const int64_t *a, const int64_t *b, int64_t n) {
  std::vector<int64_t> prod(static_cast<size_t>(n));
  for (int64_t i = 0; i < n; i++) {
    prod[static_cast<size_t>(i)] = a[i] * b[i];
  }
  return std::accumulate(prod.begin(), prod.end(), int64_t{0});
}

}

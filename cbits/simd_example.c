#include "simd.h"
#include "simd_debug.h"

int example_main() {
  uint8_t source[32] = "01234567890123456789012345678901";
  uint8_t target[33];
  avx2_memcpy(target, source, 32);
  target[32] = 0;
  printf("%s\n", target);

  // uint8_t source2[64] = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
  // uint64_t target2[1] = {0};
  // avx2_cmpeq8('0', target2, 1, source2);
  // printf("%llu\n", target2[0]);

  return 0;
}

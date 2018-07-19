#include "simd.h"

#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

void sse_cmpeq8(
    uint8_t byte,
    uint64_t *target,
    size_t target_length,
    uint8_t *source) {
  uint16_t *target16 = (uint16_t *)target;

  __m128i v_comparand = _mm_set1_epi8(byte);

  uint16_t *out_mask = (uint16_t*)target;
  size_t i;

  for (i = 0; i < target_length * 4; ++i) {
    __m128i v_data_a = *(__m128i*)(source + (i * 16));
    __m128i v_results_a = _mm_cmpeq_epi8(v_data_a, v_comparand);
    uint16_t mask = (uint16_t)_mm_movemask_epi8(v_results_a);
    target16[i] = mask;
  }
}

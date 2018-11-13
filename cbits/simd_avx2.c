#include "simd.h"

#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

void avx2_memcpy(
    uint8_t *target,
    uint8_t *source,
    size_t len) {
#if defined(AVX2_ENABLED)
  size_t aligned_len    = (len / 32) * 32;
  size_t remaining_len  = len - aligned_len;

  size_t i;

  for (i = 0; i < aligned_len; i += 32) {
    __m128i v0 = *(__m128i*)(source + i     );
    __m128i v1 = *(__m128i*)(source + i + 16);

    *(__m128i*)(target + i      ) = v0;
    *(__m128i*)(target + i + 16 ) = v1;
  }

  memcpy(target + aligned_len, source + aligned_len, remaining_len);
#endif
}

void avx2_cmpeq8(
    uint8_t byte,
    uint8_t *target,
    size_t target_length,
    uint8_t *source) {
#if defined(AVX2_ENABLED)
  uint32_t *target32 = (uint32_t *)target;

  __m256i v_comparand = _mm256_set1_epi8(byte);

  uint32_t *out_mask = (uint32_t*)target;

  size_t i;

  for (i = 0; i < target_length * 2; ++i) {
    __m256i v_data_a = *(__m256i *)(source + (i * 32));
    __m256i v_results_a = _mm256_cmpeq_epi8(v_data_a, v_comparand);
    uint32_t mask = (uint32_t)_mm256_movemask_epi8(v_results_a);
    target32[i] = mask;
  }
#endif
}

void avx2_cmpeq8_para(
    uint8_t *bytes,
    size_t bytes_length,
    uint8_t **targets,
    size_t targets_length,
    uint8_t *source) {
#if defined(AVX2_ENABLED)
  size_t i;

  for (i = 0; i < targets_length * 2; ++i) {
    size_t j;

    __m256i v_data_a = *(__m256i *)(source + (i * 32));

    for (j = 0; j < bytes_length; ++j) {
      uint8_t *target     = targets[j];
      uint32_t *target32  = (uint32_t *)target;
      __m256i v_comparand = _mm256_set1_epi8(bytes[j]);
      uint32_t *out_mask  = (uint32_t*)target;
      __m256i v_results_a = _mm256_cmpeq_epi8(v_data_a, v_comparand);
      uint32_t mask       = (uint32_t)_mm256_movemask_epi8(v_results_a);
      target32[i]         = mask;
    }
  }
#endif
}

void avx2_and_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b) {
#if defined(AVX2_ENABLED)
  size_t i;

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data_a   = *(__m256i *)(source_a + i);
    __m256i v_data_b   = *(__m256i *)(source_b + i);
    __m256i v_results  = _mm256_and_si256(v_data_a, v_data_b);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}

void avx2_and_not_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b) {
#if defined(AVX2_ENABLED)
  size_t i;

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data_a   = *(__m256i *)(source_a + i);
    __m256i v_data_b   = *(__m256i *)(source_b + i);
    __m256i v_results  = _mm256_andnot_si256(v_data_a, v_data_b);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}

void avx2_not_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source) {
#if defined(AVX2_ENABLED)
  __m256i ones = _mm256_set1_epi8(0xff);

  size_t i;

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data     = *(__m256i *)(source + i);
    __m256i v_results  = _mm256_xor_si256(v_data, ones);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}

void avx2_or_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b) {
#if defined(AVX2_ENABLED)
  size_t i;

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data_a   = *(__m256i *)(source_a + i);
    __m256i v_data_b   = *(__m256i *)(source_b + i);
    __m256i v_results  = _mm256_or_si256(v_data_a, v_data_b);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}

void avx2_xor_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b) {
#if defined(AVX2_ENABLED)
  size_t i;

  for (i = 0; i < target_length; i += 32) {
    __m256i v_data_a   = *(__m256i *)(source_a + i);
    __m256i v_data_b   = *(__m256i *)(source_b + i);
    __m256i v_results  = _mm256_xor_si256(v_data_a, v_data_b);
    *(__m256i *)(target + i) = v_results;
  }
#endif
}

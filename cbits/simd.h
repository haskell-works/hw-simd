#include <unistd.h>
#include <stdint.h>

typedef uint8_t v32si __attribute__ ((vector_size (32)));
typedef uint8_t v16si __attribute__ ((vector_size (16)));

void avx2_memcpy(
    uint8_t *target,
    uint8_t *source,
    size_t len);

void avx2_cmpeq8(
    uint8_t byte,
    uint8_t *target,
    size_t target_length,
    uint8_t *source);

void avx2_cmpeq8_para(
    uint8_t *bytes,
    size_t bytes_length,
    uint8_t **targets,
    size_t targets_length,
    uint8_t *source);

void avx2_and_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b);

void avx2_and_not_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b);

void avx2_not_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source);

void avx2_or_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b);

void avx2_xor_bits(
    uint8_t *target,
    size_t target_length,
    uint8_t *source_a,
    uint8_t *source_b);

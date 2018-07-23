#include "simd.h"

#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

void print_bits_16(uint16_t word) {
  size_t i;

  putc('|', stdout);

  for (i = 0; i < 16; ++i) {
    putc((word & (1L << i)) ? '1' : '0', stdout);
  }
  printf("|\n");
}

void print_m256_hex(v32si number) {
  char buffer[32];

  *((v32si*)buffer) = number;

  int i = 0;

  for (i = 0; i < 32; ++i) {
    printf("%02x", buffer[31 - i]);
  }

  printf("\n");
}

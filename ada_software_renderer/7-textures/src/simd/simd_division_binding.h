#ifndef SIMD_DIVISION_H
#define SIMD_DIVISION_H
#include <arm_neon.h>

float32x2_t float32x2_divide(float32x2_t A, float32x2_t B);

float32x4_t float32x4_divide(float32x4_t A, float32x4_t B);

#endif

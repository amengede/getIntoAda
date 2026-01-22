#ifndef SIMD_SUBTRACT_H
#define SIMD_SUBTRACT_H
#include <arm_neon.h>

int32x2_t int32x2_sub(int32x2_t A, int32x2_t B);

float32x2_t float32x2_sub(float32x2_t A, float32x2_t B);

float32x4_t float32x4_sub(float32x4_t A, float32x4_t B);

#endif

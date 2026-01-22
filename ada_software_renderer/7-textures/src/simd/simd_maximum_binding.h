#ifndef SIMD_MAXIMUM_H
#define SIMD_MAXIMUM_H
#include <arm_neon.h>

int32x2_t int32x2_max(int32x2_t A, int32x2_t B);

float32x2_t float32x2_max(float32x2_t A, float32x2_t B);

#endif

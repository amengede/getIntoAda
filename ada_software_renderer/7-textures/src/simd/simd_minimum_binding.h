#ifndef SIMD_MINIMUM_H
#define SIMD_MINIMUM_H
#include <arm_neon.h>

int32x2_t int32x2_min(int32x2_t A, int32x2_t B);

float32x2_t float32x2_min(float32x2_t A, float32x2_t B);
#endif

#ifndef SIMD_CONVERSION_H
#define SIMD_CONVERSION_H
#include <arm_neon.h>

float32x2_t float32x2_convert_int32x2(int32x2_t A);

float32x4_t float32x4_convert_int32x4(int32x4_t A);

int32x2_t int32x2_convert_float32x2(float32x2_t A);

uint32x4_t uint32x4_convert_float32x4(float32x4_t A);

#endif

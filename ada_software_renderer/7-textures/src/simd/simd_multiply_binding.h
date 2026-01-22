#ifndef SIMD_MULTIPLY_H
#define SIMD_MULTIPLY_H
#include <arm_neon.h>

float32x2_t float32x2_mul(float32x2_t A, float32x2_t B);

float32x4_t float32x4_mul(float32x4_t A, float32x4_t B);

uint32x4_t uint32x4_mul(uint32x4_t A, uint32x4_t B);

int32x4_t int32x4_mul(int32x4_t A, int32x4_t B);

float32x2_t float32x2_fmadd(float32x2_t A, float32x2_t B, float32x2_t C);

float32x4_t float32x4_fmadd(float32x4_t A, float32x4_t B, float32x4_t C);

#endif

#include "simd_multiply_binding.h"
#include <arm_neon.h>

float32x2_t float32x2_mul(float32x2_t A, float32x2_t B) {
    return vmul_f32(A, B);
}

float32x4_t float32x4_mul(float32x4_t A, float32x4_t B) {
    return vmulq_f32(A, B);
}

uint32x4_t uint32x4_mul(uint32x4_t A, uint32x4_t B) {
    return vmulq_u32(A, B);
}

int32x4_t int32x4_mul(int32x4_t A, int32x4_t B) {
    return vmulq_s32(A, B);
}

float32x2_t float32x2_fmadd(float32x2_t A, float32x2_t B, float32x2_t C) {
    return vfma_f32(A, B, C);
}

float32x4_t float32x4_fmadd(float32x4_t A, float32x4_t B, float32x4_t C) {
    return vfmaq_f32(A, B, C);
}

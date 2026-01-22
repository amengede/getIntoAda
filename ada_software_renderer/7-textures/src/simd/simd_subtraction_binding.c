#include "simd_subtraction_binding.h"
#include <arm_neon.h>

int32x2_t int32x2_sub(int32x2_t A, int32x2_t B) {
    return vsub_s32(A, B);
}

float32x2_t float32x2_sub(float32x2_t A, float32x2_t B) {
    return vsub_f32(A, B);
}

float32x4_t float32x4_sub(float32x4_t A, float32x4_t B) {
    return vsubq_f32(A, B);
}

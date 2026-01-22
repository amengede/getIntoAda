#include "simd_division_binding.h"
#include <arm_neon.h>

float32x2_t float32x2_divide(float32x2_t A, float32x2_t B) {
    return vdiv_f32(A, B);
}

float32x4_t float32x4_divide(float32x4_t A, float32x4_t B) {
    return vdivq_f32(A, B);
}

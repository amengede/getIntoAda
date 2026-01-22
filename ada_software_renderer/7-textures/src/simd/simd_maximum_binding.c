#include "simd_maximum_binding.h"
#include <arm_neon.h>

int32x2_t int32x2_max(int32x2_t A, int32x2_t B) {
    return vmax_s32(A, B);
}

float32x2_t float32x2_max(float32x2_t A, float32x2_t B) {
    return vmax_f32(A, B);
}

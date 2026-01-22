#include "simd_minimum_binding.h"

int32x2_t int32x2_min(int32x2_t A, int32x2_t B) {
    return vmin_s32(A, B);
}

float32x2_t float32x2_min(float32x2_t A, float32x2_t B) {
    return vmin_f32(A, B);
}

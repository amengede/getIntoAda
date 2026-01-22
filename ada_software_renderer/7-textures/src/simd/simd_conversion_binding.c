#include "simd_conversion_binding.h"
#include <arm_neon.h>

float32x2_t float32x2_convert_int32x2(int32x2_t A) {
    return vcvt_f32_s32(A);
}

float32x4_t float32x4_convert_int32x4(int32x4_t A) {
    return vcvtq_f32_s32(A);
}

int32x2_t int32x2_convert_float32x2(float32x2_t A) {
    return vcvt_s32_f32(A);
}

uint32x4_t uint32x4_convert_float32x4(float32x4_t A) {
    return vcvtq_u32_f32(A);
}

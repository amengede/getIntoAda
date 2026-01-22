#include "simd_binding.h"
#include <arm_neon.h>

/*---- Addition ----*/
int8x8_t int8x8_add(int8x8_t A, int8x8_t B) {
    return vadd_s8(A, B);
}

int8x16_t int8x16_add(int8x16_t A, int8x16_t B) {
    return vaddq_s8(A, B);
}

int16x4_t int16x4_add(int16x4_t A, int16x4_t B) {
    return vadd_s16(A, B);
}

int16x8_t int16x8_add(int16x8_t A, int16x8_t B) {
    return vaddq_s16(A, B);
}

int32x2_t int32x2_add(int32x2_t A, int32x2_t B) {
    return vadd_s32(A, B);
}

int32x4_t int32x4_add(int32x4_t A, int32x4_t B) {
    return vaddq_s32(A, B);
}

int64x2_t int64x2_add(int64x2_t A, int64x2_t B) {
    return vaddq_s64(A, B);
}

uint8x8_t uint8x8_add(uint8x8_t A, uint8x8_t B) {
    return vadd_u8(A, B);
}

uint8x16_t uint8x16_add(uint8x16_t A, uint8x16_t B) {
    return vaddq_u8(A, B);
}

uint16x4_t uint16x4_add(uint16x4_t A, uint16x4_t B) {
    return vadd_u16(A, B);
}

uint16x8_t uint16x8_add(uint16x8_t A, uint16x8_t B) {
    return vaddq_u16(A, B);
}

uint32x2_t uint32x2_add(uint32x2_t A, uint32x2_t B) {
    return vadd_u32(A, B);
}

uint32x4_t uint32x4_add(uint32x4_t A, uint32x4_t B) {
    return vaddq_u32(A, B);
}

uint64x2_t uint64x2_add(uint64x2_t A, uint64x2_t B) {
    return vaddq_u64(A, B);
}

float32x2_t float32x2_add(float32x2_t A, float32x2_t B) {
    return vadd_f32(A, B);
}

float32x4_t float32x4_add(float32x4_t A, float32x4_t B) {
    return vaddq_f32(A, B);
}

float64x2_t float64x2_add(float64x2_t A, float64x2_t B) {
    return vaddq_f64(A, B);
}

/*---- Widening Addition ----*/
int16x8_t int16x8_widening_add_int8x8_int8x8(int8x8_t A, int8x8_t B) {
    return vaddl_s8(A, B);
}

int32x4_t int32x4_widening_add_int16x4_int16x4(int16x4_t A, int16x4_t B) {
    return vaddl_s16(A, B);
}

int64x2_t int64x2_widening_add_int32x2_int32x2(int32x2_t A, int32x2_t B) {
    return vaddl_s32(A, B);
}

uint16x8_t uint16x8_widening_add_uint8x8_uint8x8(uint8x8_t A, uint8x8_t B) {
    return vaddl_u8(A, B);
}

uint32x4_t uint32x4_widening_add_uint16x4_uint16x4(
    uint16x4_t A, uint16x4_t B) {
return vaddl_u16(A, B);
}

uint64x2_t uint64x2_widening_add_uint32x2_uint32x2(
    uint32x2_t A, uint32x2_t B) {
return vaddl_u32(A, B);
}

int16x8_t int16x8_widening_add_int8x16_int8x16(int8x16_t A, int8x16_t B) {
    return vaddl_high_s8(A, B);
}

int32x4_t int32x4_widening_add_int16x8_int16x8(int16x8_t A, int16x8_t B) {
    return vaddl_high_s16(A, B);
}

int64x2_t int64x2_widening_add_int32x4_int32x4(int32x4_t A, int32x4_t B) {
    return vaddl_high_s32(A, B);
}

uint16x8_t uint16x8_widening_add_uint8x16_uint8x16(
    uint8x16_t A, uint8x16_t B) {
return vaddl_high_u8(A, B);
}

uint32x4_t uint32x4_widening_add_uint16x8_uint16x8(
    uint16x8_t A, uint16x8_t B) {
return vaddl_high_u16(A, B);
}

uint64x2_t uint64x2_widening_high_add_uint32x4_uint32x4(
    uint32x4_t A, uint32x4_t B) {
return vaddl_high_u32(A, B);
}

int16x8_t int16x8_widening_add_int16x8_int8x8(int16x8_t A, int8x8_t B) {
    return vaddw_s8(A, B);
}

int32x4_t int32x4_widening_add_int32x4_int16x4(int32x4_t A, int16x4_t B) {
    return vaddw_s16(A, B);
}

int64x2_t int64x2_widening_add_int64x2_int32x2(int64x2_t A, int32x2_t B) {
    return vaddw_s32(A, B);
}

uint16x8_t uint16x8_widening_add_uint16x8_uint8x8(
    uint16x8_t A, uint8x8_t B) {
return vaddw_u8(A, B);
}

uint32x4_t uint32x4_widening_add_uint32x4_uint16x4(
    uint32x4_t A, uint16x4_t B) {
return vaddw_u16(A, B);
}

uint64x2_t uint64x2_widening_add_uint64x2_uint32x2(
    uint64x2_t A, uint32x2_t B) {
return vaddw_u32(A, B);
}

int16x8_t int16x8_widening_add_int16x8_int8x16(int16x8_t A, int8x16_t B) {
    return vaddw_high_s8(A, B);
}

int32x4_t int32x4_widening_add_int32x4_int16x8(int32x4_t A, int16x8_t B) {
    return vaddw_high_s16(A, B);
}

int64x2_t int64x2_widening_add_int64x2_int32x4(int64x2_t A, int32x4_t B) {
    return vaddw_high_s32(A, B);
}

uint16x8_t uint16x8_widening_add_uint16x8_uint8x16(
    uint16x8_t A, uint8x16_t B) {
return vaddw_high_u8(A, B);
}

uint32x4_t uint32x4_widening_add_uint32x4_uint16x8(
    uint32x4_t A, uint16x8_t B) {
return vaddw_high_u16(A, B);
}

uint64x2_t uint64x2_widening_add_uint64x2_uint32x4(
    uint64x2_t A, uint32x4_t B) {
return vaddw_high_u32(A, B);
}

/*---- Subtract ----*/

int32x2_t int32x2_sub(int32x2_t A, int32x2_t B) {
    return vsub_s32(A, B);
}

/*---- Minimum ----*/

int32x2_t int32x2_min(int32x2_t A, int32x2_t B) {
    return vmin_s32(A, B);
}

/*---- Maximum ----*/

int32x2_t int32x2_max(int32x2_t A, int32x2_t B) {
    return vmax_s32(A, B);
}

/*---- Multiply ----*/

int32x4_t int32x2_mul(int32x4_t A, int32x4_t B) {
    return vmulq_s32(A, B);
}

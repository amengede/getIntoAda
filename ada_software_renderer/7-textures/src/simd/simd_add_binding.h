#ifndef SIMD_ADD_H
#define SIMD_ADD_H
#include <arm_neon.h>

int8x8_t int8x8_add(int8x8_t A, int8x8_t B);

int8x16_t int8x16_add(int8x16_t A, int8x16_t B);

int16x4_t int16x4_add(int16x4_t A, int16x4_t B);

int16x8_t int16x8_add(int16x8_t A, int16x8_t B);

int32x2_t int32x2_add(int32x2_t A, int32x2_t B);

int32x4_t int32x4_add(int32x4_t A, int32x4_t B);

int64x2_t int64x2_add(int64x2_t A, int64x2_t B);

uint8x8_t uint8x8_add(uint8x8_t A, uint8x8_t B);

uint8x16_t uint8x16_add(uint8x16_t A, uint8x16_t B);

uint16x4_t uint16x4_add(uint16x4_t A, uint16x4_t B);

uint16x8_t uint16x8_add(uint16x8_t A, uint16x8_t B);

uint32x2_t uint32x2_add(uint32x2_t A, uint32x2_t B);

uint32x4_t uint32x4_add(uint32x4_t A, uint32x4_t B);

uint64x2_t uint64x2_add(uint64x2_t A, uint64x2_t B);

float32x2_t float32x2_add(float32x2_t A, float32x2_t B);

float32x4_t float32x4_add(float32x4_t A, float32x4_t B);

float64x2_t float64x2_add(float64x2_t A, float64x2_t B);

int16x8_t int16x8_widening_add_int8x8_int8x8(int8x8_t A, int8x8_t B);

int32x4_t int32x4_widening_add_int16x4_int16x4(int16x4_t A, int16x4_t B);

int64x2_t int64x2_widening_add_int32x2_int32x2(int32x2_t A, int32x2_t B);

uint16x8_t uint16x8_widening_add_uint8x8_uint8x8(uint8x8_t A, uint8x8_t B);

uint32x4_t uint32x4_widening_add_uint16x4_uint16x4(
    uint16x4_t A, uint16x4_t B);

uint64x2_t uint64x2_widening_add_uint32x2_uint32x2(
    uint32x2_t A, uint32x2_t B);

int16x8_t int16x8_widening_add_int8x16_int8x16(int8x16_t A, int8x16_t B);

int32x4_t int32x4_widening_add_int16x8_int16x8(int16x8_t A, int16x8_t B);

int64x2_t int64x2_widening_add_int32x4_int32x4(int32x4_t A, int32x4_t B);

uint16x8_t uint16x8_widening_add_uint8x16_uint8x16(
    uint8x16_t A, uint8x16_t B);

uint32x4_t uint32x4_widening_add_uint16x8_uint16x8(
    uint16x8_t A, uint16x8_t B);

uint64x2_t uint64x2_widening_high_add_uint32x4_uint32x4(
    uint32x4_t A, uint32x4_t B);

int16x8_t int16x8_widening_add_int16x8_int8x8(int16x8_t A, int8x8_t B);

int32x4_t int32x4_widening_add_int32x4_int16x4(int32x4_t A, int16x4_t B);

int64x2_t int64x2_widening_add_int64x2_int32x2(int64x2_t A, int32x2_t B);

uint16x8_t uint16x8_widening_add_uint16x8_uint8x8(
    uint16x8_t A, uint8x8_t B);

uint32x4_t uint32x4_widening_add_uint32x4_uint16x4(
    uint32x4_t A, uint16x4_t B);

uint64x2_t uint64x2_widening_add_uint64x2_uint32x2(uint64x2_t A, uint32x2_t B);

int16x8_t int16x8_widening_add_int16x8_int8x16(int16x8_t A, int8x16_t B);

int32x4_t int32x4_widening_add_int32x4_int16x8(int32x4_t A, int16x8_t B);

int64x2_t int64x2_widening_add_int64x2_int32x4(int64x2_t A, int32x4_t B);

uint16x8_t uint16x8_widening_add_uint16x8_uint8x16(
    uint16x8_t A, uint8x16_t B);

uint32x4_t uint32x4_widening_add_uint32x4_uint16x8(
    uint32x4_t A, uint16x8_t B);

uint64x2_t uint64x2_widening_add_uint64x2_uint32x4(
    uint64x2_t A, uint32x4_t B);

#endif

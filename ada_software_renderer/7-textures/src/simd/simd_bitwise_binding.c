#include "simd_bitwise_binding.h"

uint32x4_t uint32x4_or(uint32x4_t A, uint32x4_t B) {
    return vorrq_u32(A, B);
}

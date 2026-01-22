with Interfaces; use Interfaces;

package SIMD is

    type Vector_256_U32 is array (0 .. 7) of Unsigned_32
        with Alignment => 32;
        pragma Machine_Attribute (Vector_256_U32, "vector_type");
        pragma Machine_Attribute (Vector_256_U32, "may_alias");

end SIMD;

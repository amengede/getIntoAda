with Interfaces; use Interfaces;

package AVX2 is

    type Vector_256_U32 is array (0 .. 7) of Unsigned_32
        with Alignment => 32, Size => 256;
    pragma Machine_Attribute (Vector_256_U32, "vector_type");
    pragma Machine_Attribute (Vector_256_U32, "may_alias");

    function "+" (Left, Right : Vector_256_U32) return Vector_256_U32;

end AVX2;

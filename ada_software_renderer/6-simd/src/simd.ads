with Interfaces; use Interfaces;
package Simd is

    type Int8x8 is array (0 .. 7) of Integer_8
        with Alignment => 8,
        Object_Size => 64;
    pragma Machine_Attribute (Int8x8, "vector_type");
    pragma Machine_Attribute (Int8x8, "may_alias");

    type Int8x16 is array (0 .. 15) of Integer_8
        with Alignment => 16,
        Object_Size => 128;
    pragma Machine_Attribute (Int8x16, "vector_type");
    pragma Machine_Attribute (Int8x16, "may_alias");

    type Int16x4 is array (0 .. 3) of Integer_16
        with Alignment => 16,
        Object_Size => 64;
    pragma Machine_Attribute (Int16x4, "vector_type");
    pragma Machine_Attribute (Int16x4, "may_alias");

    type Int16x8 is array (0 .. 7) of Integer_16
        with Alignment => 16,
        Object_Size => 128;
    pragma Machine_Attribute (Int16x8, "vector_type");
    pragma Machine_Attribute (Int16x8, "may_alias");

    type Int32x2 is array (0 .. 1) of Integer_32
        with Alignment => 16,
        Object_Size => 64;
    pragma Machine_Attribute (Int32x2, "vector_type");
    pragma Machine_Attribute (Int32x2, "may_alias");

    type Int32x4 is array (0 .. 3) of Integer_32
        with Alignment => 16,
        Object_Size => 128;
    pragma Machine_Attribute (Int32x4, "vector_type");
    pragma Machine_Attribute (Int32x4, "may_alias");

    type Int64x2 is array (0 .. 1) of Integer_64
        with Alignment => 64,
        Object_Size => 128;
    pragma Machine_Attribute (Int64x2, "vector_type");
    pragma Machine_Attribute (Int64x2, "may_alias");

    type Uint8x8 is array (0 .. 7) of Unsigned_8
        with Alignment => 8,
        Object_Size => 64;
    pragma Machine_Attribute (Uint8x8, "vector_type");
    pragma Machine_Attribute (Uint8x8, "may_alias");

    type Uint8x16 is array (0 .. 15) of Unsigned_8
        with Alignment => 16,
        Object_Size => 128;
    pragma Machine_Attribute (Uint8x16, "vector_type");
    pragma Machine_Attribute (Uint8x16, "may_alias");

    type Uint16x4 is array (0 .. 3) of Unsigned_16
        with Alignment => 16,
        Object_Size => 64;
    pragma Machine_Attribute (Uint16x4, "vector_type");
    pragma Machine_Attribute (Uint16x4, "may_alias");

    type Uint16x8 is array (0 .. 7) of Unsigned_16
        with Alignment => 16,
        Object_Size => 128;
    pragma Machine_Attribute (Uint16x8, "vector_type");
    pragma Machine_Attribute (Uint16x8, "may_alias");

    type Uint32x2 is array (0 .. 1) of Unsigned_32
        with Alignment => 16,
        Object_Size => 64;
    pragma Machine_Attribute (Uint32x2, "vector_type");
    pragma Machine_Attribute (Uint32x2, "may_alias");

    type Uint32x4 is array (0 .. 3) of Unsigned_32
        with Alignment => 16,
        Object_Size => 128;
    pragma Machine_Attribute (Uint32x4, "vector_type");
    pragma Machine_Attribute (Uint32x4, "may_alias");

    type Uint32x8 is array (0 .. 7) of Unsigned_32
        with Alignment => 16,
        Object_Size => 256;
        pragma Machine_Attribute (Uint32x8, "vector_type");
        pragma Machine_Attribute (Uint32x8, "may_alias");

    type Uint64x2 is array (0 .. 1) of Unsigned_64
        with Alignment => 64,
        Object_Size => 128;
    pragma Machine_Attribute (Uint64x2, "vector_type");
    pragma Machine_Attribute (Uint64x2, "may_alias");

    type Float32x2 is array (0 .. 1) of IEEE_Float_32
        with Alignment => 32,
        Object_Size => 64;
    pragma Machine_Attribute (Float32x2, "vector_type");
    pragma Machine_Attribute (Float32x2, "may_alias");

    type Float32x4 is array (0 .. 3) of IEEE_Float_32
        with Alignment => 32,
        Object_Size => 128;
    pragma Machine_Attribute (Float32x4, "vector_type");
    pragma Machine_Attribute (Float32x4, "may_alias");

    type Float64x2 is array (0 .. 1) of IEEE_Float_64
        with Alignment => 64,
        Object_Size => 128;
    pragma Machine_Attribute (Float64x2, "vector_type");
    pragma Machine_Attribute (Float64x2, "may_alias");

end Simd;


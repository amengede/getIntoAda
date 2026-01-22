package Simd.Adds is

    -- Addition
    function Add (A, B : Int8x8) return Int8x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int8x8_add";
    function Add (A, B : Int8x16) return Int8x16 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int8x16_add";
    function Add (A, B : Int16x4) return Int16x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int16x4_add";
    function Add (A, B : Int16x8) return Int16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int16x8_add";
    function Add (A, B : Int32x2) return Int32x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x2_add";
    function Add (A, B : Int32x4) return Int32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x4_add";
    function Add (A, B : Int64x2) return Int64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int64x2_add";
    function Add (A, B : Uint8x8) return Uint8x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint8x8_add";
    function Add (A, B : Uint8x16) return Uint8x16 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint8x16_add";
    function Add (A, B : Uint16x4) return Uint16x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint16x4_add";
    function Add (A, B : Uint16x8) return Uint16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint16x8_add";
    function Add (A, B : Uint32x2) return Uint32x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint32x2_add";
    function Add (A, B : Uint32x4) return Uint32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint32x4_add";
    function Add (A, B : Uint64x2) return Uint64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint64x2_add";
    function Add (A, B : Float32x2) return Float32x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "float32x2_add";
    function Add (A, B : Float32x4) return Float32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "float32x4_add";
    function Add (A, B : Float64x2) return Float64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "float64x2_add";

    -- Widening Addition
    function Widening_Add(A, B : Int8x8) return Int16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int16x8_widening_add_int8x8_int8x8";
    function Widening_Add(A, B : Int16x4) return Int32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x4_widening_add_int16x4_int16x4";
    function Widening_Add(A, B : Int32x2) return Int64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int64x2_widening_add_int32x2_int32x2";
    function Widening_Add(A, B : Uint8x8) return Uint16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint16x8_widening_add_uint8x8_uint8x8";
    function Widening_Add(A, B : Uint16x4) return Uint32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint32x4_widening_add_uint16x4_uint16x4";
    function Widening_Add(A, B : Uint32x2) return Uint64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint64x2_widening_add_uint32x2_uint32x2";
    function Widening_Add(A, B : Int8x16) return Int16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int16x8_widening_add_int8x16_int8x16";
    function Widening_Add(A, B : Int16x8) return Int32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x4_widening_add_int16x8_int16x8";
    function Widening_Add(A, B : Int32x4) return Int64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int64x2_widening_add_int32x4_int32x4";
    function Widening_Add(A, B : Uint8x16) return Uint16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint16x8_widening_add_uint8x16_uint8x16";
    function Widening_Add(A, B : Uint16x8) return Uint32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint32x4_widening_add_uint16x8_uint16x8";
    function Widening_Add(A, B : Uint32x4) return Uint64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint64x2_widening_high_add_uint32x4_uint32x4";
    function Widening_Add(A : Int16x8; B : Int8x8) return Int16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int16x8_widening_add_int16x8_int8x8";
    function Widening_Add(A : Int32x4; B : Int16x4) return Int32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x4_widening_add_int32x4_int16x4";
    function Widening_Add(A : Int64x2; B : Int32x2) return Int64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int64x2_widening_add_int64x2_int32x2";
    function Widening_Add(A : Uint16x8; B : Uint8x8) return Uint16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint16x8_widening_add_uint16x8_uint8x8";
    function Widening_Add(A : Uint32x4; B : Uint16x4) return Uint32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint32x4_widening_add_uint32x4_uint16x4";
    function Widening_Add(A : Uint64x2; B : Uint32x2) return Uint64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint64x2_widening_add_uint64x2_uint32x2";
    function Widening_Add(A : Int16x8; B : Int8x16) return Int16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int16x8_widening_add_int16x8_int8x16";
    function Widening_Add(A : Int32x4; B : Int16x8) return Int32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x4_widening_add_int32x4_int16x8";
    function Widening_Add(A : Int64x2; B : Int32x4) return Int64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int64x2_widening_add_int64x2_int32x4";
    function Widening_Add(A : Uint16x8; B : Uint8x16) return Uint16x8 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint16x8_widening_add_uint16x8_uint8x16";
    function Widening_Add(A : Uint32x4; B : Uint16x8) return Uint32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint32x4_widening_add_uint32x4_uint16x8";
    function Widening_Add(A : Uint64x2; B : Uint32x4) return Uint64x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint64x2_widening_add_uint64x2_uint32x4";

end Simd.Adds;

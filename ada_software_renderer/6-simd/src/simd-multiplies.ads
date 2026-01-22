package Simd.Multiplies is
    function Multiply (A, B : Float32x4) return Float32x4 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "float32x4_mul";

    function Multiply (A, B : Uint32x4) return Uint32x4 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "uint32x4_mul";

    function Multiply (A, B : Int32x4) return Int32x4 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "int32x4_mul";

    -- returns A + B * C
    function Fused_Multiply_Add (A, B, C : Float32x4) return Float32x4 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "float32x4_fmadd";
end Simd.Multiplies;

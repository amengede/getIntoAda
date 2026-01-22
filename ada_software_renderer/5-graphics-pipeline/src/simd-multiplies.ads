package Simd.Multiplies is
    function Multiply (A, B : Int32x4) return Int32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x2_mul";
end Simd.Multiplies;

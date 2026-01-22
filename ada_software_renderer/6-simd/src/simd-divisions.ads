package Simd.Divisions is
    function Divide (A, B : Float32x4) return Float32x4 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "float32x4_divide";
end Simd.Divisions;

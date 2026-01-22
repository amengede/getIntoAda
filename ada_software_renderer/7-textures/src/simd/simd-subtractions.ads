package Simd.Subtractions is

    function Subtract (A, B : Int32x2) return Int32x2 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "int32x2_sub";

    function Subtract (A, B : Float32x2) return Float32x2 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "float32x2_sub";

    function Subtract (A, B : Float32x4) return Float32x4 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "float32x4_sub";

end Simd.Subtractions;

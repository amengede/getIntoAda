package Simd.Subtractions is

    function Subtract (A, B : Int32x2) return Int32x2 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "int32x2_sub";

end Simd.Subtractions;

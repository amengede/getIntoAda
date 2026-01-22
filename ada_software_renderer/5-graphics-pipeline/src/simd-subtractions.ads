package Simd.Subtractions is

    -- Subtraction
    function Subtract (A, B : Int32x2) return Int32x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x2_sub";

end Simd.Subtractions;

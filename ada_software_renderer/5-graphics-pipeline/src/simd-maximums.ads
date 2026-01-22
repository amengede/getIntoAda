package Simd.Maximums is
    function Maximum (A, B : Int32x2) return Int32x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x2_max";
end Simd.Maximums;

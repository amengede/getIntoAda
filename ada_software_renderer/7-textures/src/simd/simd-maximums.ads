package Simd.Maximums is

    function Maximum (A, B : Int32x2) return Int32x2 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "int32x2_max";

    function Maximum (A, B : Float32x2) return Float32x2 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "float32x2_max";

end Simd.Maximums;

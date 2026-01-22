Package Simd.Conversions is
    -- Conversions
    function Convert (A : Int32x2) return Float32x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "float32x2_convert_int32x2";

    function Convert (A : Int32x4) return Float32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "float32x4_convert_int32x4";

    function Convert (A : Float32x2) return Int32x2 with
        Convention => intrinsic,
        Import => True,
        External_Name => "int32x2_convert_float32x2";

    function Convert (A : Float32x4) return Uint32x4 with
        Convention => intrinsic,
        Import => True,
        External_Name => "uint32x4_convert_float32x4";
end Simd.Conversions;

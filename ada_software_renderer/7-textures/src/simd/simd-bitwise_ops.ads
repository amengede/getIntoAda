package Simd.Bitwise_Ops is
    function Bitwise_Or (A, B : Uint32x4) return Uint32x4 with
        Convention => Intrinsic,
        Import => True,
        External_Name => "uint32x4_or";
end Simd.Bitwise_Ops;

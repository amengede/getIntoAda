package body AVX2 is

    function "+" (Left, Right : Vector_256_U32) return Vector_256_U32 is
    begin
        return [
        Left (0) + Right (0),
        Left (1) + Right (1),
        Left (2) + Right (2),
        Left (3) + Right (3),
        Left (4) + Right (4),
        Left (5) + Right (5),
        Left (6) + Right (6),
        Left (7) + Right (7)];
    end "+";

end AVX2;

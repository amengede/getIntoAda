package body Maths is

    function Signed_Area (A, B, C : Point_2D) return Integer is
        AB_X : constant Integer := B (0) - A (0);
        AB_Y : constant Integer := B (1) - A (1);
        AC_X : constant Integer := C (0) - A (0);
        AC_Y : constant Integer := C (1) - A (1);
    begin
        return (AB_X) * (AC_Y) - (AB_Y) * (AC_X);
    end Signed_Area;

    function Enclose (Triangle : Triangle_2D) return AABB is
        Min_Corner_X : constant Integer :=
            Integer'Min(Triangle (0) (0),
                        Integer'Min(Triangle (1) (0), Triangle (2) (0)));
        Min_Corner_Y : constant Integer :=
            Integer'Min(Triangle (0) (1),
                        Integer'Min(Triangle (1) (1), Triangle (2) (1)));
        Max_Corner_X : constant Integer :=
            Integer'Max(Triangle (0) (0),
                        Integer'Max(Triangle (1) (0), Triangle (2) (0)));
        Max_Corner_Y : constant Integer :=
            Integer'Max(Triangle (0) (1),
                        Integer'Max(Triangle (1) (1), Triangle (2) (1)));
    begin
        return [[Min_Corner_X, Min_Corner_Y],
                [Max_Corner_X, Max_Corner_Y]];
    end Enclose;

    function Clip (Box, Bounds : AABB) return AABB is
        Min_Corner_X : constant Integer :=
            Integer'Max(Box (0) (0), Bounds (0) (0));
        Min_Corner_Y : constant Integer :=
            Integer'Max(Box (0) (1), Bounds (0) (1));
        Max_Corner_X : constant Integer :=
            Integer'Min(Box (1) (0), Bounds (1) (0));
        Max_Corner_Y : constant Integer :=
            Integer'Min(Box (1) (1), Bounds (1) (1));
    begin
        return [[Min_Corner_X, Min_Corner_Y],
        [Max_Corner_X, Max_Corner_Y]];
    end Clip;
end Maths;

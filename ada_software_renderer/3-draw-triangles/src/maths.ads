package Maths is
    type Point_2D is array (0 .. 1) of Integer;

    function Signed_Area (A, B, C : Point_2D) return Integer;

    type Triangle_2D is array (0 .. 2) of Point_2D;

    type Point_3D is array (0 .. 2) of Integer;

    type AABB is array (0 .. 1) of Point_2D;

    function Enclose (Triangle : Triangle_2D) return AABB;

    function Clip (Box, Bounds : AABB) return AABB;

end Maths;

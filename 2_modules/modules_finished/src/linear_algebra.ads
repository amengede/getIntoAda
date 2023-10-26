package Linear_Algebra is

    type Matrix is array (1 .. 3, 1 .. 3) of Integer;

    procedure Display_Matrix (A : Matrix);

    procedure Set_Matrix (
        A : in out Matrix;
        I : Integer;
        J : Integer;
        V : Integer);

    function Trace (A : Matrix) return Integer;

    Identity : constant Matrix :=
        ((1, 0, 1),
        (0, 1, 0),
        (0, 0, 1));

    function Img (I : Integer) return String
        renames Integer'Image;

end Linear_Algebra;
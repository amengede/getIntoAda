-- With Ada.Text_IO; Use Ada.Text_IO;
with Simd.Adds; use Simd.Adds;
With Simd.Subtractions; use Simd.Subtractions;
with Simd.Minimums; use Simd.Minimums;
with Simd.Maximums; use Simd.Maximums;
with Simd.Multiplies; use Simd.Multiplies;
package body Maths is

    function Signed_Area (A, B, C : Int32x2) return Integer_32 is
        AB : constant Int32x2 := Subtract (B, A);
        AC : constant Int32x2 := Subtract (C, A);
    begin
        return AB (0) * AC (1) - AB (1) * AC (0);
    end Signed_Area;

    function Enclose (Triangle : Triangle_2D) return AABB is
        Min_Corner : constant Int32x2 := Minimum(Triangle(0),
                                                 Minimum(Triangle(1),
                                                         Triangle(2)));
        Max_Corner : constant Int32x2 := Maximum(Triangle(0),
                                                 Maximum(Triangle(1),
                                                         Triangle(2)));
    begin
        return [Min_Corner, Max_Corner];
    end Enclose;

    function Make_Tile_From_AABB (Box: AABB;
                                  Weights: Point_3D)
        return Rasterization_Tile is
        Tile : Rasterization_Tile;
    begin

        Tile.Corner := Box (0);
        Tile.Extent := Subtract (Box (1), Box(0));
        Tile.Weights := [Weights (0), Weights (1), Weights (2)];

        return Tile;
    end Make_Tile_From_AABB;

    function Subdivide_Tile (Tile: Rasterization_Tile;
                             X_Partials, Y_Partials : Point_3D)
        return Rasterization_Tile_Bundle is
        Tiles : Rasterization_Tile_Bundle;
        X_Count : constant Integer_32 := Tile.Extent (0);
        Y_Count : constant Integer_32 := Tile.Extent (1);
        Half_Extent : constant Int32x2 := [X_Count / 2,
                                           Y_Count / 2];
        New_Corner_X : constant Int32x4 :=
            Subdivide_X (Tile.Corner (0), Half_Extent (0));
        New_Corner_Y : constant Int32x4 :=
            Subdivide_Y (Tile.Corner (1), Half_Extent (1));
        New_Weights : constant Weight_Bundle :=
            Subdivide_Weights (Tile.Corner, Half_Extent, Tile.Weights, X_Partials, Y_Partials);
        Indices : constant array (0 .. 3) of Integer :=
            [0, 1, 4, 3];
        Index : Integer;
    begin
        -- Assemble tiles from points
        For I in 0 .. 3 loop
            Index := Indices (I);
            -- Corner
            Tiles (I).Corner (0) := New_Corner_X (I);
            Tiles (I).Corner (1) := New_Corner_Y (I);

            -- Extent
            Tiles (I).Extent (0) := Half_Extent (0);
            Tiles (I).Extent (1) := Half_Extent (1);

            -- Weights
            Tiles (I).Weights (0) := New_Weights (0) (I);
            Tiles (I).Weights (1) := New_Weights (1) (I);
            Tiles (I).Weights (2) := New_Weights (2) (I);
        end loop;
        return Tiles;
    end Subdivide_Tile;

    function Subdivide_X (X, Half_W : Integer_32)
        return Int32x4 is
        X_Origin : constant Int32x4 := [X, X, X, X];
        Offsets : constant Int32x4 := [0, Half_W, Half_W, 0];
    begin
        return Add (X_Origin, Offsets);
    end Subdivide_X;

    function Subdivide_Y (Y, Half_H : Integer_32)
        return Int32x4 is
        Y_Origin : constant Int32x4 := [Y, Y, Y, Y];
        Offsets : constant Int32x4 := [0, 0, Half_H, Half_H];
    begin
        return Add (Y_Origin, Offsets);
    end Subdivide_Y;

    function Subdivide_Weights (Origin, Half_Extent : Int32x2;
                                Weights : Point_3D;
                                X_Partials,
                                Y_Partials : Point_3D)
        return Weight_Bundle is
        W0 : Int32x4 :=
            [Weights (0), Weights (0), Weights (0), Weights (0)];
        W1 : Int32x4 :=
            [Weights (1), Weights (1), Weights (1), Weights (1)];
        W2 : Int32x4 :=
            [Weights (2), Weights (2), Weights (2), Weights (2)];
        dW0dX : constant Int32x4 :=
            [X_Partials (0), X_Partials (0),
            X_Partials (0), X_Partials (0)];
        dW1dX : constant Int32x4 :=
            [X_Partials (1), X_Partials (1),
            X_Partials (1), X_Partials (1)];
        dW2dX : constant Int32x4 :=
            [X_Partials (2), X_Partials (2),
            X_Partials (2), X_Partials (2)];
        dW0dY : constant Int32x4 :=
            [Y_Partials (0), Y_Partials (0),
            Y_Partials (0), Y_Partials (0)];
        dW1dY : constant Int32x4 :=
            [Y_Partials (1), Y_Partials (1),
            Y_Partials (1), Y_Partials (1)];
        dW2dY : constant Int32x4 :=
            [Y_Partials (2), Y_Partials (2),
            Y_Partials (2), Y_Partials (2)];
        X_Offsets : constant Int32x4 :=
            [0, Half_Extent (0), Half_Extent (0), 0];
        Y_Offsets : constant Int32x4 :=
            [0, 0, Half_Extent (1), Half_Extent (1)];
    begin

        W0 := Add (W0, Multiply (X_Offsets, dW0dX));
        W0 := Add (W0, Multiply (Y_Offsets, dW0dY));
        W1 := Add (W1, Multiply (X_Offsets, dW1dX));
        W1 := Add (W1, Multiply (Y_Offsets, dW1dY));
        W2 := Add (W2, Multiply (X_Offsets, dW2dX));
        W2 := Add (W2, Multiply (Y_Offsets, dW2dY));

        return [W0, W1, W2];
    end Subdivide_Weights;

    function Contains (Tile : Rasterization_Tile;
                       Pos : Int32x2) return Boolean is
        Max_Corner : constant Int32x2 := Add (Tile.Corner, Tile.Extent);
    begin
        if (Pos (0) < Tile.Corner (0)) then
            return False;
        elsif (Pos (0) > Max_Corner (0)) then
            return False;
        elsif (Pos (1) < Tile.Corner (1)) then
            return False;
        elsif (Pos (1) > Max_Corner (1)) then
            return False;
        else
            return True;
        end if;
    end Contains;

    function Get_Overlap_Status (Tile: Rasterization_Tile;
                                 Triangle: Triangle_2D;
                                 X_Partials, Y_Partials : Point_3D)
        return Overlap_Status is
        Inside_Count : Integer := 0;
        Weights : constant Weight_Bundle :=
            Subdivide_Weights (Tile.Corner, Tile.Extent, Tile.Weights,
                               X_Partials, Y_Partials);
    begin

        -- Count how many corners are inside
        for I in 0 .. 3 loop
            if (Weights (0) (I) <= 0 and
                Weights (1) (I) <= 0 and
                Weights (2) (I) <= 0) then
                Inside_Count := Inside_Count + 1;
            end if;
        end loop;

        if Inside_Count = 4 then
            return Inside;
        elsif Inside_Count > 0 then
            return Partial;
        else
            -- It's possible the triangle is partially inside the tile
            if (Contains (Tile, Triangle (0))
                or Contains (Tile, Triangle (1))
                or Contains (Tile, Triangle (2))) then
                return Partial;
            else
                return Outside;
            end if;
        end if;
    end Get_Overlap_Status;

end Maths;

-- With Ada.Text_IO; Use Ada.Text_IO;
With Simd.Subtractions; use Simd.Subtractions;
with Simd.Adds; use Simd.Adds;
with Simd.Maximums; use Simd.Maximums;
with Simd.Minimums; use Simd.Minimums;
with Simd.Multiplies; use Simd.Multiplies;
package body Maths is

    function Signed_Area (A, B, C : Int32x2) return Integer_32 is
        AB : constant Int32x2 := Subtract (B, A);
        AC : constant Int32x2 := Subtract (C, A);
    begin
        return (AB (0) * AC (1)) - (AB (1) * AC (0));
    end Signed_Area;

    function Enclose (Triangle : Triangle_2D) return AABB is
        Min_Corner : constant Int32x2 := Minimum (Triangle (0),
                                                  Minimum (Triangle (1),
                                                           Triangle (2)));
        Max_Corner : constant Int32x2 := Maximum (Triangle (0),
                                                  Maximum (Triangle (1),
                                                           Triangle (2)));
    begin
        return [Min_Corner, Max_Corner];
    end Enclose;

    function Make_Tile_From_AABB (Box: AABB;
                                  Weights: Point_3D)
        return Rasterization_Tile is
        Tile : Rasterization_Tile;
        Extent : constant Int32x2 := Subtract (Box (1), Box (0));
    begin

        Tile.Corner := [Box (0) (0), Box (0) (1)];
        Tile.Extent := [Extent (0), Extent (1)];
        Tile.Weights := [Weights (0), Weights (1), Weights (2)];

        return Tile;
    end Make_Tile_From_AABB;

    function Subdivide_Tile (Tile: Rasterization_Tile;
                             X_Partials, Y_Partials : Point_3D)
        return Rasterization_Tile_Bundle is
        Tiles : Rasterization_Tile_Bundle;
        DX : constant Integer_32 := Integer_32(0.5 * Integer (Tile.Extent (0)));
        DY : constant Integer_32 := Integer_32(0.5 * Integer (Tile.Extent (1)));
        X_Origin : constant Int32x4 :=
            [Tile.Corner (0), Tile.Corner (0),
            Tile.Corner (0), Tile.Corner (0)];
        DX_Stencil : constant Int32x4 := [0, DX, DX, 0];
        Y_Origin : constant Int32x4 :=
            [Tile.Corner (1), Tile.Corner (1),
            Tile.Corner (1), Tile.Corner (1)];
        DY_Stencil : constant Int32x4 := [0, 0, DY, DY];
        New_X : constant Int32x4 := Add (X_Origin, DX_Stencil);
        New_Y : constant Int32x4 := Add (Y_Origin, DY_Stencil);
        W0 : constant Int32x4 :=
            [Tile.Weights (0), Tile.Weights (0),
            Tile.Weights (0), Tile.Weights (0)];
        W0_X_Stencil : constant Int32x4 :=
            Multiply (DX_Stencil,
                      [X_Partials (0), X_Partials (0),
                      X_Partials (0), X_Partials (0)]);
        W0_Y_Stencil : constant Int32x4 :=
            Multiply (DY_Stencil,
                      [Y_Partials (0), Y_Partials (0),
                      Y_Partials (0), Y_Partials (0)]);
        New_W0 : constant Int32x4 := Add (W0,
                                          Add (W0_X_Stencil,
                                               W0_Y_Stencil));
        W1 : constant Int32x4 :=
            [Tile.Weights (1), Tile.Weights (1),
            Tile.Weights (1), Tile.Weights (1)];
        W1_X_Stencil : constant Int32x4 :=
            Multiply (DX_Stencil,
                      [X_Partials (1), X_Partials (1),
                      X_Partials (1), X_Partials (1)]);
        W1_Y_Stencil : constant Int32x4 :=
            Multiply (DY_Stencil,
                      [Y_Partials (1), Y_Partials (1),
                      Y_Partials (1), Y_Partials (1)]);
        New_W1 : constant Int32x4 := Add (W1,
                                          Add (W1_X_Stencil,
                                               W1_Y_Stencil));
        W2 : constant Int32x4 :=
            [Tile.Weights (2), Tile.Weights (2),
            Tile.Weights (2), Tile.Weights (2)];
        W2_X_Stencil : constant Int32x4 :=
            Multiply (DX_Stencil,
                      [X_Partials (2), X_Partials (2),
                      X_Partials (2), X_Partials (2)]);
        W2_Y_Stencil : constant Int32x4 :=
            Multiply (DY_Stencil,
                      [Y_Partials (2), Y_Partials (2),
                      Y_Partials (2), Y_Partials (2)]);
        New_W2 : constant Int32x4 := Add (W2,
                                          Add (W2_X_Stencil,
                                               W2_Y_Stencil));
    begin
        -- Assemble tiles from points
        For I in 0 .. 3 loop
            -- Corner
            Tiles (I).Corner (0) := New_X (I);
            Tiles (I).Corner (1) := New_Y (I);

            -- Extent
            Tiles (I).Extent (0) := DX;
            Tiles (I).Extent (1) := DY;

            -- Weights
            Tiles (I).Weights (0) := New_W0 (I);
            Tiles (I).Weights (1) := New_W1 (I);
            Tiles (I).Weights (2) := New_W2 (I);
        end loop;
        return Tiles;
    end Subdivide_Tile;

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

    function Get_Overlap_Status (Tile : Rasterization_Tile;
                                 Triangle : Triangle_2D;
                                 X_Partials, Y_Partials : Point_3D)
        return Overlap_Status is
        DX : constant Integer_32 := Tile.Extent (0);
        DY : constant Integer_32 := Tile.Extent (1);
        W0 : constant Integer_32 := Tile.Weights (0);
        W1 : constant Integer_32 := Tile.Weights (1);
        W2 : constant Integer_32 := Tile.Weights (2);
        Weights : constant Box_Weights :=
            [[W0, W1, W2],
            [W0 + DX * X_Partials(0),
            W1 + DX * X_Partials(1),
            W2 + DX * X_Partials(2)],
            [W0 + DX * X_Partials (0) + DY * Y_Partials (0),
            W1 + DX * X_Partials (1) + DY * Y_Partials (1),
            W2 + DX * X_Partials (2) + DY * Y_Partials (2)],
            [W0 + DY * Y_Partials (0),
            W1 + DY * Y_Partials (1),
            W2 + DY * Y_Partials (2)]];
        Inside_Count : Integer := 0;
    begin

        for I in 0 .. 3 loop
            if (Weights (I) (0) <= 0 and
                Weights (I) (1) <= 0 and
                Weights (I) (2) <= 0) then
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

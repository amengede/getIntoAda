-- With Ada.Text_IO; Use Ada.Text_IO;
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

    function Make_Tile_From_AABB (Box: AABB;
                                  Weights, X_Partials, Y_Partials: Point_3D)
        return Rasterization_Tile is
        Tile : Rasterization_Tile;
        Extent : constant Point_2D :=
            [Box (1) (0) - Box (0) (0),
            Box (1) (1) - Box (0) (1)];
        X_Inc : constant Point_3D :=
            [Extent (0) * X_Partials (0),
            Extent (0) * X_Partials (1),
            Extent (0) * X_Partials (2)];
        Y_Inc : constant Point_3D :=
            [Extent (1) * Y_Partials (0),
            Extent (1) * Y_Partials (1),
            Extent (1) * Y_Partials (2)];
    begin

        -- Corner 0: Top left
        Tile.Corners (0) (0) := Box (0) (0);
        Tile.Corners (0) (1) := Box (0) (1);
        Tile.Weights (0) (0) := Weights (0);
        Tile.Weights (0) (1) := Weights (1);
        Tile.Weights (0) (2) := Weights (2);

        -- Corner 1: Top right
        Tile.Corners (1) (0) := Box (1) (0);
        Tile.Corners (1) (1) := Box (0) (1);
        Tile.Weights (1) (0) := Weights (0) + X_Inc (0);
        Tile.Weights (1) (1) := Weights (1) + X_Inc (1);
        Tile.Weights (1) (2) := Weights (2) + X_Inc (2);

        -- Corner 2: Bottom right
        Tile.Corners (2) (0) := Box (1) (0);
        Tile.Corners (2) (1) := Box (1) (1);
        Tile.Weights (2) (0) := Weights (0) + X_Inc (0) + Y_Inc (0);
        Tile.Weights (2) (1) := Weights (1) + X_Inc (1) + Y_Inc (1);
        Tile.Weights (2) (2) := Weights (2) + X_Inc (2) + Y_Inc (2);

        -- Corner 3: Bottom left
        Tile.Corners (3) (0) := Box (0) (0);
        Tile.Corners (3) (1) := Box (1) (1);
        Tile.Weights (3) (0) := Weights (0) + Y_Inc (0);
        Tile.Weights (3) (1) := Weights (1) + Y_Inc (1);
        Tile.Weights (3) (2) := Weights (2) + Y_Inc (2);
        Evaluate_Flags (Tile);

        return Tile;
    end Make_Tile_From_AABB;

    procedure Evaluate_Flags (Tile: in out Rasterization_Tile) is
    begin
        for I in 0 .. 3 loop
            Tile.Inside (I) := (Tile.Weights (I) (0) >= 0 and
                               Tile.Weights (I) (1) >= 0 and
                               Tile.Weights (I) (2) >= 0);
            if (Tile.Inside (I)) then
                Tile.Inside_Count := Tile.Inside_Count + 1;
            end if;
        end loop;
    end Evaluate_Flags;

    function Subdivide_Tile (Tile: Rasterization_Tile;
                             X_Partials, Y_Partials : Point_3D)
        return Rasterization_Tile_Bundle is
        Tiles : Rasterization_Tile_Bundle;
        X_Count : constant Integer := Tile.Corners (2) (0) -
                                      Tile.Corners (0) (0);
        Y_Count : constant Integer := Tile.Corners (2) (1) -
                                      Tile.Corners (0) (1);
        Half_Extent : constant Point_2D := [Integer (0.5 * X_Count),
                                           Integer (0.5 * Y_Count)];
        New_Corners : constant Corner_Bundle :=
            Subdivide_Corners (Tile, Half_Extent);
        New_Flags : Flag_Bundle;
        New_Weights : constant Weight_Bundle :=
            Subdivide_Weights (Tile, New_Flags, X_Partials,
                               Y_Partials, Half_Extent);
        Indices : constant array (0 .. 3, 0 .. 3) of Integer :=
            [[0, 1, 4, 3],
            [1, 2, 5, 4],
            [4, 5, 8, 7],
            [3, 4, 7, 6]];
        Index : Integer;
    begin
        -- Assemble tiles from points
        For Tile_Index in 0 .. 3 loop
            for Corner_Index in 0 .. 3 loop
                Index := Indices (Tile_Index, Corner_Index);

                -- Corner
                Tiles (Tile_Index).
                    Corners (Corner_Index) (0) :=
                        New_Corners (Index) (0);
                Tiles (Tile_Index).
                    Corners (Corner_Index) (1) :=
                        New_Corners (Index) (1);

                -- Weights
                Tiles (Tile_Index).Weights (Corner_Index) (0) :=
                    New_Weights (Index) (0);
                Tiles (Tile_Index).Weights (Corner_Index) (1) :=
                    New_Weights (Index) (1);
                Tiles (Tile_Index).Weights (Corner_Index) (2) :=
                    New_Weights (Index) (2);

                -- Flag
                Tiles (Tile_Index).
                    Inside (Corner_Index) := New_Flags (Index);
                if Tiles (Tile_Index).Inside(Corner_Index) then
                    Tiles (Tile_Index).Inside_Count :=
                        Tiles (Tile_Index).Inside_Count + 1;
                end if;
            end loop;
        end loop;
        return Tiles;
    end Subdivide_Tile;

    function Subdivide_Corners (Tile: Rasterization_Tile;
                                Half_Extent: Point_2D)
        return Corner_Bundle is
        Corners : Corner_Bundle;
    begin
        -- 0: Northwest
        Corners (0) (0) := Tile.Corners (0) (0);
        Corners (0) (1) := Tile.Corners (0) (1);
        -- 1: North
        Corners (1) (0) := Tile.Corners (0) (0) + Half_Extent (0);
        Corners (1) (1) := Tile.Corners (0) (1);
        -- 2: Northeast
        Corners (2) (0) := Tile.Corners (1) (0);
        Corners (2) (1) := Tile.Corners (1) (1);
        -- 3: West
        Corners (3) (0) := Tile.Corners (0) (0);
        Corners (3) (1) := Tile.Corners (0) (1) + Half_Extent (1);
        -- 4: Centre
        Corners (4) (0) := Tile.Corners (0) (0) + Half_Extent (0);
        Corners (4) (1) := Tile.Corners (0) (1) + Half_Extent (1);
        -- 5: East
        Corners (5) (0) := Tile.Corners (1) (0);
        Corners (5) (1) := Tile.Corners (0) (1) + Half_Extent (1);
        -- 6: Soutwest
        Corners (6) (0) := Tile.Corners (3) (0);
        Corners (6) (1) := Tile.Corners (3) (1);
        -- 7: South
        Corners (7) (0) := Tile.Corners (0) (0) + Half_Extent (0);
        Corners (7) (1) := Tile.Corners (3) (1);
        -- 8: Northwest
        Corners (8) (0) := Tile.Corners (2) (0);
        Corners (8) (1) := Tile.Corners (2) (1);
        return Corners;
    end Subdivide_Corners;

    function Subdivide_Weights (Tile : Rasterization_Tile;
                                Flags : in out Flag_Bundle;
                                X_Partials, Y_Partials : Point_3D;
                                Half_Extent : Point_2D)
        return Weight_Bundle is
        Weights : Weight_Bundle;
        X_Inc : constant Point_3D :=
            [Half_Extent (0) * X_Partials (0),
            Half_Extent (0) * X_Partials (1),
            Half_Extent (0) * X_Partials (2)];
        Y_Inc : constant Point_3D :=
            [Half_Extent (1) * Y_Partials (0),
            Half_Extent (1) * Y_Partials (1),
            Half_Extent (1) * Y_Partials (2)];
    begin
        -- 0: Northwest
        Weights (0) (0) := Tile.Weights (0) (0);
        Weights (0) (1) := Tile.Weights (0) (1);
        Weights (0) (2) := Tile.Weights (0) (2);
        Flags (0) := Tile.Inside (0);
        -- 1: North
        Weights (1) (0) := Tile.Weights (0) (0) + X_Inc (0);
        Weights (1) (1) := Tile.Weights (0) (1) + X_Inc (1);
        Weights (1) (2) := Tile.Weights (0) (2) + X_Inc (2);
        Flags (1) := Weights (1) (0) >= 0 and
                     Weights (1) (1) >= 0 and
                     Weights (1) (2) >= 0;
        -- 2: Northeast
        Weights (2) (0) := Tile.Weights (1) (0);
        Weights (2) (1) := Tile.Weights (1) (1);
        Weights (2) (2) := Tile.Weights (1) (2);
        Flags (2) := Tile.Inside (1);
        -- 3: West
        Weights (3) (0) := Weights (0) (0) + Y_Inc (0);
        Weights (3) (1) := Weights (0) (1) + Y_Inc (1);
        Weights (3) (2) := Weights (0) (2) + Y_Inc (2);
        Flags (3) := Weights (3) (0) >= 0 and
                     Weights (3) (1) >= 0 and
                     Weights (3) (2) >= 0;
        -- 4: Centre
        Weights (4) (0) := Weights (1) (0) + Y_Inc (0);
        Weights (4) (1) := Weights (1) (1) + Y_Inc (1);
        Weights (4) (2) := Weights (1) (2) + Y_inc (2);
        Flags (4) := Weights (4) (0) >= 0 and
                     Weights (4) (1) >= 0 and
                     Weights (4) (2) >= 0;
        -- 5: East
        Weights (5) (0) := Weights (2) (0) + Y_Inc (0);
        Weights (5) (1) := Weights (2) (1) + Y_Inc (1);
        Weights (5) (2) := Weights (2) (2) + Y_Inc (2);
        Flags (5) := Weights (5) (0) >= 0 and
                     Weights (5) (1) >= 0 and
                     Weights (5) (2) >= 0;
        -- 6: Southwest
        Weights (6) (0) := Tile.Weights (3) (0);
        Weights (6) (1) := Tile.Weights (3) (1);
        Weights (6) (2) := Tile.Weights (3) (2);
        Flags (6) := Tile.Inside (3);
        -- 7: South
        Weights (7) (0) := Weights (4) (0) + Y_Inc (0);
        Weights (7) (1) := Weights (4) (1) + Y_Inc (1);
        Weights (7) (2) := Weights (4) (2) + Y_Inc (2);
        Flags (7) := Weights (7) (0) >= 0 and
                     Weights (7) (1) >= 0 and
                     Weights (7) (2) >= 0;
        -- 8: Southeast
        Weights (8) (0) := Tile.Weights (2) (0);
        Weights (8) (1) := Tile.Weights (2) (1);
        Weights (8) (2) := Tile.Weights (2) (2);
        Flags (8) := Tile.Inside (2);

        return Weights;
    end Subdivide_Weights;

    function Contains (Tile : Rasterization_Tile;
                       Pos : Point_2D) return Boolean is
    begin
        if (Pos (0) < Tile.Corners (0) (0)) then
            return False;
        elsif (Pos (0) > Tile.Corners (1) (0)) then
            return False;
        elsif (Pos (1) < Tile.Corners (0) (1)) then
            return False;
        elsif (Pos (1) > Tile.Corners (2) (1)) then
            return False;
        else
            return True;
        end if;
    end Contains;

    function Get_Overlap_Status (Tile: Rasterization_Tile;
                                Triangle: Triangle_2D)
        return Overlap_Status is
    begin
        if Tile.Inside_Count = 4 then
            return Inside;
        elsif Tile.Inside_Count > 0 then
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

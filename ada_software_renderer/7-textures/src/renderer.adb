with Constants; use Constants;
with Ada.Text_IO; use Ada.Text_IO;
with Simd.Adds; use Simd.Adds;
with Simd.Conversions; use Simd.Conversions;
With Simd.Divisions; use Simd.Divisions;
with Simd.Maximums; use Simd.Maximums;
with Simd.Minimums; use Simd.Minimums;
with Simd.Multiplies; use Simd.Multiplies;
with Simd.Bitwise_Ops; use Simd.Bitwise_Ops;
with Simd.Subtractions; use Simd.Subtractions;
with Material_Manager;
with Colors_And_Pixels; use Colors_And_Pixels;
with SDL.Video.Pixel_Formats;
with Interfaces.C;
with Threading;

package body Renderer is

    procedure Initialize (Window : SDL.Video.Windows.Window) is
        Format : SDL.Video.Pixel_Formats.Pixel_Format_Access;
    begin
        Color_Buffer := Window.Get_Surface;
        Format := SDL.Video.Surfaces.Pixel_Format (Color_Buffer);
        Red_Shift := Get_Shift_From_Mask (Unsigned_32(Format.Red_Mask));
        Red_VShift := [Red_Shift, Red_Shift, Red_Shift, Red_Shift];
        Green_Shift := Get_Shift_From_Mask (Unsigned_32(Format.Green_Mask));
        Green_VShift := [Green_Shift, Green_Shift, Green_Shift, Green_Shift];
        Blue_Shift := Get_Shift_From_Mask (Unsigned_32(Format.Blue_Mask));
        Blue_VShift := [Blue_Shift, Blue_Shift, Blue_Shift, Blue_Shift];
        Alpha_Shift := Get_Shift_From_Mask (Unsigned_32(Format.Alpha_Mask));
        Alpha_VShift := [Alpha_Shift, Alpha_Shift, Alpha_Shift, Alpha_Shift];

        Current_Texture := Material_Manager.Make_Texture ("img/cat.jpg");
    end Initialize;

    procedure Finalize is
    begin
        null;
    end Finalize;

    procedure Start_Drawing is
    begin
        Color_Buffer.Lock;
    end Start_Drawing;

    procedure End_Drawing is
    begin
        Color_Buffer.Unlock;
    end End_Drawing;

    function Map_RGBA (R, G, B, A : SDL.Video.Palettes.Colour_Component)
        return Unsigned_32 is
    begin
        Return Unsigned_32 (R) * Red_Shift or
        Unsigned_32 (G) * Green_Shift or
        Unsigned_32 (B) * Blue_Shift or
        Unsigned_32 (A) * Alpha_Shift;
    end Map_RGBA;

    function Map_RGBA (Color : Float32x4)
        return Unsigned_32 is
        Rounded : constant Uint32x4 := Convert (Multiply (Color,
                                                          [255.0,
                                                          255.0,
                                                          255.0,
                                                          255.0]));
    begin
        Return Rounded (0) * Red_Shift or
        Rounded (1) * Green_Shift or
        Rounded (2) * Blue_Shift or
        Rounded (3) * Alpha_Shift;
    end Map_RGBA;

    function Map_RGBA (R, G, B, A : Uint32x4)
        return Uint32x4 is
    begin
        Return Bitwise_Or (Bitwise_Or (Multiply (R, Red_VShift),
                                       Multiply (G, Green_VShift)),
                                       Bitwise_Or (Multiply (B, Blue_VShift),
                                                   Multiply (A, Alpha_VShift)));
    end Map_RGBA;

    procedure Write_Pixel (X, Y : Integer_32;
                           Color : Unsigned_32) is
        use type Pixel_Pointers.Pointer;
        Ptr : Pixel_Pointers.Pointer;
    begin

        if (X < 0 or X >= Width or Y < 0 or Y >= Height) then
            return;
        end if;

        Ptr := Pixel_Data.Get_Row (Color_Buffer, SDL.Coordinate (Y))
               + Interfaces.C.ptrdiff_t (X);
        Ptr.all := Color;
    end Write_Pixel;

    procedure Write_Pixel (X, Y : Integer_32;
                           Color : Uint32x8) is
        use type Pixel_Chunk_Pointers.Pointer;
        Row_Ptr : constant Pixel_Chunk_Pointers.Pointer :=
            Pixel_Chunk_Data.Get_Row (Color_Buffer,
                                      SDL.Coordinate (Y));
        Ptr : constant Pixel_Chunk_Pointers.Pointer :=
            Row_Ptr + C.ptrdiff_t (X);
    begin
        Ptr.all := Color;
    end Write_Pixel;

    procedure Clear_Rows (Y, Row_Count: Integer_32) is
        Chunk_Count : constant Integer_32 := Width / 8;
    begin
        for I in Integer_32 range 0 .. Row_Count loop
            for X in Integer_32 range 0 .. Chunk_Count loop
                Write_Pixel (X, Y + I, Current_Chunk);
            end loop;
        end loop;
    end Clear_Rows;

    procedure Clear_Screen (Color: Unsigned_32) is
        Row_Count : constant Integer_32 := 128;
        Job_Count : constant Integer_32 := Height / Row_Count;
    begin
        -- Put_Line ("Start clearing screen");
        Current_Chunk := [Color, Color, Color, Color, Color, Color, Color, Color];

        Threading.Start_Dispatching_Jobs;

        for Y in Integer_32 range 0 .. Job_Count loop
            Threading.Dispatch_Clear_Command (Y => Row_Count * Y,
                                              Row_Count => Row_Count);
        end loop;

        Threading.Wait;
        -- Put_Line ("Finished clearing screen");
    end Clear_Screen;

    procedure Draw_Triangles (Vertices : Vertex_Buffer;
                              First_Vertex, Triangle_Count: Natural) is
        Individual_Attributes : Attribute_Package;
        Corner : Int32x2;
        Area : Integer_32;
    begin

        for I in Natural range 0 .. (Triangle_Count - 1) loop

            -- Assemble Triangle
            for J in Natural range 0 .. 2 loop
                -- Put_Line (Natural(3 * I + J)'Image);
                Corner := Shade_Vertex (Vertices (3 * I + J + First_Vertex),
                                        Individual_Attributes);
                Current_Triangle (J) (0) := Corner (0);
                Current_Triangle (J) (1) := Corner (1);
                Attributes.Colors (J) (0) := Individual_Attributes.Color (0);
                Attributes.Colors (J) (1) := Individual_Attributes.Color (1);
                Attributes.Colors (J) (2) := Individual_Attributes.Color (2);
                Attributes.UVs (J) (0) := Individual_Attributes.UV (0);
                Attributes.UVs (J) (1) := Individual_Attributes.UV (1);
            end loop;

            -- Backface test
            Area := Signed_Area(Current_Triangle (0),
                                Current_Triangle (1),
                                Current_Triangle (2));
            if Area > 0 then
                goto Continue;
            end if;
            Weight_Total := Convert([Area, Area, Area, Area]);

            Select_Mip_Level;

            -- Compute Attribute edges
            Attributes.Colors (1) := Subtract (Attributes.Colors (1),
                                               Attributes.Colors (0));
            Attributes.Colors (2) := Subtract (Attributes.Colors (2),
                                               Attributes.Colors (0));
            Attributes.UVs (1) := Subtract (Attributes.UVs (1),
                                           Attributes.UVs (0));
            Attributes.UVs (2) := Subtract (Attributes.UVs (2),
                                           Attributes.UVs (0));
            -- Rasterize
            Rasterize_Triangle;
        <<Continue>>
        end loop;
    end Draw_Triangles;

    function Shade_Vertex (Vert : Vertex;
                           Attributes : out Attribute_Package)
        return Int32x2 is
        Final_Pos : Float32x4 :=
            [Vert.Position (0), Vert.Position (1), Vert.Position (2), 1.0];
    begin
        -- Color attribute
        Attributes.Color (0) := Vert.Color (0);
        Attributes.Color (1) := Vert.Color (1);
        Attributes.Color (2) := Vert.Color (2);

        -- Texcoord attribute
        Attributes.UV (0) := Vert.UV (0);
        Attributes.UV (1) := Vert.UV (1);

        -- Perspective Division
        Final_Pos := Divide (Final_Pos,
                             [Final_Pos (3),
                             Final_Pos (3),
                             Final_Pos (3),
                             Final_Pos (3)]);

        -- Screen Coordinates
        return Convert (Multiply ([0.5 * IEEE_Float_32 (Width),
                                  -0.5 * IEEE_Float_32 (Height)],
                                  Add ([Vert.Position (0),
                                       Vert.Position (1)],
                                       [1.0, -1.0])));
    end Shade_Vertex;

    procedure Select_Mip_Level is
        Bounds : constant AABB := Enclose (Current_Triangle);
        Extent : constant Float32x2 := Convert (Subtract (Bounds (1), Bounds (0)));
        UV_0 : constant Float32x2 := Minimum ( Attributes.UVs (0),
                                               Minimum (Attributes.UVs (1),
                                                        Attributes.UVs (2)));
        UV_1 : constant Float32x2 := Maximum ( Attributes.UVs (0),
                                               Maximum (Attributes.UVs (1),
                                                        Attributes.UVs (2)));
        Duv : constant Float32x2 := Subtract (UV_1, UV_0);
        DUV_DPos : constant Float32x2 := Divide (Duv, Extent);
    begin

        -- Put_Line ("Partial derivatives are: (" & DUV_DPos (0)'Image & ", " & DUV_DPos(1)'Image & ")");
        Current_Mip_Level := Material_Manager.Get_Mip_Level (Current_Texture, DUV_DPos);
    end Select_Mip_Level;

    procedure Save_Rasterization_State is
    begin
        X_Partials (0) := Current_Triangle (1) (1) -
                          Current_Triangle (2) (1);
        X_Partials (1) := Current_Triangle (2) (1) -
                          Current_Triangle (0) (1);
        X_Partials (2) := Current_Triangle (0) (1) -
                          Current_Triangle (1) (1);
        Y_Partials (0) := Current_Triangle (2) (0) -
                          Current_Triangle (1) (0);
        Y_Partials (1) := Current_Triangle (0) (0) -
                          Current_Triangle (2) (0);
        Y_Partials (2) := Current_Triangle (1) (0) -
                          Current_Triangle (0) (0);
        dW0dX :=
            [4 * X_Partials (0),
            4 * X_Partials (0),
            4 * X_Partials (0),
            4 * X_Partials (0)];
        dW1dX :=
            [4 * X_Partials (1),
            4 * X_Partials (1),
            4 * X_Partials (1),
            4 * X_Partials (1)];
        dW2dX :=
            [4 * X_Partials (2),
            4 * X_Partials (2),
            4 * X_Partials (2),
            4 * X_Partials (2)];
        dW0dY :=
            [Y_Partials (0),
            Y_Partials (0),
            Y_Partials (0),
            Y_Partials (0)];
        dW1dY :=
            [Y_Partials (1),
            Y_Partials (1),
            Y_Partials (1),
            Y_Partials (1)];
        dW2dY :=
            [Y_Partials (2),
            Y_Partials (2),
            Y_Partials (2),
            Y_Partials (2)];
        Red_0 :=
            [Attributes.Colors (0) (0),
            Attributes.Colors (0) (0),
            Attributes.Colors (0) (0),
            Attributes.Colors (0) (0)];
        Green_0 :=
            [Attributes.Colors (0) (1),
            Attributes.Colors (0) (1),
            Attributes.Colors (0) (1),
            Attributes.Colors (0) (1)];
        Blue_0 :=
            [Attributes.Colors (0) (2),
            Attributes.Colors (0) (2),
            Attributes.Colors (0) (2),
            Attributes.Colors (0) (2)];
        U_0 :=
            [Attributes.UVs (0) (0),
            Attributes.UVs (0) (0),
            Attributes.UVs (0) (0),
            Attributes.UVs (0) (0)];
        V_0 :=
            [Attributes.UVs (0) (1),
            Attributes.UVs (0) (1),
            Attributes.UVs (0) (1),
            Attributes.UVs (0) (1)];
        Red_1 :=
            [Attributes.Colors (1) (0),
            Attributes.Colors (1) (0),
            Attributes.Colors (1) (0),
            Attributes.Colors (1) (0)];
        Green_1 :=
            [Attributes.Colors (1) (1),
            Attributes.Colors (1) (1),
            Attributes.Colors (1) (1),
            Attributes.Colors (1) (1)];
        Blue_1 :=
            [Attributes.Colors (1) (2),
            Attributes.Colors (1) (2),
            Attributes.Colors (1) (2),
            Attributes.Colors (1) (2)];
        U_1 :=
            [Attributes.UVs (1) (0),
            Attributes.UVs (1) (0),
            Attributes.UVs (1) (0),
            Attributes.UVs (1) (0)];
        V_1 :=
            [Attributes.UVs (1) (1),
            Attributes.UVs (1) (1),
            Attributes.UVs (1) (1),
            Attributes.UVs (1) (1)];
        Red_2 :=
            [Attributes.Colors (2) (0),
            Attributes.Colors (2) (0),
            Attributes.Colors (2) (0),
            Attributes.Colors (2) (0)];
        Green_2 :=
            [Attributes.Colors (2) (1),
            Attributes.Colors (2) (1),
            Attributes.Colors (2) (1),
            Attributes.Colors (2) (1)];
        Blue_2 :=
            [Attributes.Colors (2) (2),
            Attributes.Colors (2) (2),
            Attributes.Colors (2) (2),
            Attributes.Colors (2) (2)];
        U_2 :=
            [Attributes.UVs (2) (0),
            Attributes.UVs (2) (0),
            Attributes.UVs (2) (0),
            Attributes.UVs (2) (0)];
        V_2 :=
            [Attributes.UVs (2) (1),
            Attributes.UVs (2) (1),
            Attributes.UVs (2) (1),
            Attributes.UVs (2) (1)];
    end Save_Rasterization_State;

    procedure Rasterize_Triangle is
        Bounds : AABB := Enclose (Current_Triangle);
        Pos : Int32x2;
        Weights : Point_3D;
    begin

        -- Clip Bounding box against screen
        Bounds (0) := Maximum ([0, 0], Bounds (0));
        Bounds (1) := Minimum ([Width - 1, Height - 1], Bounds (1));

        Pos (0) := Bounds (0) (0);
        Pos (1) := Bounds (0) (1);
        Weights (0) := Signed_Area(Current_Triangle (1),
                                   Current_Triangle (2), Pos);
        Weights (1) := Signed_Area(Current_Triangle (2),
                                   Current_Triangle (0), Pos);
        Weights (2) := Signed_Area(Current_Triangle (0),
                                   Current_Triangle (1), Pos);

        -- Save global state
        Save_Rasterization_State;

        Threading.Start_Dispatching_Jobs;
        Subdivide (Make_Tile_From_AABB (Bounds,
                                        Weights));
        Threading.Wait;
    end Rasterize_Triangle;

    procedure Subdivide (Tile : Rasterization_Tile) is
        X_Count : constant Integer_32 := Tile.Extent (0);
        Y_Count : constant Integer_32 := Tile.Extent (1);
        Sub_Tiles : Rasterization_Tile_Bundle;
        Threshold : constant Integer_32 := 32;
        Overlap : constant Overlap_Status :=
            Get_Overlap_Status (Tile, Current_Triangle,  X_Partials,  Y_Partials);
    begin
        -- Base Cases
        if Overlap = Outside then
            return;
        elsif Overlap = Inside then
            Threading.Dispatch_Raster_Command (Tile, False);
            return;
        elsif Integer_32'Min(X_Count, Y_Count) <= Threshold then
            Threading.Dispatch_Raster_Command (Tile, True);
            return;
        end if;

        -- Recursion
        Sub_Tiles := Subdivide_Tile (Tile, X_Partials, Y_Partials);
        for I in 0 .. 3 loop
            Subdivide (Sub_Tiles(I));
        end loop;
    end Subdivide;

    procedure Rasterize_With_Tests (Min_Corner, Extent : Int32x2;
                                    Weights : Point_3D) is
        Job_Count_X : constant Integer_32 := (Extent (0) + 3) / 4;
        Job_Count_Y : constant Integer_32 := Extent (1);
        X_Max : constant Integer_32 :=
            Integer_32 (Min_Corner (0) + Extent (0));
        X_Start : constant Int32x4 :=
            [Min_Corner (0),
            Min_Corner (0) + 1,
            Min_Corner (0) + 2,
            Min_Corner (0) + 3];
        X : Int32x4;
        Y : Int32x4 :=
            [Min_Corner (1),
            Min_Corner (1),
            Min_Corner (1),
            Min_Corner (1)];
        W0_Row : Int32x4 :=
            [Weights (0),
            Weights (0) + X_Partials (0),
            Weights (0) + 2 * X_Partials (0),
            Weights (0) + 3 * X_Partials (0)];
        W1_Row : Int32x4 :=
            [Weights (1),
            Weights (1) + X_Partials (1),
            Weights (1) + 2 * X_Partials (1),
            Weights (1) + 3 * X_Partials (1)];
        W2_Row : Int32x4 :=
            [Weights (2),
            Weights (2) + X_Partials (2),
            Weights (2) + 2 * X_Partials (2),
            Weights (2) + 3 * X_Partials (2)];
        W0, W1, W2 : Int32x4;
        Mask : Flags;
        Render_Something : Boolean;
    begin
        --Put_Line ("Raster Job started");
        for I in 0 .. Job_Count_Y loop

            -- restart at a new row
            W0 := W0_Row;
            W1 := W1_Row;
            W2 := W2_Row;
            X := X_Start;

            for J in 0 .. Job_Count_X loop
                for K in 0 .. 3 loop
                    Mask (K) := X (K) <= X_Max and
                                W0 (K) <= 0 and
                                W1 (K) <= 0 and
                                W2 (K) <= 0;
                end loop;
                Render_Something := Mask(0) or
                                    Mask(1) or
                                    Mask(2) or
                                    Mask(3);
                if (Render_Something) then
                    Shade_Fragment (X, Y, W1, W2, Mask);
                end if;

                -- Increment X
                W0 := Add (W0, dW0dX);
                W1 := Add (W1, dW1dX);
                W2 := Add (W2, dW2dX);
                X := Add (X, DX);
            end loop;

            -- Increment Y
            W0_Row := Add (W0_Row, dW0dY);
            W1_Row := Add (W1_Row, dW1dY);
            W2_Row := Add (W2_Row, dW2dY);
            Y := Add (Y, DY);
        end loop;
    end Rasterize_With_Tests;

    procedure Rasterize_No_Tests (Min_Corner, Extent : Int32x2;
                                    Weights : Point_3D) is
        Job_Count_X : constant Integer_32 := (Extent (0) + 3) / 4;
        Job_Count_Y : constant Integer_32 := Extent (1);
        X_Max : constant Integer_32 :=
            Integer_32 (Min_Corner (0) + Extent (0));
        X_Start : constant Int32x4 :=
            [Min_Corner (0),
            Min_Corner (0) + 1,
            Min_Corner (0) + 2,
            Min_Corner (0) + 3];
        X : Int32x4;
        Y : Int32x4 :=
            [Min_Corner (1),
            Min_Corner (1),
            Min_Corner (1),
            Min_Corner (1)];
        W0_Row : Int32x4 :=
            [Weights (0),
            Weights (0) + X_Partials (0),
            Weights (0) + 2 * X_Partials (0),
            Weights (0) + 3 * X_Partials (0)];
        W1_Row : Int32x4 :=
            [Weights (1),
            Weights (1) + X_Partials (1),
            Weights (1) + 2 * X_Partials (1),
            Weights (1) + 3 * X_Partials (1)];
        W2_Row : Int32x4 :=
            [Weights (2),
            Weights (2) + X_Partials (2),
            Weights (2) + 2 * X_Partials (2),
            Weights (2) + 3 * X_Partials (2)];
        W0, W1, W2 : Int32x4;
        Mask : Flags;
        Render_Something : Boolean;
    begin
        --Put_Line ("Raster Job started");
        for I in 0 .. Job_Count_Y loop

            -- restart at a new row
            W0 := W0_Row;
            W1 := W1_Row;
            W2 := W2_Row;
            X := X_Start;

            for J in 0 .. Job_Count_X loop
                for K in 0 .. 3 loop
                    Mask (K) := X (K) <= X_Max;
                end loop;
                Render_Something := Mask(0) or
                                    Mask(1) or
                                    Mask(2) or
                                    Mask(3);
                if (Render_Something) then
                    Shade_Fragment (X, Y, W1, W2, Mask);
                end if;

                -- Increment X
                W0 := Add (W0, dW0dX);
                W1 := Add (W1, dW1dX);
                W2 := Add (W2, dW2dX);
                X := Add (X, DX);
            end loop;

            -- Increment Y
            W0_Row := Add (W0_Row, dW0dY);
            W1_Row := Add (W1_Row, dW1dY);
            W2_Row := Add (W2_Row, dW2dY);
            Y := Add (Y, DY);
        end loop;
    end Rasterize_No_Tests;

    procedure Shade_Fragment (X, Y,
                              W1, W2 : Int32x4;
                              Mask : Flags) is
        T1 : constant Float32x4 := Divide (Convert (W1), Weight_Total);
        T2 : constant Float32x4 := Divide (Convert (W2), Weight_Total);
        U : constant Float32x4 :=
            Fused_Multiply_Add (Fused_Multiply_Add(U_0, T1, U_1),
                                T2 , U_2);
        V : constant Float32x4 :=
            Fused_Multiply_Add (Fused_Multiply_Add(V_0, T1, V_1),
                                T2 , V_2);
        Red : constant Float32x4 :=
            Fused_Multiply_Add (Fused_Multiply_Add(Red_0, T1, Red_1),
                                T2 , Red_2);
        Green : constant Float32x4 :=
            Fused_Multiply_Add (Fused_Multiply_Add(Green_0, T1, Green_1),
                                T2 , Green_2);
        Blue : constant Float32x4 :=
            Fused_Multiply_Add (Fused_Multiply_Add(Blue_0, T1, Blue_1),
                                T2 , Blue_2);
        Sampled : Float32x4;
        Final_Color : Unsigned_32;

    begin
        -- Put_Line ("Fragment shader started");
        For Lane in 0 .. 3 loop
            if Mask (Lane) then
                Sampled := Material_Manager.Sample ([U (Lane), V (Lane)]);
                Final_Color := Map_RGBA ( Multiply (Sampled,
                                                    [Red (Lane),
                                                    Green (Lane),
                                                    Blue (Lane),
                                                    1.0]));
                Write_Pixel (X (Lane), Y (Lane), Final_Color);
            end if;
        end loop;
    end Shade_Fragment;

end Renderer;

with Constants; use Constants;
with Ada.Text_IO; use Ada.Text_IO;
with Simd.Adds; use Simd.Adds;
with Simd.Conversions; use Simd.Conversions;
With Simd.Divisions; use Simd.Divisions;
with Simd.Multiplies; use Simd.Multiplies;
with Simd.Bitwise_Ops; use Simd.Bitwise_Ops;

package body Renderer is

    procedure Initialize (Window : SDL.Video.Windows.Window) is
        Format : SDL.Video.Pixel_Formats.Pixel_Format_Access;
    begin
        Color_Buffer := Window.Get_Surface;
        Format := SDL.Video.Surfaces.Pixel_Format (Color_Buffer);
        Red_Shift := Get_Shift_From_Mask (Format.Red_Mask);
        Red_VShift := [Red_Shift, Red_Shift, Red_Shift, Red_Shift];
        Green_Shift := Get_Shift_From_Mask (Format.Green_Mask);
        Green_VShift := [Green_Shift, Green_Shift, Green_Shift, Green_Shift];
        Blue_Shift := Get_Shift_From_Mask (Format.Blue_Mask);
        Blue_VShift := [Blue_Shift, Blue_Shift, Blue_Shift, Blue_Shift];
        Alpha_Shift := Get_Shift_From_Mask (Format.Alpha_Mask);
        Alpha_VShift := [Alpha_Shift, Alpha_Shift, Alpha_Shift, Alpha_Shift];
        -- Launch Worker threads
        for I in 1 .. Worker_Thread_Count loop
            Worker_Threads (I).Run (Id => I);
        end loop;
    end Initialize;

    function Get_Shift_From_Mask (Mask : SDL.Video.Pixel_Formats.Colour_Mask)
        return Unsigned_32 is
    begin
        case Mask is
            when 0 =>
                return 0;
            when 255 =>
                return 1;
            when 65280 =>
                return 2 ** 8;
            when 16711680 =>
                return 2 ** 16;
            when others =>
                return 2 ** 24;
        end case;
    end Get_Shift_From_Mask;

    procedure Finalize is
    begin
        Wait;
        Program_Running := False;
    end Finalize;

    procedure Start_Drawing is
    begin
        Command_Buffer_Offset := 0;
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
        Row_Ptr : constant Pixel_Chunk_Pointers.Pointer :=
            Pixel_Chunk_Data.Get_Row (Color_Buffer,
                                      SDL.Coordinate (Y));
        Ptr : constant Pixel_Chunk_Pointers.Pointer :=
            Row_Ptr + C.ptrdiff_t (X);
    begin
        Ptr.all := Color;
    end Write_Pixel;

    procedure Clear_Rows (Y, Row_Count: Integer_32;
                          Color : Uint32x8) is
        Chunk_Count : constant Integer_32 := Width / 8;
    begin
        for I in Integer_32 range 0 .. Row_Count loop
            for X in Integer_32 range 0 .. Chunk_Count loop
                Write_Pixel (X, Y + I, Color);
            end loop;
        end loop;
    end Clear_Rows;

    procedure Clear_Screen (Color: Unsigned_32) is
        Row_Count : constant Integer_32 := 128;
        Job_Count : constant Integer_32 := Height / Row_Count;
    begin
        Current_Chunk := [Color, Color, Color, Color, Color, Color, Color, Color];

        Job_Running := True;

        for Y in Integer_32 range 0 .. Job_Count loop
            Dispatch_Clear_Command (Y => Row_Count * Y,
                                    Row_Count => Row_Count);
        end loop;

        Wait;
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
            end loop;

            -- Backface test
            Area := Signed_Area(Current_Triangle (0),
                                Current_Triangle (1),
                                Current_Triangle (2));
            if Area > 0 then
                goto Continue;
            end if;
            Weight_Total := Convert([Area, Area, Area, Area]);

            -- Compute Attribute edges
            Attributes.Colors (1) (0) := Attributes.Colors (1) (0) -
                                     Attributes.Colors (0) (0);
            Attributes.Colors (1) (1) := Attributes.Colors (1) (1) -
                                     Attributes.Colors (0) (1);
            Attributes.Colors (1) (2) := Attributes.Colors (1) (2) -
                                     Attributes.Colors (0) (2);
            Attributes.Colors (2) (0) := Attributes.Colors (2) (0) -
                                     Attributes.Colors (0) (0);
            Attributes.Colors (2) (1) := Attributes.Colors (2) (1) -
                                     Attributes.Colors (0) (1);
            Attributes.Colors (2) (2) := Attributes.Colors (2) (2) -
                                     Attributes.Colors (0) (2);

            -- Rasterize
            Rasterize_Triangle;
        <<Continue>>
        end loop;
    end Draw_Triangles;

    function Shade_Vertex (Vert : Vertex;
                           Attributes : out Attribute_Package)
        return Int32x2 is
        Final_Pos : Vec4 := [Vert.Position (0),
                            Vert.Position (1),
                            Vert.Position (2), 1.0];
    begin
        -- Color attribute
        Attributes.Color (0) := Vert.Color (0);
        Attributes.Color (1) := Vert.Color (1);
        Attributes.Color (2) := Vert.Color (2);

        -- Perspective Division
        Final_Pos (0) := Final_Pos (0) / Final_Pos (3);
        Final_Pos (1) := Final_Pos (1) / Final_Pos (3);
        Final_Pos (2) := Final_Pos (2) / Final_Pos (3);
        Final_Pos (3) := 1.0;

        -- Screen Coordinates
        return [Integer_32 (0.5 *
                            IEEE_Float_32 (Width) *
                            (Vert.Position (0) + 1.0)),
        Integer_32 (-0.5 *
                    IEEE_Float_32 (Height) *
                    (Vert.Position (1) - 1.0))];
    end Shade_Vertex;

    procedure Rasterize_Triangle is
        Bounds : AABB := Enclose (Current_Triangle);
        Pos : Int32x2;
        Weights : Point_3D;
    begin

        -- Clip Bounding box against screen
        Bounds (0) (0) := Integer_32'Max(0, Bounds (0) (0));
        Bounds (0) (1) := Integer_32'Max(0, Bounds (0) (1));
        Bounds (1) (0) := Integer_32'Min(Width - 1, Bounds (1) (0));
        Bounds (1) (1) := Integer_32'Min(Height - 1, Bounds (1) (1));

        Pos (0) := Bounds (0) (0);
        Pos (1) := Bounds (0) (1);
        Weights (0) := Signed_Area(Current_Triangle (1),
                                   Current_Triangle (2), Pos);
        Weights (1) := Signed_Area(Current_Triangle (2),
                                   Current_Triangle (0), Pos);
        Weights (2) := Signed_Area(Current_Triangle (0),
                                   Current_Triangle (1), Pos);

        -- Save global state
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

        Job_Running := True;
        Subdivide (Make_Tile_From_AABB (Bounds,
                                        Weights));
        Wait;
    end Rasterize_Triangle;

    procedure Dispatch_Clear_Command (Y, Row_Count : Integer_32) is
        Enqueued : Boolean;
        I : constant Command_Buffer_Index := Command_Buffer_Offset;
    begin
        Command_Buffer (I) := Command_Type_Clear_Rows;
        Command_Buffer (I + 1) := Y;
        Command_Buffer (I + 2) := Row_Count;
        Work_Queue.Insert (I, Enqueued);
        if not Enqueued then
            Clear_Rows (Y, Row_Count, Current_Chunk);
        end if;
        Command_Buffer_Offset := I + 3;
    end Dispatch_Clear_Command;

    procedure Perform_Clear_Command (Index : Command_Buffer_Index)
    is
    begin
        Clear_Rows (Y => Command_Buffer (Index + 1),
                    Row_Count => Command_Buffer (Index + 2),
                    Color => Current_Chunk);
    end Perform_Clear_Command;

    procedure Dispatch_Raster_Command (Tile : Rasterization_Tile;
                                       Perform_Tests : Boolean)
    is
        Enqueued : Boolean := False;
        I : constant Command_Buffer_Index := Command_Buffer_Offset;
    begin
        Command_Buffer (I) := (if Perform_Tests then
                               Command_Type_Rasterize_Tile_With_Tests else
                               Command_Type_Rasterize_Tile_No_Tests);
        Command_Buffer (I + 1 .. I + 7) :=
            [Tile.Corner (0), Tile.Corner (1),
            Tile.Extent (0), Tile.Extent (1),
            Tile.Weights (0), Tile.Weights (1), Tile.Weights (2)];
        while not Enqueued loop
            Work_Queue.Insert (I, Enqueued);
        end loop;
        Command_Buffer_Offset := I + 8;
    end Dispatch_Raster_Command;

    procedure Perform_Raster_Command (Index : Command_Buffer_Index;
                                      Perform_Tests : Boolean) is
        Min_Corner : constant Int32x2 := [Command_Buffer (Index + 1),
                                          Command_Buffer (Index + 2)];
        Max_Corner : constant Int32x2 := [Command_Buffer (Index + 3),
                                          Command_Buffer (Index + 4)];
        Weights : constant Point_3D :=
            [Command_Buffer (Index + 5),
            Command_Buffer(Index + 6),
            Command_Buffer(Index + 7)];
    begin
        if Perform_Tests then
            Rasterize_With_Tests (Min_Corner,  Max_Corner,  Weights);
        else
            Rasterize_With_Tests (Min_Corner,  Max_Corner,  Weights);
            null;
        end if;
    end;

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
            Dispatch_Raster_Command (Tile, False);
            return;
        elsif Integer_32'Min(X_Count, Y_Count) <= Threshold then
            Dispatch_Raster_Command (Tile, True);
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
        Job_Count_X : constant Integer_32 := (Extent (0) + 1) / 2;
        Job_Count_Y : constant Integer_32 := (Extent (1) + 1) / 2;
        X_Max : constant Integer_32 :=
            Integer_32 (Min_Corner (0) + Extent (0));
        Y_Max : constant Integer_32 :=
            Integer_32 (Min_Corner (1) + Extent (1));
        X_Start : constant Int32x4 :=
            [Min_Corner (0),
            Min_Corner (0) + 1,
            Min_Corner (0) + 1,
            Min_Corner (0)];
        X : Int32x4;
        Y : Int32x4 :=
            [Min_Corner (1),
            Min_Corner (1),
            Min_Corner (1) + 1,
            Min_Corner (1) + 1];
        W0_Row : Int32x4 :=
            [Weights (0),
            Weights (0) + X_Partials (0),
            Weights (0) + X_Partials (0) + Y_Partials (0),
            Weights (0) + Y_Partials (0)];
        W1_Row : Int32x4 :=
            [Weights (1),
            Weights (1) + X_Partials (1),
            Weights (1) + X_Partials (1) + Y_Partials (1),
            Weights (1) + Y_Partials (1)];
        W2_Row : Int32x4 :=
            [Weights (2),
            Weights (2) + X_Partials (2),
            Weights (2) + X_Partials (2) + Y_Partials (2),
            Weights (2) + Y_Partials (2)];
        W0, W1, W2 : Int32x4;
        Mask : Flags;
        DPos : constant Int32x4 := [2, 2, 2, 2];
        dW0dX : constant Int32x4 :=
            [2 * X_Partials (0),
            2 * X_Partials (0),
            2 * X_Partials (0),
            2 * X_Partials (0)];
        dW1dX : constant Int32x4 :=
            [2 * X_Partials (1),
            2 * X_Partials (1),
            2 * X_Partials (1),
            2 * X_Partials (1)];
        dW2dX : constant Int32x4 :=
            [2 * X_Partials (2),
            2 * X_Partials (2),
            2 * X_Partials (2),
            2 * X_Partials (2)];
        dW0dY : constant Int32x4 :=
            [2 * Y_Partials (0),
            2 * Y_Partials (0),
            2 * Y_Partials (0),
            2 * Y_Partials (0)];
        dW1dY : constant Int32x4 :=
            [2 * Y_Partials (1),
            2 * Y_Partials (1),
            2 * Y_Partials (1),
            2 * Y_Partials (1)];
        dW2dY : constant Int32x4 :=
            [2 * Y_Partials (2),
            2 * Y_Partials (2),
            2 * Y_Partials (2),
            2 * Y_Partials (2)];
        Render_Something : Boolean;
    begin
        for I in 0 .. Job_Count_Y loop

            -- restart at a new row
            W0 := W0_Row;
            W1 := W1_Row;
            W2 := W2_Row;
            X := X_Start;

            for J in 0 .. Job_Count_X loop
                for K in 0 .. 3 loop
                    Mask (K) := X (K) <= X_Max and
                                Y (K) <= Y_Max and
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
                X := Add (X, DPos);
            end loop;

            -- Increment Y
            W0_Row := Add (W0_Row, dW0dY);
            W1_Row := Add (W1_Row, dW1dY);
            W2_Row := Add (W2_Row, dW2dY);
            Y := Add (Y, DPos);
        end loop;
    end Rasterize_With_Tests;

    procedure Shade_Fragment (X, Y,
                              W1, W2 : Int32x4;
                              Mask : Flags) is
        T1 : constant Float32x4 := Divide (Convert (W1), Weight_Total);
        T2 : constant Float32x4 := Divide (Convert (W2), Weight_Total);
        Red_0 : constant Float32x4 :=
            [Attributes.Colors (0) (0),
            Attributes.Colors (0) (0),
            Attributes.Colors (0) (0),
            Attributes.Colors (0) (0)];
        Green_0 : constant Float32x4 :=
            [Attributes.Colors (0) (1),
            Attributes.Colors (0) (1),
            Attributes.Colors (0) (1),
            Attributes.Colors (0) (1)];
        Blue_0 : constant Float32x4 :=
            [Attributes.Colors (0) (2),
            Attributes.Colors (0) (2),
            Attributes.Colors (0) (2),
            Attributes.Colors (0) (2)];
        Red_1 : constant Float32x4 :=
            [Attributes.Colors (1) (0),
            Attributes.Colors (1) (0),
            Attributes.Colors (1) (0),
            Attributes.Colors (1) (0)];
        Green_1 : constant Float32x4 :=
            [Attributes.Colors (1) (1),
            Attributes.Colors (1) (1),
            Attributes.Colors (1) (1),
            Attributes.Colors (1) (1)];
        Blue_1 : constant Float32x4 :=
            [Attributes.Colors (1) (2),
            Attributes.Colors (1) (2),
            Attributes.Colors (1) (2),
            Attributes.Colors (1) (2)];
        Red_2 : constant Float32x4 :=
            [Attributes.Colors (2) (0),
            Attributes.Colors (2) (0),
            Attributes.Colors (2) (0),
            Attributes.Colors (2) (0)];
        Green_2 : constant Float32x4 :=
            [Attributes.Colors (2) (1),
            Attributes.Colors (2) (1),
            Attributes.Colors (2) (1),
            Attributes.Colors (2) (1)];
        Blue_2 : constant Float32x4 :=
            [Attributes.Colors (2) (2),
            Attributes.Colors (2) (2),
            Attributes.Colors (2) (2),
            Attributes.Colors (2) (2)];
        Upscale_Byte : constant Float32x4 := [255.0, 255.0, 255.0, 255.0];
        Red : constant Uint32x4 :=
            Convert (
            Multiply (
            Fused_Multiply_Add (Fused_Multiply_Add(Red_0, T1, Red_1),
                                T2 , Red_2), Upscale_Byte));
        Green : constant Uint32x4 :=
            Convert (
            Multiply (
            Fused_Multiply_Add (Fused_Multiply_Add(Green_0, T1, Green_1),
                                T2 , Green_2), Upscale_Byte));
        Blue : constant Uint32x4 :=
            Convert (
            Multiply (
            Fused_Multiply_Add (Fused_Multiply_Add(Blue_0, T1, Blue_1),
                                T2 , Blue_2), Upscale_Byte));
        Alpha : constant Uint32x4 := [255, 255, 255, 255];
        Color : constant Uint32x4 := Map_RGBA (Red, Green, Blue, Alpha);

    begin
        For Lane in 0 .. 3 loop
            if Mask (Lane) then
                Write_Pixel (X (Lane), Y (Lane), Color (Lane));
            end if;
        end loop;
    end Shade_Fragment;

    procedure Wait is
        Queue_Empty : Boolean := False;
    begin
        while (Queue_Empty = False or
               Thread_Running (1) = True or
               Thread_Running (2) = True or
               Thread_Running (3) = True or
               Thread_Running (4) = True) loop
            Work_Queue.Empty (Queue_Empty);
        end loop;
        Job_Running := False;
        Command_Buffer_Offset := 0;
    end Wait;

    task body Worker_Thread is
        My_Id : Integer;
        Index : Command_Buffer_Index;
        Got_Work : Boolean;
    begin
        accept Run (Id : Integer) do
            My_Id := Id;
        end;
        while (Program_Running) loop
            -- program is running, wait for a job
            while (Job_Running) loop
                -- job is running, try to find a task to complete
                Work_Queue.Remove (Index, Got_Work);
                Thread_Running (My_Id) := Got_Work;
                if Got_Work = False then
                    goto Continue;
                end if;

                case Command_Buffer (Index) is
                    when Command_Type_Clear_Rows =>
                        Perform_Clear_Command (Index);
                    when Command_Type_Rasterize_Tile_No_Tests =>
                        Perform_Raster_Command (Index, False);
                    when Command_Type_Rasterize_Tile_With_Tests =>
                        Perform_Raster_Command (Index, True);
                    when others =>
                        null;
                end case;
    <<Continue>>
            end loop;
        end loop;
    end Worker_Thread;

end Renderer;

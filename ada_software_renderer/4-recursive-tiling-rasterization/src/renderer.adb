with Constants; use Constants;
with Ada.Text_IO; use Ada.Text_IO;

package body Renderer is

    procedure Initialize (Window : SDL.Video.Windows.Window) is
        Format : SDL.Video.Pixel_Formats.Pixel_Format_Access;
    begin
        Color_Buffer := Window.Get_Surface;
        Format := SDL.Video.Surfaces.Pixel_Format (Color_Buffer);
        Red_Shift := Get_Shift_From_Mask (Format.Red_Mask);
        Green_Shift := Get_Shift_From_Mask (Format.Green_Mask);
        Blue_Shift := Get_Shift_From_Mask (Format.Blue_Mask);
        Alpha_Shift := Get_Shift_From_Mask (Format.Alpha_Mask);
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

    procedure Write_Pixel (X, Y : Integer;
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

    procedure Write_Pixel (X, Y : Integer;
                           Color : Vector_256_U32) is
        Row_Ptr : constant Pixel_Chunk_Pointers.Pointer :=
            Pixel_Chunk_Data.Get_Row (Color_Buffer,
                                      SDL.Coordinate (Y));
        Ptr : constant Pixel_Chunk_Pointers.Pointer :=
            Row_Ptr + C.ptrdiff_t (X);
    begin
        Ptr.all := Color;
    end Write_Pixel;

    procedure Clear_Rows (Y, Row_Count: Integer;
                          Color : Vector_256_U32) is
        Chunk_Count : constant Integer := Width / 8;
    begin
        for I in 0 .. Row_Count loop
            for X in 0 .. Chunk_Count loop
                Write_Pixel (X, Y + I, Color);
            end loop;
        end loop;
    end Clear_Rows;

    procedure Clear_Screen (Color: Unsigned_32) is
        Current_Job : Draw_Command;
        Job_Enqeued : Boolean := False;
        Row_Count : constant Integer := 128;
        Job_Count : constant Integer := Height / Row_Count;
    begin
        Current_Chunk :=
            [Color, Color, Color, Color, Color, Color, Color, Color];
        Current_Job.Id := Clear_Row;
        Current_Job.Int_Args (1) := Row_Count;

        Job_Running := True;

        for Y in 0 .. Job_Count loop
            Current_Job.Int_Args (0) := Row_Count * Y;
            Work_Queue.Insert (Current_Job, Job_Enqeued);
            if Job_Enqeued = False then
                Clear_Rows (Current_Job.Int_Args (0),
                            Current_Job.Int_Args (1),
                            Current_Chunk);
            end if;
        end loop;

        Wait;
    end Clear_Screen;

    procedure Draw_Line (Point_1, Point_2 : Point_2D;
                         Color : Unsigned_32) is
        Point_a : Point_2D := [Point_1 (0), Point_1 (1)];
        Point_b : Point_2D := [Point_2 (0), Point_2 (1)];
        Slope : Point_2D := [Point_b (0) - Point_a (0),
                            Point_b (1) - Point_a (1)];
        DX : constant Integer := abs Slope (0);
        DY : constant Integer := abs Slope (1);
    begin

        Current_Color := Color;

        -- Swap endpoints if needed
        if (DX > DY and Slope (0) < 0)
            or (DY > DX and Slope (1) < 0) then
            Reorient (Point_1, Point_2, Point_a, Point_b, Slope);
        end if;

        -- Detect case and draw
        if DY = 0 then
            Draw_Horizontal_Line (Point_a, DX);
        elsif DX = 0 then
            Draw_Vertical_Line (Point_a, DY);
        elsif DY < DX then
            Draw_Bresenham_Shallow (Point_a, Slope);
        else
            Draw_Bresenham_Steep (Point_a, Slope);
        end if;
    end Draw_Line;

    procedure Reorient (Point_1, Point_2 : in Point_2D;
                        Point_a, Point_b : out Point_2D;
                        Slope : in out Point_2D) is
    begin
        Point_a (0) := Point_2 (0);
        Point_a (1) := Point_2 (1);
        Point_b (0) := Point_1 (0);
        Point_b (1) := Point_1 (1);
        Slope (0) := -Slope (0);
        Slope (1) := -Slope (1);
    end Reorient;

    procedure Draw_Horizontal_Line (Point : Point_2D;
                                    Length : Integer) is
        X_Min : constant Integer := Point (0);
        X_Max : constant Integer := Point (0) + Length;
        Y : constant Integer := Point (1);
    begin
        for X in X_Min .. X_Max loop
            Write_Pixel (X, Y, Current_Color);
        end loop;
    end Draw_Horizontal_Line;

    procedure Draw_Vertical_Line (Point : Point_2D;
                                  Length : Integer) is
        X : constant Integer := Point (0);
        Y_Min : constant Integer := Point (1);
        Y_Max : constant Integer := Point (1) + Length;
    begin
        for Y in Y_Min .. Y_Max loop
            Write_Pixel (X, Y, Current_Color);
        end loop;
    end Draw_Vertical_Line;

    procedure Draw_Bresenham_Shallow(Point, Slope : Point_2D) is
        Y_Inc : constant Integer := (if Slope (1) < 0 then -1 else 1);
        P : Integer := Y_Inc * (2 * Slope (1) - Slope (0));
        X_Min : constant Integer := Point (0);
        X_Max : constant Integer := Point (0) + Slope (0);
        Y : Integer := Point (1);
        P_Inc_no_change : constant Integer := Y_Inc * 2 * Slope (1);
        P_Inc_change : constant Integer := Y_Inc * 2 * (Slope (1) -
                                                        Y_Inc * Slope (0));
    begin
        for X in X_Min .. X_Max loop
            if P < 0 then
                P := P + P_Inc_no_change;
            else
                Y := Y + Y_Inc;
                P := P + P_Inc_change;
            end if;
            Write_Pixel (X, Y, Current_Color);
        end loop;
    end Draw_Bresenham_Shallow;

    procedure Draw_Bresenham_Steep(Point, Slope : Point_2D) is
        X_Inc : constant Integer := (if Slope (0) < 0 then -1 else 1);
        P : Integer := X_Inc * (2 * Slope (0) - Slope (1));
        X : Integer := Point (0);
        Y_Min : constant Integer := Point (1);
        Y_Max : constant Integer := Point (1) + Slope (1);
        P_Inc_no_change : constant Integer := X_Inc * 2 * Slope (0);
        P_Inc_change : constant Integer := X_Inc * 2 * (Slope (0) -
                                                        X_Inc * Slope (1));
    begin
        for Y in Y_Min .. Y_Max loop
            if P < 0 then
                P := P + P_Inc_no_change;
            else
                X := X + X_Inc;
                P := P + P_Inc_change;
            end if;
            Write_Pixel (X, Y, Current_Color);
        end loop;
    end Draw_Bresenham_Steep;

    procedure Draw_Triangle (Triangle : Triangle_2D;
                             Color : Unsigned_32) is
        Triangle_Bounds : AABB := Enclose (Triangle);
        Pos : Point_2D;
        Weights : Point_3D;
    begin

        -- Clip Bounding box against screen
        Triangle_Bounds (0) (0) :=
            Integer'Max(0, Triangle_Bounds (0) (0));
        Triangle_Bounds (0) (1) :=
            Integer'Max(0, Triangle_Bounds (0) (1));
        Triangle_Bounds (1) (0) :=
            Integer'Min(Width - 1, Triangle_Bounds (1) (0));
        Triangle_Bounds (1) (1) :=
            Integer'Min(Height - 1, Triangle_Bounds (1) (1));

        Pos (0) := Triangle_Bounds (0) (0);
        Pos (1) := Triangle_Bounds (0) (1);
        Weights (0) := Signed_Area(Triangle (1), Triangle (2), Pos);
        Weights (1) := Signed_Area(Triangle (2), Triangle (0), Pos);
        Weights (2) := Signed_Area(Triangle (0), Triangle (1), Pos);

        -- Save global state
        for Corner in 0 .. 2 loop
            Current_Triangle (Corner) (0) := Triangle (Corner) (0);
            Current_Triangle (Corner) (1) := Triangle (Corner) (1);
        end loop;
        Current_Color := Color;
        Current_Chunk := [Color, Color, Color, Color, Color, Color, Color, Color];
        X_Partials (0) := Triangle (1) (1) - Triangle (2) (1);
        X_Partials (1) := Triangle (2) (1) - Triangle (0) (1);
        X_Partials (2) := Triangle (0) (1) - Triangle (1) (1);
        Y_Partials (0) := Triangle (2) (0) - Triangle (1) (0);
        Y_Partials (1) := Triangle (0) (0) - Triangle (2) (0);
        Y_Partials (2) := Triangle (1) (0) - Triangle (0) (0);

        Job_Running := True;
        Subdivide (Make_Tile_From_AABB (Triangle_Bounds,
                                        Weights,
                                        X_Partials, Y_Partials));
        Wait;
    end Draw_Triangle;

    function Pack_Tile_Into_Buffer (Tile : in Rasterization_Tile)
        return Command_Int_Buffer is
    begin
        return [
        Tile.Corners (0) (0), Tile.Corners (0) (1),
        Tile.Corners (1) (0), Tile.Corners (1) (1),
        Tile.Corners (2) (0), Tile.Corners (2) (1),
        Tile.Corners (3) (0), Tile.Corners (3) (1),
        Tile.Weights (0) (0), Tile.Weights (0) (1), Tile.Weights (0) (2),
        Tile.Weights (1) (0), Tile.Weights (1) (1), Tile.Weights (1) (2),
        Tile.Weights (2) (0), Tile.Weights (2) (1), Tile.Weights (2) (2),
        Tile.Weights (3) (0), Tile.Weights (3) (1), Tile.Weights (3) (2)];
    end Pack_Tile_Into_Buffer;

    function Unpack_Tile_From_Buffer (Buffer : in Command_Int_Buffer)
        return Rasterization_Tile is
    begin
        return
        Rasterization_Tile'( Corners => [[Buffer (0), Buffer (1)],
                             [Buffer (2), Buffer (3)],
                             [Buffer (4), Buffer (5)],
                             [Buffer (6), Buffer (7)]],
                             Weights => [
                             [Buffer (8), Buffer(9), Buffer(10)],
                             [Buffer (11), Buffer(12), Buffer(13)],
                             [Buffer (14), Buffer(15), Buffer(16)],
                             [Buffer (17), Buffer(18), Buffer(19)]],
                             Inside => [False, False, False, False],
                             Inside_Count => 0);
    end Unpack_Tile_From_Buffer;

    procedure Subdivide (Tile : Rasterization_Tile) is
        X_Count : constant Integer := Tile.Corners (2) (0) -
                                      Tile.Corners (0) (0);
        Y_Count : constant Integer := Tile.Corners (2) (1) -
                                      Tile.Corners (0) (1);
        Sub_Tiles : Rasterization_Tile_Bundle;
        Threshold : constant Integer := 64;
        Overlap : constant Overlap_Status :=
            Get_Overlap_Status (Tile, Current_Triangle);
        Command : Draw_Command;
        Enqueued : Boolean;
    begin

        -- Base Cases
        if Overlap = Outside then
            return;
        elsif Overlap = Inside then
            Command.Id := Fill_Rectangle;
            Command.Int_Args(0) := Tile.Corners (0) (0);
            Command.Int_Args(1) := Tile.Corners (0) (1);
            Command.Int_Args(2) := Tile.Corners (2) (0);
            Command.Int_Args(3) := Tile.Corners (2) (1);
            -- Assisted mode: do it on the main thread if workers are busy
            Work_Queue.Insert (Command, Enqueued);
            if Enqueued = False then
                Fill_Rectangle (Tile.Corners (0) (0),
                                Tile.Corners (0) (1),
                                Tile.Corners (2) (0),
                                Tile.Corners (2) (1));
            end if;
            return;
        elsif Integer'Min(X_Count, Y_Count) <= Threshold then
            Command.Id := Rasterize_Tile_With_Tests;
            Command.Int_Args := Pack_Tile_Into_Buffer (Tile);
            -- Assisted mode: do it on the main thread if workers are busy
            Work_Queue.Insert (Command, Enqueued);
            if Enqueued = False then
                Rasterize_With_Tests (Tile);
            end if;
            return;
        end if;

        -- Recursion
        Sub_Tiles := Subdivide_Tile (Tile, X_Partials, Y_Partials);
        for I in 0 .. 3 loop
            Subdivide (Sub_Tiles(I));
        end loop;
    end Subdivide;

    procedure Fill_Rectangle (X_Min, Y_Min, X_Max, Y_Max : Integer) is
    begin
        for Y in Y_Min .. Y_Max loop
            for X in X_Min .. X_Max loop
                Write_Pixel (X, Y, Current_Color);
            end loop;
        end loop;
    end Fill_Rectangle;

    procedure Rasterize_With_Tests (Tile : Rasterization_Tile) is
        Weights, Weights_Row : Point_3D;
        X_Min: constant Integer := Tile.Corners (0) (0);
        X_Max: constant Integer := Tile.Corners (2) (0);
        Y_Min: constant Integer := Tile.Corners (0) (1);
        Y_Max: constant Integer := Tile.Corners (2) (1);
    begin
        Weights_Row (0) := Tile.Weights (0) (0);
        Weights_Row (1) := Tile.Weights (0) (1);
        Weights_Row (2) := Tile.Weights (0) (2);
        for Y in Y_Min .. Y_Max loop
            Weights (0) := Weights_Row (0);
            Weights (1) := Weights_Row (1);
            Weights (2) := Weights_Row (2);
            for X in X_Min .. X_Max loop
                if (Weights (0) >= 0 and
                    Weights (1) >= 0 and
                    Weights (2) >= 0) then
                    Write_Pixel (X, Y, Current_Color);
                end if;
                Weights (0) := Weights (0) + X_Partials (0);
                Weights (1) := Weights (1) + X_Partials (1);
                Weights (2) := Weights (2) + X_Partials (2);
            end loop;
            Weights_Row (0) := Weights_Row (0) + Y_Partials (0);
            Weights_Row (1) := Weights_Row (1) + Y_Partials (1);
            Weights_Row (2) := Weights_Row (2) + Y_Partials (2);
        end loop;
    end Rasterize_With_Tests;

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
    end Wait;

    task body Worker_Thread is
        My_Id : Integer;
        Current_Job : Draw_Command;
        Got_Work : Boolean;
    begin
        accept Run (Id : Integer) do
            My_Id := Id;
        end;
        while (Program_Running) loop
            -- program is running, wait for a job
            while (Job_Running) loop
                -- job is running, try to find a task to complete
                Work_Queue.Remove (Current_Job, Got_Work);
                Thread_Running (My_Id) := Got_Work;
                if Got_Work = False then
                    goto Continue;
                end if;

                case Current_Job.Id is
                    when Clear_Row =>
                        Clear_Rows (Current_Job.Int_Args (0),
                                    Current_Job.Int_Args (1),
                                    Current_Chunk);
                    when Fill_Rectangle =>
                        Fill_Rectangle (Current_Job.Int_Args (0),
                                        Current_Job.Int_Args (1),
                                        Current_Job.Int_Args (2),
                                        Current_Job.Int_Args (3));
                    when Rasterize_Tile_With_Tests =>
                        Rasterize_With_Tests (Unpack_Tile_From_Buffer (Current_Job.Int_Args));
                end case;
    <<Continue>>
            end loop;
        end loop;
    end Worker_Thread;

end Renderer;

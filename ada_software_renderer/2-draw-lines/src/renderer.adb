with Constants; use Constants;

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

        Ptr := Pixel_Data.Get_Row (Color_Buffer,
                                   SDL.Coordinate (Y))
               + C.ptrdiff_t (X);
        Ptr.all := Color;
    end Write_Pixel;

    procedure Write_Pixel (X, Y : Integer;
                           Color : AVX2.Vector_256_U32) is
        Ptr : constant Pixel_Chunk_Pointers.Pointer :=
            Pixel_Chunk_Data.Get_Row (Color_Buffer,
                                      SDL.Coordinate (Y)) +
            C.ptrdiff_t (X);
    begin
        Ptr.all := Color;
    end Write_Pixel;

    procedure Clear_Rows (Y, Row_Count: Integer;
                          Color : AVX2.Vector_256_U32) is
        Chunk_Count : constant Integer := Width / 8;
    begin
        for I in 0 .. Row_Count loop
            for X in 0 .. Chunk_Count loop
                Write_Pixel (X, Y + I, Color);
            end loop;
        end loop;
    end Clear_Rows;

    procedure Clear_Screen (Color: Unsigned_32) is
        Current_Job : Clear_Row_Command;
        Job_Enqeued : Boolean := False;
        Row_Count : constant Integer := 128;
        Job_Count : constant Integer := Height / Row_Count;
    begin
        Current_Chunk :=
            [Color, Color, Color, Color, Color, Color, Color, Color];
        Current_Job.Row_Count := Row_Count;

        Job_Running := True;

        for Y in 0 .. Job_Count loop
            Current_Job.Y := Row_Count * Y;
            Job_Enqeued := False;
            while Job_Enqeued = False loop
                Work_Queue.Insert (Current_Job, Job_Enqeued);
            end loop;
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
        Current_Job : Clear_Row_Command;
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
                Clear_Rows (Current_Job.Y,
                            Current_Job.Row_Count,
                            Current_Chunk);
    <<Continue>>
            end loop;
        end loop;
    end Worker_Thread;

end Renderer;

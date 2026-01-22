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
        Job_Running := False;
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

    procedure Write_Pixel (
        X, Y : Integer;
        Color : Unsigned_32) is
        Row_Ptr : constant Pixel_Pointers.Pointer :=
            Pixel_Data.Get_Row (Color_Buffer, SDL.Coordinate (Y));
        Ptr : constant Pixel_Pointers.Pointer := Row_Ptr + C.ptrdiff_t (X);
    begin
        Ptr.all := Color;
    end Write_Pixel;

    procedure Write_Pixel (
        X, Y : Integer;
        Color : AVX2.Vector_256_U32) is
        Row_Ptr : constant Pixel_Chunk_Pointers.Pointer :=
            Pixel_Chunk_Data.Get_Row (Color_Buffer, SDL.Coordinate (Y));
        Ptr : constant Pixel_Chunk_Pointers.Pointer :=
            Row_Ptr + C.ptrdiff_t (X);
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

    procedure Clear_Screen_Concurrent (Color: Unsigned_32) is
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
        Job_Running := False;
    end Clear_Screen_Concurrent;

    procedure Wait is
    begin
        while (Thread_Running (1) = True or
               Thread_Running (2) = True or
               Thread_Running (3) = True or
               Thread_Running (4) = True) loop
            null;
        end loop;
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

    procedure Clear_Screen (Color: Unsigned_32) is
    begin
        for Y in 0 .. Height - 1 loop
            for X in 0 .. Width - 1 loop
                Write_Pixel (X, Y, Color);
            end loop;
        end loop;
    end Clear_Screen;

    procedure Clear_Screen_SIMD (Color: Unsigned_32) is
        Chunk : constant AVX2.Vector_256_U32 :=
            [Color, Color, Color, Color, Color, Color, Color, Color];
        Chunks_Per_Row : constant Integer := Width / 8;
    begin
        for Y in 0 .. Height - 1 loop
            for X in 0 .. Chunks_Per_Row loop
                Write_Pixel (X, Y, Chunk);
            end loop;
        end loop;
    end Clear_Screen_SIMD;

end Renderer;

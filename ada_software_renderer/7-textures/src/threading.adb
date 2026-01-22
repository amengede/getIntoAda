with Renderer;
with Simd; use Simd;

package body Threading is

    procedure Initialize is
    begin
        -- Launch Worker threads
        for I in 1 .. Worker_Thread_Count loop
            Worker_Threads (I).Run (Id => I);
        end loop;
    end Initialize;

    procedure Finalize is
    begin
        Wait;
        Program_Running := False;
    end Finalize;

    procedure Start_Dispatching_Jobs is
    begin
        Job_Running := True;
    end Start_Dispatching_Jobs;

    procedure Wait is
        Queue_Empty : Boolean := False;
    begin
        while (not Queue_Empty or
               Thread_Running (1) or
               Thread_Running (2) or
               Thread_Running (3) or
               Thread_Running (4)) loop
            Work_Queue.Empty (Queue_Empty);
        end loop;
        Job_Running := False;
        Command_Buffer_Offset := 0;
    end Wait;

    procedure Dispatch_Clear_Command (Y, Row_Count : Integer_32) is
        Enqueued : Boolean;
        I : constant Command_Buffer_Index := Command_Buffer_Offset;
    begin
        Command_Buffer (I) := Command_Type_Clear_Rows;
        Command_Buffer (I + 1) := Y;
        Command_Buffer (I + 2) := Row_Count;
        Work_Queue.Insert (I, Enqueued);
        if not Enqueued then
            Renderer.Clear_Rows (Y, Row_Count);
        end if;
        Command_Buffer_Offset := I + 3;
    end Dispatch_Clear_Command;

    procedure Perform_Clear_Command (Index : Command_Buffer_Index)
    is
    begin
        Renderer.Clear_Rows (Y => Command_Buffer (Index + 1),
                             Row_Count => Command_Buffer (Index + 2));
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
            Renderer.Rasterize_With_Tests (Min_Corner,  Max_Corner,  Weights);
        else
            Renderer.Rasterize_No_Tests (Min_Corner,  Max_Corner,  Weights);
        end if;
    end;

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
end Threading;

with Constants; use Constants;
with Interfaces; use Interfaces;
with Maths; use Maths;

package Threading is
    Command_Type_Clear_Rows : constant Integer_32 := 0;
    Command_Type_Rasterize_Tile_No_Tests : constant Integer_32 := 1;
    Command_Type_Rasterize_Tile_With_Tests : constant Integer_32 := 2;

    procedure Initialize;

    procedure Finalize;

    procedure Start_Dispatching_Jobs;

    procedure Wait;

    procedure Dispatch_Clear_Command (Y, Row_Count : Integer_32);
    procedure Perform_Clear_Command (Index : Command_Buffer_Index);
    procedure Dispatch_Raster_Command (Tile : Rasterization_Tile;
                                       Perform_Tests : Boolean);
    procedure Perform_Raster_Command (Index : Command_Buffer_Index;
                                      Perform_Tests : Boolean);

private
    type Thread_Flags is array (1 .. Worker_Thread_Count) of Boolean with
        Volatile_Components,
        Atomic_Components;

    Thread_Running : Thread_Flags := [False, False, False, False];
    Job_Running : Boolean := False with Volatile, Atomic;
    Program_Running : Boolean := True with Volatile, Atomic;

    task type Worker_Thread is
        entry Run (Id : Integer);
    end Worker_Thread;

    Worker_Threads : array (1 .. Worker_Thread_Count) of Worker_Thread;

    -- Internal State
    Command_Buffer : array (Command_Buffer_Index) of Integer_32;
    Command_Buffer_Offset : Command_Buffer_Index := 0;
end Threading;

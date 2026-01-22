with Interfaces.C.Pointers;
with SDL.Video.Surfaces;
with SDL.Video.Windows;
with SDL.Video.Palettes;
with Interfaces; use Interfaces;
with SDL.Video.Pixel_Formats;
with Maths; use Maths;
with SIMD; use SIMD;
with Queues;

package Renderer is

    -- Make a new color from individual components
    function Map_RGBA (R, G,
                       B, A : SDL.Video.Palettes.Colour_Component)
        return Unsigned_32;

    -- Clear the screen
    -- @param Color Color to clear the screen with
    procedure Clear_Screen (Color : Unsigned_32);

    procedure Draw_Triangles (Vertices : Vertex_Buffer;
                              First_Vertex, Triangle_Count: Natural);

    -- Prepare the renderer for rendering
    -- @param Window Window to render to
    procedure Initialize (Window : SDL.Video.Windows.Window);

    procedure Finalize;

    -- Start drawing, this locks the screen
    procedure Start_Drawing;

    -- Finish drawing, this releases the screen
    procedure End_Drawing;

private
    type Pixel_Array is array (Integer_32 range <>) of aliased Unsigned_32;

    package Pixel_Pointers is new Interfaces.C.Pointers (
        Index => Integer_32,
        Element => Unsigned_32,
        Element_Array => Pixel_Array,
        Default_Terminator => 0);
    use type Pixel_Pointers.Pointer;

    package Pixel_Data is new SDL.Video.Surfaces.Pixel_Data (
        Element => Unsigned_32,
        Element_Pointer => Pixel_Pointers.Pointer);

    type Pixel_Chunk_Array is array (Integer_32 range <>)
        of aliased Uint32x4;

    package Pixel_Chunk_Pointers is new Interfaces.C.Pointers (
        Index => Integer_32,
        Element => Uint32x4,
        Element_Array => Pixel_Chunk_Array,
        Default_Terminator => [0, 0, 0, 0]);
    use type Pixel_Chunk_Pointers.Pointer;

    package Pixel_Chunk_Data is new SDL.Video.Surfaces.Pixel_Data (
        Element => Uint32x4,
        Element_Pointer => Pixel_Chunk_Pointers.Pointer);

    -- Underlying Surface to draw to
    Color_Buffer : SDL.Video.Surfaces.Surface;
    Red_Shift, Blue_Shift, Green_Shift, Alpha_Shift : Unsigned_32;

    function Get_Shift_From_Mask (Mask : SDL.Video.Pixel_Formats.Colour_Mask)
        return Unsigned_32;

    procedure Clear_Rows (Y, Row_Count: Integer_32;
                          Color : Uint32x4);

    -- Write an individual pixel
    -- @param Point Position to draw to
    -- @param Color Color to draw
    procedure Write_Pixel (X, Y : Integer_32; Color : Unsigned_32);

    -- Write a chunk of pixels
    -- @param Point Position to draw to
    -- Param Color Color to draw
    procedure Write_Pixel (X, Y : Integer_32; Color : Uint32x4);

    function Shade_Vertex (Vert : Vertex;
                           Attributes : out Attribute_Package) return Int32x2;

    procedure Rasterize_Triangle;

    procedure Subdivide (Tile : Rasterization_Tile);

    procedure Rasterize_With_Tests (Min_Corner, Max_Corner : Int32x2;
                                    Weights : Point_3D);

    procedure Rasterize_No_Tests (Min_Corner, Max_Corner : Int32x2;
                                  Weights : Point_3D);

    procedure Shade_Fragment (X, Y : Integer_32;
                              Weights : Point_3D);

    type Command_Buffer_Index is mod 2 ** 20;

    Command_Type_Clear_Rows : constant Integer_32 := 0;
    Command_Type_Rasterize_Tile_No_Tests : constant Integer_32 := 1;
    Command_Type_Rasterize_Tile_With_Tests : constant Integer_32 := 2;

    procedure Dispatch_Clear_Command (Y, Row_Count : Integer_32);
    procedure Perform_Clear_Command (Index : Command_Buffer_Index);
    procedure Dispatch_Raster_Command (Tile : Rasterization_Tile;
                                       Perform_Tests : Boolean);
    procedure Perform_Raster_Command (Index : Command_Buffer_Index;
                                   Perform_Tests : Boolean);

    package Work_Queues is new Queues (T => Command_Buffer_Index);
    Work_Queue : Work_Queues.Queue;

    Worker_Thread_Count : constant Integer := 4;
    type Thread_Flags is array (1 .. Worker_Thread_Count) of Boolean with Volatile_Components;

    Thread_Running : Thread_Flags := [False, False, False, False];
    Job_Running : Boolean := False with Volatile;
    Program_Running : Boolean := True with Volatile;

    procedure Wait;

    task type Worker_Thread is
        entry Run (Id : Integer);
    end Worker_Thread;

    Worker_Threads : array (1 .. Worker_Thread_Count) of Worker_Thread;

    -- State
    Current_Color : Unsigned_32;
    Current_Chunk : Uint32x4;
    Current_Triangle : Triangle_2D;
    X_Partials, Y_Partials : Point_3D;
    Attributes : Attribute_Bundle;
    Command_Buffer : array (Command_Buffer_Index) of Integer_32;
    Command_Buffer_Offset : Command_Buffer_Index := 0;

end Renderer;

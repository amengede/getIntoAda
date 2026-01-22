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

    -- Draw a line
    -- @param Point_1 First point
    -- @param Point_2 Second point
    -- @param Color Color of line
    procedure Draw_Line (Point_1, Point_2 : Point_2D;
                         Color : Unsigned_32);

    procedure Draw_Triangle (Triangle : Triangle_2D; Color : Unsigned_32);

    -- Prepare the renderer for rendering
    -- @param Window Window to render to
    procedure Initialize (Window : SDL.Video.Windows.Window); 

    procedure Finalize;

    -- Start drawing, this locks the screen
    procedure Start_Drawing;

    -- Finish drawing, this releases the screen
    procedure End_Drawing;

private
    type Pixel_Array is array (Integer range <>) of aliased Unsigned_32;

    package Pixel_Pointers is new Interfaces.C.Pointers (
        Index => Integer,
        Element => Unsigned_32,
        Element_Array => Pixel_Array,
        Default_Terminator => 0);
    use type Pixel_Pointers.Pointer;

    package Pixel_Data is new SDL.Video.Surfaces.Pixel_Data (
        Element => Unsigned_32,
        Element_Pointer => Pixel_Pointers.Pointer);

    type Pixel_Chunk_Array is array (Integer range <>)
        of aliased Vector_256_U32;

    package Pixel_Chunk_Pointers is new Interfaces.C.Pointers (
        Index => Integer,
        Element => Vector_256_U32,
        Element_Array => Pixel_Chunk_Array,
        Default_Terminator => [0, 0, 0, 0, 0, 0, 0, 0]);
    use type Pixel_Chunk_Pointers.Pointer;

    package Pixel_Chunk_Data is new SDL.Video.Surfaces.Pixel_Data (
        Element => Vector_256_U32,
        Element_Pointer => Pixel_Chunk_Pointers.Pointer);

    -- Underlying Surface to draw to
    Color_Buffer : SDL.Video.Surfaces.Surface;
    Red_Shift, Blue_Shift, Green_Shift, Alpha_Shift : Unsigned_32;

    function Get_Shift_From_Mask (Mask : SDL.Video.Pixel_Formats.Colour_Mask)
        return Unsigned_32;

    procedure Clear_Rows (Y, Row_Count: Integer;
                          Color : Vector_256_U32);

    -- Write an individual pixel
    procedure Write_Pixel (X, Y : Integer; Color : Unsigned_32);

    -- Write a chunk of pixels
    procedure Write_Pixel (X, Y : Integer; Color : Vector_256_U32);

    -- Draw a horizontal line
    -- @param Point Origin to draw from
    -- @param Length Line length
    procedure Draw_Horizontal_Line (Point : Point_2D;
                                    Length : Integer);

    -- Draw a vertical line
    -- @param Point Origin to draw from
    -- @param Length Line length
    procedure Draw_Vertical_Line (Point : Point_2D;
                                  Length : Integer);

    -- Swap two Points
    -- @param Point_1 Input point 1
    -- @param Point_2 Input point 2
    -- @param Point_a Output point 1
    -- @param Point_b Output point 2
    -- @param Slope Slope between the two points
    procedure Reorient (Point_1, Point_2 : in Point_2D;
                        Point_a, Point_b : out Point_2D;
                        Slope : in out Point_2D);

    -- Draw a shallow line
    -- @param Point line origin
    -- @param Slope line slope
    procedure Draw_Bresenham_Shallow (Point, Slope : Point_2D);

    -- Draw a steep line
    -- @param Point line origin
    -- @param Slope line slope
    procedure Draw_Bresenham_Steep (Point, Slope : Point_2D);

    type Clear_Row_Command is
        record
            Y, Row_Count : Integer;
        end record;

    package Work_Queues is new Queues (T => Clear_Row_Command);
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
    Current_Chunk : Vector_256_U32;
end Renderer;

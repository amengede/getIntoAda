with SDL.Video.Surfaces;
with SDL.Video.Windows;
with SDL.Video.Palettes;
with Interfaces; use Interfaces;
with Maths; use Maths;
with SIMD; use SIMD;

package Renderer is

    -- Make a new color from individual components
    function Map_RGBA (R, G,
                       B, A : SDL.Video.Palettes.Colour_Component)
        return Unsigned_32;

    function Map_RGBA (Color : Float32x4)
        return Unsigned_32;

    -- Make a new color from individual components
    function Map_RGBA (R, G,
                       B, A : Uint32x4)
        return Uint32x4;

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

    procedure Clear_Rows (Y, Row_Count: Integer_32);

    procedure Rasterize_With_Tests (Min_Corner, Extent : Int32x2;
                                    Weights : Point_3D);

    procedure Rasterize_No_Tests (Min_Corner, Extent : Int32x2;
                                  Weights : Point_3D);

private
    -- Underlying Surface to draw to
    Color_Buffer : SDL.Video.Surfaces.Surface;
    Red_Shift, Blue_Shift, Green_Shift, Alpha_Shift : Unsigned_32;
    Red_VShift, Blue_VShift, Green_VShift, Alpha_VShift : Uint32x4;

    -- Write an individual pixel
    -- @param Point Position to draw to
    -- @param Color Color to draw
    procedure Write_Pixel (X, Y : Integer_32; Color : Unsigned_32);

    -- Write a chunk of pixels
    -- @param Point Position to draw to
    -- Param Color Color to draw
    procedure Write_Pixel (X, Y : Integer_32; Color : Uint32x8);

    function Shade_Vertex (Vert : Vertex;
                           Attributes : out Attribute_Package) return Int32x2;

    procedure Select_Mip_Level;

    procedure Rasterize_Triangle;

    procedure Subdivide (Tile : Rasterization_Tile);

    procedure Shade_Fragment (X, Y,
                              W1, W2 : Int32x4;
                              Mask : Flags);

    -- State
    Current_Color : Unsigned_32;
    Current_Chunk : Uint32x8;
    Current_Triangle : Triangle_2D;
    Weight_Total : Float32x4;
    X_Partials, Y_Partials : Point_3D;
    Attributes : Attribute_Bundle;
    Current_Texture, Current_Mip_Level : Natural;
    DX : constant Int32x4 := [4, 4, 4, 4];
    DY : constant Int32x4 := [1, 1, 1, 1];
    dW0dX, dW1dX, dW2dX, dW0dY, dW1dY, dW2dY : Int32x4;
    Red_0, Green_0, Blue_0, U_0, V_0,
    Red_1, Green_1, Blue_1, U_1, V_1,
    Red_2, Green_2, Blue_2, U_2, V_2 : Float32x4;

end Renderer;

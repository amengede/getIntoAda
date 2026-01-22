with Interfaces.C.Pointers;
with Interfaces; use Interfaces;
with Simd; use Simd;
with SDL.Video.Surfaces;

package Colors_And_Pixels is
    type Pixel_Array is array (Integer_32 range <>) of aliased Unsigned_32;

    package Pixel_Pointers is new
    Interfaces.C.Pointers (Index => Integer_32,
                           Element => Unsigned_32,
                           Element_Array => Pixel_Array,
                           Default_Terminator => 0);
    use type Pixel_Pointers.Pointer;

    package Pixel_Data is new
    SDL.Video.Surfaces.Pixel_Data (Element => Unsigned_32,
                                   Element_Pointer => Pixel_Pointers.Pointer);

    type Pixel_Chunk_Array is array (Integer_32 range <>)
        of aliased Uint32x8;

    package Pixel_Chunk_Pointers is new
    Interfaces.C.Pointers (Index => Integer_32,
                            Element => Uint32x8,
                            Element_Array => Pixel_Chunk_Array,
                            Default_Terminator => [0, 0, 0, 0, 0, 0, 0, 0]);
    use type Pixel_Chunk_Pointers.Pointer;

    package Pixel_Chunk_Data is new
    SDL.Video.Surfaces.Pixel_Data (Element => Uint32x8,
                                   Element_Pointer => Pixel_Chunk_Pointers.Pointer);

    function Read_Pixel (Surface : SDL.Video.Surfaces.Surface;
                         X, Y : Integer_32) return Unsigned_32;

    function Get_Shift_From_Mask (Mask : Unsigned_32)
        return Unsigned_32;

    function Get_Channel (Color,
                          Mask,
                          Shift : Unsigned_32) return IEEE_Float_32;

end Colors_And_Pixels;

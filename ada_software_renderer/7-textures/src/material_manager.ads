with Interfaces; use Interfaces;
with Vectors;
with Simd; use Simd;
with SDL.Video.Surfaces;

package Material_Manager is

    procedure Initialize;

    procedure Finalize;

    function Make_Texture (Filename : String) return Natural;

    function Get_Mip_Level (Handle: Natural;
                            DUV_DPos : Float32x2)
        return Natural;

    function Sample (UV : Float32x2) return Float32x4;

private

    package Float32x4_Vectors is new Vectors (T => Float32x4);

    package Size_Vectors is new Vectors (T => Int32x2);

    package Offset_Vectors is new Vectors (T => Natural);

    type Texture is
        record
            -- Dimensions
            Sizes : aliased Size_Vectors.Vector_Access := null;
            Mip_Offsets : Offset_Vectors.Vector_Access := null;

            -- Data
            Data : Float32x4_Vectors.Vector_Access := null;

            -- Sampler
            Red_Shift,
            Green_Shift,
            Blue_Shift,
            Alpha_Shift : Unsigned_32;

            Red_Mask,
            Green_Mask,
            Blue_Mask,
            Alpha_Mask : Unsigned_32;
        end record;

    procedure Get_Color_Shifts (New_Texture : in out Texture;
                                Loaded_Image : SDL.Video.Surfaces.Surface);

    procedure Populate (New_Texture : in out Texture;
                        Loaded_Image : SDL.Video.Surfaces.Surface;
                        Level_Count : Integer_32);

    package Texture_Vectors is new Vectors (T => Texture);

    Textures : Texture_Vectors.Vector_Access := null;

    function Get_Pixel_Address (X, Y : Integer_32) return Natural;

    -- Internal State
    Mip_Offset : Natural;
    Image_Width : Integer_32;
    Converted_Size : Float32x2;
    Data : Float32x4_Vectors.Vector_Access;

end Material_Manager;

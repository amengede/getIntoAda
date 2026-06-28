with GL.Types;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with SDL;
with SDL.Images.IO;
with SDL.Video.Pixel_Formats;
with SDL.Video.Surfaces.Makers;

package body Materials is
    procedure Initialize is
        Image_Initialization_Exception : exception;
    begin
        if not Images.Initialise then
            raise Image_Initialization_Exception;
        end if;
    end Initialize;

    function From_Image (Filename : String) return Textures.Texture is
        package Types renames GL.Types;
        package Targets renames GL.Objects.Textures.Targets;
        package Pixels renames GL.Pixels;
        package IO renames SDL.Images.IO;

        use Targets;
        use Pixels;
        use Textures;
        use SDL.Video.Pixel_Formats;
        use SDL.Video.Surfaces.Makers;

        Pixel_Format : constant Pixel_Format_Access :=
            Create (Pixel_Format_RGB_888);
        Texture : Textures.Texture;
        Loaded_Data : Surfaces.Surface;
        Address : Textures.Image_Source;
        Texture_Size : SDL.Sizes;
    begin
        IO.Create (Loaded_Data, Filename);
        Convert (Loaded_Data, Loaded_Data, Pixel_Format);
        Texture_Size := Loaded_Data.Size;
        Address := Textures.Image_Source (Loaded_Data.Pixels);
        Texture.Initialize_Id;
        Texture_2D.Bind (Texture);
        Texture_2D.Load_From_Data (
            Level => 0,
            Internal_Format => RGBA8,
            Width => Types.Size (Texture_Size.Width),
            Height => Types.Size (Texture_Size.Height),
            Source_Format => RGBA,
            Source_Type => Unsigned_Byte,
            Source => Address);
        Texture_2D.Set_Minifying_Filter (Nearest);
        Texture_2D.Set_Magnifying_Filter (Nearest);
        Texture_2D.Set_X_Wrapping (Repeat);
        Texture_2D.Set_Y_Wrapping (Repeat);

        return Texture;
    end From_Image;

    procedure Finalize is
    begin
        Images.Finalise;
    end Finalize;
end Materials;

with Ada.Text_IO; use Ada.Text_IO;
with SDL.Images;
with SDL.Images.IO;
with SDL;
with Ada.Numerics.Elementary_Functions;
with SDL.Video.Surfaces.Makers;
with Colors_And_Pixels; use Colors_And_Pixels;
with System; use System;
with Simd.Conversions; use Simd.Conversions;
with Simd.Multiplies; use Simd.Multiplies;

package body Material_Manager is

    procedure Initialize is
    begin
        if not SDL.Images.Initialise then
            Put_Line ("Failed to initialize the image module!");
        end if;
        Textures := new Texture_Vectors.Vector;
    end Initialize;

    procedure Finalize is
    begin
        SDL.Images.Finalise;
    end Finalize;

    procedure Get_Color_Shifts (New_Texture : in out Texture;
                                Loaded_Image : SDL.Video.Surfaces.Surface) is
    begin

        New_Texture.Red_Mask :=
            Unsigned_32 (Loaded_Image.Pixel_Format.Red_Mask);
        New_Texture.Red_Shift :=
            Colors_And_Pixels.Get_Shift_From_Mask (New_Texture.Red_Mask);
        New_Texture.Green_Mask :=
            Unsigned_32 (Loaded_Image.Pixel_Format.Green_Mask);
        New_Texture.Green_Shift :=
            Colors_And_Pixels.Get_Shift_From_Mask (New_Texture.Green_Mask);
        New_Texture.Blue_Mask :=
            Unsigned_32 (Loaded_Image.Pixel_Format.Blue_Mask);
        New_Texture.Blue_Shift :=
            Colors_And_Pixels.Get_Shift_From_Mask (New_Texture.Blue_Mask);
        New_Texture.Alpha_Mask :=
            Unsigned_32 (Loaded_Image.Pixel_Format.Alpha_Mask);
        New_Texture.Alpha_Shift :=
            Colors_And_Pixels.Get_Shift_From_Mask (New_Texture.Alpha_Mask);
    end Get_Color_Shifts;

    procedure Allocate_Texture_Space (New_Texture : in out Texture;
                                      Loaded_Image : SDL.Video.Surfaces.Surface;
                                      Level_Count : out Integer_32)
    is
        Texture_Size : SDL.Sizes;
        Min_Size : Integer_32;
        Current_Width, Current_Height : Integer_32;
        Total_Size : Natural;
    begin
        Texture_Size := Loaded_Image.Size;
        Current_Width := Integer_32 (Texture_Size.Width);
        Current_Height := Integer_32 (Texture_Size.Height);
        Min_Size := Integer_32'Min (Current_Width, Current_Height);
        Level_Count := Integer_32(Ada.Numerics.Elementary_Functions.Log (Float (Min_Size), 2.0));
        -- Put_Line ("Mip Level Count: " & Level_Count'Image);
        Total_Size := Natural (Level_Count);
        Size_Vectors.Reserve (New_Texture.Sizes, Total_Size);
        Offset_Vectors.Reserve (New_texture.Mip_Offsets, Total_Size);
        Total_Size := Natural (4 * Current_Width * Current_Height);
        Float32x4_Vectors.Reserve (New_Texture.Data, Total_Size);

        for I in 0 .. Level_Count - 1 loop
            Size_Vectors.Append (New_Texture.Sizes, [Current_Width,
                                                     Current_Height]);
            Current_Width := Current_Width / 2;
            Current_Height := Current_Height / 2;
        end loop;
    end Allocate_Texture_Space;

    procedure Populate (New_Texture : in out Texture;
                        Loaded_Image : SDL.Video.Surfaces.Surface;
                        Level_Count : Integer_32)
    is
        Temp_Image : SDL.Video.Surfaces.Surface;
        Current_Size : Int32x2;
        Current_Width, Current_Height : Integer_32;
        Color : Unsigned_32;
        Converted : Float32x4;
        Offset : Natural;
    begin

        -- Copy Data to texture
        for Level in Natural range 0 .. Natural (Level_Count - 1) loop
            Current_Size := Size_Vectors.Get (New_Texture.Sizes, Level);
            Current_Width := Current_Size (0);
            Current_Height := Current_Size (1);

            SDL.Video.Surfaces.Makers
            .Create(Temp_Image,
                    (SDL.Dimension (Current_Width),
                     SDL.Dimension (Current_Height)),
                    BPP => 32,
                    Red_Mask => SDL.Video.Surfaces
                    .Colour_Masks(New_Texture.Red_Mask),
                    Blue_Mask => SDL.Video.Surfaces
                    .Colour_Masks(New_Texture.Blue_Mask),
                    Green_Mask => SDL.Video.Surfaces
                    .Colour_Masks(New_Texture.Green_Mask),
                    Alpha_Mask => SDL.Video.Surfaces
                    .Colour_Masks(New_Texture.Alpha_Mask));
            Temp_Image.Blit_Scaled (Loaded_Image);

            Offset := New_Texture.Data.Size;

            Offset_Vectors.Append (New_Texture.Mip_Offsets, Offset);

            for Y in 0 .. Current_Height - 1 loop
                for X in 0 .. Current_Width - 1 loop
                    Color := Colors_And_Pixels.Read_Pixel (Temp_Image, X, Y);

                    -- Get channels
                    Converted := [Colors_And_Pixels
                               .Get_Channel (Color,
                                             New_Texture.Red_Mask,
                                             New_Texture.Red_Shift),
                               Colors_And_Pixels
                               .Get_Channel (Color,
                                             New_Texture.Green_Mask,
                                             New_Texture.Green_Shift),
                               Colors_And_Pixels
                               .Get_Channel (Color,
                                             New_Texture.Blue_Mask,
                                             New_Texture.Blue_Shift),
                               Colors_And_Pixels
                               .Get_Channel (Color,
                                             New_Texture.Alpha_Mask,
                                             New_Texture.Alpha_Shift)];
                    Float32x4_Vectors.Append (New_Texture.Data, Converted);
                end loop;
            end loop;
        end loop;

        for Level in Natural range 0 .. Natural (Level_Count - 1) loop
            Current_Size := Size_Vectors.Get (New_Texture.Sizes, Level);
            Current_Width := Current_Size (0);
            Current_Height := Current_Size (1);
            Offset := Offset_Vectors.Get (New_Texture.Mip_Offsets, Level);
            Put_Line ("Mip Level "
                      & Level'Image
                      & " has size ("
                      & Current_Width'Image & ", "
                      & Current_Height'Image &
                      "), and starts at memory location " & Offset'Image);
        end loop;
    end Populate;

    function Make_Texture (Filename : String) return Natural is
        Handle : constant Natural := Textures.Size;
        Loaded_Image : SDL.Video.Surfaces.Surface;
        New_Texture : Texture;
        Level_Count : Integer_32;
    begin

        New_Texture.Sizes := new Size_Vectors.Vector;
        New_Texture.Mip_Offsets := new Offset_Vectors.Vector;
        New_Texture.Data := new Float32x4_Vectors.Vector;

        -- Load image, get info
        SDL.Images.IO.Create (Loaded_Image, Filename);
        Get_Color_Shifts (New_Texture, Loaded_Image);

        Allocate_Texture_Space (New_Texture,
                                Loaded_Image,
                                Level_Count);

        Populate (New_Texture, Loaded_Image,  Level_Count);

        Texture_Vectors.Append (Textures, New_Texture);
        return Handle;
    end Make_Texture;

    function Get_Mip_Level (Handle: Natural;
                            DUV_DPos : Float32x2) return Natural is
        Current_Texture : constant Texture := Texture_Vectors.Get (Textures, Handle);
        Current_Size : Float32x2;
        Dcoords_DPos : Float32x2;
        Threshold : constant IEEE_Float_32 := 1.0;

        Chosen_Level : Natural := 0;
        Current_Size_Int : Int32x2;
    begin
        for Level in 0 .. Current_Texture.Sizes.Size loop
            Current_Size := Convert (Size_Vectors.Get (Current_Texture.Sizes, Level));
            Dcoords_DPos := Multiply (Current_Size, DUV_DPos);
            --Put_Line ("Considering Mip Level " & Level'Image);
            --Put_Line ("Increments are ("
            --          & Dcoords_DPos (0)'Image & ", "
            --          & Dcoords_DPos (1)'Image & ")");
            if (abs Dcoords_DPos (0) <= Threshold and
                abs Dcoords_DPos (1) <= Threshold) then
                Chosen_Level := Level;
                goto break;
            end if;
        end loop;
    <<break>>

        -- Save global state
        Mip_Offset := Offset_Vectors.Get (Current_Texture.Mip_Offsets,
                                          Chosen_Level);
        Current_Size_Int := Size_Vectors.Get (Current_Texture.Sizes,
                                              Chosen_Level);
        Image_Width := Current_Size_Int (0);
        Converted_Size := Convert (Current_Size_Int);
        Data := Current_Texture.Data;

        return Chosen_Level;
    end Get_Mip_Level;

    function Get_Pixel_Address (X, Y : Integer_32) return Natural is
    begin
        Return Mip_Offset + Natural (Y * Image_Width + X);
    end Get_Pixel_Address;

    function Sample (UV : Float32x2) return Float32x4 is
        UV_clipped : Float32x2 := UV;
        UV_Actual : Int32x2;
        Sampled : Float32x4;
        Pixel_Address : Natural;
    begin
        -- "Wrap around" coordinates
        while UV_clipped(0) < 0.0 loop
            UV_clipped(0) := UV_clipped(0) + 1.0;
        end loop;
        while UV_clipped(0) > 1.0 loop
            UV_clipped(0) := UV_clipped(0) - 1.0;
        end loop;
        while UV_clipped(1) < 0.0 loop
            UV_clipped(1) := UV_clipped(1) + 1.0;
        end loop;
        while UV_clipped(1) > 1.0 loop
            UV_clipped(1) := UV_clipped(1) - 1.0;
        end loop;

        -- Get actual position of uv on texture
        UV_Actual := Convert (Multiply (Converted_Size, UV_clipped));

        -- Get color
        Pixel_Address := Get_Pixel_Address (UV_Actual (0),
                                            UV_Actual (1));
        -- Get channels
        Sampled := Float32x4_Vectors.Get (Data, Pixel_Address);

        return sampled;
    end Sample;

end Material_Manager;

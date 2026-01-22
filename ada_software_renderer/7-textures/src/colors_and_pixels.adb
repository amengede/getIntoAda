package body Colors_And_Pixels is

    function Read_Pixel (Surface : SDL.Video.Surfaces.Surface;
                         X, Y : Integer_32) return Unsigned_32 is
        Ptr : constant Pixel_Pointers.Pointer :=
            Pixel_Data.Get_Row (Surface, SDL.Coordinate (Y))
            + Interfaces.C.ptrdiff_t (X);
        Color : Unsigned_32;
    begin
        Color := Ptr.all;
        return Color;
    end Read_Pixel;

    function Get_Shift_From_Mask (Mask : Unsigned_32)
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

    function Get_Channel (Color,
                          Mask,
                          Shift : Unsigned_32) return IEEE_Float_32 is
        Temp_Color : Unsigned_32;
    begin
        if Shift = 0 then
            return 0.0;
        end if;

        -- Mask Out
        Temp_Color := Color and Mask;

        -- Shift down
        Temp_Color := Temp_Color / Shift;

        -- Normalize
        return IEEE_Float_32 (Temp_Color) / 255.0;
    end Get_Channel;

end Colors_And_Pixels;

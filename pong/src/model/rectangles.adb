with SDL;

package body Rectangles is
    procedure Initialize (Self : in out Rectangle;
                          X, Y, Width, Height : Integer;
                          Colour : Palettes.Colour) is
    begin
        Self.X := X;
        Self.Y := Y;
        Self.Width := Width;
        Self.Height := Height;
        Self.Colour := Colour;
    end Initialize;

    function Get_Colour (Self : Rectangle) return Palettes.Colour is
    begin
        return Self.Colour;
    end Get_Colour;

    function Get_Geometry (Self : Rectangle)
        return SDL.Video.Rectangles.Rectangle is
    begin
        return SDL.Video.Rectangles.Rectangle'
        (X => SDL.Coordinate (Self.X),
         Y => SDL.Coordinate (Self.Y),
         Width => SDL.Natural_Dimension (Self.Width),
         Height => SDL.Natural_Dimension (Self.Height));
    end Get_Geometry;

    procedure Move_To (Self : in out Rectangle; X, Y : Integer) is
    begin
        Self.X := X;
        Self.Y := Y;
    end Move_To;

    procedure Move_By (Self : in out Rectangle; Dx, Dy : Integer) is
    begin
        Self.X := Self.X + Dx;
        Self.Y := Self.Y + Dy;
    end Move_By;

    function Overlaps (Self, Other : Rectangle) return Boolean is
        --  Sides of first rectangle
        This_Left : constant Integer := Self.X;
        This_Right : constant Integer := Self.X + Self.Width;
        This_Top : constant Integer := Self.Y;
        This_Bottom : constant Integer := Self.Y + Self.Height;

        --  Sides of second rectangle
        Other_Left : constant Integer := Other.X;
        Other_Right : constant Integer := Other.X + Other.Width;
        Other_Top : constant Integer := Other.Y;
        Other_Bottom : constant Integer := Other.Y + Other.Height;
    begin
        if This_Left > Other_Right
            or else This_Right < Other_Left
            or else This_Top > Other_Bottom
            or else This_Bottom < Other_Top
        then
            return False;
        else
            return True;
        end if;
    end Overlaps;

    function Move_And_Bounce (Self : in out Rectangle;
                              Dx, Dy : Integer;
                              Restricted : Rectangle_Vectors.Vector)
        return Hit_Record is
        Bounce_Status : Hit_Record;
        Other : Rectangle;
    begin

        --  horizontal movement
        Self.Move_By (Dx, 0);
        for I in 0 .. Restricted.Get_Size - 1 loop
            Other := Restricted.Get (I).all;
            if Self.Overlaps (Other) then
                Bounce_Status.Horizontal := True;
                Self.Move_By (-Dx, 0);
            end if;
            exit when Bounce_Status.Horizontal;
        end loop;

        --  vertical movement
        Self.Move_By (0, Dy);
        for I in 0 .. Restricted.Get_Size - 1 loop
            Other := Restricted.Get (I).all;
            if Self.Overlaps (Other) then
                Bounce_Status.Vertical := True;
                Self.Move_By (0, -Dy);
            end if;
            exit when Bounce_Status.Vertical;
        end loop;

        return Bounce_Status;
    end Move_And_Bounce;

    procedure Move_And_Bounce (Self : Rectangle_Ptr;
                              Dx, Dy : Integer;
                              Restricted : Rectangle_Vectors.Vector) is
        Other : Rectangle;
    begin

        --  horizontal movement
        Self.Move_By (Dx, 0);
        for I in 0 .. Restricted.Get_Size - 1 loop
            Other := Restricted.Get (I).all;
            if Self.Overlaps (Other) then
                Self.Move_By (-Dx, 0);
                goto End_Horizontal_Movement;
            end if;
        end loop;
    <<End_Horizontal_Movement>>

        --  vertical movement
        Self.Move_By (0, Dy);
        for I in 0 .. Restricted.Get_Size - 1 loop
            Other := Restricted.Get (I).all;
            if Self.Overlaps (Other) then
                Self.Move_By (0, -Dy);
                return;
            end if;
        end loop;
    end Move_And_Bounce;

end Rectangles;

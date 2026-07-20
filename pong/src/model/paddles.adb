with SDL.Video.Palettes;
with Constants;
package body Paddles is

    package Palettes renames SDL.Video.Palettes;

    procedure Initialize (Self : in out Paddle;
                          X, Y : Integer; ID : Player_ID) is
        Colour : Palettes.Colour;
    begin
        if ID = Player_1 then
            Colour := Constants.RED;
        else
            Colour := Constants.BLUE;
        end if;

        Self.Rectangle.Initialize (X => X, Y => Y,
                              Width => WIDTH, Height => HEIGHT,
                              Colour => Colour);
    end Initialize;

    procedure Increase_Score (Self : in out Paddle) is
    begin
        Self.Score := Self.Score + 1;
    end Increase_Score;

    function Get_Score (Self : Paddle) return Natural is
    begin
        return Self.Score;
    end Get_Score;

    function Get_Rectangle (Self : in out Paddle)
        return Rectangles.Rectangle_Ptr is
    begin
        return Self.Rectangle'Unchecked_Access;
    end Get_Rectangle;
end Paddles;

with Constants; use Constants;
with Ada.Numerics.Discrete_Random;
with Hit_Records; use Hit_Records;

package body Balls is

    procedure Initialize (Self : in out Ball) is
    begin
        Self.X_Velocity := 0;
        Self.Y_Velocity := 0;
        Self.Rectangle.Initialize (X => 0, Y => 0,
                         Width => SIZE, Height => SIZE,
                         Colour => WHITE);
    end Initialize;

    procedure Serve (Self : in out Ball) is
        subtype Random_Range is Integer range -2 .. 2;
        package R is new
        Ada.Numerics.Discrete_Random (Random_Range);
        use R;

        G : Generator;
        X : Random_Range;
    begin
        --  position: center of screen (accounting for size)
        Self.Rectangle.Move_To (X => (WIDTH - SIZE) / 2,
                                Y => (HEIGHT - SIZE) / 2);

        --  velocity: random
        Self.X_Velocity := 0;
        while Self.X_Velocity = 0 loop
            --  keep trying until we get some horizontal motion
            X := Random (G);
            Self.X_Velocity := Integer (X);
            X := Random (G);
            Self.Y_Velocity := Integer (X);
        end loop;
    end Serve;

    procedure Update (Self : in out Ball;
                      Restricted : Rectangles.Rectangle_Vectors.Vector) is
        Bounce_Status : Hit_Record;
        use Rectangles;
    begin
        Bounce_Status := Self.Rectangle.Move_And_Bounce
                             (Dx => Self.X_Velocity,
                              Dy => Self.Y_Velocity,
                              Restricted => Restricted);
        if Bounce_Status.Horizontal then
            Self.X_Velocity := -Self.X_Velocity;
        end if;
        if Bounce_Status.Vertical then
            Self.Y_Velocity := -Self.Y_Velocity;
        end if;
    end Update;

    function Get_Rectangle (Self : in out Ball)
        return Rectangles.Rectangle_Ptr is
    begin
        return Self.Rectangle'Unchecked_Access;
    end Get_Rectangle;
end Balls;

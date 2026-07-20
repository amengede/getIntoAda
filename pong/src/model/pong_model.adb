with Constants; use Constants;
with Ada.Text_IO; use Ada.Text_IO;

package body Pong_Model is

    procedure Initialize (Self : in out Model) is
        use Balls;
        use Paddles;
        use Rectangles;
    begin
        Self.Ball.Initialize;
        Self.Players (Player_1).Initialize (X => 100, Y => 300,
                                            ID => Player_1);
        Self.Players (Player_2).Initialize (X => 700, Y => 300,
                                            ID => Player_2);
        Self.Top_Boundary.Initialize (X => 0, Y => -10,
                                      Width => WIDTH, Height => 10,
                                      Colour => WHITE);
        Self.Bottom_Boundary.Initialize (X => 0, Y => HEIGHT + 10,
                                         Width => WIDTH, Height => 10,
                                         Colour => WHITE);
        Self.Left_Boundary.Initialize (X => -10, Y => 0,
                                       Width => 10, Height => HEIGHT,
                                       Colour => WHITE);
        Self.Right_Boundary.Initialize (X => WIDTH + 10, Y => 0,
                                        Width => 10, Height => HEIGHT,
                                        Colour => WHITE);

        Self.Visible.Push_Back
            (Self.Ball.Get_Rectangle);
        Self.Visible.Push_Back
            (Self.Players (Player_1).Get_Rectangle);
        Self.Visible.Push_Back
            (Self.Players (Player_2).Get_Rectangle);

        Self.Ball_Restricted.Push_Back
            (Self.Top_Boundary'Unchecked_Access);
        Self.Ball_Restricted.Push_Back
            (Self.Bottom_Boundary'Unchecked_Access);
        Self.Ball_Restricted.Push_Back
            (Self.Players (Player_1).Get_Rectangle);
        Self.Ball_Restricted.Push_Back
            (Self.Players (Player_2).Get_Rectangle);

        Self.Paddle_Restricted.Push_Back
            (Self.Top_Boundary'Unchecked_Access);
        Self.Paddle_Restricted.Push_Back
            (Self.Bottom_Boundary'Unchecked_Access);
        Self.Paddle_Restricted.Push_Back
            (Self.Ball.Get_Rectangle);

        Self.Start_Round;
    end Initialize;

    procedure Update (Self : in out Model) is
        use Paddles;
        Ball_Rect : constant Rectangles.Rectangle_Ptr
            := Self.Ball.Get_Rectangle;
    begin
        Self.Ball.Update (Restricted => Self.Ball_Restricted);
        if Ball_Rect.Overlaps (Self.Left_Boundary) then
            Self.Players (Player_2).Increase_Score;
            Self.Start_Round;
        elsif Ball_Rect.Overlaps (Self.Right_Boundary) then
            Self.Players (Player_1).Increase_Score;
            Self.Start_Round;
        end if;
    end Update;

    procedure Move_Player (Self : in out Model;
                           Player : Paddles.Player_ID;
                           Dy : Integer) is
        Rect : Rectangles.Rectangle_Ptr
            := Self.Players (Player).Get_Rectangle;
    begin
        Rectangles.Move_And_Bounce (Self => Rect,
                                    Dx => 0, Dy => Dy,
                                    Restricted => Self.Paddle_Restricted);
    end Move_Player;

    function Get_Visible_Set (Self : Model)
        return Rectangles.Rectangle_Vectors.Vector is
    begin
        return Self.Visible;
    end Get_Visible_Set;

    function Get_Scores (Self : in out Model) return Score_Array is
        use Paddles;
        Scores : constant Score_Array :=
            [Player_1 => Self.Players (Player_1).Get_Score,
            Player_2 => Self.Players (Player_2).Get_Score];
    begin
        return Scores;
    end Get_Scores;

    procedure Start_Round (Self : in out Model) is
    begin
        Self.Ball.Serve;
    end Start_Round;
end Pong_Model;

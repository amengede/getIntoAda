with Paddles;
package body Pong_View is

    procedure Update (Self : in out View;
                      Visible : Rectangles.Rectangle_Vectors.Vector;
                      Scores : Pong_Model.Score_Array;
                     Window : SDL.Video.Windows.Window) is
        use Paddles;
    begin
        Self.Clear_Screen;

        for I in 0 .. Visible.Get_Size - 1 loop
            Self.Draw_Rectangle (Visible.Get (I).all);
        end loop;

        Self.Draw_Text (Text => Scores (Player_1)'Image, X => 100, Y => 100);
        Self.Draw_Text (Text => Scores (Player_2)'Image, X => 700, Y => 100);

        Self.Finish_Drawing (Window);
    end Update;
end Pong_View;

package body Title_View is

    procedure Update (Self : in out View;
                     Window : SDL.Video.Windows.Window) is
    begin
        Self.Clear_Screen;
        Self.Draw_Text (Text => "Pong", X => 300, Y => 200);
        Self.Draw_Text (Text => "Press any key to begin", X => 200, Y => 300);
        Self.Finish_Drawing (Window);
    end Update;
end Title_View;

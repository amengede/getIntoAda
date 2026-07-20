with Title_Controller;
with Pong_Controller;
with SDL.Video.Windows;
with SDL.TTFs;
with SDL.Video.Windows.Makers;
with Constants; use Constants;

procedure Pong is
    Window : SDL.Video.Windows.Window;
    Controller_1 : Title_Controller.Controller;
    Controller_2 : Pong_Controller.Controller;
begin

    if not SDL.Initialise (Flags => SDL.Enable_Screen) then
        return;
    end if;

    if not SDL.TTFs.Initialise then
        return;
    end if;

    SDL.Video.Windows.Makers.Create
        (Win => Window,
        Title => "Pong",
        Position => SDL.Natural_Coordinates'(X => 10, Y => 10),
        Size => SDL.Positive_Sizes'(WIDTH, HEIGHT),
        Flags => 0);

    Controller_1.Initialize (Window);
    Controller_1.Start (Window);

    Controller_2.Initialize (Window);
    Controller_2.Start (Window);

end Pong;

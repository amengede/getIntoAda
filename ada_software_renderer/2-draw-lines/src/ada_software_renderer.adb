with SDL.Video.Windows.Makers;
with SDL.Events.Events;
with SDL.Timers;
with Constants;
with Renderer;
with Interfaces;
with Maths;

procedure Ada_Software_Renderer is

    use Constants;
    use Maths;
    use Interfaces;

    -- Is the program running?
    Running : Boolean := True;

    -- Main Window for drawing and event detection
    Window : SDL.Video.Windows.Window;
    -- For handling events as they are polled
    Event : SDL.Events.Events.Events;
    -- Color with which to clear the screen
    Color : Unsigned_32;
    -- Time in milliseconds at frame commencement
    Time_Previous : SDL.Timers.Milliseconds;
    -- Time in milliseconds after frame logic
    Time_Current : SDL.Timers.Milliseconds;
    Frametime : Integer;
    Frame_Count : Integer := 0;
    -- Framerate of program
    Framerate : Integer;

    use type SDL.Events.Event_Types;

begin
   
    if not SDL.Initialise (Flags => SDL.Enable_Screen) then
        return;
    end if;

    SDL.Video.Windows.Makers.Create
        (Win => Window,
        Title => "Test Window",
        Position => SDL.Natural_Coordinates'(X => 10, Y => 10),
        Size => SDL.Positive_Sizes'(Width, Height),
        Flags => 2); 
    Renderer.Initialize (Window);

    Time_Previous := SDL.Timers.Ticks;

    while Running loop

        -- Event Handling
        while SDL.Events.Events.Poll (Event) loop
            if Event.Common.Event_Type = SDL.Events.Quit then
                Running := False;
            end if;
        end loop;

        -- Rendering
        Renderer.Start_Drawing;
        Color := Renderer.Map_RGBA(R => 64, G => 64, B => 64, A => 255);
        Renderer.Clear_Screen (Color);
        -- Red: Horizontal Lines
        Color := Renderer.Map_RGBA(R => 255, G => 0, B => 0, A => 255);
        Renderer.Draw_Line ([10, 200], [510, 200], Color);
        Renderer.Draw_Line ([510, 100], [10, 100], Color);
        -- Green: Vertical Lines
        Color := Renderer.Map_RGBA(R => 0, G => 255, B => 0, A => 255);
        Renderer.Draw_Line ([200, 10], [200, 410], Color);
        Renderer.Draw_Line ([100, 410], [100, 10], Color);
        -- Blue: Shallow Lines
        Color := Renderer.Map_RGBA(R => 0, G => 0, B => 255, A => 255);
        Renderer.Draw_Line ([20, 300], [600, 400], Color);
        Renderer.Draw_Line ([600, 350], [20, 250], Color);
        Renderer.Draw_Line ([20, 400], [600, 300], Color);
        Renderer.Draw_Line ([600, 250], [20, 350], Color);
        -- Purple: Steep Lines
        Color := Renderer.Map_RGBA(R => 192, G => 0, B => 255, A => 255);
        Renderer.Draw_Line ([420, 30], [520, 400], Color);
        Renderer.Draw_Line ([480, 400], [380, 30], Color);
        Renderer.Draw_Line ([520, 30], [420, 400], Color);
        Renderer.Draw_Line ([380, 400], [480, 30], Color);
        Renderer.End_Drawing;

        Window.Update_Surface;

        -- Framerate Calculation
        Time_Current := SDL.Timers.Ticks;
        Frametime := Integer(Time_Current) - Integer(Time_Previous);
        Frame_Count := Frame_Count + 1;
        if (Frametime > 1000) then
            Framerate := Frame_Count * 1000 / Frametime;
            Window.Set_Title (Framerate'Image);
            Time_Previous := Time_Current;
            Frame_Count := 0;
        end if;
    end loop;

    Renderer.Finalize;
    Window.Finalize;
    SDL.Finalise;
end Ada_Software_Renderer;

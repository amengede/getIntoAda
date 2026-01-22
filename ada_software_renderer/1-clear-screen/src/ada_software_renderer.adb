with SDL.Video.Windows.Makers;
with SDL.Events.Events;
with SDL.Timers;
with Constants;
with Renderer;
with Interfaces;

procedure Ada_Software_Renderer is

    use Constants;
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
        Color := Renderer.Map_RGBA(R => 32, G => 64, B => 128, A => 255);
        -- Counter_Start := Integer_64 (SDL.Timers.Performance.Get_Counter);
        Renderer.Clear_Screen_Concurrent (Color);
        -- Counter_End := Integer_64 (SDL.Timers.Performance.Get_Counter) - Counter_Start;
        -- Put_Line ("Clear Screen: " & Counter_End'Image);
        Renderer.End_Drawing;

        Window.Update_Surface;

        -- Framerate Calculation
        Time_Current := SDL.Timers.Ticks;
        Frametime := Integer (Time_Current) - Integer (Time_Previous);
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

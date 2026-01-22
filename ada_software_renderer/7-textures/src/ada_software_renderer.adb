with SDL.Video.Windows.Makers;
with SDL.Events.Events;
with SDL.Timers;
with Constants;
with Renderer;
with Interfaces;
with Maths;
with Material_Manager;
with Threading;

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

    Vertices : constant Vertex_Buffer :=
        [Vertex' ([-1.0, -1.0, 0.0, 0.0], [1.0, 0.0, 0.0, 0.0], [0.0, 1.0]),
        Vertex' ([1.0, -1.0, 0.0, 0.0], [0.0, 1.0, 0.0, 0.0], [1.0, 1.0]),
        Vertex' ([0.0, 1.0, 0.0, 0.0], [0.0, 1.0, 1.0, 0.0], [0.5, 0.0])];

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
    Threading.Initialize;
    Material_Manager.Initialize;
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
        Color := Renderer.Map_RGBA(R => 64,
                                   G => 64, B => 64, A => 255);
        Renderer.Clear_Screen (Color);
        -- Make a Triangle!
        Renderer.Draw_Triangles (Vertices,
                                 First_Vertex => 0,
                                 Triangle_Count => 1);
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
    Threading.Finalize;
    Material_Manager.Finalize;
    Window.Finalize;
    SDL.Finalise;
end Ada_Software_Renderer;

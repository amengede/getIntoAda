with SDL.Video.Windows.Makers;
with SDL.Events.Events;
with SDL.Timers;
with Constants; use Constants;
with Renderer;
with GL;
with SDL.Video.Windows;
with SDL.Video.GL;

procedure Opengl_Renderer is
   -- Is the program running?
   Running : Boolean := True;

   -- Main Window for drawing and event detection
   Window : SDL.Video.Windows.Window;
   -- For handling events as they are polled
   Event : SDL.Events.Events.Events;
   -- Time in milliseconds at frame commencement
   Time_Previous : SDL.Timers.Milliseconds;
   -- Time in milliseconds after frame logic
   Time_Current : SDL.Timers.Milliseconds;
   Frametime : Integer;
   Frame_Count : Integer := 0;
   Framerate : Integer;

   use type SDL.Events.Event_Types;

   GL_Context : SDL.Video.GL.Contexts;
begin
   if not SDL.Initialise then
      return;
   end if;

   SDL.Video.GL.Set_Core_Context_Profile (4, 6);

   SDL.Video.Windows.Makers.Create
      (Win => Window,
       Title => "Test Window",
       Position => SDL.Natural_Coordinates'(X => 10, Y => 10),
       Size => SDL.Positive_Sizes'(Width, Height),
       Flags => SDL.Video.Windows.OpenGL);
   SDL.Video.GL.Create (GL_Context, Window);
   SDL.Video.GL.Set_Current (GL_Context, Window);

   GL.Init;

   Renderer.Initialize;

   Time_Previous := SDL.Timers.Ticks;

   while Running loop

      -- Event Handling
      while SDL.Events.Events.Poll (Event) loop
         if Event.Common.Event_Type = SDL.Events.Quit then
            Running := False;
         end if;
      end loop;

      -- Render
      Renderer.Clear_Screen;

      -- Present
      SDL.Video.GL.Swap (Window);

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

   Window.Finalize;
   SDL.Finalise;
end Opengl_Renderer;

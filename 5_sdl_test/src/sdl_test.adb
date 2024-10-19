with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Events.Events;

procedure Sdl_Test is

   -- pragma Linker_Options("-lSDL2");

   Screen_Width   : constant := 640;
   Screen_Height  : constant := 480;
   Running        : Boolean  := True;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Event    : SDL.Events.Events.Events;

   use type SDL.Events.Event_Types;

begin
   
   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      return;
   end if;

   SDL.Video.Windows.Makers.Create (Win      => Window,
                                    Title    => "SDL !!!111!!!",
                                    Position => SDL.Natural_Coordinates'(X => 10, Y => 10),
                                    Size     => SDL.Positive_Sizes'(Screen_Width, Screen_Height),
                                    Flags    => 0);
   SDL.Video.Renderers.Makers.Create (Renderer, Window.Get_Surface);
   Renderer.Set_Draw_Colour ((64, 16, 48, 255));

   -- Main loop
   while Running loop

      -- Events
      while SDL.Events.Events.Poll (Event) loop
         if Event.Common.Event_Type = SDL.Events.Quit then
            Running := False;
         end if;
      end loop;

      -- Rendering
      Renderer.Fill (Rectangle => (0, 0, Screen_Width, Screen_Height));
      Window.Update_Surface;
   end loop;

   Window.Finalize;
   SDL.Finalise;

end Sdl_Test;

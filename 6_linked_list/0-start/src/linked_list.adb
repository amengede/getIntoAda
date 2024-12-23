with SDL.Video.Windows.Makers;
with SDL.Video.Rectangles;
with SDL.Video.Renderers.Makers;
with SDL.Events.Events;

procedure Linked_List is

   Width   : constant := 800;
   Height  : constant := 800;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Event    : SDL.Events.Events.Events;
   Running  : Boolean := True;

   use type SDL.Events.Event_Types;

   procedure Render is
   begin
      Renderer.Set_Draw_Colour ((0, 0, 0, 255));
      Renderer.Fill
         (Rectangle => SDL.Video.Rectangles.Rectangle'(0, 0, Width, Height));
   end Render;

begin
   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      return;
   end if;

   SDL.Video.Windows.Makers.Create
     (Win      => Window,
      Title    => "Linked List",
      Position => SDL.Natural_Coordinates'(X => 10, Y => 10),
      Size     => SDL.Positive_Sizes'(Width, Height),
      Flags    => 0);
   SDL.Video.Renderers.Makers.Create (Renderer, Window.Get_Surface);

   while Running loop

      while SDL.Events.Events.Poll (Event) loop
         if Event.Common.Event_Type = SDL.Events.Quit then
            Running := False;
         end if;
      end loop;

      Render;
      Window.Update_Surface;

   end loop;

   Window.Finalize;
   SDL.Finalise;
end Linked_List;
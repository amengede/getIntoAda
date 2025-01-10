--  Dependencies
with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.TTFs.Makers;
with Types; use Types;

package body Renderer is

   function Initialize return Boolean is
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      if not TTFs.Initialise then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create
      (Win      => Window,
         Title    => "Linked List",
         Position => SDL.Natural_Coordinates'(X => 10, Y => 10),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => 0);
      Renderers.Makers.Create (Renderer, Window.Get_Surface);

      TTFs.Makers.Create (Font, File_Name => "font.ttf", Point_Size => 24);

      return True;

   end Initialize;

   procedure Clear_Screen is
   begin
      Renderer.Set_Draw_Colour ((0, 0, 0, 255));
      Renderer.Fill
         (Rectangle => Rectangles.Rectangle'(0, 0, Width, Height));
   end Clear_Screen;

   procedure Render (Head : Person_List.Link) is

      --  Use Types
      use type SDL.C.int;

      --  Variables
      Node_Width : constant := 128;
      Draw_X : SDL.C.int;
      Box_X : constant := 20;
      Box_Y : constant := 200;
      Box_Width : constant := 64;
      Box_Height : constant := 48;
      Arrow_X : constant := Box_X + Box_Width;
      Arrow_Length : constant := Node_Width - Box_Width;
      Arrow_Head_Size : constant := 20;
      Arrow_Y : constant := Box_Y + Box_Height / 2;
      Node : Person_List.Link := Head;
      Node_Count : constant Integer := Person_List.Length (Head);
      Text_Surface : Surfaces.Surface;
      Text_Texture : Textures.Texture;
   begin
      Renderer.Set_Draw_Colour ((0, 128, 0, 255));
      Draw_X := 0;
      for i in 1 .. Node_Count loop
         Draw_X := Draw_X + 128;
         Renderer.Draw (Rectangle => Rectangles.Rectangle'(
            Box_X + Draw_X, Box_Y, Box_Width, Box_Height));
         Renderer.Draw (Line => Rectangles.Line_Segment'(
            SDL.Coordinates'(Arrow_X + Draw_X, Arrow_Y),
            SDL.Coordinates'(Arrow_X + Arrow_Length + Draw_X, Arrow_Y)));
         Renderer.Draw (Line => Rectangles.Line_Segment'(
            SDL.Coordinates'(
               Arrow_X + Arrow_Length - Arrow_Head_Size + Draw_X,
               Arrow_Y + Arrow_Head_Size),
            SDL.Coordinates'(Arrow_X + Arrow_Length + Draw_X, Arrow_Y)));
         Renderer.Draw (Line => Rectangles.Line_Segment'(
            SDL.Coordinates'(
               Arrow_X + Arrow_Length - Arrow_Head_Size + Draw_X,
               Arrow_Y - Arrow_Head_Size),
            SDL.Coordinates'(Arrow_X + Arrow_Length + Draw_X, Arrow_Y)));

         Text_Surface := TTFs.Render_Solid
              (Font,
               Text   => Image (Node.Value),
               Colour => (225, 0, 0, 255));

         Textures.Makers.Create (Text_Texture, Renderer, Text_Surface);
         Renderers.Copy (Renderer, Text_Texture, To => (
            X => 20.0 + 128.0 * Float (i),
            Y => 204.0,
            Width  => 64.0,
            Height => 48.0));
         Node := Node.Next;
      end loop;
   end Render;

   procedure Update is
   begin
      Window.Update_Surface;
   end Update;

   procedure Finalize is
   begin
      Window.Finalize;
      SDL.Finalise;
   end Finalize;
end Renderer;
--  Dependencies
with SDL.Video.Windows.Makers;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures.Makers;
with SDL.Video.Textures;
with SDL.Events.Events;
with SDL.TTFs.Makers;
with List;
with Types; use Types;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Linked_List is

   --  Rename dependencies for quality of life
   package Rectangles renames SDL.Video.Rectangles;
   package Renderers renames SDL.Video.Renderers;
   package Surfaces renames SDL.Video.Surfaces;
   package Textures renames SDL.Video.Textures;
   package Events renames SDL.Events.Events;
   package TTFs renames SDL.TTFs;

   --  Instantiate generic linked list data structure
   package Person_List is new List (T => Person, Key => Key);

   --  Use Types
   use type SDL.Events.Event_Types;

   --  Variables
   Width   : constant := 800;
   Height  : constant := 800;
   Window   : SDL.Video.Windows.Window;
   Renderer : Renderers.Renderer;
   Event    : Events.Events;
   Running  : Boolean := True;
   Font : TTFs.Fonts;
   My_List  : Person_List.Link := null;
   
   --  Helpers
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

      --  Clear Screen
      Renderer.Set_Draw_Colour ((0, 0, 0, 255));
      Renderer.Fill
         (Rectangle => Rectangles.Rectangle'(0, 0, Width, Height));

      --  Draw Nodes
      Renderer.Set_Draw_Colour ((0, 128, 0, 255));
      Draw_X := 0;
      for i in 1 .. Node_Count loop
         Draw_X := Draw_X + 128;
         Renderer.Draw (Rectangle => Rectangles.Rectangle'(
            Box_X + Draw_X, Box_Y, Box_Width, Box_Height));
         Renderer.Draw (Line => Rectangles.Line_Segment' (
            SDL.Coordinates' (Arrow_X + Draw_X, Arrow_Y), 
            SDL.Coordinates' (Arrow_X + Arrow_Length + Draw_X, Arrow_Y)));
         Renderer.Draw (Line => Rectangles.Line_Segment' (
            SDL.Coordinates' (Arrow_X + Arrow_Length - Arrow_Head_Size + Draw_X, Arrow_Y + Arrow_Head_Size), 
            SDL.Coordinates' (Arrow_X + Arrow_Length + Draw_X, Arrow_Y)));
         Renderer.Draw (Line => Rectangles.Line_Segment' (
            SDL.Coordinates' (Arrow_X + Arrow_Length - Arrow_Head_Size + Draw_X, Arrow_Y - Arrow_Head_Size), 
            SDL.Coordinates' (Arrow_X + Arrow_Length + Draw_X, Arrow_Y)));
         
         Text_Surface := TTFs.Render_Solid
              (Font,
               Text   => Image (Node.Value),
               Colour => (225, 0, 0, 255));

         Textures.Makers.Create (Text_Texture, Renderer, Text_Surface);
         Renderers.Copy (Renderer, Text_Texture, To => (X => 20.0 + 128.0 * Float (i),
                                                         Y => 204.0,
                                                         Width  => 64.0,
                                                         Height => 48.0));
         Node := Node.Next;
      end loop;

      
   end Render;

   procedure Print (Head : Person_List.Link) is
      Current_Node : Person_List.Link := Head;
      Node_Count : constant Integer := Person_List.Length (Head);
   begin
      Put_Line ("List length:" & Node_Count'Image);

      for i in 1 .. Node_Count loop
         Put (Image (Current_Node.Value) & ", ");
         Current_Node := Current_Node.Next;
      end loop;
      Put_Line ("");

   end Print;

begin

   --  Initialize data
   My_List := null;
   Print (My_List);
   Person_List.Insert (My_List, Person'(12, To_Unbounded_String ("A")));
   Print (My_List);
   Person_List.Insert (My_List, Person'(3, To_Unbounded_String ("B")));
   Print (My_List);
   Person_List.Insert (My_List, Person'(45, To_Unbounded_String ("C")));
   Print (My_List);
   Person_List.Insert (My_List, Person'(2, To_Unbounded_String ("D")));
   Print (My_List);

   Person_List.Remove (My_List);
   Print (My_List);

   Person_List.Insert (My_List, Person'(20, To_Unbounded_String ("E")));
   Print (My_List);

   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      return;
   end if;

   if not TTFs.Initialise then
      return;
   end if;

   SDL.Video.Windows.Makers.Create
     (Win      => Window,
      Title    => "Linked List",
      Position => SDL.Natural_Coordinates'(X => 10, Y => 10),
      Size     => SDL.Positive_Sizes'(Width, Height),
      Flags    => 0);
   Renderers.Makers.Create (Renderer, Window.Get_Surface);

   TTFs.Makers.Create (Font, File_Name => "font.ttf", Point_Size => 24);

   while Running loop

      while Events.Poll (Event) loop
         if Event.Common.Event_Type = SDL.Events.Quit then
            Running := False;
         end if;
      end loop;

      Render (My_List);
      Window.Update_Surface;

   end loop;

   Window.Finalize;
   SDL.Finalise;
end Linked_List;
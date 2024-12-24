with SDL.Video.Windows.Makers;
with SDL.Video.Rectangles;
with SDL.Video.Renderers.Makers;
with SDL.Events.Events;

with List;

with Types; use Types;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Linked_List is

   package Person_List is new List (T => Person, Key => Key);

   Width   : constant := 800;
   Height  : constant := 800;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Event    : SDL.Events.Events.Events;
   Running  : Boolean := True;

   My_List  : Person_List.Link := null;

   use type SDL.Events.Event_Types;

   procedure Render is
   begin
      Renderer.Set_Draw_Colour ((0, 0, 0, 255));
      Renderer.Fill
         (Rectangle => SDL.Video.Rectangles.Rectangle'(0, 0, Width, Height));
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
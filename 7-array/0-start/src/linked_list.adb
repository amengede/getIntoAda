--  Dependencies
with Renderer;
with SDL.Events.Events;
with Common; use Common;
with Types; use Types;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Linked_List is

   --  Rename dependencies for quality of life
   package Events renames SDL.Events.Events;

   --  Use Types
   use type SDL.Events.Event_Types;

   --  Variables
   Event    : Events.Events;
   Running  : Boolean := True;
   My_List  : Person_List.Link := null;

   --  Helpers
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

   if not Renderer.Initialize then
      return;
   end if;

   while Running loop

      while Events.Poll (Event) loop
         if Event.Common.Event_Type = SDL.Events.Quit then
            Running := False;
         end if;
      end loop;

      Renderer.Clear_Screen;
      Renderer.Render (My_List);
      Renderer.Update;

   end loop;

   Renderer.Finalize;
end Linked_List;
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
   My_Array : Person_Vectors.Vector;
   My_Hashmap : Person_Hashmaps.Hash_Map;
   Person_A : constant Person := Person'(12, To_Unbounded_String ("A"));
   Person_B : constant Person := Person'(3, To_Unbounded_String ("B"));
   Person_C : constant Person := Person'(45, To_Unbounded_String ("C"));
   Person_D : constant Person := Person'(2, To_Unbounded_String ("D"));
   Person_E : constant Person := Person'(20, To_Unbounded_String ("E"));

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

   procedure Print (Arr : in out Person_Vectors.Vector) is
   begin

      Put_Line ("Capacity:" & Arr.Get_Capacity'Image);

      Put ("[");
      for I in 0 .. Arr.Get_Size - 1 loop
         Put (To_String (Arr.Get (I).Name) & ", ");
      end loop;
      Put_Line ("]");

   end Print;

begin

   --  Initialize data
   My_List := null;
   Print (My_List);
   Person_List.Insert (My_List, Person_A);
   Print (My_List);
   Person_List.Insert (My_List, Person_B);
   Print (My_List);
   Person_List.Insert (My_List, Person_C);
   Print (My_List);
   Person_List.Insert (My_List, Person_D);
   Print (My_List);

   Person_List.Remove (My_List);
   Print (My_List);

   Person_List.Insert (My_List, Person_E);
   Print (My_List);

   Print (My_Array);
   My_Array.Push_Back (Person_A);
   Print (My_Array);
   My_Array.Push_Back (Person_B);
   Print (My_Array);
   My_Array.Push_Back (Person_C);
   Print (My_Array);
   My_Array.Push_Back (Person_D);
   Print (My_Array);
   My_Array.Push_Back (Person_E);
   Print (My_Array);

   My_Hashmap.Print;
   My_Hashmap.Insert (Person_A.Name, Person_A);
   My_Hashmap.Print;
   My_Hashmap.Insert (Person_B.Name, Person_B);
   My_Hashmap.Print;
   My_Hashmap.Insert (Person_C.Name, Person_C);
   My_Hashmap.Print;
   My_Hashmap.Insert (Person_D.Name, Person_D);
   My_Hashmap.Print;
   My_Hashmap.Insert (Person_E.Name, Person_E);
   My_Hashmap.Print;
   My_Hashmap.Remove (Person_E.Name);
   My_Hashmap.Print;

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

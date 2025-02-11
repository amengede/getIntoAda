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
   My_Array : Person_Array.MyArray_Access := null;
   My_Hashmap : Person_Hashmap.HashMap_Access := null;
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

   procedure Print (Arr : Person_Array.MyArray_Access) is
   begin

      Put_Line ("Capacity:" & Arr.Capacity'Image);

      Put ("[");
      for i in 0 .. Arr.Capacity - 1 loop
         Put (To_String (Person_Array.Get (Arr, i).Name) & ", ");
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

   My_Array := new Person_Array.MyArray;
   Print (My_Array);
   Person_Array.Insert (My_Array, 12, Person_A);
   Print (My_Array);
   Person_Array.Insert (My_Array, 3, Person_B);
   Print (My_Array);
   Person_Array.Insert (My_Array, 45, Person_C);
   Print (My_Array);
   Person_Array.Insert (My_Array, 2, Person_D);
   Print (My_Array);
   Person_Array.Insert (My_Array, 20, Person_E);
   Print (My_Array);

   My_Hashmap := new Person_Hashmap.HashMap;
   Person_Hashmap.Print (My_Hashmap);
   Person_Hashmap.Insert (My_Hashmap, Person_A);
   Person_Hashmap.Print (My_Hashmap);
   Person_Hashmap.Insert (My_Hashmap, Person_B);
   Person_Hashmap.Print (My_Hashmap);
   Person_Hashmap.Insert (My_Hashmap, Person_C);
   Person_Hashmap.Print (My_Hashmap);
   Person_Hashmap.Insert (My_Hashmap, Person_D);
   Person_Hashmap.Print (My_Hashmap);
   Person_Hashmap.Insert (My_Hashmap, Person_E);
   Person_Hashmap.Print (My_Hashmap);
   Person_Hashmap.Remove (My_Hashmap, 20);
   Person_Hashmap.Print (My_Hashmap);

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
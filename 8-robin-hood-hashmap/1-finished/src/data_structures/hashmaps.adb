with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body HashMaps is

   procedure Insert (
      Target : Elements_Access;
      Capacity : Uint32; New_Data : T) is
      Orphan : Element;
      New_Element : Element := Element'(
         Probe_Length => 0,
         Data => New_Data,
         Free => False);
      I : Uint32 := 0;
      Insert_Pos : Uint32 := Key (New_Data) and (Capacity - 1);
   begin

      while I < Capacity loop
         exit when Target (Insert_Pos).Free;
         if Target (Insert_Pos).Probe_Length < New_Element.Probe_Length then
            Orphan := Target (Insert_Pos);
            Target (Insert_Pos) := New_Element;
            New_Element := Orphan;
         end if;

         New_Element.Probe_Length := New_Element.Probe_Length + 1;

         Insert_Pos := (Insert_Pos + 1);
         if Insert_Pos = Capacity then
            Insert_Pos := 0;
         end if;
         I := I + 1;
      end loop;

      Target (Insert_Pos) := New_Element;
   end Insert;

   procedure Free is new Ada.Unchecked_Deallocation
      (Object => Elements_Type, Name => Elements_Access);

   procedure ReHash (Target : HashMap_Access) is
      New_Elements : constant Elements_Access :=
         new Elements_Type (0 .. 2 * Target.Capacity);
      Old_Elements : Elements_Access := Target.Elements;
   begin

      --  Attempt to re-insert elements
      for i in 0 .. Target.Capacity - 1 loop
         Insert (New_Elements, Target.Capacity * 2, Old_Elements (i).Data);
      end loop;
      Free (Old_Elements);
      Target.Elements := New_Elements;
      Target.Capacity := Target.Capacity * 2;
   end ReHash;

   procedure Insert (Target : HashMap_Access; Data : T) is
   begin

      if Target.Size = Target.Capacity then
         ReHash (Target);
      end if;

      Insert (Target.Elements, Target.Capacity, Data);

      Target.Size := Target.Size + 1;

   end Insert;

   function Has (
      Target : HashMap_Access;
      Element_Key : Uint32) return Boolean is

      Search_Position : Uint32 := Element_Key and (Target.Capacity - 1);
      Current : Element;
      I : Uint32 := 0;
   begin
         while I < Target.Capacity loop
            Current := Target.Elements (Search_Position);

            if Key (Current.Data) = Element_Key then
               return True;
            end if;

            exit when Current.Probe_Length < I;

            I := I + 1;
            Search_Position := Search_Position + 1;
            if Search_Position = Target.Capacity then
               Search_Position := 0;
            end if;
         end loop;
         return False;
   end Has;

   function Get (Target : HashMap_Access; Element_Key : Uint32) return T is
   begin
      return Target.Elements (Element_Key).Data;
   end Get;

   procedure Remove (Target : HashMap_Access; Element_Key : Uint32) is
      Deletion_Pos : Uint32 := Element_Key and (Target.Capacity - 1);
      Inspection_Pos : Uint32;
   begin

      for I in 0 .. Target.Capacity loop
         exit when Key (Target.Elements (Deletion_Pos).Data) = Element_Key;

         Deletion_Pos := Deletion_Pos + 1;
         if Deletion_Pos = Target.Capacity then
            Deletion_Pos := 0;
         end if;

      end loop;

      for I in 0 .. Target.Capacity - 1 loop
         Target.Elements (Deletion_Pos).Free := True;

         Inspection_Pos := Deletion_Pos + 1;
         if Inspection_Pos = Target.Capacity then
            Inspection_Pos := 0;
         end if;

         exit when Target.Elements (Inspection_Pos).Probe_Length = 0;
         Target.Elements (Deletion_Pos) :=
            Target.Elements (Inspection_Pos);
         Target.Elements (Deletion_Pos).Probe_Length :=
            Target.Elements (Deletion_Pos).Probe_Length - 1;

         Deletion_Pos := Deletion_Pos + 1;
         if Deletion_Pos = Target.Capacity then
            Deletion_Pos := 0;
         end if;

      end loop;

      Target.Size := Target.Size - 1;
   end Remove;

   procedure Print (Target : HashMap_Access) is
   begin
      Put_Line ("Size: " & Target.Size'Image);
      Put_Line ("Capacity: " & Target.Capacity'Image);

      Put ("[");
      for I in 0 .. Target.Capacity - 1 loop
         if Target.Elements (I).Free then
            Put ("-");
         else
            Put (Key (Target.Elements (I).Data)'Image);
            Put ("(" & Target.Elements (I).Probe_Length'Image & ")");
         end if;
         Put (", ");
      end loop;

      Put_Line ("]");
   end Print;
end HashMaps;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body HashMaps is

   procedure Insert (
      Data : Elements_Ptr;
      Capacity : Natural;
      New_Key : K;
      New_Value : V) is
      Orphan : Element;
      New_Element : Element :=
            Element'(Probe_Length => 0,
                     Key => New_Key,
                     Value => New_Value,
                     Free => False);
      Insert_Pos : Natural := Hash (New_Key) mod Capacity;
   begin

      for I in 0..Capacity - 1 loop
         exit when Data (Insert_Pos).Free or Data (Insert_Pos).Key = New_Key;
         if Data (Insert_Pos).Probe_Length < New_Element.Probe_Length then
            Orphan := Data (Insert_Pos);
            Data (Insert_Pos) := New_Element;
            New_Element := Orphan;
         end if;

         New_Element.Probe_Length := New_Element.Probe_Length + 1;

         Insert_Pos := (Insert_Pos + 1) mod Capacity;
      end loop;

      Data (Insert_Pos) := New_Element;
   end Insert;

   procedure Free is new Ada.Unchecked_Deallocation
                               (Object => Elements_Type,
                                Name => Elements_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation
                               (Object => Internal_Hash_Map,
                                Name => Internal_HashMap_Ptr);

   procedure Re_Hash (Self : in out Hash_Map) is
      New_Elements : constant Elements_Ptr :=
         new Elements_Type (0 .. 2 * Self.Data.Capacity - 1);
      Old_Elements : Elements_Ptr := Self.Data.Elements;
      Old_Capacity : Natural := Self.Data.Capacity;
   begin

      --  Attempt to re-insert elements
      for I in 0 .. Old_Capacity - 1 loop
            Insert (Data => New_Elements,
                    Capacity => Old_Capacity * 2,
                    New_Key => Old_Elements (I).Key,
                    New_Value => Old_Elements (I).Value);
      end loop;
      Free (Old_Elements);
      Self.Data.Elements := New_Elements;
      Self.Data.Capacity := Old_Capacity * 2;
   end Re_Hash;

   procedure Insert (Self : in out Hash_Map; Key : K; Value : V) is
      Load_Factor : Float;
   begin

      if Self.Data = null then
            Self.Data := new Internal_Hash_Map;
      end if;

      Load_Factor := Float(Self.Data.Size) / Float(Self.Data.Capacity);

      if Load_Factor > 0.9 then
         Self.Re_Hash;
      end if;

      Insert (Self.Data.Elements, Self.Data.Capacity, Key, Value);

      Self.Data.Size := Self.Data.Size + 1;

   end Insert;

   function Has (
      Self : in out Hash_Map;
      Key : K) return Boolean is

      Search_Position : Natural;
      Current : Element;
   begin

         if Self.Data = null then
               return False;
         end if;

         Search_Position := Hash(Key) mod Self.Data.Capacity;
         for I in 0..Self.Data.Capacity - 1 loop
            Current := Self.Data.Elements (Search_Position);

            if Current.Key = Key then
               return True;
            end if;

            exit when Current.Probe_Length < I;

            Search_Position := (Search_Position + 1) mod Self.Data.Capacity;
         end loop;
         return False;
   end Has;

   function Get (Self : in out Hash_Map; Key : K) return V is
         Search_Position : Natural;
         Current : Element;
   begin
         if Self.Data = null then
               raise Key_Error with "Accessing empty Hashmap.";
         end if;

         Search_Position := Hash(Key) mod Self.Data.Capacity;
         for I in 0..Self.Data.Capacity - 1 loop
               Current := Self.Data.Elements (Search_Position);

               if Current.Key = Key then
                     return Current.Value;
               end if;

               exit when Current.Probe_Length < I;

               Search_Position := (Search_Position + 1)
                                  mod Self.Data.Capacity;
         end loop;
         raise Key_Error with "Key not present in Hashmap.";
   end Get;

   procedure Remove (Self : in out Hash_Map; Key : K) is
      Deletion_Pos : Natural;
      Inspection_Pos : Natural;
      Found : Boolean := False;
   begin

         if Self.Data = null then
               raise Key_Error with "Attempting to remove from empty Hashmap.";
         end if;

         Deletion_Pos := Hash(Key) mod Self.Data.Capacity;

         for I in 0 .. Self.Data.Capacity - 1 loop
               Found := Self.Data.Elements (Deletion_Pos).Key = Key;
               exit when Found;

               Deletion_Pos := (Deletion_Pos + 1) mod Self.Data.Capacity;
         end loop;

         if not Found then
               raise Key_Error with "Deleting non-existant element";
         end if;

         for I in 0 .. Self.Data.Capacity - 1 loop
               Self.Data.Elements (Deletion_Pos).Free := True;

               Inspection_Pos := (Deletion_Pos + 1) mod Self.Data.Capacity;
               exit when Self.Data.Elements (Inspection_Pos).Probe_Length = 0;

               Self.Data.Elements (Deletion_Pos) :=
                     Self.Data.Elements (Inspection_Pos);
               Self.Data.Elements (Deletion_Pos).Probe_Length :=
                     Self.Data.Elements (Deletion_Pos).Probe_Length - 1;

               Deletion_Pos := (Deletion_Pos + 1) mod Self.Data.Capacity;
         end loop;

         Self.Data.Size := Self.Data.Size - 1;
   end Remove;

   procedure Print (Self : in out Hash_Map) is
   begin
         if Self.Data = null then
               Put_Line ("Empty");
               return;
         end if;

         Put_Line ("Size: " & Self.Data.Size'Image);
         Put_Line ("Capacity: " & Self.Data.Capacity'Image);

      Put ("[");
      for I in 0 .. Self.Data.Capacity - 1 loop
         if Self.Data.Elements (I).Free then
            Put ("-");
         else
            Put (Self.Data.Elements (I).Key'Image);
            Put ("(" & Self.Data.Elements (I).Probe_Length'Image & ")");
         end if;
         Put (", ");
      end loop;

      Put_Line ("]");
   end Print;

   procedure Adjust (Self : in out Hash_Map) is
   begin
         if Self.Data = null then
               return;
         end if;

         Self.Data.Reference_Count := Self.Data.Reference_Count + 1;
   end Adjust;

   procedure Finalize (Self : in out Hash_Map) is
   begin
         if Self.Data = null then
               return;
         end if;

         Self.Data.Reference_Count := Self.Data.Reference_Count - 1;

         if Self.Data.Reference_Count = 0 then
               Free (Self.Data.Elements);
               Free (Self.Data);
               Self.Data := null;
         end if;
   end Finalize;
end HashMaps;

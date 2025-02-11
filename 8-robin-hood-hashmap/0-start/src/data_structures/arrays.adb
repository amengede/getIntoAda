with Ada.Unchecked_Deallocation;

package body Arrays is

   procedure Free is new Ada.Unchecked_Deallocation
      (Object => Elements_Type, Name => Elements_Access);

   procedure Resize (Target : MyArray_Access) is
      New_Elements : constant Elements_Access :=
         new Elements_Type (0 .. 2 * Target.Capacity);
      Old_Elements : Elements_Access := Target.Elements;
   begin
      New_Elements (0 .. Target.Capacity) :=
         Old_Elements (0 .. Target.Capacity);
      Free (Old_Elements);
      Target.Elements := New_Elements;
      Target.Capacity := Target.Capacity * 2;
   end Resize;

   procedure Insert (Target : MyArray_Access; Index : Natural; Element : T) is
   begin

      while Index >= Target.Capacity loop
         Resize (Target);
      end loop;

      Target.Elements (Index) := Element;

   end Insert;

   function Get (Target : MyArray_Access; Index : Natural) return T is
   begin
      return Target.Elements (Index);
   end Get;

end Arrays;
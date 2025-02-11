with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Types is

   type Person is
      record
         ID : Integer;
         Name : Unbounded_String := To_Unbounded_String ("_");
      end record;
      
   function Key (P : Person) return Integer;

   function Image (P : Person) return String;
end Types;
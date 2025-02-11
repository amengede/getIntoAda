with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Primitives; use Primitives;
package Types is

   type Person is
      record
         ID : Uint32;
         Name : Unbounded_String := To_Unbounded_String ("_");
      end record;

   function Key (P : Person) return Uint32;

   function Image (P : Person) return String;
end Types;
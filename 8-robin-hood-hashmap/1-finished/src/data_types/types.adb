with Ada.Containers;
with Ada.Strings.Unbounded.Hash;

package body Types is
   function Key (P : Person) return Uint32 is
   begin
      return P.ID;
   end Key;

   function Image (P : Person) return String is
   begin
      return P.ID'Image & ": " & To_String (P.Name);
   end Image;

   function Hash_Name (Name : Unbounded_String) return Natural is
   begin
      return Natural (Hash (Name));
   end Hash_Name;
end Types;

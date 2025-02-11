package body Types is
   function Key (P : Person) return Uint32 is
   begin
      return P.ID;
   end Key;

   function Image (P : Person) return String is
   begin
      return P.ID'Image & ": " & To_String (P.Name);
   end Image;
end Types;
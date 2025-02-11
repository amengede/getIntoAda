with Primitives; use Primitives;
generic
   type T is private;
   with function Key (Item : T) return Uint32;
package HashMaps is

   type Element is
      record
         Probe_Length : Uint32 := 0;
         Data : T;
         Free : Boolean := True;
      end record;

   type Elements_Type is array (Uint32 range <>) of Element;
   type Elements_Access is access Elements_Type;

   type HashMap is
      record
         Elements : Elements_Access := new Elements_Type (0 .. 1);
         Size : Uint32 := 0;
         Capacity : Uint32 := 1;
      end record;

   type HashMap_Access is access HashMap;

   procedure Insert (Target : HashMap_Access; Data : T);

   function Has (Target : HashMap_Access; Element_Key : Uint32) return Boolean;

   function Get (Target : HashMap_Access; Element_Key : Uint32) return T;

   procedure Remove (Target : HashMap_Access; Element_Key : Uint32);

   procedure Print (Target : HashMap_Access);

end HashMaps;

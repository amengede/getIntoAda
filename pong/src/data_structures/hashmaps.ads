with Primitives; use Primitives;
with Ada.Finalization;
generic
   type K is private;
   type V is private;
   with function Hash (Item : K) return Natural;
   Default_Key : K;
package HashMaps is

   Key_Error : exception;

   type Hash_Map is tagged private;

   procedure Insert (Self : in out Hash_Map; Key : K; Value : V);

   function Has (Self : in out Hash_Map; Key : K) return Boolean;

   function Get (Self : in out Hash_Map; Key : K) return V;

   procedure Remove (Self : in out Hash_Map; Key : K);

   procedure Print (Self : in out Hash_Map);

private

   type Element is
      record
         Probe_Length : Natural := 0;
         Key : K := Default_Key;
         Value : V;
         Free : Boolean := True;
      end record;

   type Elements_Type is array (Natural range <>) of Element;
   type Elements_Ptr is access Elements_Type;

   type Internal_Hash_Map is
      record
         Elements : Elements_Ptr := new Elements_Type (0 .. 1);
         Size : Natural := 0;
         Capacity : Natural := 1;
         Reference_Count : Natural := 1;
      end record;

   type Internal_HashMap_Ptr is access Internal_Hash_Map;

   type Hash_Map is new Ada.Finalization.Controlled with
      record
         Data : Internal_HashMap_Ptr := null;
      end record;

   overriding
   procedure Adjust (Self : in out Hash_Map);

   overriding
   procedure Finalize (Self : in out Hash_Map);

end HashMaps;

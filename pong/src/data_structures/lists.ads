with Primitives; use Primitives;
generic
   type T is private;
   with function Key (Item : T) return Uint32;
package Lists is

   type Node;

   type Link is access Node;

   type Node is
      record
         Value : T;
         Next : Link;
   end record;

   procedure Insert (Head : in out Link; Element : T);

   procedure Remove (Head : in out Link);

   function Length (Head : Link) return Integer;
end Lists;

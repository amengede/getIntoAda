generic
   type T is private;
package Marray is

   type Elements_Type is array (Natural range <>) of T;
   type Elements_Access is access Elements_Type;

   type MyArray is
      record
         Elements: Elements_Access := new Elements_Type (0 .. 1);
         Capacity: Natural := 1;
      end record;
   
   type MyArray_Access is access MyArray;
   
   procedure Insert (Target: MyArray_Access; Index: Natural; Element: T);

   function Get (Target: MyArray_Access; Index: Natural) return T;
end Marray;

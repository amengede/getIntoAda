--  Dependencies
with Lists;
with Arrays;
with Types; use Types;

package Common is

   --  Instantiate generic linked list data structure
   package Person_List is new Lists (T => Person, Key => Key);
   package Person_Array is new Arrays (T => Person);

end Common;
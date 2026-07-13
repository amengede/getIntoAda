--  Dependencies
with List;
with Vectors;
with Types; use Types;

package Common is

   --  Instantiate generic linked list data structure
   package Person_List is new List (T => Person, Key => Key);
   package Person_Array is new Vectors (T => Person);

end Common;

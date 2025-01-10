--  Dependencies
with List;
with Marray;
with Types; use Types;

package Common is

   --  Instantiate generic linked list data structure
   package Person_List is new List (T => Person, Key => Key);
   package Person_Array is new Marray (T => Person);

end Common;
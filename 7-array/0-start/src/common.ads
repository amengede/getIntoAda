--  Dependencies
with List;
with Types; use Types;

package Common is

   --  Instantiate generic linked list data structure
   package Person_List is new List (T => Person, Key => Key);

end Common;
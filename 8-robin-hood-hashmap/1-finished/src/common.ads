--  Dependencies
with Lists;
with Vectors;
with HashMaps;
with Types; use Types;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Common is

   --  Instantiate generic linked list data structure
   package Person_List is new Lists (T => Person, Key => Key);
   package Person_Vectors is new Vectors (T => Person);
   package Person_Hashmaps is
      new HashMaps (K => Unbounded_String,
                    V => Person,
                    Hash => Hash_Name,
                    Default_Key => To_Unbounded_String ("???"));

end Common;

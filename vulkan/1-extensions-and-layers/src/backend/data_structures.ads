with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
-------------------------------------------------------------------------------
--  Data Structures
--
--  Common data structure initialization
-------------------------------------------------------------------------------

package Data_Structures is
    package String_Vectors is new Ada.Containers.Vectors (
        Index_Type => Natural, 
        Element_Type => Ada.Strings.Unbounded.Unbounded_String);
end Data_Structures;

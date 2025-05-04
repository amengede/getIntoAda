-- This file is part of VulkAda.

-- VulkAda is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- VulkAda is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.

-- You should have received a copy of the GNU Lesser General Public
-- License along with VulkAda.
-- If not, see <http://www.gnu.org/licenses/>.

-- Copyright 2024 Phaser Cat Games LLC

-- C interface for the texture gather bias LOD extension

with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Texture_Gather_Bias_LODs_C is
    procedure To_Ada(Ada_Struct: in out Texture_LOD_Gather_Format_Properties;
                     C_Struct: in Texture_LOD_Gather_Format_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Supports_Texture_Gather_LOD_Bias :=
            Utilities.To_Ada(C_Struct.Supports_Texture_Gather_LOD_Bias);
    end To_Ada;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Texture_LOD_Gather_Format_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Texture_LOD_Gather_Format_Properties,
                         Texture_LOD_Gather_Format_Properties_C,
                         Texture_LOD_Gather_Format_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Structure(Ada_Struct.Record_Type) is
            when Texture_LOD_Gather_Format_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Texture_LOD_Gather_Format_Properties_C_Access);
                begin
                    To_Ada(Texture_LOD_Gather_Format_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
        end case;
    end To_Ada;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Structure(Next.Record_Type) is
            when Texture_LOD_Gather_Format_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Texture_LOD_Gather_Format_Properties_C,
                         Texture_LOD_Gather_Format_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Texture_Gather_Bias_LODs_C;


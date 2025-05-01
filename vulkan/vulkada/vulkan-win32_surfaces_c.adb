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

-- C interface for the Win32 surface extension

with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Win32_Surfaces_C is
    function To_C(Struct: in Win32_Surfaces.Create_Info)
        return Win32_Surface_Create_Info_C is
        WSCIC: Win32_Surface_Create_Info_C;
    begin
        WSCIC.Next := Extension_Records.To_C(Struct.Next);
        WSCIC.Flags := Struct.Flags;
        WSCIC.Instance := Struct.Instance;
        WSCIC.Window := Struct.Window;

        return WSCIC;
    end To_C;

    procedure Free(Struct: in out Win32_Surface_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Win32_Surface_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Win32_Surfaces.Create_Info,
                         Win32_Surface_Create_Info_C,
                         Win32_Surface_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Structure(Next.Record_Type) is
            when Win32_Surface_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Win32_Surface_Create_Info_C,
                         Win32_Surface_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Win32_Surfaces_C;


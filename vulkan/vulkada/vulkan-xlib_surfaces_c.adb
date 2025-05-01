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

-- C interface for the Xlib surface extension

with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Xlib_Surfaces_C is
    function To_C(Struct: in Xlib_Surfaces.Create_Info)
        return Xlib_Surface_Create_Info_C is
        XSCIC: Xlib_Surface_Create_Info_C;
    begin
        XSCIC.Next := Extension_Records.To_C(Struct.Next);
        XSCIC.Flags := Struct.Flags;
        XSCIC.Dpy := Struct.Dpy;
        XSCIC.Window := Struct.Window;

        return XSCIC;
    end To_C;

    procedure Free(Struct: in out Xlib_Surface_Create_Info_C) is
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
            when Xlib_Surface_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Xlib_Surfaces.Create_Info,
                         Xlib_Surface_Create_Info_C,
                         Xlib_Surface_Create_Info_C_Access);
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
            when Xlib_Surface_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Xlib_Surface_Create_Info_C,
                         Xlib_Surface_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Xlib_Surfaces_C;


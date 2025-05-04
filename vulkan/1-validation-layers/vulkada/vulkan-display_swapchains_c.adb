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

-- C interface for the display swapchain extension

with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Display_Swapchains_C is
    function To_C(Struct: in Display_Present_Info)
        return Display_Present_Info_C is
        DPIC: Display_Present_Info_C;
    begin
        DPIC.Next := Extension_Records.To_C(Struct.Next);
        DPIC.Src_Rect := Struct.Src_Rect;
        DPIC.Dst_Rect := Struct.Dst_Rect;
        DPIC.Persistent := Utilities.To_C(Struct.Persistent);

        return DPIC;
    end To_C;

    procedure Free(Struct: in out Display_Present_Info_C) is
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
            when Display_Present_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Display_Present_Info,
                         Display_Present_Info_C,
                         Display_Present_Info_C_Access);
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
            when Display_Present_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Present_Info_C,
                         Display_Present_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Display_Swapchains_C;


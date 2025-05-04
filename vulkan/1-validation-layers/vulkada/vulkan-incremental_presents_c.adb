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

-- C interface for the increment present extension

with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Incremental_Presents_C is
    function To_C(Struct: in Present_Region) return Present_Region_C is
        procedure To_C_Array is new Utilities.To_C_Array(Rect_Layer_Arrays,
                                                         Rect_Layer_Vectors);

        PRC: Present_Region_C;
    begin
        To_C_Array(PRC.Rectangle_Count, Struct.Rectangles, PRC.Rectangles);

        return PRC;
    end To_C;

    procedure Free(Struct: in out Present_Region_C) is
    begin
        Rect_Layer_Arrays.Free(Struct.Rectangles);
    end Free;

    function To_C(Struct: in Present_Regions) return Present_Regions_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Present_Region_C_Arrays, Present_Region_Vectors);

        PRC: Present_Regions_C;
        Dummy: Interfaces.Unsigned_32;
    begin
        PRC.Next := Extension_Records.To_C(Struct.Next);
        PRC.Swapchain_Count := Struct.Swapchain_Count;
        To_C_Array(Dummy, Struct.Regions, PRC.Regions);

        return PRC;
    end To_C;

    procedure Free(Struct: in out Present_Regions_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Present_Region_C_Arrays.Free(Struct.Regions, Free'Access);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Present_Regions_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Present_Regions,
                         Present_Regions_C,
                         Present_Regions_C_Access);
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
            when Present_Regions_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Present_Regions_C,
                         Present_Regions_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Incremental_Presents_C;


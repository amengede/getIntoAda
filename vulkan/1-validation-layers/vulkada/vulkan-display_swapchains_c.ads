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

with Vulkan.C;

private package Vulkan.Display_Swapchains_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in Display_Present_Info_Type;

    -- C interface records.
    type Display_Present_Info_C is
    record
        Record_Type: In_Structure_Type := Display_Present_Info_Type;
        Next: C.In_Structure_C_Access;
        Src_Rect: Rect_2D;
        Dst_Rect: Rect_2D;
        Persistent: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Display_Present_Info_C_Access is access Display_Present_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Display_Present_Info)
        return Display_Present_Info_C;
    procedure Free(Struct: in out Display_Present_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Display_Swapchains_C;


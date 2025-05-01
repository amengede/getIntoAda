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

with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.Incremental_Presents_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Present_Regions_Type;

    -- C interface records.
    package Rect_Layer_Arrays is new C_Arrays(Rect_Layer);

    type Present_Region_C is
    record
        Rectangle_Count: Interfaces.Unsigned_32;
        Rectangles: Rect_Layer_Arrays.Pointer;
    end record
        with Convention => C;

    package Present_Region_C_Arrays is new C_Arrays(Present_Region_C);

    type Present_Regions_C is
    record
        Record_Type: In_Structure_Type := Present_Regions_Type;
        Next: C.In_Structure_C_Access;
        Swapchain_Count: Interfaces.Unsigned_32;
        Regions: Present_Region_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Present_Regions_C_Access is access Present_Regions_C
        with Convention => C;
    
    -- Conversion subprograms.
    function To_C(Struct: in Present_Region) return Present_Region_C;
    procedure Free(Struct: in out Present_Region_C);

    function To_C(Struct: in Present_Regions) return Present_Regions_C;
    procedure Free(Struct: in out Present_Regions_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Incremental_Presents_C;


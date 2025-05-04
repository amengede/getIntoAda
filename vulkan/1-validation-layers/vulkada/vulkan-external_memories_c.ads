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

-- C interface for the external memory extension

with Vulkan.C;

private package Vulkan.External_Memories_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            External_Memory_Image_Create_Info_NV_Type |
            Export_Memory_Allocate_Info_NV_Type;

    -- C interface records.
    type External_Memory_Image_Create_Info_NV_C is
    record
        Record_Type: In_Structure_Type :=
            External_Memory_Image_Create_Info_NV_Type;
        Next: C.In_Structure_C_Access;
        Handle_Types: External_Memory_Handle_Type_Flags_NV;
    end record
        with Convention => C;

    type External_Memory_Image_Create_Info_NV_C_Access is
        access External_Memory_Image_Create_Info_NV_C
        with Convention => C;

    type Export_Memory_Allocate_Info_NV_C is
    record
        Record_Type: In_Structure_Type := Export_Memory_Allocate_Info_NV_Type;
        Next: C.In_Structure_C_Access;
        Handle_Types: External_Memory_Handle_Type_Flags_NV;
    end record
        with Convention => C;

    type Export_Memory_Allocate_Info_NV_C_Access is
        access Export_Memory_Allocate_Info_NV_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in External_Memory_Image_Create_Info_NV)
        return External_Memory_Image_Create_Info_NV_C;
    procedure Free(Struct: in out External_Memory_Image_Create_Info_NV_C);

    function To_C(Struct: in Export_Memory_Allocate_Info_NV)
        return Export_Memory_Allocate_Info_NV_C;
    procedure Free(Struct: in out Export_Memory_Allocate_Info_NV_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.External_Memories_C;


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

-- C interface for the map memory 2 extension

with Vulkan.C;

private package Vulkan.Map_Memory_2_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Memory_Map_Info_Type |
            Memory_Unmap_Info_Type;

    -- C interface records.
    type Memory_Map_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Map_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Memory_Map_Flags;
        Memory: Device_Memory;
        Offset: Device_Size;
        Size: Device_Size;
    end record
        with Convention => C;

    type Memory_Map_Info_C_Access is access Memory_Map_Info_C
        with Convention => C;

    type Memory_Unmap_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Unmap_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Memory_Unmap_Flags;
        Memory: Device_Memory;
    end record
        with Convention => C;

    type Memory_Unmap_Info_C_Access is access Memory_Unmap_Info_C
        with Convention => C;
    
    -- Conversion subprograms.
    function To_C(Struct: in Memory_Map_Info) return Memory_Map_Info_C;
    procedure Free(Struct: in out Memory_Map_Info_C);

    function To_C(Struct: in Memory_Unmap_Info) return Memory_Unmap_Info_C;
    procedure Free(Struct: in out Memory_Unmap_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Map_Memory_2_C;


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

-- C interface for the dedicated allocation extension

with Vulkan.C;

private package Vulkan.Dedicated_Allocations_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Dedicated_Allocation_Image_Create_Info_Type |
            Dedicated_Allocation_Buffer_Create_Info_Type |
            Dedicated_Allocation_Memory_Allocate_Info_Type;

    -- C interface records.
    type Dedicated_Allocation_Image_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Dedicated_Allocation_Image_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Dedicated_Allocation: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Dedicated_Allocation_Image_Create_Info_C_Access is
        access Dedicated_Allocation_Image_Create_Info_C
        with Convention => C;

    type Dedicated_Allocation_Buffer_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Dedicated_Allocation_Buffer_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Dedicated_Allocation: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Dedicated_Allocation_Buffer_Create_Info_C_Access is
        access Dedicated_Allocation_Buffer_Create_Info_C
        with Convention => C;

    type Dedicated_Allocation_Memory_Allocate_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Dedicated_Allocation_Memory_Allocate_Info_Type;
        Next: C.In_Structure_C_Access;
        Image: Vulkan.Image;
        Buffer: Vulkan.Buffer;
    end record
        with Convention => C;

    type Dedicated_Allocation_Memory_Allocate_Info_C_Access is
        access Dedicated_Allocation_Memory_Allocate_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Dedicated_Allocation_Image_Create_Info)
        return Dedicated_Allocation_Image_Create_Info_C;
    procedure Free(Struct: in out Dedicated_Allocation_Image_Create_Info_C);

    function To_C(Struct: in Dedicated_Allocation_Buffer_Create_Info)
        return Dedicated_Allocation_Buffer_Create_Info_C;
    procedure Free(Struct: in out Dedicated_Allocation_Buffer_Create_Info_C);

    function To_C(Struct: in Dedicated_Allocation_Memory_Allocate_Info)
        return Dedicated_Allocation_Memory_Allocate_Info_C;
    procedure Free(Struct: in out Dedicated_Allocation_Memory_Allocate_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Dedicated_Allocations_C;


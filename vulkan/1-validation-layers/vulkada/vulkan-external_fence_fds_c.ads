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

-- C interface for the external fence FD extension

with Vulkan.C;

private package Vulkan.External_Fence_FDs_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Import_Fence_FD_Info_Type |
            Fence_Get_FD_Info_Type;

    -- C interface records.
    type Import_Fence_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Import_Fence_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Fence: Vulkan.Fence;
        Flags: Fence_Import_Flags;
        Handle_Type: External_Fence_Handle_Type_Flags;
        FD: File_Descriptor;
    end record
        with Convention => C;

    type Import_Fence_FD_Info_C_Access is access Import_Fence_FD_Info_C
        with Convention => C;

    type Fence_Get_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Fence_Get_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Fence: Vulkan.Fence;
        Handle_Type: External_Fence_Handle_Type_Flags;
    end record
        with Convention => C;

    type Fence_Get_FD_Info_C_Access is access Fence_Get_FD_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Import_Fence_FD_Info)
        return Import_Fence_FD_Info_C;
    procedure Free(Struct: in out Import_Fence_FD_Info_C);

    function To_C(Struct: in Fence_Get_FD_Info) return Fence_Get_FD_Info_C;
    procedure Free(Struct: in out Fence_Get_FD_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.External_Fence_FDs_C;


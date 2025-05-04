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

-- C interface for the external semaphore FD extension

with Vulkan.C;

private package Vulkan.External_Semaphore_FDs_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Import_Semaphore_FD_Info_Type |
            Semaphore_Get_FD_Info_Type;

    -- C interface records.
    type Import_Semaphore_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Import_Semaphore_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Semaphore: Vulkan.Semaphore;
        Flags: Semaphore_Import_Flags;
        Handle_Type: External_Semaphore_Handle_Type_Flags;
        FD: File_Descriptor;
    end record
        with Convention => C;

    type Import_Semaphore_FD_Info_C_Access is access Import_Semaphore_FD_Info_C
        with Convention => C;

    type Semaphore_Get_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Semaphore_Get_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Semaphore: Vulkan.Semaphore;
        Handle_Type: External_Semaphore_Handle_Type_Flags;
    end record
        with Convention => C;

    type Semaphore_Get_FD_Info_C_Access is access Semaphore_Get_FD_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Import_Semaphore_FD_Info)
        return Import_Semaphore_FD_Info_C;
    procedure Free(Struct: in out Import_Semaphore_FD_Info_C);

    function To_C(Struct: in Semaphore_Get_FD_Info)
        return Semaphore_Get_FD_Info_C;
    procedure Free(Struct: in out Semaphore_Get_FD_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.External_Semaphore_FDs_C;


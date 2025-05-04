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

-- C interface for the external memory FD extension

with Vulkan.C;

private package Vulkan.External_Memory_FDs_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Import_Memory_FD_Info_Type |
            Memory_FD_Properties_Type |
            Memory_Get_FD_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Import_Memory_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Import_Memory_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Type: External_Memory_Handle_Type_Flags;
        FD: File_Descriptor;
    end record
        with Convention => C;

    type Import_Memory_FD_Info_C_Access is access Import_Memory_FD_Info_C
        with Convention => C;

    type Memory_FD_Properties_C is
    record
        Record_Type: Out_Structure_Type := Memory_FD_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Memory_Type_Bits: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Memory_FD_Properties_C_Access is access Memory_FD_Properties_C
        with Convention => C;

    type Memory_Get_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Get_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Memory: Device_Memory;
        Handle_Type: External_Memory_Handle_Type_Flags;
    end record
        with Convention => C;

    type Memory_Get_FD_Info_C_Access is access Memory_Get_FD_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Import_Memory_FD_Info)
        return Import_Memory_FD_Info_C;
    procedure Free(Struct: in out Import_Memory_FD_Info_C);

    procedure To_Ada(Ada_Struct: in out Memory_FD_Properties;
                     C_Struct: in Memory_FD_Properties_C);
    
    function To_C(Struct: in Memory_Get_FD_Info) return Memory_Get_FD_Info_C;
    procedure Free(Struct: in out Memory_Get_FD_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.External_Memory_FDs_C;


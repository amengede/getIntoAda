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

-- C interface for the binary import extension

with Interfaces.C.Strings;
with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.Binary_Imports_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Cu_Module_Create_Info_Type |
            Cu_Function_Create_Info_Type |
            Cu_Launch_Info_Type;

    -- C interface records.
    type Cu_Module_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Cu_Module_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Data_Size: Interfaces.C.size_t;
        Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Cu_Module_Create_Info_C_Access is access Cu_Module_Create_Info_C
        with Convention => C;

    type Cu_Function_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Cu_Function_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Module: Cu_Module;
        Name: Interfaces.C.Strings.chars_ptr;
    end record
        with Convention => C;

    type Cu_Function_Create_Info_C_Access is access Cu_Function_Create_Info_C
        with Convention => C;

    package Void_Pointer_Arrays is
        new C_Arrays(Interfaces.C.Extensions.void_ptr);

    type Cu_Launch_Info_C is
    record
        Record_Type: In_Structure_Type := Cu_Launch_Info_Type;
        Next: C.In_Structure_C_Access;
        Cu_Function: Vulkan.Cu_Function;
        Grid_Dim_X: Interfaces.Unsigned_32;
        Grid_Dim_Y: Interfaces.Unsigned_32;
        Grid_Dim_Z: Interfaces.Unsigned_32;
        Block_Dim_X: Interfaces.Unsigned_32;
        Block_Dim_Y: Interfaces.Unsigned_32;
        Block_Dim_Z: Interfaces.Unsigned_32;
        Shared_Mem_Bytes: Interfaces.Unsigned_32;
        Param_Count: Interfaces.C.size_t;
        Params: Void_Pointer_Arrays.Pointer;
        Extra_Count: Interfaces.C.size_t;
        Extras: Void_Pointer_Arrays.Pointer;
    end record
        with Convention => C;

    type Cu_Launch_Info_C_Access is access Cu_Launch_Info_C;

    -- Conversion subprograms.
    function To_C(Struct: in Cu_Module_Create_Info)
        return Cu_Module_Create_Info_C;
    procedure Free(Struct: in out Cu_Module_Create_Info_C);

    function To_C(Struct: in Cu_Function_Create_Info)
        return Cu_Function_Create_Info_C;
    procedure Free(Struct: in out Cu_Function_Create_Info_C);

    function To_C(Struct: in Cu_Launch_Info) return Cu_Launch_Info_C;
    procedure Free(Struct: in out Cu_Launch_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Binary_Imports_C;


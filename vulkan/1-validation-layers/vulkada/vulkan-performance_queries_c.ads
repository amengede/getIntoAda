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

-- C interface for the performance query extension

with Vulkan.C;

private package Vulkan.Performance_Queries_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Performance_Query_Features_Type |
            Physical_Device_Performance_Query_Properties_Type |
            Performance_Counter_Type |
            Performance_Counter_Description_Type |
            Query_Pool_Performance_Create_Info_Type |
            Acquire_Profiling_Lock_Info_Type |
            Performance_Query_Submit_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Physical_Device_Performance_Query_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Performance_Query_Features_Type;
        Next: C.Out_Structure_C_Access;
        Performance_Counter_Query_Pools: Interfaces.Unsigned_32;
        Performance_Counter_Multiple_Query_Pools: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Performance_Query_Features_C_Access is
        access Physical_Device_Performance_Query_Features_C
        with Convention => C;

    type Physical_Device_Performance_Query_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Performance_Query_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Allow_Command_Buffer_Query_Copies: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Performance_Query_Properties_C_Access is
        access Physical_Device_Performance_Query_Properties_C
        with Convention => C;

    type Performance_Counter_C is
    record
        Record_Type: Out_Structure_Type := Performance_Counter_Type;
        Next: C.Out_Structure_C_Access;
        Unit: Performance_Counter_Unit;
        Scope: Performance_Counter_Scope;
        Storage: Performance_Counter_Storage;
        UUID: Vulkan.UUID;
    end record
        with Convention => C;

    type Performance_Counter_C_Access is access Performance_Counter_C
        with Convention => C;

    type Performance_Counter_Description_C is
    record
        Record_Type: Out_Structure_Type := Performance_Counter_Description_Type;
        Next: C.Out_Structure_C_Access;
        Flags: Performance_Counter_Description_Flags;
        Name: Interfaces.C.char_array(1 .. Max_Description_Size);
        Category: Interfaces.C.char_array(1 .. Max_Description_Size);
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
    end record
        with Convention => C;

    type Performance_Counter_Description_C_Access is
        access Performance_Counter_Description_C
        with Convention => C;

    type Query_Pool_Performance_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Query_Pool_Performance_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Queue_Family_Index: Vulkan.Queue_Family_Index;
        Counter_Index_Count: Interfaces.Unsigned_32;
        Counter_Indices: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Query_Pool_Performance_Create_Info_C_Access is
        access Query_Pool_Performance_Create_Info_C
        with Convention => C;

    type Acquire_Profiling_Lock_Info_C is
    record
        Record_Type: In_Structure_Type := Acquire_Profiling_Lock_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Acquire_Profiling_Lock_Flags;
        Timeout: Interfaces.Unsigned_64;
    end record
        with Convention => C;

    type Acquire_Profiling_Lock_Info_C_Access is
        access Acquire_Profiling_Lock_Info_C
        with Convention => C;

    type Performance_Query_Submit_Info_C is
    record
        Record_Type: In_Structure_Type := Performance_Query_Submit_Info_Type;
        Next: C.In_Structure_C_Access;
        Counter_Pass_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Performance_Query_Submit_Info_C_Access is
        access Performance_Query_Submit_Info_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Performance_Query_Features;
         C_Struct: in Physical_Device_Performance_Query_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Performance_Query_Properties;
         C_Struct: in Physical_Device_Performance_Query_Properties_C);

    procedure To_Ada(Ada_Struct: in out Performance_Counter;
                     C_Struct: in Performance_Counter_C);

    procedure To_Ada(Ada_Struct: in out Performance_Counter_Description;
                     C_Struct: in Performance_Counter_Description_C);

    function To_C(Struct: in Query_Pool_Performance_Create_Info)
        return Query_Pool_Performance_Create_Info_C;
    procedure Free(Struct: in out Query_Pool_Performance_Create_Info_C);

    function To_C(Struct: in Acquire_Profiling_Lock_Info)
        return Acquire_Profiling_Lock_Info_C;
    procedure Free(Struct: in out Acquire_Profiling_Lock_Info_C);

    function To_C(Struct: in Performance_Query_Submit_Info)
        return Performance_Query_Submit_Info_C;
    procedure Free(Struct: in out Performance_Query_Submit_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Performance_Queries_C;


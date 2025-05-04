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

-- C interface for the pipeline executable properties extension

with Vulkan.C;

private package Vulkan.Pipeline_Executable_Properties_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Pipeline_Executable_Properties_Features_Type |
            Pipeline_Info_Type |
            Pipeline_Executable_Properties_Type |
            Pipeline_Executable_Info_Type |
            Pipeline_Executable_Statistic_Type |
            Pipeline_Executable_Internal_Representation_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;
    
    -- C interface records.
    type Physical_Device_Pipeline_Executable_Properties_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Executable_Properties_Features_Type;
        Next: C.Out_Structure_C_Access;
        Pipeline_Executable_Info: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Executable_Properties_Features_C_Access is
        access Physical_Device_Pipeline_Executable_Properties_Features_C
        with Convention => C;

    type Pipeline_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Info_Type;
        Next: C.In_Structure_C_Access;
        Pipeline: Vulkan.Pipeline;
    end record
        with Convention => C;

    type Pipeline_Info_C_Access is access Pipeline_Info_C
        with Convention => C;

    type Pipeline_Executable_Properties_C is
    record
        Record_Type: Out_Structure_Type := Pipeline_Executable_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Stages: Shader_Stage_Flags;
        Name: Interfaces.C.char_array(1 .. Max_Description_Size);
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
        Subgroup_Size: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Executable_Properties_C_Access is
        access Pipeline_Executable_Properties_C
        with Convention => C;

    type Pipeline_Executable_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Executable_Info_Type;
        Next: C.In_Structure_C_Access;
        Pipeline: Vulkan.Pipeline;
        Executable_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Executable_Info_C_Access is access Pipeline_Executable_Info_C
        with Convention => C;

    type Pipeline_Executable_Statistic_C
        (Format_Type: Pipeline_Executable_Statistic_Format) is
    record
        Record_Type: Out_Structure_Type := Pipeline_Executable_Statistic_Type;
        Next: C.Out_Structure_C_Access;
        Name: Interfaces.C.char_array(1 .. Max_Description_Size);
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
        Format: Pipeline_Executable_Statistic_Format := Format_Type;

        case Format_Type is
            when Bool32 =>
                B32: Interfaces.Unsigned_32;
            when Int64 =>
                I64: Interfaces.Integer_64;
            when Uint64 =>
                U64: Interfaces.Unsigned_64;
            when Float64 =>
                F64: Interfaces.C.double;
        end case;
    end record
        with Convention => C,
             Unchecked_Union;

    type Pipeline_Executable_Statistic_C_Access is
        access Pipeline_Executable_Statistic_C
        with Convention => C;

    type Pipeline_Executable_Internal_Representation_C is
    record
        Record_Type: Out_Structure_Type :=
            Pipeline_Executable_Internal_Representation_Type;
        Next: C.Out_Structure_C_Access;
        Name: Interfaces.C.char_array(1 .. Max_Description_Size);
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
        Is_Text: Interfaces.Unsigned_32;
        Data_Size: Interfaces.C.size_t;
        Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Pipeline_Executable_Internal_Representation_C_Access is
        access Pipeline_Executable_Internal_Representation_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada
        (Ada_Struct: in out
            Physical_Device_Pipeline_Executable_Properties_Features;
         C_Struct: in
            Physical_Device_Pipeline_Executable_Properties_Features_C);

    function To_C(Struct: in Pipeline_Info) return Pipeline_Info_C;
    procedure Free(Struct: in out Pipeline_Info_C);

    procedure To_Ada(Ada_Struct: in out Pipeline_Executable_Properties;
                     C_Struct: in Pipeline_Executable_Properties_C);

    function To_C(Struct: in Pipeline_Executable_Info)
        return Pipeline_Executable_Info_C;
    procedure Free(Struct: in out Pipeline_Executable_Info_C);

    procedure To_Ada(Ada_Struct: in out Pipeline_Executable_Statistic;
                     C_Struct: in Pipeline_Executable_Statistic_C);

    procedure To_Ada
        (Ada_Struct: in out Pipeline_Executable_Internal_Representation;
         C_Struct: in Pipeline_Executable_Internal_Representation_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Pipeline_Executable_Properties_C;


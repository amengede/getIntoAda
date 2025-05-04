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

-- C interface for the pipeline robustness extension

with Vulkan.C;

private package Vulkan.Pipeline_Robustness_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Pipeline_Robustness_Features_Type |
            Physical_Device_Pipeline_Robustness_Properties_Type |
            Pipeline_Robustness_Create_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Physical_Device_Pipeline_Robustness_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Robustness_Features_Type;
        Next: C.Out_Structure_C_Access;
        Pipeline_Robustness: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Features_C_Access is
        access Physical_Device_Pipeline_Robustness_Features_C
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Robustness_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Default_Robustness_Storage_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Uniform_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Vertex_Inputs: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Images: Pipeline_Robustness_Image_Behavior;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Properties_C_Access is
        access Physical_Device_Pipeline_Robustness_Properties_C
        with Convention => C;

    type Pipeline_Robustness_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Robustness_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Storage_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Uniform_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Vertex_Inputs: Pipeline_Robustness_Buffer_Behavior;
        Images: Pipeline_Robustness_Image_Behavior;
    end record
        with Convention => C;

    type Pipeline_Robustness_Create_Info_C_Access is
        access Pipeline_Robustness_Create_Info_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Robustness_Features;
         C_Struct: in Physical_Device_Pipeline_Robustness_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Robustness_Properties;
         C_Struct: in Physical_Device_Pipeline_Robustness_Properties_C);

    function To_C(Struct: in Pipeline_Robustness_Create_Info)
        return Pipeline_Robustness_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Robustness_Create_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Pipeline_Robustness_C;


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

-- C interface for the transform feedback extension

with Vulkan.C;

private package Vulkan.Transform_Feedbacks_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Transform_Feedback_Features_Type |
            Physical_Device_Transform_Feedback_Properties_Type |
            Pipeline_Rasterization_State_Stream_Create_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Physical_Device_Transform_Feedback_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Transform_Feedback_Features_Type;
        Next: C.Out_Structure_C_Access;
        Transform_Feedback: Interfaces.Unsigned_32;
        Geometry_Streams: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Transform_Feedback_Features_C_Access is
        access Physical_Device_Transform_Feedback_Features_C
        with Convention => C;

    type Physical_Device_Transform_Feedback_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Transform_Feedback_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Transform_Feedback_Streams: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffers: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffer_Size: Device_Size;
        Max_Transform_Feedback_Stream_Data_Size: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffer_Data_Size: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffer_Data_Stride: Interfaces.Unsigned_32;
        Transform_Feedback_Queries: Interfaces.Unsigned_32;
        Transform_Feedback_Streams_Lines_Triangles: Interfaces.Unsigned_32;
        Transform_Feedback_Rasterization_Stream_Select: Interfaces.Unsigned_32;
        Transform_Feedback_Draw: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Transform_Feedback_Properties_C_Access is
        access Physical_Device_Transform_Feedback_Properties_C
        with Convention => C;

    type Pipeline_Rasterization_State_Stream_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Rasterization_State_Stream_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Pipeline_Rasterization_State_Stream_Create_Flags;
        Rasterization_Stream: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Rasterization_State_Stream_Create_Info_C_Access is
        access Pipeline_Rasterization_State_Stream_Create_Info_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Transform_Feedback_Features;
         C_Struct: in Physical_Device_Transform_Feedback_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Transform_Feedback_Properties;
         C_Struct: in Physical_Device_Transform_Feedback_Properties_C);

    function To_C(Struct: in Pipeline_Rasterization_State_Stream_Create_Info)
        return Pipeline_Rasterization_State_Stream_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Rasterization_State_Stream_Create_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Transform_Feedbacks_C;


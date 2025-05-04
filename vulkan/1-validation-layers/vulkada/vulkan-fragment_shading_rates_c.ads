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

-- C interface to the fragment shading rate extension

with Vulkan.C;
with Vulkan.C_V1_2;

private package Vulkan.Fragment_Shading_Rates_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Fragment_Shading_Rate_Attachment_Info_Type |
            Pipeline_Fragment_Shading_Rate_State_Create_Info_Type |
            Physical_Device_Fragment_Shading_Rate_Features_Type |
            Physical_Device_Fragment_Shading_Rate_Properties_Type |
            Physical_Device_Fragment_Shading_Rate_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Fragment_Shading_Rate_Attachment_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Fragment_Shading_Rate_Attachment_Info_Type;
        Next: C.In_Structure_C_Access;
        Fragment_Shading_Rate_Attachment:
            C_V1_2.Attachment_Reference_2_C_Access;
        Shading_Rate_Attachment_Texel_Size: Extent_2D;
    end record
        with Convention => C;

    type Fragment_Shading_Rate_Attachment_Info_C_Access is
        access Fragment_Shading_Rate_Attachment_Info_C
        with Convention => C;

    type Pipeline_Fragment_Shading_Rate_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Fragment_Shading_Rate_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Fragment_Size: Extent_2D;
        Combiner_Ops: Fragment_Shading_Rate_Combiner_Op_Array;
    end record
        with Convention => C;

    type Pipeline_Fragment_Shading_Rate_State_Create_Info_C_Access is
        access Pipeline_Fragment_Shading_Rate_State_Create_Info_C
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Fragment_Shading_Rate_Features_Type;
        Next: C.Out_Structure_C_Access;
        Pipeline_Fragment_Shading_Rate: Interfaces.Unsigned_32;
        Primitive_Fragment_Shading_Rate: Interfaces.Unsigned_32;
        Attachment_Fragment_Shading_Rate: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_Features_C_Access is
        access Physical_Device_Fragment_Shading_Rate_Features_C
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Fragment_Shading_Rate_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Min_Fragment_Shading_Rate_Attachment_Texel_Size: Extent_2D;
        Max_Fragment_Shading_Rate_Attachment_Texel_Size: Extent_2D;
        Max_Fragment_Shading_Rate_Attachment_Texel_Size_Aspect_Ratio:
            Interfaces.Unsigned_32;
        Primitive_Fragment_Shading_Rate_With_Multiple_Viewports:
            Interfaces.Unsigned_32;
        Layered_Shading_Rate_Attachments: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Non_Trivial_Combiner_Ops: Interfaces.Unsigned_32;
        Max_Fragment_Size: Extent_2D;
        Max_Fragment_Size_Aspect_Ratio: Interfaces.Unsigned_32;
        Max_Fragment_Shading_Rate_Coverage_Samples: Interfaces.Unsigned_32;
        Max_Fragment_Shading_Rate_Rasterization_Samples: Sample_Count_Flags;
        Fragment_Shading_Rate_With_Shader_Depth_Stencil_Writes:
            Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Sample_Mask: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Shader_Sample_Mask: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Conservative_Rasterization:
            Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Fragment_Shader_Interlock:
            Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Custom_Sample_Locations:
            Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Strict_Multiply_Combiner: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_Properties_C_Access is
        access Physical_Device_Fragment_Shading_Rate_Properties_C
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Fragment_Shading_Rate_Type;
        Next: C.Out_Structure_C_Access;
        Sample_Counts: Sample_Count_Flags;
        Fragment_Size: Extent_2D;
    end record
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_C_Access is
        access Physical_Device_Fragment_Shading_Rate_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Fragment_Shading_Rate_Attachment_Info)
        return Fragment_Shading_Rate_Attachment_Info_C;
    procedure Free(Struct: in out Fragment_Shading_Rate_Attachment_Info_C);

    function To_C(Struct: in Pipeline_Fragment_Shading_Rate_State_Create_Info)
        return Pipeline_Fragment_Shading_Rate_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Fragment_Shading_Rate_State_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Fragment_Shading_Rate_Features;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Fragment_Shading_Rate_Properties;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_Properties_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Fragment_Shading_Rate;
                     C_Struct: in Physical_Device_Fragment_Shading_Rate_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);

end Vulkan.Fragment_Shading_Rates_C;


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

-- Copyright 2025 Phaser Cat Games LLC

-- C interface for AMD records

with Vulkan.C;
with Vulkan.C_Arrays;
with Vulkan.Extensions.AMD;

private package Vulkan.C_AMD is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Attachment_Sample_Count_Info_Type |
            Pipeline_Rasterization_State_Rasterization_Order_Type |
            Texture_LOD_Gather_Format_Properties_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    package Sample_Count_Flags_Arrays is new C_Arrays(Sample_Count_Flags);

    type Attachment_Sample_Count_Info_C is
    record
        Record_Type: In_Structure_Type := Attachment_Sample_Count_Info_Type;
        Next: C.In_Structure_C_Access;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Samples: Sample_Count_Flags_Arrays.Pointer;
        Depth_Stencil_Attachment_Samples: Sample_Count_Flags;
    end record
        with Convention => C;

    type Attachment_Sample_Count_Info_C_Access is
        access Attachment_Sample_Count_Info_C
        with Convention => C;

    type Pipeline_Rasterization_State_Rasterization_Order_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Rasterization_State_Rasterization_Order_Type;
        Next: C.In_Structure_C_Access;
        Rasterization_Order: Extensions.AMD.Rasterization_Order;
    end record
        with Convention => C;

    type Pipeline_Rasterization_State_Rasterization_Order_C_Access is
        access Pipeline_Rasterization_State_Rasterization_Order_C
        with Convention => C;

    type Texture_LOD_Gather_Format_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Texture_LOD_Gather_Format_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Supports_Texture_Gather_LOD_Bias: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Texture_LOD_Gather_Format_Properties_C_Access is
        access Texture_LOD_Gather_Format_Properties_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Extensions.AMD.Attachment_Sample_Count_Info)
        return Attachment_Sample_Count_Info_C;
    procedure Free(Struct: in out Attachment_Sample_Count_Info_C);

    function To_C
        (Struct:
            in Extensions.AMD.Pipeline_Rasterization_State_Rasterization_Order)
        return Pipeline_Rasterization_State_Rasterization_Order_C;
    procedure Free
        (Struct: in out Pipeline_Rasterization_State_Rasterization_Order_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.AMD.Texture_LOD_Gather_Format_Properties;
         C_Struct: in Texture_LOD_Gather_Format_Properties_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_AMD;


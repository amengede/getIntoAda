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

-- C interface for the dynamic rendering extension

with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.Dynamic_Rendering_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Rendering_Fragment_Shading_Rate_Attachment_Info_Type |
            Rendering_Fragment_Density_Map_Attachment_Info_Type |
            Attachment_Sample_Count_Info_Type |
            Multiview_Per_View_Attributes_Info_Type;

    -- C interface records.
    type Rendering_Fragment_Shading_Rate_Attachment_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Rendering_Fragment_Shading_Rate_Attachment_Info_Type;
        Next: C.In_Structure_C_Access;
        Image_View: Vulkan.Image_View;
        Image_Layout: Vulkan.Image_Layout;
        Shading_Rate_Attachment_Texel_Size: Extent_2D;
    end record
        with Convention => C;

    type Rendering_Fragment_Shading_Rate_Attachment_Info_C_Access is
        access Rendering_Fragment_Shading_Rate_Attachment_Info_C
        with Convention => C;

    type Rendering_Fragment_Density_Map_Attachment_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Rendering_Fragment_Density_Map_Attachment_Info_Type;
        Next: C.In_Structure_C_Access;
        Image_View: Vulkan.Image_View;
        Image_Layout: Vulkan.Image_Layout;
    end record
        with Convention => C;

    type Rendering_Fragment_Density_Map_Attachment_Info_C_Access is
        access Rendering_Fragment_Density_Map_Attachment_Info_C
        with Convention => C;

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

    type Multiview_Per_View_Attributes_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Multiview_Per_View_Attributes_Info_Type;
        Next: C.In_Structure_C_Access;
        Per_View_Attributes: Interfaces.Unsigned_32;
        Per_View_Attributes_Position_X_Only: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Multiview_Per_View_Attributes_Info_C_Access is
        access Multiview_Per_View_Attributes_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Rendering_Fragment_Shading_Rate_Attachment_Info)
        return Rendering_Fragment_Shading_Rate_Attachment_Info_C;
    procedure Free
        (Struct: in out Rendering_Fragment_Shading_Rate_Attachment_Info_C);

    function To_C(Struct: in Rendering_Fragment_Density_Map_Attachment_Info)
        return Rendering_Fragment_Density_Map_Attachment_Info_C;
    procedure Free
        (Struct: in out Rendering_Fragment_Density_Map_Attachment_Info_C);

    function To_C(Struct: in Attachment_Sample_Count_Info)
        return Attachment_Sample_Count_Info_C;
    procedure Free(Struct: in out Attachment_Sample_Count_Info_C);

    function To_C(Struct: in Multiview_Per_View_Attributes_Info)
        return Multiview_Per_View_Attributes_Info_C;
    procedure Free(Struct: in out Multiview_Per_View_Attributes_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Dynamic_Rendering_C;


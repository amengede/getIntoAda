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

-- C interface for the maintenance 5 extension

with Vulkan.C;
with Vulkan.C_V1_2;

private package Vulkan.Maintenance_5_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Maintenance_5_Features_Type |
            Physical_Device_Maintenance_5_Properties_Type |
            Rendering_Area_Info_Type |
            Image_Subresource_2_Type |
            Device_Image_Subresource_Info_Type |
            Subresource_Layout_2_Type |
            Pipeline_Create_Flags_2_Create_Info_Type |
            Buffer_Usage_Flags_2_Create_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Physical_Device_Maintenance_5_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_5_Features_Type;
        Next: C.Out_Structure_C_Access;
        Maintenance_5: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_5_Features_C_Access is
        access Physical_Device_Maintenance_5_Features_C
        with Convention => C;

    type Physical_Device_Maintenance_5_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_5_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Early_Fragment_Multisample_Coverage_After_Sample_Counting:
            Interfaces.Unsigned_32;
        Early_Fragment_Sample_Mask_Test_Before_Sample_Counting:
            Interfaces.Unsigned_32;
        Depth_Stencil_Swizzle_One_Support: Interfaces.Unsigned_32;
        Polygon_Mode_Point_Size: Interfaces.Unsigned_32;
        Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram:
            Interfaces.Unsigned_32;
        Non_Strict_Wide_Lines_Use_Parallelogram: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_5_Properties_C_Access is
        access Physical_Device_Maintenance_5_Properties_C
        with Convention => C;

    type Rendering_Area_Info_C is
    record
        Record_Type: In_Structure_Type := Rendering_Area_Info_Type;
        Next: C.In_Structure_C_Access;
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Formats: C_V1_2.Format_Arrays.Pointer;
        Depth_Attachment_Format: Format;
        Stencil_Attachment_Format: Format;
    end record
        with Convention => C;

    type Rendering_Area_Info_C_Access is access Rendering_Area_Info_C
        with Convention => C;

    type Image_Subresource_2_C is
    record
        Record_Type: Out_Structure_Type := Image_Subresource_2_Type;
        Next: C.Out_Structure_C_Access;
        Image_Subresource: Vulkan.Image_Subresource;
    end record
        with Convention => C;

    type Image_Subresource_2_C_Access is access Image_Subresource_2_C
        with Convention => C;

    type Device_Image_Subresource_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Image_Subresource_Info_Type;
        Next: C.In_Structure_C_Access;
        Create_Info: C.Image_Create_Info_C_Access;
        Subresource: Image_Subresource_2_C_Access;
    end record
        with Convention => C;

    type Device_Image_Subresource_Info_C_Access is
        access Device_Image_Subresource_Info_C
        with Convention => C;

    type Subresource_Layout_2_C is
    record
        Record_Type: Out_Structure_Type := Subresource_Layout_2_Type;
        Next: C.Out_Structure_C_Access;
        Subresource_Layout: Vulkan.Subresource_Layout;
    end record
        with Convention => C;

    type Subresource_Layout_2_C_Access is access Subresource_Layout_2_C
        with Convention => C;

    type Pipeline_Create_Flags_2_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Create_Flags_2_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Pipeline_Create_Flags_2;
    end record
        with Convention => C;

    type Pipeline_Create_Flags_2_Create_Info_C_Access is
        access Pipeline_Create_Flags_2_Create_Info_C
        with Convention => C;

    type Buffer_Usage_Flags_2_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Buffer_Usage_Flags_2_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Buffer_Usage_Flags_2;
    end record
        with Convention => C;

    type Buffer_Usage_Flags_2_Create_Info_C_Access is
        access Buffer_Usage_Flags_2_Create_Info_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada(Ada_Struct: in out Physical_Device_Maintenance_5_Features;
                     C_Struct: in Physical_Device_Maintenance_5_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_5_Properties;
         C_Struct: in Physical_Device_Maintenance_5_Properties_C);

    function To_C(Struct: in Rendering_Area_Info) return Rendering_Area_Info_C;
    procedure Free(Struct: in out Rendering_Area_Info_C);

    procedure To_Ada(Ada_Struct: in out Image_Subresource_2;
                     C_Struct: in Image_Subresource_2_C);

    function To_C(Struct: in Device_Image_Subresource_Info)
        return Device_Image_Subresource_Info_C;
    procedure Free(Struct: in out Device_Image_Subresource_Info_C);

    procedure To_Ada(Ada_Struct: in out Subresource_Layout_2;
                     C_Struct: in Subresource_Layout_2_C);

    function To_C(Struct: in Pipeline_Create_Flags_2_Create_Info)
        return Pipeline_Create_Flags_2_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Create_Flags_2_Create_Info_C);

    function To_C(Struct: in Buffer_Usage_Flags_2_Create_Info)
        return Buffer_Usage_Flags_2_Create_Info_C;
    procedure Free(Struct: in out Buffer_Usage_Flags_2_Create_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Maintenance_5_C;


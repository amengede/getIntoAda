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

-- C interface for NVX records

with Interfaces.C.Strings;
with Vulkan.C;
with Vulkan.C_Arrays;
with Vulkan.Extensions.NVX;

private package Vulkan.C_NVX is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Multiview_Per_View_Attributes_Info_Type |
            Cu_Module_Create_Info_Type |
            Cu_Function_Create_Info_Type |
            Cu_Launch_Info_Type |
            Image_View_Handle_Info_Type |
            Image_View_Address_Properties_Type |
            Physical_Device_Multiview_Per_View_Attributes_Properties_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
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
        Module: Extensions.NVX.Cu_Module;
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
        Cu_Function: Extensions.NVX.Cu_Function;
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

    type Image_View_Handle_Info_C is
    record
        Record_Type: In_Structure_Type := Image_View_Handle_Info_Type;
        Next: C.In_Structure_C_Access;
        Image_View: Vulkan.Image_View;
        Descriptor_Type: Vulkan.Descriptor_Type;
        Sampler: Vulkan.Sampler;
    end record
        with Convention => C;

    type Image_View_Handle_Info_C_Access is access Image_View_Handle_Info_C
        with Convention => C;

    type Image_View_Address_Properties_C is
    record
        Record_Type: Out_Structure_Type := Image_View_Address_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Device_Address: Vulkan.Device_Address;
        Size: Device_Size;
    end record
        with Convention => C;

    type Image_View_Address_Properties_C_Access is
        access Image_View_Address_Properties_C
        with Convention => C;

    type Physical_Device_Multiview_Per_View_Attributes_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Multiview_Per_View_Attributes_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Per_View_Position_All_Components: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Multiview_Per_View_Attributes_Properties_C_Access is
        access Physical_Device_Multiview_Per_View_Attributes_Properties_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Extensions.NVX.Multiview_Per_View_Attributes_Info)
        return Multiview_Per_View_Attributes_Info_C;
    procedure Free(Struct: in out Multiview_Per_View_Attributes_Info_C);

    function To_C(Struct: in Extensions.NVX.Cu_Module_Create_Info)
        return Cu_Module_Create_Info_C;
    procedure Free(Struct: in out Cu_Module_Create_Info_C);

    function To_C(Struct: in Extensions.NVX.Cu_Function_Create_Info)
        return Cu_Function_Create_Info_C;
    procedure Free(Struct: in out Cu_Function_Create_Info_C);

    function To_C(Struct: in Extensions.NVX.Cu_Launch_Info)
        return Cu_Launch_Info_C;
    procedure Free(Struct: in out Cu_Launch_Info_C);

    function To_C(Struct: in Extensions.NVX.Image_View_Handle_Info)
        return Image_View_Handle_Info_C;
    procedure Free(Struct: in out Image_View_Handle_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.NVX.Image_View_Address_Properties;
         C_Struct: in Image_View_Address_Properties_C);

    procedure To_Ada
       (Ada_Struct:
            in out
        Extensions.NVX.Physical_Device_Multiview_Per_View_Attributes_Properties;
         C_Struct: 
            in Physical_Device_Multiview_Per_View_Attributes_Properties_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_NVX;


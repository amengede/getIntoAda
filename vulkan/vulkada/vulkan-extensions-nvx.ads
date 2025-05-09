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

-- Nvidia experimental extensions root package

package Vulkan.Extensions.NVX is
    -- Handle types.
    type Cu_Module is new Object_Handle;
    type Cu_Function is new Object_Handle;

    No_Cu_Module: constant Cu_Module := Cu_Module(System.Null_Address);
    No_Cu_Function: constant Cu_Function := Cu_Function(System.Null_Address);

    -- Records.
    type Multiview_Per_View_Attributes_Info is new In_Structure
        (Multiview_Per_View_Attributes_Info_Type) with
    record
        Per_View_Attributes: Boolean;
        Per_View_Attributes_Position_X_Only: Boolean;
    end record;

    type Cu_Module_Create_Info is new In_Structure
        (Cu_Module_Create_Info_Type) with
    record
        Data_Size: Interfaces.C.size_t;
        Data: Interfaces.C.Extensions.void_ptr;
    end record;

    type Cu_Function_Create_Info is new In_Structure
        (Cu_Function_Create_Info_Type) with
    record
        Module: Cu_Module;
        Name: Ada.Strings.Unbounded.Unbounded_String;
    end record;

    package Void_Pointer_Vectors is new Ada.Containers.Vectors
        (Positive, Interfaces.C.Extensions.void_ptr, System."=");

    type Cu_Launch_Info is new In_Structure(Cu_Launch_Info_Type) with
    record
        Cu_Function: NVX.Cu_Function;
        Grid_Dim_X: Interfaces.Unsigned_32;
        Grid_Dim_Y: Interfaces.Unsigned_32;
        Grid_Dim_Z: Interfaces.Unsigned_32;
        Block_Dim_X: Interfaces.Unsigned_32;
        Block_Dim_Y: Interfaces.Unsigned_32;
        Block_Dim_Z: Interfaces.Unsigned_32;
        Shared_Mem_Bytes: Interfaces.Unsigned_32;
        Params: Void_Pointer_Vectors.Vector;
        Extras: Void_Pointer_Vectors.Vector;
    end record;

    type Image_View_Handle_Info is new In_Structure
        (Image_View_Handle_Info_Type) with
    record
        Image_View: Vulkan.Image_View;
        Descriptor_Type: Vulkan.Descriptor_Type;
        Sampler: Vulkan.Sampler;
    end record;

    type Image_View_Address_Properties is new Out_Structure
        (Image_View_Address_Properties_Type) with
    record
        Device_Address: Vulkan.Device_Address;
        Size: Device_Size;
    end record;

    type Physical_Device_Multiview_Per_View_Attributes_Properties is
        new Out_Structure
            (Physical_Device_Multiview_Per_View_Attributes_Properties_Type) with
    record
        Per_View_Position_All_Components: Boolean;
    end record;
end Vulkan.Extensions.NVX;


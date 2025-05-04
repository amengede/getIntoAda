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

-- C interface for the image view handle extension

with Vulkan.C;

private package Vulkan.Image_View_Handles_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Image_View_Handle_Info_Type |
            Image_View_Address_Properties_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;
    
    -- C interface records.
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

    -- Conversion subprograms.
    function To_C(Struct: in Image_View_Handle_Info)
        return Image_View_Handle_Info_C;
    procedure Free(Struct: in out Image_View_Handle_Info_C);

    procedure To_Ada(Ada_Struct: in out Image_View_Address_Properties;
                     C_Struct: in Image_View_Address_Properties_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Image_View_Handles_C;


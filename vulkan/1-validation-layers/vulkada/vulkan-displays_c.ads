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

-- C interface for the displays extension

with Vulkan.C;
with Interfaces.C.Strings;

private package Vulkan.Displays_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in Display_Mode_Create_Info_Type |
                                              Display_Surface_Create_Info_Type;

    -- C interface records.
    type Display_Mode_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Display_Mode_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Display_Mode_Create_Flags;
        Parameters: Display_Mode_Parameters;
    end record
        with Convention => C;

    type Display_Mode_Create_Info_C_Access is access Display_Mode_Create_Info_C
        with Convention => C;

    type Display_Properties_C is
    record
        Display: Vulkan.Display;
        Display_Name: Interfaces.C.Strings.chars_ptr;
        Physical_Dimensions: Extent_2D;
        Physical_Resolution: Extent_2D;
        Supported_Transforms: Surface_Transform_Flags;
        Plane_Reorder_Possible: Interfaces.Unsigned_32;
        Persistent_Content: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Display_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Display_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Display_Surface_Create_Flags;
        Display_Mode: Vulkan.Display_Mode;
        Plane_Index: Interfaces.Unsigned_32;
        Plane_Stack_Index: Interfaces.Unsigned_32;
        Transform: Surface_Transform_Flags;
        Global_Alpha: Interfaces.C.C_float;
        Alpha_Mode: Display_Plane_Alpha_Flags;
        Image_Extent: Extent_2D;
    end record
        with Convention => C;

    type Display_Surface_Create_Info_C_Access is
        access Display_Surface_Create_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Display_Mode_Create_Info)
        return Display_Mode_Create_Info_C;
    procedure Free(Struct: in out Display_Mode_Create_Info_C);

    function To_C(Struct: in Display_Properties) return Display_Properties_C;
    function To_Ada(DPC: Display_Properties_C) return Display_Properties;
    procedure Free(Struct: in out Display_Properties_C);

    function To_C(Struct: in Display_Surface_Create_Info)
        return Display_Surface_Create_Info_C;
    procedure Free(Struct: in out Display_Surface_Create_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Displays_C;


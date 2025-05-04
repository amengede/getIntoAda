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

-- C interface for the get display properties 2 extension

with Vulkan.C;
with Vulkan.Displays_C;

private package Vulkan.Get_Display_Properties_2_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Display_Properties_2_Type |
            Display_Plane_Properties_2_Type |
            Display_Mode_Properties_2_Type |
            Display_Plane_Info_2_Type |
            Display_Plane_Capabilities_2_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Display_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Display_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Display_Properties: Displays_C.Display_Properties_C;
    end record
        with Convention => C;

    type Display_Properties_2_C_Access is access Display_Properties_2_C
        with Convention => C;

    type Display_Plane_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Display_Plane_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Display_Plane_Properties: Vulkan.Display_Plane_Properties;
    end record
        with Convention => C;

    type Display_Plane_Properties_2_C_Access is
        access Display_Plane_Properties_2_C
        with Convention => C;

    type Display_Mode_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Display_Mode_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Display_Mode_Properties: Vulkan.Display_Mode_Properties;
    end record
        with Convention => C;

    type Display_Mode_Properties_2_C_Access is
        access Display_Mode_Properties_2_C
        with Convention => C;

    type Display_Plane_Info_2_C is
    record
        Record_Type: In_Structure_Type := Display_Plane_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Mode: Display_Mode;
        Plane_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Display_Plane_Info_2_C_Access is access Display_Plane_Info_2_C
        with Convention => C;

    type Display_Plane_Capabilities_2_C is
    record
        Record_Type: Out_Structure_Type := Display_Plane_Capabilities_2_Type;
        Next: C.Out_Structure_C_Access;
        Capabilities: Display_Plane_Capabilities;
    end record
        with Convention => C;

    type Display_Plane_Capabilities_2_C_Access is
        access Display_Plane_Capabilities_2_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada(Ada_Struct: in out Display_Properties_2;
                     C_Struct: in Display_Properties_2_C);

    procedure To_Ada(Ada_Struct: in out Display_Plane_Properties_2;
                     C_Struct: in Display_Plane_Properties_2_C);

    procedure To_Ada(Ada_Struct: in out Display_Mode_Properties_2;
                     C_Struct: in Display_Mode_Properties_2_C);

    function To_C(Struct: in Display_Plane_Info_2)
        return Display_Plane_Info_2_C;
    procedure Free(Struct: in out Display_Plane_Info_2_C);

    procedure To_Ada(Ada_Struct: in out Display_Plane_Capabilities_2;
                     C_Struct: in Display_Plane_Capabilities_2_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Get_Display_Properties_2_C;


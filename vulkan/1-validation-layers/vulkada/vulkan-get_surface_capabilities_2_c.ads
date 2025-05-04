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

-- C interface for the get surface capabilities 2 extension

with Vulkan.C;

private package Vulkan.Get_Surface_Capabilities_2_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Surface_Info_2_Type |
            Surface_Capabilities_2_Type |
            Surface_Format_2_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Physical_Device_Surface_Info_2_C is
    record
        Record_Type: In_Structure_Type := Physical_Device_Surface_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Surface: Vulkan.Surface;
    end record
        with Convention => C;

    type Physical_Device_Surface_Info_2_C_Access is
        access Physical_Device_Surface_Info_2_C
        with Convention => C;

    type Surface_Capabilities_2_C is
    record
        Record_Type: Out_Structure_Type := Surface_Capabilities_2_Type;
        Next: C.Out_Structure_C_Access;
        Surface_Capabilities: Vulkan.Surface_Capabilities;
    end record
        with Convention => C;

    type Surface_Capabilities_2_C_Access is access Surface_Capabilities_2_C
        with Convention => C;

    type Surface_Format_2_C is
    record
        Record_Type: Out_Structure_Type := Surface_Format_2_Type;
        Next: C.Out_Structure_C_Access;
        Surface_Format: Vulkan.Surface_Format;
    end record
        with Convention => C;

    type Surface_Format_2_C_Access is access Surface_Format_2_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Physical_Device_Surface_Info_2)
        return Physical_Device_Surface_Info_2_C;
    procedure Free(Struct: in out Physical_Device_Surface_Info_2_C);

    procedure To_Ada(Ada_Struct: in out Surface_Capabilities_2;
                     C_Struct: in Surface_Capabilities_2_C);

    procedure To_Ada(Ada_Struct: in out Surface_Format_2;
                     C_Struct: in Surface_Format_2_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Get_Surface_Capabilities_2_C;


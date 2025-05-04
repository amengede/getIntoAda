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

-- C interface for the debug marker extension

with Interfaces.C.Strings;
with Vulkan.C;

private package Vulkan.Debug_Markers_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Debug_Marker_Object_Name_Info_Type |
            Debug_Marker_Object_Tag_Info_Type |
            Debug_Marker_Marker_Info_Type;

    -- C interface records.
    type Debug_Marker_Object_Name_Info_C is
    record
        Record_Type: In_Structure_Type := Debug_Marker_Object_Name_Info_Type;
        Next: C.In_Structure_C_Access;
        Object_Type: Debug_Report_Object_Type;
        Object: Object_Handle;
        Object_Name: Interfaces.C.Strings.chars_ptr;
    end record
        with Convention => C;

    type Debug_Marker_Object_Name_Info_C_Access is
        access Debug_Marker_Object_Name_Info_C
        with Convention => C;

    type Debug_Marker_Object_Tag_Info_C is
    record
        Record_Type: In_Structure_Type := Debug_Marker_Object_Tag_Info_Type;
        Next: C.In_Structure_C_Access;
        Object_Type: Debug_Report_Object_Type;
        Object: Object_Handle;
        Tag_Name: Interfaces.Unsigned_64;
        Tag_Size: Interfaces.C.size_t;
        Tag: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Debug_Marker_Object_Tag_Info_C_Access is
        access Debug_Marker_Object_Tag_Info_C
        with Convention => C;

    type Debug_Marker_Marker_Info_C is
    record
        Record_Type: In_Structure_Type := Debug_Marker_Marker_Info_Type;
        Next: C.In_Structure_C_Access;
        Marker_Name: Interfaces.C.Strings.chars_ptr;
        Color: Debug_Color;
    end record
        with Convention => C;

    type Debug_Marker_Marker_Info_C_Access is access Debug_Marker_Marker_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Debug_Marker_Object_Name_Info)
        return Debug_Marker_Object_Name_Info_C;
    procedure Free(Struct: in out Debug_Marker_Object_Name_Info_C);

    function To_C(Struct: in Debug_Marker_Object_Tag_Info)
        return Debug_Marker_Object_Tag_Info_C;
    procedure Free(Struct: in out Debug_Marker_Object_Tag_Info_C);

    function To_C(Struct: in Debug_Marker_Marker_Info)
        return Debug_Marker_Marker_Info_C;
    procedure Free(Struct: in out Debug_Marker_Marker_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Debug_Markers_C;


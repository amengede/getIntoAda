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

-- C interface for the Xcb surface extension

with Vulkan.C;
with Vulkan.Xcb;
with Vulkan.Xcb_Surfaces;

private package Vulkan.Xcb_Surfaces_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Xcb_Surface_Create_Info_Type;

    -- C interface records.
    type Xcb_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Xcb_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Xcb_Surfaces.Create_Flags;
        Connection: Xcb.Connection;
        Window: Xcb.Window;
    end record
        with Convention => C;

    type Xcb_Surface_Create_Info_C_Access is access Xcb_Surface_Create_Info_C
        with Convention => C;
   
    -- Conversion subprograms.
    function To_C(Struct: in Xcb_Surfaces.Create_Info)
        return Xcb_Surface_Create_Info_C;
    procedure Free(Struct: in out Xcb_Surface_Create_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Xcb_Surfaces_C;


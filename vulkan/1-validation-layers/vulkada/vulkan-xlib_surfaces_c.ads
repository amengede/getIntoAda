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

-- C interface for the Xlib surface extension

with Vulkan.C;
with Vulkan.Xlib;
with Vulkan.Xlib_Surfaces;

private package Vulkan.Xlib_Surfaces_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Xlib_Surface_Create_Info_Type;

    -- C interface records.
    type Xlib_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Xlib_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Xlib_Surfaces.Create_Flags;
        Dpy: Xlib.Display;
        Window: Xlib.Window;
    end record
        with Convention => C;

    type Xlib_Surface_Create_Info_C_Access is access Xlib_Surface_Create_Info_C
        with Convention => C;
   
    -- Conversion subprograms.
    function To_C(Struct: in Xlib_Surfaces.Create_Info)
        return Xlib_Surface_Create_Info_C;
    procedure Free(Struct: in out Xlib_Surface_Create_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Xlib_Surfaces_C;


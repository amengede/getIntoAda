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

-- C interface for the Metal surface extension

with Vulkan.C;
with Vulkan.Metal;
with Vulkan.Metal_Surfaces;

private package Vulkan.Metal_Surfaces_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Metal_Surface_Create_Info_Type;

    -- C interface records.
    type Metal_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Metal_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Metal_Surfaces.Create_Flags;
        Layer: Metal.Layer;
    end record
        with Convention => C;

    type Metal_Surface_Create_Info_C_Access is
        access Metal_Surface_Create_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Metal_Surfaces.Create_Info)
        return Metal_Surface_Create_Info_C;
    procedure Free(Struct: in out Metal_Surface_Create_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Metal_Surfaces_C;


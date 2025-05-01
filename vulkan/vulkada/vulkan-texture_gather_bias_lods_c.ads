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

-- C interface for the texture gather bias LOD extension

with Vulkan.C;

private package Vulkan.Texture_Gather_Bias_LODs_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Texture_LOD_Gather_Format_Properties_Type;

    -- C interface records.
    type Texture_LOD_Gather_Format_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Texture_LOD_Gather_Format_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Supports_Texture_Gather_LOD_Bias: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Texture_LOD_Gather_Format_Properties_C_Access is
        access Texture_LOD_Gather_Format_Properties_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada(Ada_Struct: in out Texture_LOD_Gather_Format_Properties;
                     C_Struct: in Texture_LOD_Gather_Format_Properties_C);

    -- Extension record conversion.
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Texture_Gather_Bias_LODs_C;


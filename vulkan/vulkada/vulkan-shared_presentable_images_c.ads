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

-- C interface for the shared presentable image extension

with Vulkan.C;

private package Vulkan.Shared_Presentable_Images_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Shared_Present_Surface_Capabilities_Type;

    -- C interface records.
    type Shared_Present_Surface_Capabilities_C is
    record
        Record_Type: Out_Structure_Type :=
            Shared_Present_Surface_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Shared_Present_Supported_Usage_Flags: Image_Usage_Flags;
    end record 
        with Convention => C;

    type Shared_Present_Surface_Capabilities_C_Access is
        access Shared_Present_Surface_Capabilities_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada(Ada_Struct: in out Shared_Present_Surface_Capabilities;
                     C_Struct: in Shared_Present_Surface_Capabilities_C);

    -- Extension record conversion.
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Shared_Presentable_Images_C;


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

-- Package for working with extension record lists

with Vulkan.C;

private package Vulkan.Extension_Records is
    -- Convert a list of extensions records into C format.
    -- Returns a pointer to the list head.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;

    -- Convert a C structure chain back to Ada.
    procedure To_Ada(Ada_Struct: in out Out_Structure_Access;
                     Next: in C.Out_Structure_C_Access);

    -- Deallocate a C extension list.
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Extension_Records;


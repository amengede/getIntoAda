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

-- Copyright 2025 Phaser Cat Games LLC

-- Generic package for handling C arrays

private generic
    type Element is private;
package Vulkan.C_Arrays is
    pragma Elaborate_Body;

    type Element_Array is array (Positive range <>) of aliased Element;
    type Array_Access is access all Element_Array;
    type Pointer is access all Element
        with Convention => C,
             Storage_Size => 0;
    type Free_Element is not null access procedure (E: in out Element);

    -- Allocate a new C array and return an access to it.
    function Allocate(Size: in Positive) return Array_Access
        with Post => Allocate'Result /= null;

    -- Deallocate the array.
    procedure Free(P: in out Pointer)
        with Post => P = null;
    procedure Free(P: in out Pointer; Release_Element: in Free_Element)
        with Post => P = null;
end Vulkan.C_Arrays;


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

-- Common data types for the Std_Video extensions

package Vulkan.Extensions.Std_Video is
    type Reserved_Array is array (Positive range <>) of Interfaces.Unsigned_8
        with Convention => C;

    type Reserved_16 is array (Positive range <>) of Interfaces.Unsigned_16
        with Convention => C;
end Vulkan.Extensions.Std_Video;


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

-- IMG extensions root package

package Vulkan.Extensions.IMG is
    -- Records.
    type Physical_Device_Relaxed_Line_Rasterization_Features is
        new Out_Structure
            (Physical_Device_Relaxed_Line_Rasterization_Features_Type) with
    record
        Relaxed_Line_Rasterization: Boolean;
    end record;
end Vulkan.Extensions.IMG;


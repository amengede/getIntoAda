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

-- Operations for the clip space W scaling extension

with Vulkan.Extensions.NV;

package Vulkan.Extensions.NV_Clip_Space_W_Scaling is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdSetViewportWScalingNV
    procedure Set_Viewport_W_Scaling
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Viewport: in Interfaces.Unsigned_32;
         Viewport_W_Scaling: in NV.Viewport_W_Scaling)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    procedure Set_Viewport_W_Scaling
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Viewport: in Interfaces.Unsigned_32;
         Viewport_W_Scalings: in NV.Viewport_W_Scaling_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Viewport_W_Scalings.Is_Empty;
end Vulkan.Extensions.NV_Clip_Space_W_Scaling;


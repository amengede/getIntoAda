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

-- Operations for the ray tracing maintenance 1 extension

package Vulkan.Ray_Tracing_Maintenance_1 is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdTraceRaysIndirect2KHR
    procedure Trace_Rays_Indirect_2(Command_Buffer: in Vulkan.Command_Buffer;
                                    Indirect_Device_Address: in Device_Address)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;
end Vulkan.Ray_Tracing_Maintenance_1;


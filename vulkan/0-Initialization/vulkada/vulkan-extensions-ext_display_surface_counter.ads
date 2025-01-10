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

-- Operations for the display surface counter extension

with Vulkan.Extensions.EXT;
with Vulkan.Extensions.KHR;

package Vulkan.Extensions.EXT_Display_Surface_Counter is
    use type KHR.Surface;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceSurfaceCapabilities2EXT
    function Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Capabilities: in out EXT.Surface_Capabilities_2) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface,
             Post => Get_Surface_Capabilities'Result in Success |
                                                        Out_Of_Host_Memory |
                                                        Out_Of_Device_Memory |
                                                        Surface_Lost;

    procedure Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Capabilities: in out EXT.Surface_Capabilities_2)
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;

    function Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return EXT.Surface_Capabilities_2
        with Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;
end Vulkan.Extensions.EXT_Display_Surface_Counter;


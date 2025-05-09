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

-- Operations for the get surface capabilities 2 extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Get_Surface_Capabilities_2 is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceSurfaceCapabilities2KHR
    function Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in KHR.Physical_Device_Surface_Info_2;
         Surface_Capabilities: in out KHR.Surface_Capabilities_2) return Result
        with Pre => Physical_Device /= No_Physical_Device,
             Post => Get_Surface_Capabilities'Result in Success |
                                                        Out_Of_Host_Memory |
                                                        Out_Of_Device_Memory |
                                                        Surface_Lost;

    function Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in KHR.Physical_Device_Surface_Info_2)
        return KHR.Surface_Capabilities_2
        with Inline,
             Pre => Physical_Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceSurfaceFormats2KHR
    function Surface_Format_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in KHR.Physical_Device_Surface_Info_2)
        return Interfaces.Unsigned_32
        with Pre => Physical_Device /= No_Physical_Device;

    function Get_Surface_Formats
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in KHR.Physical_Device_Surface_Info_2;
         Surface_Formats: in out KHR.Surface_Format_2_Vectors.Vector)
            return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    not Surface_Formats.Is_Empty,
             Post => Get_Surface_Formats'Result in Success |
                                                   Incomplete |
                                                   Out_Of_Host_Memory |
                                                   Out_Of_Device_Memory |
                                                   Surface_Lost;

    function Get_Surface_Formats
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in KHR.Physical_Device_Surface_Info_2)
        return KHR.Surface_Format_2_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;
end Vulkan.Extensions.KHR_Get_Surface_Capabilities_2;


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

-- Operations for the surface extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Surface is
    use type KHR.Surface;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkDestroySurfaceKHR
    procedure Destroy(Instance: in Vulkan.Instance;
                      Surface: in out KHR.Surface;
                      Allocator: aliased in Allocation_Callbacks)
        with Pre => Instance /= No_Instance,
             Post => Surface = KHR.No_Surface;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Surface: in out KHR.Surface)
        with Pre => Instance /= No_Instance,
             Post => Surface = KHR.No_Surface;

    -- vkGetPhysicalDeviceSurfaceSupportKHR
    function Get_Physical_Device_Surface_Support
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Surface: in KHR.Surface;
         Supported: out Boolean) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface,
             Post => Get_Physical_Device_Surface_Support'Result
                        in Success |
                           Out_Of_Host_Memory |
                           Out_Of_Device_Memory |
                           Surface_Lost;

    function Get_Physical_Device_Surface_Support
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Surface: in KHR.Surface) return Boolean
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;

    -- vkGetPhysicalDeviceSurfaceCapabilitiesKHR
    function Get_Physical_Device_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Capabilities: out KHR.Surface_Capabilities) return Result
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface,
             Post => Get_Physical_Device_Surface_Capabilities'Result
                        in Success |
                           Out_Of_Host_Memory |
                           Out_Of_Device_Memory |
                           Surface_Lost;

    function Get_Physical_Device_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return KHR.Surface_Capabilities
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;

    -- vkGetPhysicalDeviceSurfaceFormatsKHR
    function Surface_Format_Count(Physical_Device: in Vulkan.Physical_Device;
                                  Surface: in KHR.Surface)
        return Interfaces.Unsigned_32
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;

    function Get_Physical_Device_Surface_Formats
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Formats: in out KHR.Surface_Format_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface,
             Post => Get_Physical_Device_Surface_Formats'Result
                        in Success |
                           Incomplete |
                           Out_Of_Host_Memory |
                           Out_Of_Device_Memory |
                           Surface_Lost;

    function Get_Physical_Device_Surface_Formats
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return KHR.Surface_Format_Vectors.Vector
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;

    -- vkGetPhysicalDeviceSurfacePresentModesKHR
    function Surface_Present_Modes_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return Interfaces.Unsigned_32
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;

    function Get_Physical_Device_Surface_Present_Modes
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Formats: in out KHR.Present_Mode_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface,
             Post => Get_Physical_Device_Surface_Present_Modes'Result
                        in Success |
                           Incomplete |
                           Out_Of_Host_Memory |
                           Out_Of_Device_Memory |
                           Surface_Lost;

    function Get_Physical_Device_Surface_Present_Modes
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return KHR.Present_Mode_Vectors.Vector
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;
end Vulkan.Extensions.KHR_Surface;


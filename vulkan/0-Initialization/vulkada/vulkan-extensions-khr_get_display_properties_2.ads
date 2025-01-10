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

-- Operations for the get display properties 2 extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Get_Display_Properties_2 is
    use type KHR.Display;
    use type KHR.Display_Mode;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceDisplayProperties2KHR
    function Display_Properties_Count
        (Physical_Device: in Vulkan.Physical_Device)
        return Interfaces.Unsigned_32
        with Pre => Physical_Device /= No_Physical_Device;

    function Get_Display_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out KHR.Display_Properties_2_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    not Properties.Is_Empty,
             Post => Get_Display_Properties'Result in Success |
                                                      Incomplete |
                                                      Out_Of_Host_Memory |
                                                      Out_Of_Device_Memory;

    function Get_Display_Properties(Physical_Device: in Vulkan.Physical_Device)
        return KHR.Display_Properties_2_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceDisplayPlaneProperties2KHR
    function Display_Plane_Properties_Count
        (Physical_Device: in Vulkan.Physical_Device)
        return Interfaces.Unsigned_32
        with Pre => Physical_Device /= No_Physical_Device;

    function Get_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out KHR.Display_Plane_Properties_2_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    not Properties.Is_Empty,
             Post => Get_Display_Plane_Properties'Result in
                Success |
                Incomplete |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device)
        return KHR.Display_Plane_Properties_2_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkGetDisplayModeProperties2KHR
    function Display_Mode_Properties_Count
        (Physical_Device: in Vulkan.Physical_Device; Display: in KHR.Display)
        return Interfaces.Unsigned_32
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display;

    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in KHR.Display;
         Properties: in out KHR.Display_Mode_Properties_2_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display and
                    not Properties.Is_Empty,
             Post => Get_Display_Mode_Properties'Result in
                Success |
                Incomplete |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in KHR.Display)
        return KHR.Display_Mode_Properties_2_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display;

    -- vkGetDisplayPlaneCapabilities2KHR
    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Display_Plane_Info: in KHR.Display_Plane_Info_2;
         Capabilities: in out KHR.Display_Plane_Capabilities_2) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Display_Plane_Info.Mode /= KHR.No_Display_Mode,
             Post => Get_Display_Plane_Capabilities'Result in
                Success |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Display_Plane_Info: in KHR.Display_Plane_Info_2)
        return KHR.Display_Plane_Capabilities_2
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Display_Plane_Info.Mode /= KHR.No_Display_Mode;
end Vulkan.Extensions.KHR_Get_Display_Properties_2;


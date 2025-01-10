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

-- Operations for the display extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Display is
    use type KHR.Display;
    use type KHR.Display_Mode;
    use type KHR.Surface;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceDisplayPropertiesKHR
    function Get_Physical_Device_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out KHR.Display_Properties_Vectors.Vector) return Result
        with Pre => Physical_Device /= No_Physical_Device,
             Post => Get_Physical_Device_Properties'Result in
                Success |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Physical_Device_Properties
        (Physical_Device: in Vulkan.Physical_Device)
        return KHR.Display_Properties_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceDisplayPlanePropertiesKHR
    function Get_Physical_Device_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out KHR.Display_Plane_Properties_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device,
             Post => Get_Physical_Device_Display_Plane_Properties'Result in
                Success |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Physical_Device_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device)
        return KHR.Display_Plane_Properties_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkGetDisplayPlaneSupportedDisplaysKHR
    function Get_Display_Plane_Supported_Displays
        (Physical_Device: in Vulkan.Physical_Device;
         Plane_Index: in Interfaces.Unsigned_32;
         Displays: in out KHR.Display_Vectors.Vector) return Result
        with Pre => Physical_Device /= No_Physical_Device,
             Post => Get_Display_Plane_Supported_Displays'Result in
                Success |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Display_Plane_Supported_Displays
        (Physical_Device: in Vulkan.Physical_Device;
         Plane_Index: in Interfaces.Unsigned_32)
        return KHR.Display_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkGetDisplayModePropertiesKHR
    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in KHR.Display;
         Properties: in out KHR.Display_Mode_Properties_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display,
             Post => Get_Display_Mode_Properties'Result in Success |
                                                           Out_Of_Host_Memory |
                                                           Out_Of_Device_Memory;

    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in KHR.Display)
        return KHR.Display_Mode_Properties_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display;

    -- vkCreateDisplayModeKHR
    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Mode: out KHR.Display_Mode) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed and
                     (if Create'Result = Success then
                        Mode /= KHR.No_Display_Mode);

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Display_Mode
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display,
             Post => Create'Result /= KHR.No_Display_Mode;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info;
                    Mode: out KHR.Display_Mode) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed and
                     (if Create'Result = Success then
                        Mode /= KHR.No_Display_Mode);

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info)
        return KHR.Display_Mode
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display,
             Post => Create'Result /= KHR.No_Display_Mode;

    -- vkGetDisplayPlaneCapabilitiesKHR
    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Mode: in KHR.Display_Mode;
         Plane_Index: in Interfaces.Unsigned_32;
         Capabilities: out KHR.Display_Plane_Capabilities) return Result
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Mode /= KHR.No_Display_Mode,
             Post => Get_Display_Plane_Capabilities'Result in
                Success |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Mode: in KHR.Display_Mode;
         Plane_Index: in Interfaces.Unsigned_32)
        return KHR.Display_Plane_Capabilities
        with Pre => Physical_Device /= No_Physical_Device and
                    Mode /= KHR.No_Display_Mode;

    -- vkCreateDisplayPlaneSurfaceKHR
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out KHR.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Surface /= KHR.No_Surface);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= KHR.No_Surface;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info;
                    Surface: out KHR.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Surface /= KHR.No_Surface);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info)
        return KHR.Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= KHR.No_Surface;
end Vulkan.Extensions.KHR_Display;


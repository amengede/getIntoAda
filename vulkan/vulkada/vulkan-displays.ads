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

-- Operations for the displays extension

package Vulkan.Displays is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceDisplayPropertiesKHR
    function Get_Physical_Device_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out Display_Properties_Vectors.Vector) return Result
        with Pre => Physical_Device /= No_Physical_Device,
             Post => Get_Physical_Device_Properties'Result in
                Success |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Physical_Device_Properties
        (Physical_Device: in Vulkan.Physical_Device)
        return Display_Properties_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceDisplayPlanePropertiesKHR
    function Get_Physical_Device_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out Display_Plane_Properties_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device,
             Post => Get_Physical_Device_Display_Plane_Properties'Result in
                Success |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Physical_Device_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device)
        return Display_Plane_Properties_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkGetDisplayPlaneSupportedDisplaysKHR
    function Get_Display_Plane_Supported_Displays
        (Physical_Device: in Vulkan.Physical_Device;
         Plane_Index: in Interfaces.Unsigned_32;
         Displays: in out Display_Vectors.Vector) return Result
        with Pre => Physical_Device /= No_Physical_Device,
             Post => Get_Display_Plane_Supported_Displays'Result in
                Success |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Display_Plane_Supported_Displays
        (Physical_Device: in Vulkan.Physical_Device;
         Plane_Index: in Interfaces.Unsigned_32)
        return Display_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkGetDisplayModePropertiesKHR
    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in Vulkan.Display;
         Properties: in out Display_Mode_Properties_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= No_Display,
             Post => Get_Display_Mode_Properties'Result in Success |
                                                           Out_Of_Host_Memory |
                                                           Out_Of_Device_Memory;

    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in Vulkan.Display)
        return Display_Mode_Properties_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= No_Display;

    -- vkCreateDisplayModeKHR
    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in Vulkan.Display;
                    Create_Info: in Display_Mode_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Mode: out Display_Mode) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= No_Display,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed and
                     (if Create'Result = Success then Mode /= No_Display_Mode);

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in Vulkan.Display;
                    Create_Info: in Display_Mode_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Display_Mode
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= No_Display,
             Post => Create'Result /= No_Display_Mode;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in Vulkan.Display;
                    Create_Info: in Display_Mode_Create_Info;
                    Mode: out Display_Mode) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= No_Display,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed and
                     (if Create'Result = Success then Mode /= No_Display_Mode);

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in Vulkan.Display;
                    Create_Info: in Display_Mode_Create_Info)
        return Display_Mode
        with Pre => Physical_Device /= No_Physical_Device and
                    Display /= No_Display,
             Post => Create'Result /= No_Display_Mode;

    -- vkGetDisplayPlaneCapabilitiesKHR
    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Mode: in Display_Mode;
         Plane_Index: in Interfaces.Unsigned_32;
         Capabilities: out Display_Plane_Capabilities) return Result
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Mode /= No_Display_Mode,
             Post => Get_Display_Plane_Capabilities'Result in
                Success |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Mode: in Display_Mode;
         Plane_Index: in Interfaces.Unsigned_32)
        return Display_Plane_Capabilities
        with Pre => Physical_Device /= No_Physical_Device and
                    Mode /= No_Display_Mode;

    -- vkCreateDisplayPlaneSurfaceKHR
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Display_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out Vulkan.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Display_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks) return Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= No_Surface;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Display_Surface_Create_Info;
                    Surface: out Vulkan.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Display_Surface_Create_Info) return Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= No_Surface;
end Vulkan.Displays;


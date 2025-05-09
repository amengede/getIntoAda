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

-- Operations for the display control extension

with Vulkan.Extensions.KHR;
with Vulkan.Extensions.EXT;

package Vulkan.Extensions.EXT_Display_Control is
    use type KHR.Display;
    use type KHR.Swapchain;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkDisplayPowerControlEXT
    function Display_Power_Control
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Power_Info: in EXT.Display_Power_Info) return Result
        with Pre => Device /= No_Device and
                    Display /= KHR.No_Display,
             Post => Display_Power_Control'Result in Success |
                                                     Out_Of_Host_Memory;

    procedure Display_Power_Control
         (Device: in Vulkan.Device;
          Display: in KHR.Display;
          Display_Power_Info: in EXT.Display_Power_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Display /= KHR.No_Display;

    -- vkRegisterDeviceEventEXT
    function Register_Device_Event(Device: in Vulkan.Device;
                                   Device_Event_Info: in EXT.Device_Event_Info;
                                   Allocator: aliased in Allocation_Callbacks;
                                   Fence: out Vulkan.Fence) return Result
        with Pre => Device /= No_Device,
             Post => Register_Device_Event'Result in Success |
                                                     Out_Of_Host_Memory;
    function Register_Device_Event(Device: in Vulkan.Device;
                                   Device_Event_Info: in EXT.Device_Event_Info;
                                   Allocator: aliased in Allocation_Callbacks)
        return Fence
        with Pre => Device /= No_Device,
             Post => Register_Device_Event'Result /= No_Fence;

    function Register_Device_Event(Device: in Vulkan.Device;
                                   Device_Event_Info: in EXT.Device_Event_Info;
                                   Fence: out Vulkan.Fence) return Result
        with Pre => Device /= No_Device,
             Post => Register_Device_Event'Result in Success |
                                                     Out_Of_Host_Memory;

    function Register_Device_Event(Device: in Vulkan.Device;
                                   Device_Event_Info: in EXT.Device_Event_Info)
        return Fence
        with Pre => Device /= No_Device,
             Post => Register_Device_Event'Result /= No_Fence;

    -- vkRegisterDisplayEventEXT
    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info;
         Allocator: aliased in Allocation_Callbacks;
         Fence: out Vulkan.Fence) return Result
        with Pre => Device /= No_Device and
                    Display /= KHR.No_Display,
             Post => Register_Display_Event'Result in Success |
                                                      Out_Of_Host_Memory;

    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info;
         Allocator: aliased in Allocation_Callbacks) return Fence
        with Pre => Device /= No_Device and
                    Display /= KHR.No_Display,
             Post => Register_Display_Event'Result /= No_Fence;

    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info;
         Fence: out Vulkan.Fence) return Result
        with Pre => Device /= No_Device and
                    Display /= KHR.No_Display,
             Post => Register_Display_Event'Result in Success |
                                                      Out_Of_Host_Memory;
    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info) return Fence
        with Pre => Device /= No_Device and
                    Display /= KHR.No_Display,
             Post => Register_Display_Event'Result /= No_Fence;

    -- vkGetSwapchainCounterEXT
    function Get_Swapchain_Counter(Device: in Vulkan.Device;
                                   Swapchain: in KHR.Swapchain;
                                   Counter: in EXT.Surface_Counter_Flags;
                                   Counter_Value: out Interfaces.Unsigned_64)
        return Result
        with Inline,
             Pre => Device /= No_Device and Swapchain /= KHR.No_Swapchain,
             Post => Get_Swapchain_Counter'Result in Success |
                                                     Out_Of_Host_Memory |
                                                     Device_Lost |
                                                     Out_Of_Date;

    function Get_Swapchain_Counter(Device: in Vulkan.Device;
                                   Swapchain: in KHR.Swapchain;
                                   Counter: in EXT.Surface_Counter_Flags)
        return Interfaces.Unsigned_64
        with Pre => Device /= No_Device and Swapchain /= KHR.No_Swapchain;
end Vulkan.Extensions.EXT_Display_Control;


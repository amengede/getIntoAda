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

-- Operations for the display control extension

with Vulkan.Core;
with Vulkan.C_EXT;
with Vulkan.Exceptions;

package body Vulkan.Extensions.EXT_Display_Control is
    -- Loaded extension functions.
    type vkDisplayPowerControlEXT_Access is
        access function(Device: in Vulkan.Device;
                        Display: in KHR.Display;
                        Display_Power_Info: in C_EXT.Display_Power_Info_C)
        return Result
        with Convention => C;

    vkDisplayPowerControlEXT: vkDisplayPowerControlEXT_Access;

    type vkRegisterDeviceEventEXT_Access is
        access function(Device: in Vulkan.Device;
                        Device_Event_Info: in C_EXT.Device_Event_Info_C;
                        Allocator: access constant Allocation_Callbacks;
                        Fence: out Vulkan.Fence) return Result
        with Convention => C;

    vkRegisterDeviceEventEXT: vkRegisterDeviceEventEXT_Access;

    type vkRegisterDisplayEventEXT_Access is
        access function(Device: in Vulkan.Device;
                        Display: in KHR.Display;
                        Display_Event_Info: in C_EXT.Display_Event_Info_C;
                        Allocator: access constant Allocation_Callbacks;
                        Fence: out Vulkan.Fence) return Result
        with Convention => C;

    vkRegisterDisplayEventEXT: vkRegisterDisplayEventEXT_Access;

    type vkGetSwapchainCounterEXT_Access is
        access function(Device: in Vulkan.Device;
                        Swapchain: in KHR.Swapchain;
                        Counter: in EXT.Surface_Counter_Flags;
                        Counter_Value: out Interfaces.Unsigned_64) return Result
        with Convention => C;

    vkGetSwapchainCounterEXT: vkGetSwapchainCounterEXT_Access;

    -- Common Register_Device_Event implementation.
    function Register_Device_Event
        (Device: in Vulkan.Device;
         Device_Event_Info: in EXT.Device_Event_Info;
         Allocator: access constant Allocation_Callbacks;
         Fence: out Vulkan.Fence) return Result;

    -- Common Register_Display_Event implementation.
    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info;
         Allocator: access constant Allocation_Callbacks;
         Fence: out Vulkan.Fence) return Result;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkDisplayPowerControlEXT_Access);
        procedure Load is new Load_Pointer(vkRegisterDeviceEventEXT_Access);
        procedure Load is new Load_Pointer(vkRegisterDisplayEventEXT_Access);
        procedure Load is new Load_Pointer(vkGetSwapchainCounterEXT_Access);
    begin
        Load(vkDisplayPowerControlEXT, "vkDisplayPowerControlEXT");
        Load(vkRegisterDeviceEventEXT, "vkRegisterDeviceEventEXT");
        Load(vkRegisterDisplayEventEXT, "vkRegisterDisplayEventEXT");
        Load(vkGetSwapchainCounterEXT, "vkGetSwapchainCounterEXT");
    end Load_Extension;

    function Display_Power_Control
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Power_Info: in EXT.Display_Power_Info) return Result is
        Info_C: C_EXT.Display_Power_Info_C := C_EXT.To_C(Display_Power_Info);
        Result: Vulkan.Result;
    begin
        Result := vkDisplayPowerControlEXT(Device, Display, Info_C);
        C_EXT.Free(Info_C);

        return Result;
    end Display_Power_Control;

    procedure Display_Power_Control
         (Device: in Vulkan.Device;
          Display: in KHR.Display;
          Display_Power_Info: in EXT.Display_Power_Info) is
    begin
        Exceptions.Check(Display_Power_Control(Device,
                                               Display,
                                               Display_Power_Info));
    end Display_Power_Control;

    function Register_Device_Event(Device: in Vulkan.Device;
                                   Device_Event_Info: in EXT.Device_Event_Info;
                                   Allocator: aliased in Allocation_Callbacks;
                                   Fence: out Vulkan.Fence) return Result is
    begin
        return Register_Device_Event(Device,
                                     Device_Event_Info,
                                     Allocator'Access,
                                     Fence);
    end Register_Device_Event;

    function Register_Device_Event
        (Device: in Vulkan.Device;
         Device_Event_Info: in EXT.Device_Event_Info;
         Allocator: aliased in Allocation_Callbacks) return Fence is
        F: Fence;
    begin
        Exceptions.Check(Register_Device_Event(Device,
                                               Device_Event_Info,
                                               Allocator'Access,
                                               F));

        return F;
    end Register_Device_Event;

    function Register_Device_Event(Device: in Vulkan.Device;
                                   Device_Event_Info: in EXT.Device_Event_Info;
                                   Fence: out Vulkan.Fence) return Result is
    begin
        return Register_Device_Event(Device, Device_Event_Info, null, Fence);
    end Register_Device_Event;

    function Register_Device_Event(Device: in Vulkan.Device;
                                   Device_Event_Info: in EXT.Device_Event_Info)
        return Fence is
        F: Fence;
    begin
        Exceptions.Check(Register_Device_Event(Device,
                                               Device_Event_Info,
                                               null,
                                               F));

        return F;
    end Register_Device_Event;

    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info;
         Allocator: aliased in Allocation_Callbacks;
         Fence: out Vulkan.Fence) return Result is
    begin
        return Register_Display_Event(Device,
                                      Display,
                                      Display_Event_Info,
                                      Allocator'Access,
                                      Fence);
    end Register_Display_Event;

    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info;
         Allocator: aliased in Allocation_Callbacks) return Fence is
        F: Fence;
    begin
        Exceptions.Check(Register_Display_Event(Device,
                                                Display,
                                                Display_Event_Info,
                                                Allocator'Access,
                                                F));

        return F;
    end Register_Display_Event;

    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info;
         Fence: out Vulkan.Fence) return Result is
    begin
        return Register_Display_Event(Device,
                                      Display,
                                      Display_Event_Info,
                                      null,
                                      Fence);
    end Register_Display_Event;

    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info) return Fence is
        F: Fence;
    begin
        Exceptions.Check(Register_Display_Event(Device,
                                                Display,
                                                Display_Event_Info,
                                                null,
                                                F));

        return F;
    end Register_Display_Event;

    function Get_Swapchain_Counter(Device: in Vulkan.Device;
                                   Swapchain: in KHR.Swapchain;
                                   Counter: in EXT.Surface_Counter_Flags;
                                   Counter_Value: out Interfaces.Unsigned_64)
        return Result is
    begin
        return vkGetSwapchainCounterEXT(Device,
                                        Swapchain,
                                        Counter,
                                        Counter_Value);
    end Get_Swapchain_Counter;

    function Get_Swapchain_Counter(Device: in Vulkan.Device;
                                   Swapchain: in KHR.Swapchain;
                                   Counter: in EXT.Surface_Counter_Flags)
        return Interfaces.Unsigned_64 is
        Value: Interfaces.Unsigned_64;
    begin
        Exceptions.Check(Get_Swapchain_Counter(Device,
                                               Swapchain,
                                               Counter,
                                               Value));

        return Value;
    end Get_Swapchain_Counter;

    function Register_Device_Event
        (Device: in Vulkan.Device;
         Device_Event_Info: in EXT.Device_Event_Info;
         Allocator: access constant Allocation_Callbacks;
         Fence: out Vulkan.Fence) return Result is
        Info_C: C_EXT.Device_Event_Info_C := C_EXT.To_C(Device_Event_Info);
        Result: Vulkan.Result;
    begin
        Result := vkRegisterDeviceEventEXT(Device, Info_C, Allocator, Fence);
        C_EXT.Free(Info_C);

        return Result;
    end Register_Device_Event;

    function Register_Display_Event
        (Device: in Vulkan.Device;
         Display: in KHR.Display;
         Display_Event_Info: in EXT.Display_Event_Info;
         Allocator: access constant Allocation_Callbacks;
         Fence: out Vulkan.Fence) return Result is
        Info_C: C_EXT.Display_Event_Info_C := C_EXT.To_C(Display_Event_Info);
        Result: Vulkan.Result;
    begin
        Result := vkRegisterDisplayEventEXT(Device,
                                            Display,
                                            Info_C,
                                            Allocator,
                                            Fence);
        C_EXT.Free(Info_C);

        return Result;
    end Register_Display_Event;
end Vulkan.Extensions.EXT_Display_Control;


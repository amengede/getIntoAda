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

-- Surface related subprograms

package body Vulkan.Surfaces is
    procedure Destroy(Instance: in Vulkan.Instance;
                      Surface: in out Vulkan.Surface;
                      Allocator: aliased in Allocation_Callbacks) is
    begin
        C.vkDestroySurfaceKHR(Instance, Surface, Allocator'Access);
        Surface := No_Surface;
    end Destroy;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Surface: in out Vulkan.Surface) is
    begin
        C.vkDestroySurfaceKHR(Instance, Surface, null);
        Surface := No_Surface;
    end Destroy;

    function Get_Support
        (Device: in Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Surface: in Vulkan.Surface) return Boolean is
        use type Interfaces.Unsigned_32;

        Supported: Interfaces.Unsigned_32 := 0;
    begin
        if C.vkGetPhysicalDeviceSurfaceSupportKHR
            (Device, Queue_Family_Index, Surface, Supported) /= Success then
            return False;
        end if;

        return Supported /= 0;
    end Get_Support;
    
    function Get_Capabilities
        (Device: in Physical_Device;
         Surface: in Vulkan.Surface) return Surface_Capabilities is
        Capabilities: Surface_Capabilities;
        Dummy: Result;
    begin
        Dummy := C.vkGetPhysicalDeviceSurfaceCapabilitiesKHR(Device,
                                                             Surface,
                                                             Capabilities);

        return Capabilities;
    end Get_Capabilities;

    function Get_Present_Modes
        (Device: in Vulkan.Device;
         Surface: in Vulkan.Surface) return Device_Group_Present_Mode_Flags is
        Flags: Device_Group_Present_Mode_Flags;
        Dummy: Result;
    begin
        Dummy := C.vkGetDeviceGroupSurfacePresentModesKHR(Device,
                                                          Surface,
                                                          Flags);

        return Flags;
    end Get_Present_Modes;
end Vulkan.Surfaces;


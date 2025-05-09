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

-- Operations for the Wayland surface extension

with Vulkan.Core;
with Vulkan.C_KHR;
with Vulkan.Utilities;
with Vulkan.Platform_Surfaces;

package body Vulkan.Extensions.KHR_Wayland_Surface is
    -- Loaded extension functions.
    type vkCreateWaylandSurfaceKHR_Access is
        access function
            (Instance: in Vulkan.Instance;
             Create_Info: in C_KHR.Wayland_Surface_Create_Info_C;
             Allocation: access constant Allocation_Callbacks;
             Surface: out KHR.Surface) return Result
        with Convention => C;

    vkCreateWaylandSurfaceKHR: vkCreateWaylandSurfaceKHR_Access;

    type vkGetPhysicalDeviceWaylandPresentationSupportKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Queue_Family_Index: in Vulkan.Queue_Family_Index;
                        Display: in Wayland.Display)
        return Interfaces.Unsigned_32
        with Convention => C;

    vkGetPhysicalDeviceWaylandPresentationSupportKHR:
        vkGetPhysicalDeviceWaylandPresentationSupportKHR_Access;

    -- Common surface implementation.
    package Surfaces_Common is
        new Platform_Surfaces(KHR.Wayland_Surface_Create_Info,
                              C_KHR.Wayland_Surface_Create_Info_C,
                              C_KHR.To_C,
                              C_KHR.Free,
                              vkCreateWaylandSurfaceKHR_Access,
                              vkCreateWaylandSurfaceKHR);

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCreateWaylandSurfaceKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceWaylandPresentationSupportKHR_Access);
    begin
        Load(vkCreateWaylandSurfaceKHR, "vkCreateWaylandSurfaceKHR");
        Load(vkGetPhysicalDeviceWaylandPresentationSupportKHR,
             "vkGetPhysicalDeviceWaylandPresentationSupportKHR");
    end Load_Extension;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Wayland_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out KHR.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Wayland_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Surface renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Wayland_Surface_Create_Info;
                    Surface: out KHR.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Wayland_Surface_Create_Info)
        return KHR.Surface renames Surfaces_Common.Create;
    
    function Get_Physical_Device_Presentation_Support
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Display: in Wayland.Display) return Boolean is
    begin
        return Utilities.To_Ada
            (vkGetPhysicalDeviceWaylandPresentationSupportKHR(Physical_Device,
                                                              Queue_Family_Index,
                                                              Display));
    end Get_Physical_Device_Presentation_Support;
end Vulkan.Extensions.KHR_Wayland_Surface;


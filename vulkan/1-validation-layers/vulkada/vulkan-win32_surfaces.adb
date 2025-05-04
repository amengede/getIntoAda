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

-- Operations for the Win32 surface extension

with Vulkan.Core;
with Vulkan.Win32_Surfaces_C;
with Vulkan.Utilities;
with Vulkan.Platform_Surfaces;

package body Vulkan.Win32_Surfaces is
    -- Loaded extension functions.
    type vkCreateWin32SurfaceKHR_Access is
        access function
            (Instance: in Vulkan.Instance;
             Create_Info: in Win32_Surfaces_C.Win32_Surface_Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Surface: out Vulkan.Surface) return Result
        with Convention => C;

    vkCreateWin32SurfaceKHR: vkCreateWin32SurfaceKHR_Access;

    type vkGetPhysicalDeviceWin32PresentationSupportKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Queue_Family_Index: in Vulkan.Queue_Family_Index)
        return Interfaces.Unsigned_32
        with Convention => C;

    vkGetPhysicalDeviceWin32PresentationSupportKHR:
        vkGetPhysicalDeviceWin32PresentationSupportKHR_Access;

    -- Common surface implementation.
    package Surfaces_Common is
        new Platform_Surfaces(Create_Info,
                              Win32_Surfaces_C.Win32_Surface_Create_Info_C,
                              Win32_Surfaces_C.To_C,
                              Win32_Surfaces_C.Free,
                              vkCreateWin32SurfaceKHR_Access,
                              vkCreateWin32SurfaceKHR);

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCreateWin32SurfaceKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceWin32PresentationSupportKHR_Access);
    begin
        Load(vkCreateWin32SurfaceKHR, "vkCreateWin32SurfaceKHR");
        Load(vkGetPhysicalDeviceWin32PresentationSupportKHR,
             "vkGetPhysicalDeviceWin32PresentationSupportKHR");
    end Load_Extension;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Win32_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out Vulkan.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Win32_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Surface renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Win32_Surfaces.Create_Info;
                    Surface: out Vulkan.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Win32_Surfaces.Create_Info) return Surface
        renames Surfaces_Common.Create;
    
    function Get_Physical_Device_Presentation_Support
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index) return Boolean is
    begin
        return Utilities.To_Ada
            (vkGetPhysicalDeviceWin32PresentationSupportKHR(Physical_Device,
                                                           Queue_Family_Index));
    end Get_Physical_Device_Presentation_Support;
end Vulkan.Win32_Surfaces;


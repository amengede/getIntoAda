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

-- Operations for the Xcb surface extension

with Vulkan.Core;
with Vulkan.Xcb_Surfaces_C;
with Vulkan.Utilities;
with Vulkan.Platform_Surfaces;

package body Vulkan.Xcb_Surfaces is
    -- Loaded extension functions.
    type vkCreateXcbSurfaceKHR_Access is
        access function
            (Instance: in Vulkan.Instance;
             Create_Info: in Xcb_Surfaces_C.Xcb_Surface_Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Surface: out Vulkan.Surface) return Result
        with Convention => C;

    vkCreateXcbSurfaceKHR: vkCreateXcbSurfaceKHR_Access;

    type vkGetPhysicalDeviceXcbPresentationSupportKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Queue_Family_Index: in Vulkan.Queue_Family_Index;
                        Connection: in Xcb.Connection;
                        Visual_ID: in Xcb.Visual_ID)
        return Interfaces.Unsigned_32
        with Convention => C;

    vkGetPhysicalDeviceXcbPresentationSupportKHR:
        vkGetPhysicalDeviceXcbPresentationSupportKHR_Access;

    -- Common surface implementation.
    package Surfaces_Common is
        new Platform_Surfaces(Create_Info,
                              Xcb_Surfaces_C.Xcb_Surface_Create_Info_C,
                              Xcb_Surfaces_C.To_C,
                              Xcb_Surfaces_C.Free,
                              vkCreateXcbSurfaceKHR_Access,
                              vkCreateXcbSurfaceKHR);

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCreateXcbSurfaceKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceXcbPresentationSupportKHR_Access);
    begin
        Load(vkCreateXcbSurfaceKHR, "vkCreateXcbSurfaceKHR");
        Load(vkGetPhysicalDeviceXcbPresentationSupportKHR,
             "vkGetPhysicalDeviceXcbPresentationSupportKHR");
    end Load_Extension;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Xcb_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out Vulkan.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Xcb_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Surface renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Xcb_Surfaces.Create_Info;
                    Surface: out Vulkan.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Xcb_Surfaces.Create_Info) return Surface
        renames Surfaces_Common.Create;
    
    function Get_Physical_Device_Presentation_Support
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Connection: in Xcb.Connection;
         Visual_ID: in Xcb.Visual_ID) return Boolean is
    begin
        return Utilities.To_Ada
            (vkGetPhysicalDeviceXcbPresentationSupportKHR(Physical_Device,
                                                          Queue_Family_Index,
                                                          Connection,
                                                          Visual_ID));
    end Get_Physical_Device_Presentation_Support;
end Vulkan.Xcb_Surfaces;


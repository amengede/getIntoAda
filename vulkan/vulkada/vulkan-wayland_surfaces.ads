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

-- Operations for the Wayland surface extension

with Vulkan.Wayland;

package Vulkan.Wayland_Surfaces is
    -- Extension bitfields.
    type Create_Flags is new Flags;

    Create_Flags_No_Bit: constant Create_Flags := 0;

    -- Extension records.
    type Create_Info is new In_Structure(Wayland_Surface_Create_Info_Type) with
    record
        Flags: Create_Flags := Create_Flags_No_Bit;
        Display: Wayland.Display;
        Surface: Wayland.Surface;
    end record;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCreateWaylandSurfaceKHR
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Wayland_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out Vulkan.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Surface /= No_Surface);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Wayland_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks) return Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= No_Surface;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Wayland_Surfaces.Create_Info;
                    Surface: out Vulkan.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Surface /= No_Surface);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Wayland_Surfaces.Create_Info) return Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= No_Surface;

    -- vkGetPhysicalDeviceWaylandPresentationSupportKHR
    function Get_Physical_Device_Presentation_Support
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Display: in Wayland.Display) return Boolean
        with Inline,
             Pre => Physical_Device /= No_Physical_Device;
end Vulkan.Wayland_Surfaces;


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

with Vulkan.Win32;

package Vulkan.Win32_Surfaces is
    -- Extension bitfields.
    type Create_Flags is new Flags;

    Create_Flags_No_Bit: constant Create_Flags := 0;

    -- Extension records.
    type Create_Info is new In_Structure(Win32_Surface_Create_Info_Type) with
    record
        Flags: Create_Flags := Create_Flags_No_Bit;
        Instance: Win32.Instance;
        Window: Win32.Window;
    end record;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCreateWin32SurfaceKHR
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Win32_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out Vulkan.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Surface /= No_Surface);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Win32_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks) return Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= No_Surface;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Win32_Surfaces.Create_Info;
                    Surface: out Vulkan.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Surface /= No_Surface);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Win32_Surfaces.Create_Info) return Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= No_Surface;

    -- vkGetPhysicalDeviceWin32PresentationSupportKHR
    function Get_Physical_Device_Presentation_Support
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index) return Boolean
        with Inline,
             Pre => Physical_Device /= No_Physical_Device;
end Vulkan.Win32_Surfaces;


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

-- Operations for the Metal surface extension

with Vulkan.Extensions.EXT;
with Vulkan.Extensions.KHR;
with Vulkan.Metal;

package Vulkan.Extensions.EXT_Metal_Surface is
    use type KHR.Surface;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCreateMetalSurfaceEXT
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Metal_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out KHR.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Native_Window_In_Use and
                     (if Create'Result = Success then
                        Surface /= KHR.No_Surface);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Metal_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= KHR.No_Surface;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Metal_Surface_Create_Info;
                    Surface: out KHR.Surface) return Result
        with Pre => Instance /= No_Instance,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Native_Window_In_Use and
                     (if Create'Result = Success then
                        Surface /= KHR.No_Surface);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Metal_Surface_Create_Info)
        return KHR.Surface
        with Pre => Instance /= No_Instance,
             Post => Create'Result /= KHR.No_Surface;
end Vulkan.Extensions.EXT_Metal_Surface;


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

-- Operations for the map memory 2 extension

with Interfaces.C.Pointers;
with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Map_Memory_2 is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkMapMemory2KHR
    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    function Map_2_With_Result(Device: in Vulkan.Device;
                               Memory_Map_Info: in KHR.Memory_Map_Info;
                               Data: out Pointers.Pointer) return Result
        with Pre => Device /= No_Device and
                    Memory_Map_Info.Memory /= No_Device_Memory and
                    Memory_Map_Info.Size /= 0,
             Post => Map_2_With_Result'Result in Success |
                                                 Out_Of_Host_Memory |
                                                 Out_Of_Device_Memory |
                                                 Memory_Map_Failed;

    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    function Map_2_With_Exception(Device: in Vulkan.Device;
                                  Memory_Map_Info: in KHR.Memory_Map_Info)
        return Pointers.Pointer
        with Pre => Device /= No_Device and
                    Memory_Map_Info.Memory /= No_Device_Memory and
                    Memory_Map_Info.Size /= 0,
             Post => Pointers."/="(Map_2_With_Exception'Result, null);

    -- vkUnmapMemory2KHR
    procedure Unmap(Device: in Vulkan.Device;
                    Memory_Unmap_Info: in KHR.Memory_Unmap_Info)
        with Pre => Device /= No_Device and
                    Memory_Unmap_Info.Memory /= No_Device_Memory;
end Vulkan.Extensions.KHR_Map_Memory_2;


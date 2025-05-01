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

-- Operations for the external memory FD extension

package Vulkan.External_Memory_FDs is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetMemoryFdKHR
    function Get_Memory_FD(Device: in Vulkan.Device;
                           Get_FD_Info: in Memory_Get_FD_Info;
                           FD: out File_Descriptor) return Result
        with Pre => Device /= No_Device and
                    Get_FD_Info.Memory /= No_Device_Memory and
                    Get_FD_Info.Handle_Type in
                        External_Memory_Handle_Type_Opaque_FD_Bit |
                        External_Memory_Handle_Type_DMA_Buf_Bit,
             Post => Get_Memory_FD'Result in Success |
                                             Too_Many_Objects |
                                             Out_Of_Host_Memory;

    function Get_Memory_FD(Device: in Vulkan.Device;
                           Get_FD_Info: in Memory_Get_FD_Info)
        return File_Descriptor
        with Pre => Device /= No_Device and
                    Get_FD_Info.Memory /= No_Device_Memory and
                    Get_FD_Info.Handle_Type in
                        External_Memory_Handle_Type_Opaque_FD_Bit |
                        External_Memory_Handle_Type_DMA_Buf_Bit;

    -- vkGetMemoryFdPropertiesKHR
    function Get_Memory_FD_Properties
        (Device: in Vulkan.Device;
         Handle_Type: in External_Memory_Handle_Type_Flags;
         FD: in File_Descriptor;
         Memory_FD_Properties: out Vulkan.Memory_FD_Properties) return Result
        with Pre => Device /= No_Device and
                    Handle_Type /= External_Memory_Handle_Type_Opaque_FD_Bit,
             Post => Get_Memory_FD_Properties'Result in Success |
                                                        Out_Of_Host_Memory |
                                                        Invalid_External_Handle;

    function Get_Memory_FD_Properties
        (Device: in Vulkan.Device;
         Handle_Type: in External_Memory_Handle_Type_Flags;
         FD: in File_Descriptor) return Memory_FD_Properties
        with Pre => Device /= No_Device and
                    Handle_Type /= External_Memory_Handle_Type_Opaque_FD_Bit;
end Vulkan.External_Memory_FDs;


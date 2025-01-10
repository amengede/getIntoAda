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

-- Operations for the external fence FD extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_External_Fence_FD is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkImportFenceFdKHR
    function Import_Fence_FD
        (Device: in Vulkan.Device;
         Import_Fence_FD_Info: in KHR.Import_Fence_FD_Info) return Result
        with Pre => Device /= No_Device and
                    Import_Fence_FD_Info.Fence /= No_Fence,
             Post => Import_Fence_FD'Result in Success |
                                               Out_Of_Host_Memory |
                                               Invalid_External_Handle;

    procedure Import_Fence_FD
        (Device: in Vulkan.Device;
         Import_Fence_FD_Info: in KHR.Import_Fence_FD_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Import_Fence_FD_Info.Fence /= No_Fence;

    -- vkGetFenceFdKHR
    function Get_Fence_FD(Device: in Vulkan.Device;
                          Get_FD_Info: in KHR.Fence_Get_FD_Info;
                          FD: out File_Descriptor) return Result
        with Pre => Device /= No_Device and
                    Get_FD_Info.Fence /= No_Fence,
             Post => Get_Fence_FD'Result in Success |
                                            Too_Many_Objects |
                                            Out_Of_Host_Memory;

    function Get_Fence_FD(Device: in Vulkan.Device;
                          Get_FD_Info: in KHR.Fence_Get_FD_Info)
        return File_Descriptor
        with Inline,
             Pre => Device /= No_Device and
                    Get_FD_Info.Fence /= No_Fence;
end Vulkan.Extensions.KHR_External_Fence_FD;


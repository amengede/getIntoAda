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

-- Operations for the external semaphore FD extension

package Vulkan.External_Semaphore_FDs is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkImportSemaphoreFdKHR
    function Import_Semaphore_FD
        (Device: in Vulkan.Device;
         Import_Semaphore_FD_Info: in Vulkan.Import_Semaphore_FD_Info)
        return Result
        with Pre => Device /= No_Device and
                    Import_Semaphore_FD_Info.Semaphore /= No_Semaphore,
             Post => Import_Semaphore_FD'Result in Success |
                                                   Out_Of_Host_Memory |
                                                   Invalid_External_Handle;

    procedure Import_Semaphore_FD
        (Device: in Vulkan.Device;
         Import_Semaphore_FD_Info: in Vulkan.Import_Semaphore_FD_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Import_Semaphore_FD_Info.Semaphore /= No_Semaphore;

    -- vkGetSemaphoreFdKHR
    function Get_Semaphore_FD(Device: in Vulkan.Device;
                              Get_FD_Info: in Semaphore_Get_FD_Info;
                              FD: out File_Descriptor) return Result
        with Pre => Device /= No_Device and
                    Get_FD_Info.Semaphore /= No_Semaphore,
             Post => Get_Semaphore_FD'Result in Success |
                                                Too_Many_Objects |
                                                Out_Of_Host_Memory;

    function Get_Semaphore_FD(Device: in Vulkan.Device;
                              Get_FD_Info: in Semaphore_Get_FD_Info)
        return File_Descriptor
        with Pre => Device /= No_Device and
                    Get_FD_Info.Semaphore /= No_Semaphore;
end Vulkan.External_Semaphore_FDs;


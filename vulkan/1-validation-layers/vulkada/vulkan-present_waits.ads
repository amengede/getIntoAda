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

-- Operations for the present wait extension

package Vulkan.Present_Waits is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkWaitForPresentKHR
    function Wait_For_Present(Device: in Vulkan.Device;
                              Swapchain: in Vulkan.Swapchain;
                              Present_ID, Timeout: in Interfaces.Unsigned_64)
        return Result
        with Inline,
             Pre => Device /= No_Device and Swapchain /= No_Swapchain,
             Post => Wait_For_Present'Result in Success |
                                                Vulkan.Timeout |
                                                Suboptimal |
                                                Out_Of_Host_Memory |
                                                Out_Of_Device_Memory |
                                                Device_Lost |
                                                Out_Of_Date |
                                                Surface_Lost |
                                                Full_Screen_Exclusive_Mode_Lost;
end Vulkan.Present_Waits;


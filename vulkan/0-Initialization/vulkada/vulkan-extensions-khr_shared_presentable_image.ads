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

-- Operations for the shared presentable image extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Shared_Presentable_Image is
    use type KHR.Swapchain;
    
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetSwapchainStatusKHR
    function Get_Swapchain_Status(Device: in Vulkan.Device;
                                  Swapchain: in KHR.Swapchain) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Swapchain /= KHR.No_Swapchain,
             Post => Get_Swapchain_Status'Result in
                Success |
                Suboptimal |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory |
                Device_Lost |
                Out_Of_Date |
                Surface_Lost |
                Full_Screen_Exclusive_Mode_Lost;
end Vulkan.Extensions.KHR_Shared_Presentable_Image;


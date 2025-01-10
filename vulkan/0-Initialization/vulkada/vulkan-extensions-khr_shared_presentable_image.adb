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

with Vulkan.Core;

package body Vulkan.Extensions.KHR_Shared_Presentable_Image is
    -- Load extension functions.
    type vkGetSwapchainStatusKHR_Access is
        access function(Device: in Vulkan.Device;
                        Swapchain: in KHR.Swapchain) return Result
        with Convention => C;

    vkGetSwapchainStatusKHR: vkGetSwapchainStatusKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkGetSwapchainStatusKHR_Access);
    begin
        Load(vkGetSwapchainStatusKHR, "vkGetSwapchainStatusKHR");
    end Load_Extension;

    function Get_Swapchain_Status(Device: in Vulkan.Device;
                                  Swapchain: in KHR.Swapchain)
        return Result is
    begin
        return vkGetSwapchainStatusKHR(Device, Swapchain);
    end Get_Swapchain_Status;
end Vulkan.Extensions.KHR_Shared_Presentable_Image;


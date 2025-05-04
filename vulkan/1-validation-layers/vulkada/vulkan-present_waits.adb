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

with Vulkan.Present_Waits_C;
with Vulkan.Core;

package body Vulkan.Present_Waits is
    -- Loaded extension functions.
    type vkWaitForPresentKHR_Access is
        access function(Device: in Vulkan.Device;
                        Swapchain: in Vulkan.Swapchain;
                        Present_ID, Timeout: in Interfaces.Unsigned_64)
        return Result
        with Convention => C;

    vkWaitForPresentKHR: vkWaitForPresentKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkWaitForPresentKHR_Access);
    begin
        Load(vkWaitForPresentKHR, "vkWaitForPresentKHR");
    end Load_Extension;

    function Wait_For_Present(Device: in Vulkan.Device;
                              Swapchain: in Vulkan.Swapchain;
                              Present_ID, Timeout: in Interfaces.Unsigned_64)
        return Result is
    begin
        return vkWaitForPresentKHR(Device, Swapchain, Present_ID, Timeout);
    end Wait_For_Present;
end Vulkan.Present_Waits;


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

-- Swapchain related subprograms

package body Vulkan.Swapchains is
    function Acquire_Next_Image(Device: in Vulkan.Device;
                                Acquire_Info: in Acquire_Next_Image_Info;
                                Image_Index: out Interfaces.Unsigned_32)
                                    return Result is
        C_Acquire_Info: C.Acquire_Next_Image_Info_C := C.To_C(Acquire_Info);
        Result: Vulkan.Result;
    begin
        Result := C.vkAcquireNextImage2KHR(Device,
                                           C_Acquire_Info,
                                           Image_Index);
        C.Free(C_Acquire_Info);

        return Result;
    end Acquire_Next_Image;
end Vulkan.Swapchains;


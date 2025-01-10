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

-- Operations for the display timing extension

with Vulkan.Extensions.GOOGLE;
with Vulkan.Extensions.KHR;

package Vulkan.Extensions.GOOGLE_Display_Timing is
    use type KHR.Swapchain;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetRefreshCycleDurationGOOGLE
    function Get_Refresh_Cycle_Duration
        (Device: in Vulkan.Device;
         Swapchain: in KHR.Swapchain;
         Display_Timing_Properties: out GOOGLE.Refresh_Cycle_Duration)
        return Result
        with Inline,
             Pre => Device /= No_Device and Swapchain /= KHR.No_Swapchain,
             Post => Get_Refresh_Cycle_Duration'Result in Success |
                                                          Out_Of_Host_Memory |
                                                          Device_Lost |
                                                          Surface_Lost;

    function Get_Refresh_Cycle_Duration(Device: in Vulkan.Device;
                                        Swapchain: in KHR.Swapchain)
         return GOOGLE.Refresh_Cycle_Duration
        with Pre => Device /= No_Device and Swapchain /= KHR.No_Swapchain;

    -- vkGetPastPresentationTimingGOOGLE
    function Get_Past_Presentation_Timing_Count(Device: in Vulkan.Device;
                                                Swapchain: in KHR.Swapchain)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Device and Swapchain /= KHR.No_Swapchain;

    function Get_Past_Presentation_Timing
        (Device: in Vulkan.Device;
         Swapchain: in KHR.Swapchain;
         Presentation_Timings:
            in out GOOGLE.Past_Presentation_Timing_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    Swapchain /= KHR.No_Swapchain and
                    not Presentation_Timings.Is_Empty,
             Post => Get_Past_Presentation_Timing'Result in Success |
                                                            Incomplete |
                                                            Out_Of_Host_Memory |
                                                            Device_Lost |
                                                            Out_Of_Date |
                                                            Surface_Lost;

    function Get_Past_Presentation_Timing(Device: in Vulkan.Device;
                                          Swapchain: in KHR.Swapchain)
        return GOOGLE.Past_Presentation_Timing_Vectors.Vector
        with Pre => Device /= No_Device and Swapchain /= KHR.No_Swapchain;
end Vulkan.Extensions.GOOGLE_Display_Timing;


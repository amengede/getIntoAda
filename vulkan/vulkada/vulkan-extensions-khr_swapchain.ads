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

-- Copyright 2025 Phaser Cat Games LLC

-- Operations for the swapchain extensions

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Swapchain is
    use type Interfaces.Unsigned_32;
    use type KHR.Swapchain;
    use type KHR.Surface;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCreateSwapchainKHR
    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Swapchain: out KHR.Swapchain) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Device_Lost |
                                      Surface_Lost |
                                      Native_Window_In_Use |
                                      Initialization_Failed and
                     (if Create'Result = Success then
                        Swapchain /= KHR.No_Swapchain);
     
    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Swapchain
        with Pre => Device /= No_Device,
             Post => Create'Result /= KHR.No_Swapchain;
               
    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info;
                    Swapchain: out KHR.Swapchain) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Device_Lost |
                                      Surface_Lost |
                                      Native_Window_In_Use |
                                      Initialization_Failed and
                     (if Create'Result = Success then
                        Swapchain /= KHR.No_Swapchain);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info)
        return KHR.Swapchain
        with Pre => Device /= No_Device,
             Post => Create'Result /= KHR.No_Swapchain;

    -- vkDestroySwapchainKHR
    procedure Destroy(Device: in Vulkan.Device;
                      Swapchain: in out KHR.Swapchain;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Swapchain = KHR.No_Swapchain;

    procedure Destroy(Device: in Vulkan.Device; Swapchain: in out KHR.Swapchain)
        with Inline,
             Pre => Device /= No_Device,
             Post => Swapchain = KHR.No_Swapchain;

    -- vkGetSwapchainImagesKHR
    function Swapchain_Images_Count(Device: in Vulkan.Device;
                                    Swapchain: in KHR.Swapchain)
        return Interfaces.Unsigned_32
        with Inline,
             Pre => Device /= No_Device and
                    Swapchain /= KHR.No_Swapchain;

    function Get_Swapchain_Images(Device: in Vulkan.Device;
                                  Swapchain: in KHR.Swapchain;
                                  Swapchain_Images: in out Image_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    Swapchain /= KHR.No_Swapchain,
             Post => Get_Swapchain_Images'Result in Success |
                                                    Incomplete |
                                                    Out_Of_Host_Memory |
                                                    Out_Of_Device_Memory and
                     (if Get_Swapchain_Images'Result = Success then
                        (for all Image of Swapchain_Images
                            => Image /= No_Image));

    function Get_Swapchain_Images(Device: in Vulkan.Device;
                                  Swapchain: in KHR.Swapchain)
        return Image_Vectors.Vector
        with Inline,
             Pre => Device /= No_Device and
                    Swapchain /= KHR.No_Swapchain,
             Post => (for all Image of Get_Swapchain_Images'Result =>
                        Image /= No_Image);

    -- vkAcquireNextImageKHR
    function Acquire_Next_Image(Device: in Vulkan.Device;
                                Swapchain: in KHR.Swapchain;
                                Timeout: in Interfaces.Unsigned_64;
                                Semaphore: in Vulkan.Semaphore;
                                Fence: in Vulkan.Fence;
                                Image_Index: out Interfaces.Unsigned_32)
        return Result
        with Inline,
             Pre => Device /= No_Device and
                    Swapchain /= KHR.No_Swapchain,
             Post => Acquire_Next_Image'Result
                        in Success |
                           Vulkan.Timeout |
                           Not_Ready |
                           Suboptimal |
                           Out_Of_Host_Memory |
                           Out_Of_Device_Memory |
                           Device_Lost |
                           Out_Of_Date |
                           Surface_Lost |
                           Full_Screen_Exclusive_Mode_Lost;

    function Acquire_Next_Image(Device: in Vulkan.Device;
                                Swapchain: in KHR.Swapchain;
                                Timeout: in Interfaces.Unsigned_64;
                                Semaphore: in Vulkan.Semaphore;
                                Fence: in Vulkan.Fence)
        return Interfaces.Unsigned_32
        with Inline,
             Pre => Device /= No_Device and
                    Swapchain /= KHR.No_Swapchain;

    -- vkQueuePresentKHR
    function Queue_Present(Queue: in Vulkan.Queue;
                           Present_Info: in KHR.Present_Info) return Result
        with Pre => Queue /= No_Queue and
                    (for all Semaphore of Present_Info.Wait_Semaphores
                        => Semaphore /= No_Semaphore) and
                    (for all Swapchain of Present_Info.Swapchains
                        => Swapchain /= KHR.No_Swapchain),
             Post => Queue_Present'Result in Success |
                                             Suboptimal |
                                             Out_Of_Host_Memory |
                                             Out_Of_Device_Memory |
                                             Device_Lost |
                                             Out_Of_Date |
                                             Surface_Lost |
                                             Full_Screen_Exclusive_Mode_Lost;

    procedure Queue_Present(Queue: in Vulkan.Queue;
                            Present_Info: in KHR.Present_Info)
        with Inline,
             Pre => Queue /= No_Queue and
                    (for all Semaphore of Present_Info.Wait_Semaphores
                        => Semaphore /= No_Semaphore) and
                    (for all Swapchain of Present_Info.Swapchains
                        => Swapchain /= KHR.No_Swapchain);

    -- vkGetDeviceGroupPresentCapabilitiesKHR
    function Get_Device_Group_Present_Capabilities
        (Device: in Vulkan.Device;
         Device_Group_Present_Capabilities:
            in out KHR.Device_Group_Present_Capabilities) return Result
        with Pre => Device /= No_Device,
             Post => Get_Device_Group_Present_Capabilities'Result
                in Success |
                   Out_Of_Host_Memory |
                   Out_Of_Device_Memory;

    procedure Get_Device_Group_Present_Capabilities
        (Device: in Vulkan.Device;
         Device_Group_Present_Capabilities:
            in out KHR.Device_Group_Present_Capabilities)
        with Inline,
             Pre => Device /= No_Device;

    function Get_Device_Group_Present_Capabilities(Device: in Vulkan.Device)
        return KHR.Device_Group_Present_Capabilities
        with Inline,
             Pre => Device /= No_Device;

    -- vkGetDeviceGroupSurfacePresentModesKHR
    function Get_Device_Group_Surface_Present_Modes
        (Device: in Vulkan.Device;
         Surface: in KHR.Surface;
         Modes: out KHR.Device_Group_Present_Mode_Flags) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Surface /= KHR.No_Surface,
             Post => Get_Device_Group_Surface_Present_Modes'Result
                        in Success |
                           Out_Of_Host_Memory |
                           Out_Of_Device_Memory |
                           Surface_Lost;

    function Get_Device_Group_Surface_Present_Modes(Device: in Vulkan.Device;
                                                    Surface: in KHR.Surface)
        return KHR.Device_Group_Present_Mode_Flags
        with Inline,
             Pre => Device /= No_Device and
                    Surface /= KHR.No_Surface;

    -- vkGetPhysicalDevicePresentRectanglesKHR
    function Present_Rectangles_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return Interfaces.Unsigned_32
        with Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;

    function Get_Physical_Device_Present_Rectangles
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Rects: in out Rect_2D_Vectors.Vector) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface,
             Post => Get_Physical_Device_Present_Rectangles'Result
                        in Success |
                           Incomplete |
                           Out_Of_Host_Memory |
                           Out_Of_Device_Memory;

    function Get_Physical_Device_Present_Rectangles
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return Rect_2D_Vectors.Vector
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Surface /= KHR.No_Surface;

    -- vkAcquireNextImage2KHR
    function Acquire_Next_Image_2(Device: in Vulkan.Device;
                                  Acquire_Info: in KHR.Acquire_Next_Image_Info;
                                  Image_Index: out Interfaces.Unsigned_32)
        return Result
        with Pre => Device /= No_Device and
                    Acquire_Info.Swapchain /= KHR.No_Swapchain and
                    (Acquire_Info.Semaphore /= No_Semaphore or
                     Acquire_Info.Fence /= No_Fence) and
                    Acquire_Info.Device_Mask /= 0,
             Post => Acquire_Next_Image_2'Result
                        in Success |
                           Timeout |
                           Not_Ready |
                           Suboptimal |
                           Out_Of_Host_Memory |
                           Out_Of_Device_Memory |
                           Device_Lost |
                           Out_Of_Date |
                           Surface_Lost |
                           Full_Screen_Exclusive_Mode_Lost;

    function Acquire_Next_Image_2(Device: in Vulkan.Device;
                                  Acquire_Info: in KHR.Acquire_Next_Image_Info)
        return Interfaces.Unsigned_32
        with Inline,
             Pre => Device /= No_Device and
                    Acquire_Info.Swapchain /= KHR.No_Swapchain and
                    (Acquire_Info.Semaphore /= No_Semaphore or
                     Acquire_Info.Fence /= No_Fence) and
                    Acquire_Info.Device_Mask /= 0;
end Vulkan.Extensions.KHR_Swapchain;


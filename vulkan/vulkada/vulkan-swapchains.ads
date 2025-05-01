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

private with Vulkan.Objects_Common;
private with Vulkan.C;
private with Vulkan.Utilities;

package Vulkan.Swapchains is
    -- vkCreateSwapchainKHR
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Swapchain: out Vulkan.Swapchain) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Device_Lost |
                                      Surface_Lost |
                                      Native_Window_In_Use |
                                      Initialization_Failed and
                     (if Create'Result = Success then
                        Swapchain /= No_Swapchain);
     
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Swapchain
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Swapchain;
               
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info;
                    Swapchain: out Vulkan.Swapchain) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Device_Lost |
                                      Surface_Lost |
                                      Native_Window_In_Use |
                                      Initialization_Failed and
                     (if Create'Result = Success then
                        Swapchain /= No_Swapchain);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info) return Swapchain
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Swapchain;

    -- vkDestroySwapchainKHR
    procedure Destroy(Device: in Vulkan.Device;
                      Swapchain: in out Vulkan.Swapchain;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Swapchain = No_Swapchain;

    procedure Destroy(Device: in Vulkan.Device;
                      Swapchain: in out Vulkan.Swapchain)
        with Inline,
             Pre => Device /= No_Device,
             Post => Swapchain = No_Swapchain;

    -- vkGetSwapchainImagesKHR
    function Get_Images(Device: in Vulkan.Device;
                        Swapchain: in Vulkan.Swapchain)
                            return Image_Vectors.Vector
        with Pre => Device /= No_Device and
                    Swapchain /= No_Swapchain;

    -- vkAcquireNextImageKHR
    function Acquire_Next_Image(Device: in Vulkan.Device;
                                Swapchain: in Vulkan.Swapchain;
                                Timeout: in Interfaces.Unsigned_64;
                                Semaphore: in Vulkan.Semaphore;
                                Fence: in Vulkan.Fence;
                                Image_Index: out Interfaces.Unsigned_32)
                                    return Result
        with Pre => Device /= No_Device and
                    Swapchain /= No_Swapchain,
             Post => Acquire_Next_Image'Result in
                        Success |
                        Vulkan.Timeout |
                        Not_Ready |
                        Suboptimal |
                        Out_Of_Host_Memory |
                        Out_Of_Device_Memory |
                        Device_Lost |
                        Out_Of_Date |
                        Surface_Lost |
                        Full_Screen_Exclusive_Mode_Lost;

    -- vkAcquireNextImage2KHR
    function Acquire_Next_Image(Device: in Vulkan.Device;
                                Acquire_Info: in Acquire_Next_Image_Info;
                                Image_Index: out Interfaces.Unsigned_32)
                                    return Result
        with Pre => Device /= No_Device,
             Post => Acquire_Next_Image'Result in
                        Success |
                        Timeout |
                        Not_Ready |
                        Suboptimal |
                        Out_Of_Host_Memory |
                        Out_Of_Device_Memory |
                        Device_Lost |
                        Out_Of_Date |
                        Surface_Lost |
                        Full_Screen_Exclusive_Mode_Lost;

private
    package Swapchains_Common is
        new Objects_Common(Swapchain_Create_Info,
                           C.Swapchain_Create_Info_C,
                           Swapchain,
                           No_Swapchain,
                           C.To_C,
                           C.Free,
                           C.vkCreateSwapchainKHR,
                           C.vkDestroySwapchainKHR);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Swapchain: out Vulkan.Swapchain) return Result
        renames Swapchains_Common.Create;
                    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Swapchain
        renames Swapchains_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info;
                    Swapchain: out Vulkan.Swapchain) return Result
        renames Swapchains_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info) return Swapchain
        renames Swapchains_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Swapchain: in out Vulkan.Swapchain;
                      Allocator: aliased in Allocation_Callbacks)
        renames Swapchains_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Swapchain: in out Vulkan.Swapchain)
        renames Swapchains_Common.Destroy;

    function Get_Images_Array is
        new Utilities.Get_Array_2(Device,
                                  Swapchain,
                                  Image_Vectors,
                                  C.vkGetSwapchainImagesKHR);

    function Get_Images(Device: in Vulkan.Device;
                        Swapchain: in Vulkan.Swapchain)
                            return Image_Vectors.Vector
        renames Get_Images_Array;

    function Acquire_Next_Image(Device: in Vulkan.Device;
                                Swapchain: in Vulkan.Swapchain;
                                Timeout: in Interfaces.Unsigned_64;
                                Semaphore: in Vulkan.Semaphore;
                                Fence: in Vulkan.Fence;
                                Image_Index: out Interfaces.Unsigned_32)
                                    return Result
        renames C.vkAcquireNextImageKHR;
end Vulkan.Swapchains;


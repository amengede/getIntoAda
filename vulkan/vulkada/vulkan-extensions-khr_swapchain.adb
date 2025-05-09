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

with Vulkan.Core;
with Vulkan.C_KHR;
with Vulkan.Exceptions;
with Vulkan.Objects_Common_Access;
with Vulkan.Extension_Records;

package body Vulkan.Extensions.KHR_Swapchain is
    -- Loaded extension functions.
    type vkCreateSwapchainKHR_Access is
        access function(Device: in Vulkan.Device;
                        Create_Info: in C_KHR.Swapchain_Create_Info_C;
                        Allocator: access constant Allocation_Callbacks;
                        Swapchain: out KHR.Swapchain) return Result
        with Convention => C;

    vkCreateSwapchainKHR: vkCreateSwapchainKHR_Access;

    type vkDestroySwapchainKHR_Access is
        access procedure(Device: in Vulkan.Device;
                         Swapchain: in KHR.Swapchain;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroySwapchainKHR: vkDestroySwapchainKHR_Access;

    type vkGetSwapchainImagesKHR_Access is
        access function(Device: in Vulkan.Device;
                        Swapchain: in KHR.Swapchain;
                        Swapchain_Image_Count: in out Interfaces.Unsigned_32;
                        Swapchain_Images: access Image) return Result
        with Convention => C;

    vkGetSwapchainImagesKHR: vkGetSwapchainImagesKHR_Access;

    type vkAcquireNextImageKHR_Access is
        access function(Device: in Vulkan.Device;
                        Swapchain: in KHR.Swapchain;
                        Timeout: in Interfaces.Unsigned_64;
                        Semaphore: in Vulkan.Semaphore;
                        Fence: in Vulkan.Fence;
                        ImageIndex: out Interfaces.Unsigned_32) return Result
        with Convention => C;

    vkAcquireNextImageKHR: vkAcquireNextImageKHR_Access;

    type vkQueuePresentKHR_Access is
        access function(Queue: in Vulkan.Queue;
                        Present_Info: in C_KHR.Present_Info_C) return Result
        with Convention => C;

    vkQueuePresentKHR: vkQueuePresentKHR_Access;

    type vkGetDeviceGroupPresentCapabilitiesKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Device_Group_Present_Capabilities:
                out C_KHR.Device_Group_Present_Capabilities_C) return Result
        with Convention => C;

    vkGetDeviceGroupPresentCapabilitiesKHR:
        vkGetDeviceGroupPresentCapabilitiesKHR_Access;

    type vkGetDeviceGroupSurfacePresentModesKHR_Access is
        access function(Device: in Vulkan.Device;
                        Surface: in KHR.Surface;
                        Modes: out KHR.Device_Group_Present_Mode_Flags)
        return Result
        with Convention => C;

    vkGetDeviceGroupSurfacePresentModesKHR:
        vkGetDeviceGroupSurfacePresentModesKHR_Access;

    type vkGetPhysicalDevicePresentRectanglesKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Surface: in KHR.Surface;
                        Rect_Count: in out Interfaces.Unsigned_32;
                        Rects: access Rect_2D) return Result
        with Convention => C;

    vkGetPhysicalDevicePresentRectanglesKHR:
        vkGetPhysicalDevicePresentRectanglesKHR_Access;

    type vkAcquireNextImage2KHR_Access is
        access function(Device: in Vulkan.Device;
                        Acquire_Info: in C_KHR.Acquire_Next_Image_Info_C;
                        Image_Index: out Interfaces.Unsigned_32) return Result
        with Convention => C;

    vkAcquireNextImage2KHR: vkAcquireNextImage2KHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCreateSwapchainKHR_Access);
        procedure Load is new Load_Pointer(vkDestroySwapchainKHR_Access);
        procedure Load is new Load_Pointer(vkGetSwapchainImagesKHR_Access);
        procedure Load is new Load_Pointer(vkAcquireNextImageKHR_Access);
        procedure Load is new Load_Pointer(vkQueuePresentKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetDeviceGroupPresentCapabilitiesKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetDeviceGroupSurfacePresentModesKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDevicePresentRectanglesKHR_Access);
        procedure Load is new Load_Pointer(vkAcquireNextImage2KHR_Access);
    begin
        Load(vkCreateSwapchainKHR, "vkCreateSwapchainKHR");
        Load(vkDestroySwapchainKHR, "vkDestroySwapchainKHR");
        Load(vkGetSwapchainImagesKHR, "vkGetSwapchainImagesKHR");
        Load(vkAcquireNextImageKHR, "vkAcquireNextImageKHR");
        Load(vkQueuePresentKHR, "vkQueuePresentKHR");
        Load(vkGetDeviceGroupPresentCapabilitiesKHR,
             "vkGetDeviceGroupPresentCapabilitiesKHR");
        Load(vkGetDeviceGroupSurfacePresentModesKHR,
             "vkGetDeviceGroupSurfacePresentModesKHR");
        Load(vkGetPhysicalDevicePresentRectanglesKHR,
             "vkGetPhysicalDevicePresentRectanglesKHR");
        Load(vkAcquireNextImage2KHR, "vkAcquireNextImage2KHR");
    end Load_Extension;

    package Swapchains_Common is new Objects_Common_Access
        (KHR.Swapchain_Create_Info,
         C_KHR.Swapchain_Create_Info_C,
         KHR.Swapchain,
         KHR.No_Swapchain,
         C_KHR.To_C,
         C_KHR.Free,
         vkCreateSwapchainKHR_Access,
         vkDestroySwapchainKHR_Access,
         vkCreateSwapchainKHR,
         vkDestroySwapchainKHR);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Swapchain: out KHR.Swapchain) return Result
        renames Swapchains_Common.Create;
     
    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Swapchain renames Swapchains_Common.Create;
               
    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info;
                    Swapchain: out KHR.Swapchain) return Result
        renames Swapchains_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info)
        return KHR.Swapchain renames Swapchains_Common.Create;
    
    procedure Destroy(Device: in Vulkan.Device;
                      Swapchain: in out KHR.Swapchain;
                      Allocator: aliased in Allocation_Callbacks)
        renames Swapchains_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device; Swapchain: in out KHR.Swapchain)
        renames Swapchains_Common.Destroy;

    function Swapchain_Images_Count(Device: in Vulkan.Device;
                                    Swapchain: in KHR.Swapchain)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(vkGetSwapchainImagesKHR(Device,
                                                 Swapchain,
                                                 Count,
                                                 null));

        return Count;
    end Swapchain_Images_Count;

    function Get_Swapchain_Images(Device: in Vulkan.Device;
                                  Swapchain: in KHR.Swapchain;
                                  Swapchain_Images: in out Image_Vectors.Vector)
        return Result is
        Count: Interfaces.Unsigned_32 := Swapchain_Images_Count(Device,
                                                                Swapchain);
        Images_C: array (1 .. Count) of aliased Image;
        Result: Vulkan.Result;
    begin
        if Count = 0 then
            Swapchain_Images.Clear;

            return Success;
        end if;

        Result := vkGetSwapchainImagesKHR(Device,
                                          Swapchain,
                                          Count,
                                          Images_C(1)'Access);

        if Result = Success then
            Swapchain_Images.Clear;

            for Image of Images_C loop
                Swapchain_Images.Append(Image);
            end loop;
        end if;
        
        return Result;
    end Get_Swapchain_Images;

    function Get_Swapchain_Images(Device: in Vulkan.Device;
                                  Swapchain: in KHR.Swapchain)
        return Image_Vectors.Vector is
        Images: Image_Vectors.Vector;
    begin
        Exceptions.Check(Get_Swapchain_Images(Device, Swapchain, Images));

        return Images;
    end Get_Swapchain_Images;

    function Acquire_Next_Image(Device: in Vulkan.Device;
                                Swapchain: in KHR.Swapchain;
                                Timeout: in Interfaces.Unsigned_64;
                                Semaphore: in Vulkan.Semaphore;
                                Fence: in Vulkan.Fence;
                                Image_Index: out Interfaces.Unsigned_32)
        return Result is
    begin
        return vkAcquireNextImageKHR(Device,
                                     Swapchain,
                                     Timeout,
                                     Semaphore,
                                     Fence,
                                     Image_Index);
    end Acquire_Next_Image;
    
    function Acquire_Next_Image(Device: in Vulkan.Device;
                                Swapchain: in KHR.Swapchain;
                                Timeout: in Interfaces.Unsigned_64;
                                Semaphore: in Vulkan.Semaphore;
                                Fence: in Vulkan.Fence)
        return Interfaces.Unsigned_32 is
        Image_Index: Interfaces.Unsigned_32;
    begin
        Exceptions.Check(Acquire_Next_Image(Device,
                                            Swapchain,
                                            Timeout,
                                            Semaphore,
                                            Fence,
                                            Image_Index));

        return Image_Index;
    end Acquire_Next_Image;

    function Queue_Present(Queue: in Vulkan.Queue;
                           Present_Info: in KHR.Present_Info) return Result is
        C_Info: C_KHR.Present_Info_C := C_KHR.To_C(Present_Info);
        Result: Vulkan.Result;
    begin
        Result := vkQueuePresentKHR(Queue, C_Info);
        C_KHR.Free(C_Info);

        return Result;
    end Queue_Present;

    procedure Queue_Present(Queue: in Vulkan.Queue;
                            Present_Info: in KHR.Present_Info) is
    begin
        Exceptions.Check(Queue_Present(Queue, Present_Info));
    end Queue_Present;

    function Get_Device_Group_Present_Capabilities
        (Device: in Vulkan.Device;
         Device_Group_Present_Capabilities:
            in out KHR.Device_Group_Present_Capabilities) return Result is
        C_Capabilities: C_KHR.Device_Group_Present_Capabilities_C;
        Result: Vulkan.Result;
    begin
        C_Capabilities.Next :=
            Extension_Records.To_C(Device_Group_Present_Capabilities.Next);
        Result := vkGetDeviceGroupPresentCapabilitiesKHR(Device,
                                                         C_Capabilities);
        
        if Result = Success then
            C_KHR.To_Ada(Device_Group_Present_Capabilities, C_Capabilities);
        end if;

        Extension_Records.Free(C_Capabilities.Next);

        return Result;
    end Get_Device_Group_Present_Capabilities;

    procedure Get_Device_Group_Present_Capabilities
        (Device: in Vulkan.Device;
         Device_Group_Present_Capabilities:
            in out KHR.Device_Group_Present_Capabilities) is
    begin
        Exceptions.Check
            (Get_Device_Group_Present_Capabilities
                (Device, Device_Group_Present_Capabilities));
    end Get_Device_Group_Present_Capabilities;

    function Get_Device_Group_Present_Capabilities(Device: in Vulkan.Device)
        return KHR.Device_Group_Present_Capabilities is
        Capabilities: KHR.Device_Group_Present_Capabilities;
    begin
        Exceptions.Check
            (Get_Device_Group_Present_Capabilities(Device, Capabilities));

        return Capabilities;
    end Get_Device_Group_Present_Capabilities;

    function Get_Device_Group_Surface_Present_Modes
        (Device: in Vulkan.Device;
         Surface: in KHR.Surface;
         Modes: out KHR.Device_Group_Present_Mode_Flags) return Result is
    begin
        return vkGetDeviceGroupSurfacePresentModesKHR(Device, Surface, Modes);
    end Get_Device_Group_Surface_Present_Modes;

    function Get_Device_Group_Surface_Present_Modes(Device: in Vulkan.Device;
                                                    Surface: in KHR.Surface)
        return KHR.Device_Group_Present_Mode_Flags is
        Flags: KHR.Device_Group_Present_Mode_Flags;
    begin
        Exceptions.Check(Get_Device_Group_Surface_Present_Modes(Device,
                                                                Surface,
                                                                Flags));

        return Flags;
    end Get_Device_Group_Surface_Present_Modes;

    function Present_Rectangles_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check
            (vkGetPhysicalDevicePresentRectanglesKHR(Physical_Device,
                                                     Surface,
                                                     Count,
                                                     null));

        return Count;
    end Present_Rectangles_Count;
    
    function Get_Physical_Device_Present_Rectangles
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Rects: in out Rect_2D_Vectors.Vector) return Result is
        Count: Interfaces.Unsigned_32 :=
            Present_Rectangles_Count(Physical_Device, Surface);
        C_Rects: array (1 .. Count) of aliased Rect_2D;
        Result: Vulkan.Result;
    begin
        if Count = 0 then
            Rects.Clear;

            return Success;
        end if;

        Result := vkGetPhysicalDevicePresentRectanglesKHR(Physical_Device,
                                                          Surface,
                                                          Count,
                                                          C_Rects(1)'Access);

        if Result = Success then
            Rects.Clear;

            for Rect of C_Rects loop
                Rects.Append(Rect);
            end loop;
        end if;

        return Result;
    end Get_Physical_Device_Present_Rectangles;

    function Get_Physical_Device_Present_Rectangles
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return Rect_2D_Vectors.Vector is
        Rects: Rect_2D_Vectors.Vector;
    begin
        Exceptions.Check
            (Get_Physical_Device_Present_Rectangles(Physical_Device,
                                                    Surface,
                                                    Rects));

        return Rects;
    end Get_Physical_Device_Present_Rectangles;

    function Acquire_Next_Image_2(Device: in Vulkan.Device;
                                  Acquire_Info: in KHR.Acquire_Next_Image_Info;
                                  Image_Index: out Interfaces.Unsigned_32)
        return Result is
        C_Info: C_KHR.Acquire_Next_Image_Info_C := C_KHR.To_C(Acquire_Info);
        Result: Vulkan.Result;
    begin
        Result := vkAcquireNextImage2KHR(Device, C_Info, Image_Index);
        C_KHR.Free(C_Info);

        return Result;
    end Acquire_Next_Image_2;

    function Acquire_Next_Image_2(Device: in Vulkan.Device;
                                  Acquire_Info: in KHR.Acquire_Next_Image_Info)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32;
    begin
        Exceptions.Check(Acquire_Next_Image_2(Device, Acquire_Info, Count));

        return Count;
    end Acquire_Next_Image_2;
end Vulkan.Extensions.KHR_Swapchain;


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

-- Image related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;
private with Vulkan.Utilities;

package Vulkan.Images is
    -- vkCreateImage
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Image: out Vulkan.Image) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Image /= No_Image);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Image
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Image;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_Create_Info;
                    Image: out Vulkan.Image) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Image /= No_Image);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_Create_Info) return Image
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Image;

    -- vkDestroyImage
    procedure Destroy(Device: in Vulkan.Device;
                      Image: in out Vulkan.Image;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Image = No_Image;

    procedure Destroy(Device: in Vulkan.Device;
                      Image: in out Vulkan.Image)
        with Inline,
             Pre => Device /= No_Device,
             Post => Image = No_Image;

    -- vkGetImageSubresourceLayout
    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Image: in Vulkan.Image;
                                     Subresource: in Image_Subresource;
                                     Layout: out Subresource_Layout)
        with Inline,
             Pre => Device /= No_Device and Image /= No_Image;

    -- vkBindImageMemory
    function Bind(Device: in Vulkan.Device;
                  Image: in Vulkan.Image;
                  Memory: in Device_Memory;
                  Offset: in Device_Size) return Result
        with Pre => Device /= No_Device and
                    Image /= No_Image and
                    Memory /= No_Device_Memory,
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory;

    procedure Bind(Device: in Vulkan.Device;
                   Image: in Vulkan.Image;
                   Memory: in Device_Memory;
                   Offset: in Device_Size)
        with Inline,
             Pre => Device /= No_Device and
                    Image /= No_Image and
                    Memory /= No_Device_Memory;

    -- vkGetImageMemoryRequirements
    function Get_Memory_Requirements(Device: in Vulkan.Device;
                                     Image: in Vulkan.Image)
        return Memory_Requirements
        with Inline,
             Pre => Device /= No_Device and Image /= No_Image;

    -- vkGetImageSparseMemoryRequirements
    function Get_Sparse_Memory_Requirements(Device: in Vulkan.Device;
                                            Image: in Vulkan.Image)
        return Sparse_Image_Memory_Requirements_Vectors.Vector
        with Pre => Device /= No_Device and Image /= No_Image;

    -- Vulkan 1.1
    -- vkGetImageMemoryRequirements2
    procedure Get_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Image_Memory_Requirements_Info_2;
         Requirements: in out Memory_Requirements_2)
        with Pre => Device /= No_Device and Info.Image /= No_Image;

    function Get_Memory_Requirements(Device: in Vulkan.Device;
                                     Info: in Image_Memory_Requirements_Info_2)
        return Memory_Requirements_2
        with Pre => Device /= No_Device and Info.Image /= No_Image;

    -- vkGetImageSparseMemoryRequirements2 
    function Get_Sparse_Memory_Requirements_Count
        (Device: in Vulkan.Device;
         Info: in Image_Sparse_Memory_Requirements_Info_2)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Device and Info.Image /= No_Image;

    procedure Get_Sparse_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Image_Sparse_Memory_Requirements_Info_2;
         Requirements:
            in out Sparse_Image_Memory_Requirements_2_Vectors.Vector)
        with Pre => Device /= No_Device and Info.Image /= No_Image;

    function Get_Sparse_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Image_Sparse_Memory_Requirements_Info_2)
            return Sparse_Image_Memory_Requirements_2_Vectors.Vector
        with Pre => Device /= No_Device and Info.Image /= No_Image;

    -- Vulkan 1.4
    -- vkGetImageSubresourceLayout2
    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Image: in Vulkan.Image;
                                     Info: in Image_Subresource_2;
                                     Layout: in out Subresource_Layout_2)
        with Pre => Device /= No_Device and
                    Image /= No_Image;

    function Get_Subresource_Layout(Device: in Vulkan.Device;
                                    Image: in Vulkan.Image;
                                    Info: in Image_Subresource_2)
        return Subresource_Layout_2
        with Inline,
             Pre => Device /= No_Device and
                    Image /= No_Image;

    -- vkCopyMemoryToImage
    function Copy_Memory_To_Image
        (Device: in Vulkan.Device;
         Copy_Memory_To_Image_Info: in Vulkan.Copy_Memory_To_Image_Info)
        return Result
        with Pre => Device /= No_Device and
                    Copy_Memory_To_Image_Info.Dst_Image /= No_Image and
                    not Copy_Memory_To_Image_Info.Regions.Is_Empty,
             Post => Copy_Memory_To_Image'Result in Success |
                                                    Initialization_Failed |
                                                    Out_Of_Host_Memory |
                                                    Out_Of_Device_Memory |
                                                    Memory_Map_Failed;
 
    procedure Copy_Memory_To_Image
        (Device: in Vulkan.Device;
         Copy_Memory_To_Image_Info: in Vulkan.Copy_Memory_To_Image_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Copy_Memory_To_Image_Info.Dst_Image /= No_Image and
                    not Copy_Memory_To_Image_Info.Regions.Is_Empty;

    -- vkCopyImageToMemory
    function Copy_Image_To_Memory
        (Device: in Vulkan.Device;
         Copy_Image_To_Memory_Info: in Vulkan.Copy_Image_To_Memory_Info)
        return Result
        with Pre => Device /= No_Device and
                    Copy_Image_To_Memory_Info.Src_Image /= No_Image and
                    not Copy_Image_To_Memory_Info.Regions.Is_Empty,
             Post => Copy_Image_To_Memory'Result in Success |
                                                    Initialization_Failed |
                                                    Out_Of_Host_Memory |
                                                    Out_Of_Device_Memory |
                                                    Memory_Map_Failed;
 
    procedure Copy_Image_To_Memory
        (Device: in Vulkan.Device;
         Copy_Image_To_Memory_Info: in Vulkan.Copy_Image_To_Memory_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Copy_Image_To_Memory_Info.Src_Image /= No_Image and
                    not Copy_Image_To_Memory_Info.Regions.Is_Empty;

    -- vkCopyImageToImage
    function Copy_Image_To_Image
        (Device: in Vulkan.Device;
         Copy_Image_To_Image_Info: in Vulkan.Copy_Image_To_Image_Info)
        return Result
        with Pre => Device /= No_Device and
                    Copy_Image_To_Image_Info.Src_Image /= No_Image and
                    Copy_Image_To_Image_Info.Dst_Image /= No_Image and
                    not Copy_Image_To_Image_Info.Regions.Is_Empty,
             Post => Copy_Image_To_Image'Result in Success |
                                                    Initialization_Failed |
                                                    Out_Of_Host_Memory |
                                                    Out_Of_Device_Memory |
                                                    Memory_Map_Failed;
 
    procedure Copy_Image_To_Image
        (Device: in Vulkan.Device;
         Copy_Image_To_Image_Info: in Vulkan.Copy_Image_To_Image_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Copy_Image_To_Image_Info.Src_Image /= No_Image and
                    Copy_Image_To_Image_Info.Dst_Image /= No_Image and
                    not Copy_Image_To_Image_Info.Regions.Is_Empty;

    -- vkTransitionImageLayout
    function Transition_Image_Layout
        (Device: in Vulkan.Device;
         Transitions: in Host_Image_Layout_Transition_Info_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    not Transitions.Is_Empty and
                    (for all Info of Transitions => Info.Image /= No_Image),
             Post => Transition_Image_Layout'Result in Success |
                                                       Initialization_Failed |
                                                       Out_Of_Host_Memory |
                                                       Out_Of_Device_Memory |
                                                       Memory_Map_Failed;

    function Transition_Image_Layout
        (Device: in Vulkan.Device;
         Transition: in Host_Image_Layout_Transition_Info) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Transition.Image /= No_Image,
             Post => Transition_Image_Layout'Result in Success |
                                                       Initialization_Failed |
                                                       Out_Of_Host_Memory |
                                                       Out_Of_Device_Memory |
                                                       Memory_Map_Failed;

    procedure Transition_Image_Layout
        (Device: in Vulkan.Device;
         Transitions: in Host_Image_Layout_Transition_Info_Vectors.Vector)
        with Inline,
             Pre => Device /= No_Device and
                    not Transitions.Is_Empty and
                    (for all Info of Transitions => Info.Image /= No_Image);

    procedure Transition_Image_Layout
        (Device: in Vulkan.Device;
         Transition: in Host_Image_Layout_Transition_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Transition.Image /= No_Image;

private
    package Images_Common is
        new Objects_Common(Image_Create_Info,
                           C.Image_Create_Info_C,
                           Image,
                           No_Image,
                           C.To_C,
                           C.Free,
                           C.vkCreateImage,
                           C.vkDestroyImage);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Image: out Vulkan.Image) return Result
        renames Images_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Image
        renames Images_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_Create_Info;
                    Image: out Vulkan.Image) return Result
        renames Images_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_Create_Info) return Image
        renames Images_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Image: in out Vulkan.Image;
                      Allocator: aliased in Allocation_Callbacks)
        renames Images_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Image: in out Vulkan.Image)
        renames Images_Common.Destroy;

    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Image: in Vulkan.Image;
                                     Subresource: in Image_Subresource;
                                     Layout: out Subresource_Layout)
        renames C.vkGetImageSubresourceLayout;

    function Bind(Device: in Vulkan.Device;
                  Image: in Vulkan.Image;
                  Memory: in Device_Memory;
                  Offset: in Device_Size) return Result
        renames C.vkBindImageMemory;

    function Get_Sparse_Image_Memory_Requirements_Array is
        new Utilities.Get_Array_2_Proc(Device,
                                       Image,
                                       Sparse_Image_Memory_Requirements_Vectors,
                                       C.vkGetImageSparseMemoryRequirements);

    function Get_Sparse_Memory_Requirements(Device: in Vulkan.Device;
                                            Image: in Vulkan.Image)
        return Sparse_Image_Memory_Requirements_Vectors.Vector
        renames Get_Sparse_Image_Memory_Requirements_Array;
end Vulkan.Images;


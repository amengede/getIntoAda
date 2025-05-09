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

-- Device routines

with Interfaces.C.Pointers;

private with Vulkan.C;

package Vulkan.Devices is
    use type Interfaces.Unsigned_32;

    -- vkGetDeviceProcAddr
    generic
        type Proc(<>) is private;
    function Get_Proc_Addr(Device: in Vulkan.Device;
                           Name: in String) return Proc
        with Pre => Device /= No_Device;

    -- vkCreateDevice
    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Create_Info: in Device_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Device: out Vulkan.Device) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    (for all QCI of Create_Info.Queue_Create_Infos =>
                        (for all Priority of QCI.Priorities =>
                            Priority in 0.0 .. 1.0)),
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed |
                                      Extension_Not_Present |
                                      Feature_Not_Present |
                                      Too_Many_Objects |
                                      Device_Lost and
                     (if Create'Result = Success then Device /= No_Device);

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Create_Info: in Device_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Device
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    (for all QCI of Create_Info.Queue_Create_Infos =>
                        (for all Priority of QCI.Priorities =>
                            Priority in 0.0 .. 1.0)),
             Post => Create'Result /= No_Device;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Create_Info: in Device_Create_Info;
                    Device: out Vulkan.Device) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    (for all QCI of Create_Info.Queue_Create_Infos =>
                        (for all Priority of QCI.Priorities =>
                            Priority in 0.0 .. 1.0)),
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed |
                                      Extension_Not_Present |
                                      Feature_Not_Present |
                                      Too_Many_Objects |
                                      Device_Lost and
                     (if Create'Result = Success then Device /= No_Device);

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Create_Info: in Device_Create_Info) return Device
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    (for all QCI of Create_Info.Queue_Create_Infos =>
                        (for all Priority of QCI.Priorities =>
                            Priority in 0.0 .. 1.0)),
             Post => Create'Result /= No_Device;

    -- vkDestroyDevice
    procedure Destroy(Device: in out Vulkan.Device;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Post => Device = No_Device;

    procedure Destroy(Device: in out Vulkan.Device)
        with Inline,
             Post => Device = No_Device;

    -- vkDeviceWaitIdle
    function Wait_Idle(Device: in Vulkan.Device) return Result
        with Pre => Device /= No_Device,
             Post => Wait_Idle'Result in Success |
                                         Out_Of_Host_Memory |
                                         Out_Of_Device_Memory |
                                         Device_Lost;

    procedure Wait_Idle(Device: in Vulkan.Device)
        with Inline,
             Pre => Device /= No_Device;

    -- vkAllocateMemory
    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Memory_Allocate_Info;
                      Allocator: aliased in Allocation_Callbacks;
                      Memory: out Device_Memory) return Result
        with Pre => Device /= No_Device,
             Post => Allocate'Result in Success |
                                        Out_Of_Host_Memory |
                                        Out_Of_Device_Memory |
                                        Invalid_External_Handle |
                                        Invalid_Opaque_Capture_Address;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Memory_Allocate_Info;
                      Allocator: aliased in Allocation_Callbacks)
                        return Device_Memory
        with Inline,
             Pre => Device /= No_Device,
             Post => Allocate'Result /= No_Device_Memory;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Memory_Allocate_Info;
                      Memory: out Device_Memory) return Result
        with Pre => Device /= No_Device,
             Post => Allocate'Result in Success |
                                        Out_Of_Host_Memory |
                                        Out_Of_Device_Memory |
                                        Invalid_External_Handle |
                                        Invalid_Opaque_Capture_Address;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Memory_Allocate_Info)
                        return Device_Memory
        with Inline,
             Pre => Device /= No_Device,
             Post => Allocate'Result /= No_Device_Memory;

    -- vkFreeMemory
    procedure Free(Device: in Vulkan.Device;
                   Memory: in out Device_Memory;
                   Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Memory = No_Device_Memory;

    procedure Free(Device: in Vulkan.Device;
                   Memory: in out Device_Memory)
        with Inline,
             Pre => Device /= No_Device,
             Post => Memory = No_Device_Memory;

    -- vkMapMemory
    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    function Map_With_Result(Device: in Vulkan.Device;
                             Memory: in Device_Memory;
                             Offset, Size: in Device_Size;
                             Data: out Pointers.Pointer) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Memory /= No_Device_Memory and
                    Size /= 0,
             Post => Map_With_Result'Result in Success |
                                               Out_Of_Host_Memory |
                                               Out_Of_Device_Memory |
                                               Memory_Map_Failed and
                     (if Map_With_Result'Result = Success then
                        Pointers."/="(Data, null));    

    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    function Map_With_Exception(Device: in Vulkan.Device;
                                Memory: in Device_Memory;
                                Offset, Size: in Device_Size)
                                    return Pointers.Pointer
        with Inline,
             Pre => Device /= No_Device and
                    Memory /= No_Device_Memory and
                    Size /= 0,
             Post => Pointers."/="(Map_With_Exception'Result, null);

    -- vkUnmapMemory
    procedure Unmap(Device: in Vulkan.Device; Memory: in Device_Memory)
        with Pre => Device /= No_Device and
                    Memory /= No_Device_Memory;

    -- vkFlushMappedMemoryRanges
    function Flush(Device: in Vulkan.Device;
                   Ranges: in Mapped_Memory_Range_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    not Ranges.Is_Empty,
             Post => Flush'Result in Success |
                                     Out_Of_Host_Memory |
                                     Out_Of_Device_Memory;
  
    procedure Flush(Device: in Vulkan.Device;
                    Ranges: in Mapped_Memory_Range_Vectors.Vector)
        with Inline,
             Pre => Device /= No_Device and
                    not Ranges.Is_Empty;

    function Flush(Device: in Vulkan.Device;
                   Memory_Range: in Mapped_Memory_Range) return Result
        with Pre => Device /= No_Device,
             Post => Flush'Result in Success |
                                     Out_Of_Host_Memory |
                                     Out_Of_Device_Memory;

    procedure Flush(Device: in Vulkan.Device;
                    Memory_Range: in Mapped_Memory_Range)
        with Inline,
             Pre => Device /= No_Device;

    -- vkInvalidateMappedMemoryRanges
    function Invalidate(Device: in Vulkan.Device;
                        Ranges: in Mapped_Memory_Range_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    not Ranges.Is_Empty,
             Post => Invalidate'Result in Success |
                                          Out_Of_Host_Memory |
                                          Out_Of_Device_Memory;

    procedure Invalidate(Device: in Vulkan.Device;
                         Ranges: in Mapped_Memory_Range_Vectors.Vector)
        with Inline,
             Pre => Device /= No_Device and
                    not Ranges.Is_Empty;

    function Invalidate(Device: in Vulkan.Device;
                        Memory_Range: in Mapped_Memory_Range)
        return Result
        with Pre => Device /= No_Device,
             Post => Invalidate'Result in Success |
                                          Out_Of_Host_Memory |
                                          Out_Of_Device_Memory;

    procedure Invalidate(Device: in Vulkan.Device;
                         Memory_Range: in Mapped_Memory_Range)
        with Inline,
             Pre => Device /= No_Device;

    -- vkGetDeviceMemoryCommitment
    function Get_Memory_Commitment(Device: in Vulkan.Device;
                                   Memory: in Device_Memory) return Device_Size
        with Inline,
             Pre => Device /= No_Device and
                    Memory /= No_Device_Memory;
    
    -- Vulkan 1.1
    -- vkGetDeviceGroupPeerMemoryFeatures
    function Get_Device_Group_Peer_Memory_Features
        (Device: in Vulkan.Device;
         Heap_Index,
         Local_Device_Index,
         Remote_Device_Index: in Interfaces.Unsigned_32)
            return Peer_Memory_Feature_Flags
        with Inline,
             Pre => Device /= No_Device and
                    Local_Device_Index /= Remote_Device_Index;

    -- vkGetDescriptorSetLayoutSupport
    procedure Get_Descriptor_Set_Layout_Support
        (Device: in Vulkan.Device;
         Create_Info: in Descriptor_Set_Layout_Create_Info;
         Support: in out Descriptor_Set_Layout_Support)
        with Pre => Device /= No_Device;

    function Get_Descriptor_Set_Layout_Support
        (Device: in Vulkan.Device;
         Create_Info: in Descriptor_Set_Layout_Create_Info)
        return Descriptor_Set_Layout_Support
        with Pre => Device /= No_Device;

    -- Vulkan 1.2
    -- vkGetBufferDeviceAddress
    function Get_Device_Address(Device: in Vulkan.Device;
                                Info: in Buffer_Device_Address_Info)
        return Device_Address
        with Pre => Device /= No_Device and Info.Buffer /= No_Buffer;

    -- vkGetBufferOpaqueCaptureAddress
    function Get_Opaque_Capture_Address(Device: in Vulkan.Device;
                                        Info: in Buffer_Device_Address_Info)
        return Interfaces.Unsigned_64
        with Pre => Device /= No_Device and Info.Buffer /= No_Buffer;

    -- vkGetDeviceMemoryOpaqueCaptureAddress
    function Get_Opaque_Capture_Address
        (Device: in Vulkan.Device;
         Info: in Device_Memory_Opaque_Capture_Address_Info)
        return Interfaces.Unsigned_64
        with Pre => Device /= No_Device and Info.Memory /= No_Device_Memory;

    -- Vulkan 1.3
    -- vkGetDeviceBufferMemoryRequirements
    procedure Get_Buffer_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Buffer_Memory_Requirements;
         Memory_Requirements: in out Memory_Requirements_2)
        with Pre => Device /= No_Device;

    function Get_Buffer_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Buffer_Memory_Requirements)
        return Memory_Requirements_2
        with Inline,
             Pre => Device /= No_Device;

    -- vkGetDeviceImageMemoryRequirements
    procedure Get_Image_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements;
         Memory_Requirements: in out Memory_Requirements_2)
        with Pre => Device /= No_Device;

    function Get_Image_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements)
        return Memory_Requirements_2
        with Inline,
             Pre => Device /= No_Device;

    -- vkGetDeviceImageSparseMemoryRequirements
    function Image_Sparse_Memory_Requirements_Count
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Device;

    procedure Get_Image_Sparse_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements;
         Sparse_Memory_Requirements:
            in out Sparse_Image_Memory_Requirements_2_Vectors.Vector)
        with Pre => Device /= No_Device;

    function Get_Image_Sparse_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements)
        return Sparse_Image_Memory_Requirements_2_Vectors.Vector
        with Pre => Device /= No_Device;

    -- Vulkan 1.4
    -- vkMapMemory2
    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    function Map_2_With_Result(Device: in Vulkan.Device;
                               Memory_Map_Info: in Vulkan.Memory_Map_Info;
                               Data: out Pointers.Pointer) return Result
        with Pre => Device /= No_Device and
                    Memory_Map_Info.Memory /= No_Device_Memory and
                    Memory_Map_Info.Size /= 0,
             Post => Map_2_With_Result'Result in Success |
                                                 Out_Of_Host_Memory |
                                                 Out_Of_Device_Memory |
                                                 Memory_Map_Failed;

    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    function Map_2_With_Exception(Device: in Vulkan.Device;
                                  Memory_Map_Info: in Vulkan.Memory_Map_Info)
        return Pointers.Pointer
        with Pre => Device /= No_Device and
                    Memory_Map_Info.Memory /= No_Device_Memory and
                    Memory_Map_Info.Size /= 0,
             Post => Pointers."/="(Map_2_With_Exception'Result, null);

    -- vkUnmapMemory2
    function Unmap(Device: in Vulkan.Device;
                   Memory_Unmap_Info: in Vulkan.Memory_Unmap_Info) return Result
        with Pre => Device /= No_Device and
                    Memory_Unmap_Info.Memory /= No_Device_Memory,
             Post => Unmap'Result in Success |
                                     Memory_Map_Failed;

    procedure Unmap(Device: in Vulkan.Device;
                    Memory_Unmap_Info: in Vulkan.Memory_Unmap_Info)
        with Pre => Device /= No_Device and
                    Memory_Unmap_Info.Memory /= No_Device_Memory;

    -- vkGetRenderingAreaGranularity
    function Get_Granularity(Device: in Vulkan.Device;
                             Rendering_Area_Info: in Vulkan.Rendering_Area_Info)
        return Extent_2D
        with Pre => Device /= No_Device;

    -- vkGetDeviceImageSubresourceLayout
    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Info: in Device_Image_Subresource_Info;
                                     Layout: in out Subresource_Layout_2)
        with Pre => Device /= No_Device;

    function Get_Subresource_Layout(Device: in Vulkan.Device;
                                    Info: in Device_Image_Subresource_Info)
        return Subresource_Layout_2
        with Inline,
             Pre => Device /= No_Device;

private
    function Wait_Idle(Device: in Vulkan.Device) return Result
        renames C.vkDeviceWaitIdle;
    
    procedure Unmap(Device: in Vulkan.Device; Memory: in Device_Memory)
        renames C.vkUnmapMemory;
end Vulkan.Devices;


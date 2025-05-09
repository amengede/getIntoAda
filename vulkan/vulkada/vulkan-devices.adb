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

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with Vulkan.Utilities;
with Vulkan.Exceptions;
with Vulkan.Extension_Records;
with Vulkan.C_V1_1;
with Vulkan.C_V1_2;
with Vulkan.C_V1_3;
with Vulkan.C_V1_4;

package body Vulkan.Devices is
    function Get_Proc_Addr(Device: in Vulkan.Device;
                           Name: in String) return Proc is
        function To_Proc is
            new Ada.Unchecked_Conversion(C.Void_Function, Proc);

        Raw_Proc: C.Void_Function;
        C_Name: aliased Interfaces.C.char_array := Interfaces.C.To_C(Name);
    begin
        Raw_Proc := C.vkGetDeviceProcAddr(Device, C_Name'Unchecked_Access);

        return To_Proc(Raw_Proc);
    end Get_Proc_Addr;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Create_Info: in Device_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Device: out Vulkan.Device) return Result is
        Create_Info_C: C.Device_Create_Info_C := C.To_C(Create_Info);
        Enabled_Layer_Names: Interfaces.C.Strings.chars_ptr_array :=
            Utilities.To_C(Create_Info.Enabled_Layer_Names);
        Enabled_Extension_Names: Interfaces.C.Strings.chars_ptr_array :=
            Utilities.To_C(Create_Info.Enabled_Extension_Names);
        Result: Vulkan.Result;
    begin
        if Enabled_Layer_Names'Length > 0 then
            Create_Info_C.Enabled_Layer_Names :=
                Enabled_Layer_Names
                    (Enabled_Layer_Names'First)'Unchecked_Access;
        end if;

        if Enabled_Extension_Names'Length > 0 then
            Create_Info_C.Enabled_Extension_Names :=
                Enabled_Extension_Names
                    (Enabled_Extension_Names'First)'Unchecked_Access;
        end if;

        Result := C.vkCreateDevice(Physical_Device,
                                   Create_Info_C,
                                   Allocator'Access,
                                   Device);
        Utilities.Free(Enabled_Layer_Names);
        Utilities.Free(Enabled_Extension_Names);
        C.Free(Create_Info_C);

        return Result;
    end Create;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Create_Info: in Device_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Device is
        D: Device;
    begin
        Exceptions.Check(Create(Physical_Device,
                                Create_Info,
                                Allocator,
                                D));

        return D;
    end Create;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Create_Info: in Device_Create_Info;
                    Device: out Vulkan.Device) return Result is
        Create_Info_C: C.Device_Create_Info_C := C.To_C(Create_Info);
        Enabled_Extension_Names: Interfaces.C.Strings.chars_ptr_array :=
            Utilities.To_C(Create_Info.Enabled_Extension_Names);
        Result: Vulkan.Result;
    begin
        if Enabled_Extension_Names'Length > 0 then
            Create_Info_C.Enabled_Extension_Names :=
                Enabled_Extension_Names
                    (Enabled_Extension_Names'First)'Unchecked_Access;
        end if;

        Result := C.vkCreateDevice(Physical_Device,
                                   Create_Info_C,
                                   null,
                                   Device);
        C.Free(Create_Info_C);

        return Result;
    end Create;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Create_Info: in Device_Create_Info) return Device is
        D: Device;
    begin
        Exceptions.Check(Create(Physical_Device, Create_Info, D));

        return D;
    end Create;

    procedure Destroy(Device: in out Vulkan.Device;
                      Allocator: aliased in Allocation_Callbacks) is
    begin
        c.vkDestroyDevice(Device, Allocator'Access);
        Device := No_Device;
    end Destroy;

    procedure Destroy(Device: in out Vulkan.Device) is
    begin
        c.vkDestroyDevice(Device, null);
        Device := No_Device;
    end Destroy;

    procedure Wait_Idle(Device: in Vulkan.Device) is
    begin
       Exceptions.Check(Wait_Idle(Device));
    end Wait_Idle;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Memory_Allocate_Info;
                      Allocator: aliased in Allocation_Callbacks;
                      Memory: out Device_Memory) return Result is
        Memory_Allocate_Info_C: C.Memory_Allocate_Info_C :=
            C.To_C(Allocate_Info);
        Result: Vulkan.Result;
    begin
        Result := C.vkAllocateMemory(Device,
                                     Memory_Allocate_Info_C,
                                     Allocator'Access,
                                     Memory);
        C.Free(Memory_Allocate_Info_C);

        return Result;
    end Allocate;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Memory_Allocate_Info;
                      Allocator: aliased in Allocation_Callbacks)
                        return Device_Memory is
        DM: Device_Memory;
    begin
        Exceptions.Check(Allocate(Device, Allocate_Info, Allocator, DM));

        return DM;
    end Allocate;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Memory_Allocate_Info;
                      Memory: out Device_Memory) return Result is
        Memory_Allocate_Info_C: C.Memory_Allocate_Info_C :=
            C.To_C(Allocate_Info);
        Result: Vulkan.Result;
    begin
        Result := C.vkAllocateMemory(Device,
                                     Memory_Allocate_Info_C,
                                     null,
                                     Memory);
        C.Free(Memory_Allocate_Info_C);

        return Result;
    end Allocate;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Memory_Allocate_Info)
                        return Device_Memory is
        DM: Device_Memory;
    begin
        Exceptions.Check(Allocate(Device, Allocate_Info, DM));

        return DM;
    end Allocate;

    procedure Free(Device: in Vulkan.Device;
                   Memory: in out Device_Memory;
                   Allocator: aliased in Allocation_Callbacks) is
    begin
        C.vkFreeMemory(Device, Memory, Allocator'Access);
        Memory := No_Device_Memory;
    end Free;

    procedure Free(Device: in Vulkan.Device;
                   Memory: in out Device_Memory) is
    begin
        C.vkFreeMemory(Device, Memory, null);
        Memory := No_Device_Memory;
    end Free;

    function Map_With_Result(Device: in Vulkan.Device;
                             Memory: in Device_Memory;
                             Offset, Size: in Device_Size;
                             Data: out Pointers.Pointer) return Result is
    begin
        return C.vkMapMemory(Device, Memory, Offset, Size, 0, Data'Address);
    end Map_With_Result;

    function Map_With_Exception(Device: in Vulkan.Device;
                                Memory: in Device_Memory;
                                Offset, Size: in Device_Size)
                                    return Pointers.Pointer is
        P: Pointers.Pointer;
    begin
        Exceptions.Check(C.vkMapMemory(Device,
                                       Memory,
                                       Offset,
                                       Size,
                                       0,
                                       P'Address));

        return P;
    end Map_With_Exception;

    function Flush(Device: in Vulkan.Device;
                   Ranges: in Mapped_Memory_Range_Vectors.Vector)
        return Result is
        C_Ranges: array (1 .. Positive(Ranges.Length)) of
            aliased C.Mapped_Memory_Range_C
            with Convention => C;
        Result: Vulkan.Result;
    begin
        for X in C_Ranges'Range loop
            C_Ranges(X) := C.To_C(Ranges(X));
        end loop;

        Result := C.vkFlushMappedMemoryRanges
            (Device,
             Interfaces.Unsigned_32(C_Ranges'Length),
             C_Ranges(1)'Access);

        for R of C_Ranges loop
            C.Free(R);
        end loop;

        return Result;
    end Flush;

    procedure Flush(Device: in Vulkan.Device;
                    Ranges: in Mapped_Memory_Range_Vectors.Vector) is
    begin
        Exceptions.Check(Flush(Device, Ranges));
    end Flush;

    function Flush(Device: in Vulkan.Device;
                   Memory_Range: in Mapped_Memory_Range) return Result is
        C_Range: aliased C.Mapped_Memory_Range_C := C.To_C(Memory_Range);
        Result: Vulkan.Result;
    begin
        Result := C.vkFlushMappedMemoryRanges(Device,
                                              1,
                                              C_Range'Access);
        C.Free(C_Range);

        return Result;
    end Flush;

    procedure Flush(Device: in Vulkan.Device;
                    Memory_Range: in Mapped_Memory_Range) is
    begin
        Exceptions.Check(Flush(Device, Memory_Range));
    end Flush;

    function Invalidate(Device: in Vulkan.Device;
                        Ranges: in Mapped_Memory_Range_Vectors.Vector)
        return Result is
        C_Ranges: array (1 .. Positive(Ranges.Length)) of
            aliased C.Mapped_Memory_Range_C
            with Convention => C;
        Result: Vulkan.Result;
    begin
        for X in C_Ranges'Range loop
            C_Ranges(X) := C.To_C(Ranges(X));
        end loop;

        Result := C.vkInvalidateMappedMemoryRanges
            (Device,
             Interfaces.Unsigned_32(C_Ranges'Length),
             C_Ranges(1)'Access);

        for R of C_Ranges loop
            C.Free(R);
        end loop;

        return Result;
    end Invalidate;

    procedure Invalidate(Device: in Vulkan.Device;
                         Ranges: in Mapped_Memory_Range_Vectors.Vector) is
    begin
        Exceptions.Check(Invalidate(Device, Ranges));
    end Invalidate;

    function Invalidate(Device: in Vulkan.Device;
                        Memory_Range: in Mapped_Memory_Range)
        return Result is
        C_Range: aliased C.Mapped_Memory_Range_C := C.To_C(Memory_Range);
        Result: Vulkan.Result;
    begin
        Result := C.vkInvalidateMappedMemoryRanges(Device, 1, C_Range'Access);
        C.Free(C_Range);

        return Result;
    end Invalidate;

    procedure Invalidate(Device: in Vulkan.Device;
                         Memory_Range: in Mapped_Memory_Range) is
    begin
        Exceptions.Check(Invalidate(Device, Memory_Range));
    end Invalidate;

    function Get_Memory_Commitment(Device: in Vulkan.Device;
                                   Memory: in Device_Memory)
        return Device_Size is
        Size: Device_Size := 0;
    begin
        C.vkGetDeviceMemoryCommitment(Device, Memory, Size);

        return Size;
    end Get_Memory_Commitment;

    function Get_Device_Group_Peer_Memory_Features
        (Device: in Vulkan.Device;
         Heap_Index,
         Local_Device_Index,
         Remote_Device_Index: in Interfaces.Unsigned_32)
            return Peer_Memory_Feature_Flags is
        Flags: Peer_Memory_Feature_Flags;
    begin
        C_V1_1.vkGetDeviceGroupPeerMemoryFeatures(Device,
                                                  Heap_Index,
                                                  Local_Device_Index,
                                                  Remote_Device_Index,
                                                  Flags);

        return Flags;
    end Get_Device_Group_Peer_Memory_Features;

    procedure Get_Descriptor_Set_Layout_Support
        (Device: in Vulkan.Device;
         Create_Info: in Descriptor_Set_Layout_Create_Info;
         Support: in out Descriptor_Set_Layout_Support) is
        C_Create_Info: C.Descriptor_Set_Layout_Create_Info_C := 
            C.To_C(Create_Info);
        C_Support: C_V1_1.Descriptor_Set_Layout_Support_C;
    begin
        C_Support.Next := Extension_Records.To_C(Support.Next);
        C_V1_1.vkGetDescriptorSetLayoutSupport(Device,
                                               C_Create_Info,
                                               C_Support);
        C.Free(C_Create_Info);
        C_V1_1.To_Ada(Support, C_Support);
        Extension_Records.Free(C_Support.Next);
    end Get_Descriptor_Set_Layout_Support;

    function Get_Descriptor_Set_Layout_Support
        (Device: in Vulkan.Device;
         Create_Info: in Descriptor_Set_Layout_Create_Info)
        return Descriptor_Set_Layout_Support is
        Properties: Descriptor_Set_Layout_Support;
    begin
        Get_Descriptor_Set_Layout_Support(Device, Create_Info, Properties);

        return Properties;
    end Get_Descriptor_Set_Layout_Support;
    
    function Get_Device_Address(Device: in Vulkan.Device;
                                Info: in Buffer_Device_Address_Info)
        return Device_Address is
        C_Info: C_V1_2.Buffer_Device_Address_Info_C := C_V1_2.To_C(Info);
        Result: Device_Address;
    begin
        Result := C_V1_2.vkGetBufferDeviceAddress(Device, C_Info);
        C_V1_2.Free(C_Info);

        return Result;
    end Get_Device_Address;
    
    function Get_Opaque_Capture_Address(Device: in Vulkan.Device;
                                        Info: in Buffer_Device_Address_Info)
        return Interfaces.Unsigned_64 is
        C_Info: C_V1_2.Buffer_Device_Address_Info_C := C_V1_2.To_C(Info);
        Result: Interfaces.Unsigned_64;
    begin
        Result := C_V1_2.vkGetBufferOpaqueCaptureAddress(Device, C_Info);
        C_V1_2.Free(C_Info);

        return Result;
    end Get_Opaque_Capture_Address;
    
    function Get_Opaque_Capture_Address
        (Device: in Vulkan.Device;
         Info: in Device_Memory_Opaque_Capture_Address_Info)
        return Interfaces.Unsigned_64 is
        C_Info: C_V1_2.Device_Memory_Opaque_Capture_Address_Info_C :=
            C_V1_2.To_C(Info);
        Result: Interfaces.Unsigned_64;
    begin
        Result := C_V1_2.vkGetDeviceMemoryOpaqueCaptureAddress(Device, C_Info);
        C_V1_2.Free(C_Info);

        return Result;
    end Get_Opaque_Capture_Address;
    
    procedure Get_Buffer_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Buffer_Memory_Requirements;
         Memory_Requirements: in out Memory_Requirements_2) is
        Info_C: C_V1_3.Device_Buffer_Memory_Requirements_C :=
            C_V1_3.To_C(Info);
        Memory_Requirements_C: C_V1_1.Memory_Requirements_2_C;
    begin
        Memory_Requirements_C.Next :=
            Extension_Records.To_C(Memory_Requirements.Next);
        C_V1_3.vkGetDeviceBufferMemoryRequirements(Device,
                                                   Info_C,
                                                   Memory_Requirements_C);
        C_V1_3.Free(Info_C);
        C_V1_1.To_Ada(Memory_Requirements, Memory_Requirements_C);
        Extension_Records.Free(Memory_Requirements_C.Next);
    end Get_Buffer_Memory_Requirements;
    
    function Get_Buffer_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Buffer_Memory_Requirements)
        return Memory_Requirements_2 is
        Memory_Requirements: Memory_Requirements_2;
    begin
        Get_Buffer_Memory_Requirements(Device, Info, Memory_Requirements);

        return Memory_Requirements;
    end Get_Buffer_Memory_Requirements;

    procedure Get_Image_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements;
         Memory_Requirements: in out Memory_Requirements_2) is
        Info_C: C_V1_3.Device_Image_Memory_Requirements_C :=
            C_V1_3.To_C(Info);
        Memory_Requirements_C: C_V1_1.Memory_Requirements_2_C;
    begin
        Memory_Requirements_C.Next :=
            Extension_Records.To_C(Memory_Requirements.Next);
        C_V1_3.vkGetDeviceImageMemoryRequirements(Device,
                                                   Info_C,
                                                   Memory_Requirements_C);
        C_V1_3.Free(Info_C);
        C_V1_1.To_Ada(Memory_Requirements, Memory_Requirements_C);
        Extension_Records.Free(Memory_Requirements_C.Next);
    end Get_Image_Memory_Requirements;
    
    function Get_Image_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements)
        return Memory_Requirements_2 is
        Memory_Requirements: Memory_Requirements_2;
    begin
        Get_Image_Memory_Requirements(Device, Info, Memory_Requirements);

        return Memory_Requirements;
    end Get_Image_Memory_Requirements;
    
    function Image_Sparse_Memory_Requirements_Count
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
        Info_C: C_V1_3.Device_Image_Memory_Requirements_C := C_V1_3.To_C(Info);
    begin
        C_V1_3.vkGetDeviceImageSparseMemoryRequirements(Device,
                                                        Info_C,
                                                        Count,
                                                        null);
        C_V1_3.Free(Info_C);

        return Count;
    end Image_Sparse_Memory_Requirements_Count;
    
    procedure Get_Image_Sparse_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements;
         Sparse_Memory_Requirements:
            in out Sparse_Image_Memory_Requirements_2_Vectors.Vector) is
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Sparse_Memory_Requirements.Length);
        Info_C: C_V1_3.Device_Image_Memory_Requirements_C := C_V1_3.To_C(Info);
        Requirements_C: array (1 .. Positive(Count)) of
            aliased C_V1_1.Sparse_Image_Memory_Requirements_2_C;
    begin
        for X in Requirements_C'Range loop
            Requirements_C(X).Next :=
                Extension_Records.To_C(Sparse_Memory_Requirements(X).Next);
        end loop;

        C_V1_3.vkGetDeviceImageSparseMemoryRequirements
            (Device, Info_C, Count, Requirements_C(1)'Access);

        C_V1_3.Free(Info_C);

        for X in Requirements_C'Range loop
            C_V1_1.To_Ada(Sparse_Memory_Requirements(X), Requirements_C(X));
            Extension_Records.Free(Requirements_C(X).Next);
        end loop;
    end Get_Image_Sparse_Memory_Requirements;
    
    function Get_Image_Sparse_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Device_Image_Memory_Requirements)
        return Sparse_Image_Memory_Requirements_2_Vectors.Vector is
        Requirements: Sparse_Image_Memory_Requirements_2_Vectors.Vector;
        Item: Sparse_Image_Memory_Requirements_2;
    begin
        Requirements.Append
            (Item, Ada.Containers.Count_Type
                    (Image_Sparse_Memory_Requirements_Count(Device, Info)));
        Get_Image_Sparse_Memory_Requirements(Device, Info, Requirements);

        return Requirements;
    end Get_Image_Sparse_Memory_Requirements;

    function Map_2_With_Result(Device: in Vulkan.Device;
                               Memory_Map_Info: in Vulkan.Memory_Map_Info;
                               Data: out Pointers.Pointer) return Result is
        C_Info: C_V1_4.Memory_Map_Info_C := C_V1_4.To_C(Memory_Map_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_4.vkMapMemory2(Device, C_Info, Data'Address);
        C_V1_4.Free(C_Info);

        return Result;
    end Map_2_With_Result;
    
    function Map_2_With_Exception(Device: in Vulkan.Device;
                                  Memory_Map_Info: in Vulkan.Memory_Map_Info)
        return Pointers.Pointer is
        function Map_2 is new Map_2_With_Result(Pointers);

        Data: Pointers.Pointer;
    begin
        Exceptions.Check(Map_2(Device, Memory_Map_Info, Data));

        return Data;
    end Map_2_With_Exception;

    function Unmap(Device: in Vulkan.Device;
                   Memory_Unmap_Info: in Vulkan.Memory_Unmap_Info)
        return Result is
        C_Info: C_V1_4.Memory_Unmap_Info_C := C_V1_4.To_C(Memory_Unmap_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_4.vkUnmapMemory2(Device, C_Info);
        C_V1_4.Free(C_Info);

        return Result;
    end Unmap;

    procedure Unmap(Device: in Vulkan.Device;
                    Memory_Unmap_Info: in Vulkan.Memory_Unmap_Info) is
    begin
        Exceptions.Check(Unmap(Device, Memory_Unmap_Info));
    end Unmap;

    function Get_Granularity(Device: in Vulkan.Device;
                             Rendering_Area_Info: in Vulkan.Rendering_Area_Info)
        return Extent_2D is
        Info_C: C_V1_4.Rendering_Area_Info_C :=
            C_V1_4.To_C(Rendering_Area_Info);
        Granularity: Extent_2D;
    begin
        C_V1_4.vkGetRenderingAreaGranularity(Device, Info_C, Granularity);
        C_V1_4.Free(Info_C);

        return Granularity;
    end Get_Granularity;

    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Info: in Device_Image_Subresource_Info;
                                     Layout: in out Subresource_Layout_2) is
        Info_C: C_V1_4.Device_Image_Subresource_Info_C := C_V1_4.To_C(InfO);
        Layout_C: C_V1_4.Subresource_Layout_2_C;
    begin
        Layout_C.Next := Extension_Records.To_C(Layout.Next);
        C_V1_4.vkGetDeviceImageSubresourceLayout(Device, Info_C, Layout_C);
        
        C_V1_4.Free(Info_C);
        C_V1_4.To_Ada(Layout, Layout_C);
        Extension_Records.Free(Layout_C.Next);
    end Get_Subresource_Layout;
    
    function Get_Subresource_Layout(Device: in Vulkan.Device;
                                    Info: in Device_Image_Subresource_Info)
        return Subresource_Layout_2 is
        Layout: Subresource_Layout_2;
    begin
        Get_Subresource_Layout(Device, Info, Layout);

        return Layout;
    end Get_Subresource_Layout;
end Vulkan.Devices;


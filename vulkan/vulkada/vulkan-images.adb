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

with Vulkan.Extension_Records;
with Vulkan.Exceptions;
with Vulkan.C_V1_1;
with Vulkan.C_V1_4;

package body Vulkan.Images is
    procedure Bind(Device: in Vulkan.Device;
                   Image: in Vulkan.Image;
                   Memory: in Device_Memory;
                   Offset: in Device_Size) is
    begin
        Exceptions.Check(Bind(Device, Image, Memory, Offset));
    end Bind;

    function Get_Memory_Requirements(Device: in Vulkan.Device;
                                     Image: in Vulkan.Image)
        return Memory_Requirements is
        Requirements: Memory_Requirements;
    begin
        C.vkGetImageMemoryRequirements(Device, Image, Requirements);

        return Requirements;
    end Get_Memory_Requirements;

    procedure Get_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Image_Memory_Requirements_Info_2;
         Requirements: in out Memory_Requirements_2) is
        C_Info: C_V1_1.Image_Memory_Requirements_Info_2_C := C_V1_1.To_C(Info);
        C_Requirements: C_V1_1.Memory_Requirements_2_C;
    begin
        C_Requirements.Next := Extension_Records.To_C(Requirements.Next);
        C_V1_1.vkGetImageMemoryRequirements2(Device, C_Info, C_Requirements);
        C_V1_1.Free(C_Info);
        C_V1_1.To_Ada(Requirements, C_Requirements);
        Extension_Records.Free(C_Requirements.Next);
    end Get_Memory_Requirements;

    function Get_Memory_Requirements(Device: in Vulkan.Device;
                                     Info: in Image_Memory_Requirements_Info_2)
        return Memory_Requirements_2 is
        Requirements: Memory_Requirements_2;
    begin
        Get_Memory_Requirements(Device, Info, Requirements);

        return Requirements;
    end Get_Memory_Requirements;

    function Get_Sparse_Memory_Requirements_Count
        (Device: in Vulkan.Device;
         Info: in Image_Sparse_Memory_Requirements_Info_2)
        return Interfaces.Unsigned_32 is
        C_Info: C_V1_1.Image_Sparse_Memory_Requirements_Info_2_C :=
            C_V1_1.To_C(Info);
        Count: Interfaces.Unsigned_32 := 0;
    begin
        C_V1_1.vkGetImageSparseMemoryRequirements2(Device,
                                                   C_Info,
                                                   Count,
                                                   null);
        C_V1_1.Free(C_Info);

        return Count;
    end Get_Sparse_Memory_Requirements_Count;

    procedure Get_Sparse_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Image_Sparse_Memory_Requirements_Info_2;
         Requirements:
            in out Sparse_Image_Memory_Requirements_2_Vectors.Vector) is
        use type Interfaces.Unsigned_32;

        C_Info: C_V1_1.Image_Sparse_Memory_Requirements_Info_2_C :=
            C_V1_1.To_C(Info);
        Count: Interfaces.Unsigned_32 := 0;
    begin
        C_V1_1.vkGetImageSparseMemoryRequirements2(Device,
                                                   C_Info,
                                                   Count,
                                                   null);

        if Count = 0 then
            C_V1_1.Free(C_Info);

            return;
        end if;

        declare
            C_Requirements: array (1 .. Count) of
                aliased C_V1_1.Sparse_Image_Memory_Requirements_2_C
                with Convention => C;
            Index: Interfaces.Unsigned_32 := 1;
        begin
            for R of Requirements loop
                C_Requirements(Index).Next := Extension_Records.To_C(R.Next);
                Index := Index + 1;
            end loop;

            C_V1_1.vkGetImageSparseMemoryRequirements2
                (Device,
                 C_Info,
                 Count,
                 C_Requirements(1)'Unchecked_Access);

            Index := 1;

            for R of Requirements loop
                C_V1_1.To_Ada(R, C_Requirements(Index));
                Extension_Records.Free(C_Requirements(Index).Next);
                Index := Index + 1;
            end loop;
        end;

        C_V1_1.Free(C_Info);
    end Get_Sparse_Memory_Requirements;

    function Get_Sparse_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Image_Sparse_Memory_Requirements_Info_2)
            return Sparse_Image_Memory_Requirements_2_Vectors.Vector is
        Requirements: Sparse_Image_Memory_Requirements_2_Vectors.Vector;
        Item: Sparse_Image_Memory_Requirements_2;
        Count: Interfaces.Unsigned_32;
    begin
        Count := Get_Sparse_Memory_Requirements_Count(Device, Info);
        Requirements.Append(Item, Ada.Containers.Count_Type(Count));
        Get_Sparse_Memory_Requirements(Device, Info, Requirements);

        return Requirements;
    end Get_Sparse_Memory_Requirements;

    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Image: in Vulkan.Image;
                                     Info: in Image_Subresource_2;
                                     Layout: in out Subresource_Layout_2) is
        Info_C: C_V1_4.Image_Subresource_2_C;
        Layout_C: C_V1_4.Subresource_Layout_2_C;
    begin
        Info_C.Next := Extension_Records.To_C(Info.Next);
        Info_C.Image_Subresource := Info.Image_Subresource;
        Layout_C.Next := Extension_Records.To_C(Layout.Next);
        C_V1_4.vkGetImageSubresourceLayout2(Device, Image, Info_C, Layout_C);

        Extension_Records.Free(Info_C.Next);
        C_V1_4.To_Ada(Layout, Layout_C);
        Extension_Records.Free(Layout_C.Next);
    end Get_Subresource_Layout;

    function Get_Subresource_Layout(Device: in Vulkan.Device;
                                    Image: in Vulkan.Image;
                                    Info: in Image_Subresource_2)
        return Subresource_Layout_2 is
        Layout: Subresource_Layout_2;
    begin
        Get_Subresource_Layout(Device, Image, Info, Layout);

        return Layout;
    end Get_Subresource_Layout;

    function Copy_Memory_To_Image
        (Device: in Vulkan.Device;
         Copy_Memory_To_Image_Info: in Vulkan.Copy_Memory_To_Image_Info)
        return Result is
        C_Info: C_V1_4.Copy_Memory_To_Image_Info_C :=
            C_V1_4.To_C(Copy_Memory_To_Image_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_4.vkCopyMemoryToImage(Device, C_Info);
        C_V1_4.Free(C_Info);

        return Result;
    end Copy_Memory_To_Image;

    procedure Copy_Memory_To_Image
        (Device: in Vulkan.Device;
         Copy_Memory_To_Image_Info: in Vulkan.Copy_Memory_To_Image_Info) is
    begin
        Exceptions.Check(Copy_Memory_To_Image(Device,
                                              Copy_Memory_To_Image_Info));
    end Copy_Memory_To_Image;

    function Copy_Image_To_Memory
        (Device: in Vulkan.Device;
         Copy_Image_To_Memory_Info: in Vulkan.Copy_Image_To_Memory_Info)
        return Result is
        C_Info: C_V1_4.Copy_Image_To_Memory_Info_C :=
            C_V1_4.To_C(Copy_Image_To_Memory_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_4.vkCopyImageToMemory(Device, C_Info);
        C_V1_4.Free(C_Info);

        return Result;
    end Copy_Image_To_Memory;

    procedure Copy_Image_To_Memory
        (Device: in Vulkan.Device;
         Copy_Image_To_Memory_Info: in Vulkan.Copy_Image_To_Memory_Info) is
    begin
        Exceptions.Check(Copy_Image_To_Memory(Device,
                                              Copy_Image_To_Memory_Info));
    end Copy_Image_To_Memory;

    function Copy_Image_To_Image
        (Device: in Vulkan.Device;
         Copy_Image_To_Image_Info: in Vulkan.Copy_Image_To_Image_Info)
        return Result is
        C_Info: C_V1_4.Copy_Image_To_Image_Info_C :=
            C_V1_4.To_C(Copy_Image_To_Image_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_4.vkCopyImageToImage(Device, C_Info);
        C_V1_4.Free(C_Info);

        return Result;
    end Copy_Image_To_Image;

    procedure Copy_Image_To_Image
        (Device: in Vulkan.Device;
         Copy_Image_To_Image_Info: in Vulkan.Copy_Image_To_Image_Info) is
    begin
        Exceptions.Check(Copy_Image_To_Image(Device,
                                             Copy_Image_To_Image_Info));
    end Copy_Image_To_Image;
    
    function Transition_Image_Layout
        (Device: in Vulkan.Device;
         Transitions: in Host_Image_Layout_Transition_Info_Vectors.Vector)
        return Result is
        C_Info: array (1 .. Positive(Transitions.Length)) of
            aliased C_V1_4.Host_Image_Layout_Transition_Info_C;
        Result: Vulkan.Result;
    begin
        for X in C_Info'Range loop
            C_Info(X) := C_V1_4.To_C(Transitions(X));
        end loop;

        Result := C_V1_4.vkTransitionImageLayout
            (Device,
             Interfaces.Unsigned_32(Transitions.Length),
             C_Info(1)'Access);

        for Info of C_Info loop
            C_V1_4.Free(Info);
        end loop;

        return Result;
    end Transition_Image_Layout;

    function Transition_Image_Layout
        (Device: in Vulkan.Device;
         Transition: in Host_Image_Layout_Transition_Info) return Result is
    begin
        return Transition_Image_Layout
            (Device,
             Host_Image_Layout_Transition_Info_Vectors.To_Vector(Transition,
                                                                 1));
    end Transition_Image_Layout;

    procedure Transition_Image_Layout
        (Device: in Vulkan.Device;
         Transitions: in Host_Image_Layout_Transition_Info_Vectors.Vector) is
    begin
        Exceptions.Check(Transition_Image_Layout(Device, Transitions));
    end Transition_Image_Layout;

    procedure Transition_Image_Layout
        (Device: in Vulkan.Device;
         Transition: in Host_Image_Layout_Transition_Info) is
    begin
        Exceptions.Check(Transition_Image_Layout(Device, Transition));
    end Transition_Image_Layout;
end Vulkan.Images;


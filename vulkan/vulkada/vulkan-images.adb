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

-- Image related subprograms

with Vulkan.Extension_Records;
with Vulkan.Exceptions;
with Vulkan.C_V1_1;

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
end Vulkan.Images;


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

-- Buffer related subprograms

with Vulkan.Extension_Records;
with Vulkan.Exceptions;
with Vulkan.C_V1_1;

package body Vulkan.Buffers is
    procedure Bind(Device: in Vulkan.Device;
                   Buffer: in Vulkan.Buffer;
                   Memory: in Device_Memory;
                   Offset: in Device_Size) is
    begin
        Exceptions.Check(Bind(Device, Buffer, Memory, Offset));
    end Bind;

    function Get_Memory_Requirements(Device: in Vulkan.Device;
                                     Buffer: in Vulkan.Buffer)
        return Memory_Requirements is
        Requirements: Memory_Requirements;
    begin
        C.vkGetBufferMemoryRequirements(Device, Buffer, Requirements);

        return Requirements;
    end Get_Memory_Requirements;

    function Bind(Device: in Vulkan.Device;
                  Bind_Info: in Bind_Buffer_Memory_Info) return Result is
        Bind_Buffer_Memory_Info_C: aliased C_V1_1.Bind_Buffer_Memory_Info_C :=
            C_V1_1.To_C(Bind_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_1.vkBindBufferMemory2(Device,
                                             1,
                                             Bind_Buffer_Memory_Info_C'Access);
        C_V1_1.Free(Bind_Buffer_Memory_Info_C);

        return Result;
    end Bind;

    procedure Bind(Device: in Vulkan.Device;
                   Bind_Info: in Bind_Buffer_Memory_Info) is
    begin
        Exceptions.Check(Bind(Device, Bind_Info));
    end Bind;

    function Bind(Device: in Vulkan.Device;
                  Bind_Infos: in Bind_Buffer_Memory_Info_Vectors.Vector)
        return Result is
        Bind_Infos_C: array (1 .. Positive(Bind_Infos.Length))
                        of aliased C_V1_1.Bind_Buffer_Memory_Info_C;
        Result: Vulkan.Result;
        Index: Positive := 1;
    begin
        for Bind_Info of Bind_Infos loop
            Bind_Infos_C(Index) := C_V1_1.To_C(Bind_Info);
            Index := Index + 1;
        end loop;

        Result := C_V1_1.vkBindBufferMemory2
                    (Device,
                     Interfaces.Unsigned_32(Bind_Infos_C'Length),
                     Bind_Infos_C(1)'Access);

        for Bind_Info of Bind_Infos_C loop
            C_V1_1.Free(Bind_Info);
        end loop;

        return Result;
    end Bind;
        
    procedure Bind(Device: in Vulkan.Device;
                   Bind_Infos: in Bind_Buffer_Memory_Info_Vectors.Vector) is
    begin
        Exceptions.Check(Bind(Device, Bind_Infos));
    end Bind;

    function Bind(Device: in Vulkan.Device;
                  Bind_Info: in Bind_Image_Memory_Info) return Result is
        Bind_Image_Memory_Info_C: aliased C_V1_1.Bind_Image_Memory_Info_C :=
            C_V1_1.To_C(Bind_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_1.vkBindImageMemory2(Device,
                                            1,
                                            Bind_Image_Memory_Info_C'Access);
        C_V1_1.Free(Bind_Image_Memory_Info_C);

        return Result;
    end Bind;

    procedure Bind(Device: in Vulkan.Device;
                   Bind_Info: in Bind_Image_Memory_Info) is
    begin
        Exceptions.Check(Bind(Device, Bind_Info));
    end Bind;

    function Bind(Device: in Vulkan.Device;
                  Bind_Infos: in Bind_Image_Memory_Info_Vectors.Vector)
        return Result is
        Bind_Infos_C: array (1 .. Positive(Bind_Infos.Length))
                        of aliased C_V1_1.Bind_Image_Memory_Info_C;
        Result: Vulkan.Result;
        Index: Positive := 1;
    begin
        for Bind_Info of Bind_Infos loop
            Bind_Infos_C(Index) := C_V1_1.To_C(Bind_Info);
            Index := Index + 1;
        end loop;

        Result := C_V1_1.vkBindImageMemory2
                    (Device,
                     Interfaces.Unsigned_32(Bind_Infos_C'Length),
                     Bind_Infos_C(1)'Access);

        for Bind_Info of Bind_Infos_C loop
            C_V1_1.Free(Bind_Info);
        end loop;

        return Result;
    end Bind;
        
    procedure Bind(Device: in Vulkan.Device;
                   Bind_Infos: in Bind_Image_Memory_Info_Vectors.Vector) is
    begin
        Exceptions.Check(Bind(Device, Bind_Infos));
    end Bind;

    procedure Get_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Buffer_Memory_Requirements_Info_2;
         Memory_Requirements: in out Memory_Requirements_2) is
        C_Info: C_V1_1.Buffer_Memory_Requirements_Info_2_C := C_V1_1.To_C(Info);
        C_Requirements: C_V1_1.Memory_Requirements_2_C;
    begin
        C_Requirements.Next := Extension_Records.To_C(Memory_Requirements.Next);
        C_V1_1.vkGetBufferMemoryRequirements2(Device, C_Info, C_Requirements);
        C_V1_1.To_Ada(Memory_Requirements, C_Requirements);
        Extension_Records.Free(C_Requirements.Next);
    end Get_Memory_Requirements;

    function Get_Memory_Requirements(Device: in Vulkan.Device;
                                     Info: in Buffer_Memory_Requirements_Info_2)
        return Memory_Requirements_2 is
        Requirements: Memory_Requirements_2;
    begin
        Get_Memory_Requirements(Device, Info, Requirements);

        return Requirements;
    end Get_Memory_Requirements;
end Vulkan.Buffers;


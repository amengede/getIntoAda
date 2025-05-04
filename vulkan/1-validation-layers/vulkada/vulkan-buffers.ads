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

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Buffers is
    -- vkCreateBuffer
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Buffer: out Vulkan.Buffer) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Usage /= Buffer_Usage_No_Bit,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Invalid_Opaque_Capture_Address and
                     (if Create'Result = Success then Buffer /= No_Buffer);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_Create_Info;
                    Allocator: aliased in Allocation_Callbacks) return Buffer
        with Pre => Device /= No_Device and
                    Create_Info.Usage /= Buffer_Usage_No_Bit,
             Post => Create'Result /= No_Buffer;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_Create_Info;
                    Buffer: out Vulkan.Buffer) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Usage /= Buffer_Usage_No_Bit,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Invalid_Opaque_Capture_Address and
                     (if Create'Result = Success then Buffer /= No_Buffer);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_Create_Info) return Buffer
        with Pre => Device /= No_Device and
                    Create_Info.Usage /= Buffer_Usage_No_Bit,
             Post => Create'Result /= No_Buffer;

    -- vkDestroyBuffer
    procedure Destroy(Device: in Vulkan.Device;
                      Buffer: in out Vulkan.Buffer;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Buffer = No_Buffer;

    procedure Destroy(Device: in Vulkan.Device;
                      Buffer: in out Vulkan.Buffer)
        with Inline,
             Pre => Device /= No_Device,
             Post => Buffer = No_Buffer;

    -- vkBindBufferMemory
    function Bind(Device: in Vulkan.Device;
                  Buffer: in Vulkan.Buffer;
                  Memory: in Device_Memory;
                  Offset: in Device_Size) return Result
        with Pre => Device /= No_Device and
                    Buffer /= No_Buffer and
                    Memory /= No_Device_Memory,
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Invalid_Opaque_Capture_Address;
 
    procedure Bind(Device: in Vulkan.Device;
                   Buffer: in Vulkan.Buffer;
                   Memory: in Device_Memory;
                   Offset: in Device_Size)
        with Inline,
             Pre => Device /= No_Device and
                    Buffer /= No_Buffer and
                    Memory /= No_Device_Memory;

    -- vkGetBufferMemoryRequirements
    function Get_Memory_Requirements(Device: in Vulkan.Device;
                                     Buffer: in Vulkan.Buffer)
        return Memory_Requirements
        with Inline,
             Pre => Device /= No_Device and
                    Buffer /= No_Buffer;

    -- Vulkan 1.1
    -- vkBindBufferMemory2
    function Bind(Device: in Vulkan.Device;
                  Bind_Info: in Bind_Buffer_Memory_Info) return Result
        with Pre => Device /= No_Device and
                    Bind_Info.Buffer /= No_Buffer and
                    Bind_Info.Memory /= No_Device_Memory,
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Invalid_Opaque_Capture_Address;

    procedure Bind(Device: in Vulkan.Device;
                   Bind_Info: in Bind_Buffer_Memory_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Bind_Info.Buffer /= No_Buffer and
                    Bind_Info.Memory /= No_Device_Memory;

    function Bind(Device: in Vulkan.Device;
                  Bind_Infos: in Bind_Buffer_Memory_Info_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    not Bind_Infos.Is_Empty and
                    (for all BI of Bind_Infos =>
                        BI.Buffer /= No_Buffer and
                        BI.Memory /= No_Device_Memory),
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Invalid_Opaque_Capture_Address;

    procedure Bind(Device: in Vulkan.Device;
                   Bind_Infos: in Bind_Buffer_Memory_Info_Vectors.Vector)
        with Inline,
             Pre => Device /= No_Device and
                    not Bind_Infos.Is_Empty and
                    (for all BI of Bind_Infos =>
                        BI.Buffer /= No_Buffer and
                        BI.Memory /= No_Device_Memory);

    -- vkBindImageMemory2
    function Bind(Device: in Vulkan.Device;
                  Bind_Info: in Bind_Image_Memory_Info) return Result
        with Pre => Device /= No_Device and
                    Bind_Info.Image /= No_Image and
                    Bind_Info.Memory /= No_Device_Memory,
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Invalid_Opaque_Capture_Address;

    procedure Bind(Device: in Vulkan.Device;
                   Bind_Info: in Bind_Image_Memory_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Bind_Info.Image /= No_Image and
                    Bind_Info.Memory /= No_Device_Memory;

    function Bind(Device: in Vulkan.Device;
                  Bind_Infos: in Bind_Image_Memory_Info_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    not Bind_Infos.Is_Empty and
                    (for all BI of Bind_Infos =>
                        BI.Image /= No_Image and
                        BI.Memory /= No_Device_Memory),
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Invalid_Opaque_Capture_Address;

    procedure Bind(Device: in Vulkan.Device;
                   Bind_Infos: in Bind_Image_Memory_Info_Vectors.Vector)
        with Inline,
             Pre => Device /= No_Device and
                    not Bind_Infos.Is_Empty and
                    (for all BI of Bind_Infos =>
                        BI.Image /= No_Image and
                        BI.Memory /= No_Device_Memory);

    -- vkGetBufferMemoryRequirements2
    procedure Get_Memory_Requirements
        (Device: in Vulkan.Device;
         Info: in Buffer_Memory_Requirements_Info_2;
         Memory_Requirements: in out Memory_Requirements_2)
        with Pre => Device /= No_Device and Info.Buffer /= No_Buffer;
                                
    function Get_Memory_Requirements(Device: in Vulkan.Device;
                                     Info: in Buffer_Memory_Requirements_Info_2)
        return Memory_Requirements_2
        with Pre => Device /= No_Device and Info.Buffer /= No_Buffer;

private
    package Buffers_Common is
        new Objects_Common(Buffer_Create_Info,
                           C.Buffer_Create_Info_C,
                           Buffer,
                           No_Buffer,
                           C.To_C,
                           C.Free,
                           C.vkCreateBuffer,
                           C.vkDestroyBuffer);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Buffer: out Vulkan.Buffer) return Result
        renames Buffers_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_Create_Info;
                    Allocator: aliased in Allocation_Callbacks) return Buffer
        renames Buffers_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_Create_Info;
                    Buffer: out Vulkan.Buffer) return Result
        renames Buffers_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_Create_Info) return Buffer
        renames Buffers_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Buffer: in out Vulkan.Buffer;
                      Allocator: aliased in Allocation_Callbacks)
        renames Buffers_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Buffer: in out Vulkan.Buffer)
        renames Buffers_Common.Destroy;

    function Bind(Device: in Vulkan.Device;
                  Buffer: in Vulkan.Buffer;
                  Memory: in Device_Memory;
                  Offset: in Device_Size) return Result
        renames C.vkBindBufferMemory;
end Vulkan.Buffers;


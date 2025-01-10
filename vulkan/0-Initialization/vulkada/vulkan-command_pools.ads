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

-- Command pool related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Command_Pools is
    use type Interfaces.Unsigned_32;

    -- vkCreateCommandPool
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Command_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Command_Pool: out Vulkan.Command_Pool) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Next = null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                         Command_Pool /= No_Command_Pool);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Command_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Command_Pool
        with Pre => Device /= No_Device and
                    Create_Info.Next = null,
             Post => Create'Result /= No_Command_Pool;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Command_Pool_Create_Info;
                    Command_Pool: out Vulkan.Command_Pool) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Next = null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                         Command_Pool /= No_Command_Pool);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Command_Pool_Create_Info)
        return Command_Pool
        with Pre => Device /= No_Device and
                    Create_Info.Next = null,
             Post => Create'Result /= No_Command_Pool;

    -- vkDestroyCommandPool
    procedure Destroy(Device: in Vulkan.Device;
                      Command_Pool: in out Vulkan.Command_Pool;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Command_Pool = No_Command_Pool;

    procedure Destroy(Device: in Vulkan.Device;
                      Command_Pool: in out Vulkan.Command_Pool)
        with Inline,
             Pre => Device /= No_Device,
             Post => Command_Pool = No_Command_Pool;

    -- vkResetCommandPool
    function Reset(Device: in Vulkan.Device;
                   Command_Pool: in Vulkan.Command_Pool;
                   Flags: in Command_Pool_Reset_Flags :=
                       Command_Pool_Reset_No_Bit) return Result
        with Pre => Device /= No_Device and
                    Command_Pool /= No_Command_Pool,
             Post => Reset'Result in Success |
                                     Out_Of_Device_Memory;

    procedure Reset(Device: in Vulkan.Device;
                    Command_Pool: in Vulkan.Command_Pool;
                    Flags: in Command_Pool_Reset_Flags :=
                        Command_Pool_Reset_No_Bit)
        with Inline,
             Pre => Device /= No_Device and
                    Command_Pool /= No_Command_Pool;

    -- vkAllocateCommandBuffers
    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Command_Buffer_Allocate_Info;
                      Command_Buffers: in out Command_Buffer_Vectors.Vector)
                          return Result
        with Pre => Device /= No_Device,
             Post => Allocate'Result in Success |
                                        Out_Of_Host_Memory |
                                        Out_Of_Device_Memory;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Command_Buffer_Allocate_Info)
                        return Command_Buffer_Vectors.Vector
        with Inline,
             Pre => Device /= No_Device,
             Post => (for all Buffer of Allocate'Result =>
                        Buffer /= No_Command_Buffer);

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Command_Buffer_Allocate_Info;
                      Command_Buffer: out Vulkan.Command_Buffer)
                        return Result
        with Pre => Device /= No_Device and
                    Allocate_Info.Command_Buffer_Count = 1,
             Post => Allocate'Result in Success |
                                        Out_Of_Host_Memory |
                                        Out_Of_Device_Memory;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Command_Buffer_Allocate_Info)
                        return Command_Buffer
        with Inline,
             Pre => Device /= No_Device and
                    Allocate_Info.Command_Buffer_Count = 1,
             Post => Allocate'Result /= No_Command_Buffer;

    -- vkFreeCommandBuffers
    procedure Free(Device: in Vulkan.Device;
                   Command_Pool: in Vulkan.Command_Pool;
                   Command_Buffers: in out Command_Buffer_Vectors.Vector)
        with Pre => Device /= No_Device and
                    not Command_Buffers.Is_Empty,
             Post => Command_Buffers.Is_Empty;

    procedure Free(Device: in Vulkan.Device;
                   Command_Pool: in Vulkan.Command_Pool;
                   Command_Buffer: in out Vulkan.Command_Buffer)
        with Pre => Device /= No_Device,
             Post => Command_Buffer = No_Command_Buffer;

    -- vkBeginCommandBuffer
    function Begin_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                          Begin_Info: in Command_Buffer_Begin_Info) return Result
        with Pre => Command_BUffer /= No_Command_Buffer,
             Post => Begin_Buffer'Result in Success |
                                            Out_Of_Host_Memory |
                                            Out_Of_Device_Memory;

    procedure Begin_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                           Begin_Info: in Command_Buffer_Begin_Info)
        with Inline,
             Pre => Command_BUffer /= No_Command_Buffer;

    -- vkEndCommandBuffer
    function End_Buffer(Command_Buffer: in Vulkan.Command_Buffer) return Result
        with Pre => Command_Buffer /= No_Command_Buffer,
             Post => End_Buffer'Result in Success |
                                          Out_Of_Host_Memory |
                                          Out_Of_Device_Memory;

    procedure End_Buffer(Command_Buffer: in Vulkan.Command_Buffer)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkResetCommandBuffer
    function Reset_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                          Flags: in Command_Buffer_Reset_Flags :=
                              Command_Buffer_Reset_No_Bit) return Result
        with Pre => Command_Buffer /= No_Command_Buffer,
             Post => Reset_Buffer'Result in Success |
                                            Out_Of_Device_Memory;

    procedure Reset_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                           Flags: in Command_Buffer_Reset_Flags :=
                               Command_Buffer_Reset_No_Bit)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- Vulkan 1.1
    -- vkTrimCommandPool
    procedure Trim(Device: in Vulkan.Device;
                   Command_Pool: in Vulkan.Command_Pool;
                   Flags: in Command_Pool_Trim_Flags :=
                       Command_Pool_Trim_No_Bit)
        with Inline,
             Pre => Device /= No_Device and
                    Command_Pool /= No_Command_Pool;

private
    package Command_Pools_Common is
        new Objects_Common(Command_Pool_Create_Info,
                           C.Command_Pool_Create_Info_C,
                           Command_Pool,
                           No_Command_Pool,
                           C.To_C,
                           C.Free,
                           C.vkCreateCommandPool,
                           C.vkDestroyCommandPool);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Command_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Command_Pool: out Vulkan.Command_Pool) return Result
        renames Command_Pools_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Command_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Command_Pool
        renames Command_Pools_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Command_Pool_Create_Info;
                    Command_Pool: out Vulkan.Command_Pool) return Result
        renames Command_Pools_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Command_Pool_Create_Info)
                        return Command_Pool
        renames Command_Pools_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Command_Pool: in out Vulkan.Command_Pool;
                      Allocator: aliased in Allocation_Callbacks)
        renames Command_Pools_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Command_Pool: in out Vulkan.Command_Pool)
        renames Command_Pools_Common.Destroy;

    function Reset(Device: in Vulkan.Device;
                   Command_Pool: in Vulkan.Command_Pool;
                   Flags: in Command_Pool_Reset_Flags :=
                       Command_Pool_Reset_No_Bit) return Result
        renames C.vkResetCommandPool;
    
    function End_Buffer(Command_Buffer: in Vulkan.Command_Buffer) return Result
        renames C.vkEndCommandBuffer;
    
    function Reset_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                          Flags: in Command_Buffer_Reset_Flags :=
                            Command_Buffer_Reset_No_Bit) return Result
        renames C.vkResetCommandBuffer;
end Vulkan.Command_Pools;


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

with Vulkan.Exceptions;
with Vulkan.C_V1_1;

package body Vulkan.Command_Pools is
    procedure Reset(Device: in Vulkan.Device;
                    Command_Pool: in Vulkan.Command_Pool;
                    Flags: in Command_Pool_Reset_Flags :=
                        Command_Pool_Reset_No_Bit) is
    begin
        Exceptions.Check(Reset(Device, Command_Pool, Flags));
    end Reset;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Command_Buffer_Allocate_Info;
                      Command_Buffers: in out Command_Buffer_Vectors.Vector)
                          return Result is
        Allocate_Info_C: C.Command_Buffer_Allocate_Info_C;
        Result: Vulkan.Result;
        Command_Buffers_C: array (1 .. Positive(Allocate_Info.Command_Buffer_Count))
                                of aliased Command_Buffer
            with Convention => C;
    begin
        Allocate_Info_C := C.To_C(Allocate_Info);
        Result := C.vkAllocateCommandBuffers(Device,
                                             Allocate_Info_C,
                                             Command_Buffers_C(1)'Access);
        C.Free(Allocate_Info_C);

        Command_Buffers.Clear;

        for X of Command_Buffers_C loop
            Command_Buffers.Append(X);
        end loop;

        return Result;
    end Allocate;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Command_Buffer_Allocate_Info)
                        return Command_Buffer_Vectors.Vector is
        Command_Buffers: Command_Buffer_Vectors.Vector;
    begin
        Exceptions.Check(Allocate(Device, Allocate_Info, Command_Buffers));

        return Command_Buffers;
    end Allocate;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Command_Buffer_Allocate_Info;
                      Command_Buffer: out Vulkan.Command_Buffer)
                        return Result is
        Allocate_Info_C: C.Command_Buffer_Allocate_Info_C;
        Result : Vulkan.Result;
        Local_Buffer: aliased Vulkan.Command_Buffer;
    begin
        Allocate_Info_C := C.To_C(Allocate_Info);
        Result := C.vkAllocateCommandBuffers(Device,
                                             Allocate_Info_C,
                                             Local_Buffer'Access);

        C.Free(Allocate_Info_C);
        Command_Buffer := Local_Buffer;

        return Result;
    end Allocate;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Command_Buffer_Allocate_Info)
                        return Command_Buffer is
        Command_Buffer: Vulkan.Command_Buffer;
    begin
        Exceptions.Check(Allocate(Device, Allocate_Info, Command_Buffer));

        return Command_Buffer;
    end Allocate;

    procedure Free(Device: in Vulkan.Device;
                   Command_Pool: in Vulkan.Command_Pool;
                   Command_Buffers: in out Command_Buffer_Vectors.Vector) is
        
        Command_Buffers_C: array (1 .. Positive(Command_Buffers.Length))
                                of aliased Command_Buffer
            with Convention => C;
    begin
        for X in Command_Buffers_C'Range loop
            Command_Buffers_C(X) := Command_Buffers(X);
        end loop;

        C.vkFreeCommandBuffers(Device,
                               Command_Pool,
                               Interfaces.Unsigned_32(Command_Buffers.Length),
                               Command_Buffers_C(1)'Access);
        Command_Buffers.Clear;
    end Free;

    procedure Free(Device: in Vulkan.Device;
                   Command_Pool: in Vulkan.Command_Pool;
                   Command_Buffer: in out Vulkan.Command_Buffer) is
        Local_Buffer: aliased Vulkan.Command_Buffer := Command_Buffer;
    begin
        C.vkFreeCommandBuffers(Device,
                               Command_Pool,
                               1,
                               Local_Buffer'Access);
        Command_Buffer := No_Command_Buffer;
    end Free;

    function Begin_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                          Begin_Info: in Command_Buffer_Begin_Info) return Result is
        Begin_Info_C: C.Command_Buffer_Begin_Info_C := C.To_C(Begin_Info);
        Result: Vulkan.Result;
    begin
        Result := C.vkBeginCommandBuffer(Command_Buffer,
                                         Begin_Info_C);
        C.Free(Begin_Info_C);

        return Result;
    end Begin_Buffer;
    
    procedure Begin_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                           Begin_Info: in Command_Buffer_Begin_Info) is
    begin
        Exceptions.Check(Begin_Buffer(Command_Buffer, Begin_Info));
    end Begin_Buffer;

    procedure End_Buffer(Command_Buffer: in Vulkan.Command_Buffer) is
    begin
        Exceptions.Check(End_Buffer(Command_Buffer));
    end End_Buffer;

    procedure Reset_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                           Flags: in Command_Buffer_Reset_Flags :=
                               Command_Buffer_Reset_No_Bit) is
    begin
        Exceptions.Check(Reset_Buffer(Command_Buffer, Flags));
    end Reset_Buffer;

    procedure Trim(Device: in Vulkan.Device;
                   Command_Pool: in Vulkan.Command_Pool;
                   Flags: in Command_Pool_Trim_Flags :=
                       Command_Pool_Trim_No_Bit) is
    begin
        C_V1_1.vkTrimCommandPool(Device, Command_Pool, Flags);
    end Trim;
end Vulkan.Command_Pools;


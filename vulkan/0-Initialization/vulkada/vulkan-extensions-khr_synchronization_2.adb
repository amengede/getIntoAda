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

-- Operations for the synchronization 2 extension

with Vulkan.C_NV;
with Vulkan.Core;
with Vulkan.Extension_Records;

package body Vulkan.Extensions.KHR_Synchronization_2 is
    -- Loaded extension functions.
    type vkCmdWriteBufferMarker2AMD_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Stage: in Pipeline_Stage_Flags_2;
                         Dst_Buffer: in Buffer;
                         Dst_Offset: in Device_Size;
                         Marker: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdWriteBufferMarker2AMD: vkCmdWriteBufferMarker2AMD_Access;

    type vkGetQueueCheckpointData2NV_Access is
        access procedure
            (Queue: in Vulkan.Queue;
             Checkpoint_Data_Count: in out Interfaces.Unsigned_32;
             Checkpoint_Data: access C_NV.Checkpoint_Data_2_C)
        with Convention => C;

    vkGetQueueCheckpointData2NV: vkGetQueueCheckpointData2NV_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdWriteBufferMarker2AMD_Access);
        procedure Load is new Load_Pointer(vkGetQueueCheckpointData2NV_Access);
    begin
        Load(vkCmdWriteBufferMarker2AMD, "vkCmdWriteBufferMarker2AMD");
        Load(vkGetQueueCheckpointData2NV, "vkGetQueueCheckpointData2NV");
    end Load_Extension;

    procedure Write_Buffer_Marker_2(Command_Buffer: in Vulkan.Command_Buffer;
                                    Stage: in Pipeline_Stage_Flags_2;
                                    Dst_Buffer: in Buffer;
                                    Dst_Offset: in Device_Size;
                                    Marker: in Interfaces.Unsigned_32) is
    begin
        vkCmdWriteBufferMarker2AMD(Command_Buffer,
                                   Stage,
                                   Dst_Buffer,
                                   Dst_Offset,
                                   Marker);
    end Write_Buffer_Marker_2;
    
    function Checkpoint_Data_Count(Queue: in Vulkan.Queue)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        vkGetQueueCheckpointData2NV(Queue, Count, null);

        return Count;
    end Checkpoint_Data_Count;
    
    procedure Get_Checkpoint_Data
        (Queue: in Vulkan.Queue;
         Checkpoint_Data: in out NV.Checkpoint_Data_2_Vectors.Vector) is
    begin
        if Checkpoint_Data.Is_Empty then
            return;
        end if;

        declare
            Count: Interfaces.Unsigned_32 :=
                Interfaces.Unsigned_32(Checkpoint_Data.Length);
            C_Data: array (1 .. Positive(Count)) of
                aliased C_NV.Checkpoint_Data_2_C;
        begin
            for X in C_Data'Range loop
                C_Data(X).Next :=
                    Extension_Records.To_C(Checkpoint_Data(X).Next);
            end loop;

            vkGetQueueCheckpointData2NV(Queue, Count, C_Data(1)'Access);

            for X in C_Data'Range loop
                C_NV.To_Ada(Checkpoint_Data(X), C_Data(X));
                Extension_Records.Free(C_Data(X).Next);
            end loop;
        end;
    end Get_Checkpoint_Data;

    function Get_Checkpoint_Data(Queue: in Vulkan.Queue)
        return NV.Checkpoint_Data_2_Vectors.Vector is
        Count: Interfaces.Unsigned_32 := Checkpoint_Data_Count(Queue);
        Data: NV.Checkpoint_Data_2_Vectors.Vector;
        Datum: NV.Checkpoint_Data_2;
    begin
        Data.Append(Datum, Ada.Containers.Count_Type(Count));
        Get_Checkpoint_Data(Queue, Data);

        return Data;
    end Get_Checkpoint_Data;
end Vulkan.Extensions.KHR_Synchronization_2;


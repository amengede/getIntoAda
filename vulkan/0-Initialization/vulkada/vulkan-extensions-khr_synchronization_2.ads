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

with Vulkan.Extensions.NV;

package Vulkan.Extensions.KHR_Synchronization_2 is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdWriteBufferMarker2AMD
    procedure Write_Buffer_Marker_2(Command_Buffer: in Vulkan.Command_Buffer;
                                    Stage: in Pipeline_Stage_Flags_2;
                                    Dst_Buffer: in Buffer;
                                    Dst_Offset: in Device_Size;
                                    Marker: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Dst_Buffer /= No_Buffer;

    -- vkGetQueueCheckpointData2NV
    function Checkpoint_Data_Count(Queue: in Vulkan.Queue)
        return Interfaces.Unsigned_32
        with Inline,
             Pre => Queue /= No_Queue;

    procedure Get_Checkpoint_Data
        (Queue: in Vulkan.Queue;
         Checkpoint_Data: in out NV.Checkpoint_Data_2_Vectors.Vector)
        with Pre => Queue /= No_Queue;

    function Get_Checkpoint_Data(Queue: in Vulkan.Queue)
        return NV.Checkpoint_Data_2_Vectors.Vector
        with Pre => Queue /= No_Queue;
end Vulkan.Extensions.KHR_Synchronization_2;


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

-- Operations for the video decode queue extension

with Vulkan.C_KHR;
with Vulkan.Core;

package body Vulkan.Extensions.KHR_Video_Decode_Queue is
    -- Loaded extension functions.
    type vkCmdDecodeVideoKHR_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Decode_Info: in C_KHR.Video_Decode_Info_C)
        with Convention => C;

    vkCmdDecodeVideoKHR: vkCmdDecodeVideoKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdDecodeVideoKHR_Access);
    begin
        Load(vkCmdDecodeVideoKHR, "vkCmdDecodeVideoKHR");
    end Load_Extension;
    
    procedure Decode_Video(Command_Buffer: in Vulkan.Command_Buffer;
                           Decode_Info: in KHR.Video_Decode_Info) is
        Decode_Info_C: C_KHR.Video_Decode_Info_C := C_KHR.To_C(Decode_Info);
    begin
        vkCmdDecodeVideoKHR(Command_Buffer, Decode_Info_C);
        C_KHR.Free(Decode_Info_C);
    end Decode_Video;
end Vulkan.Extensions.KHR_Video_Decode_Queue;


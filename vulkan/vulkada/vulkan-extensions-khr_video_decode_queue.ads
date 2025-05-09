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

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Video_Decode_Queue is
    use type KHR.Video_Picture_Resource_Info_Access;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdDecodeVideoKHR
    procedure Decode_Video(Command_Buffer: in Vulkan.Command_Buffer;
                           Decode_Info: in KHR.Video_Decode_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Decode_Info.Src_Buffer /= No_Buffer and
                    Decode_Info.Dst_Picture_Resource.Image_View_Binding /=
                    No_Image_View and
                    (for all Slot of Decode_Info.Reference_Slots =>
                        (if Slot.Picture_Resource /= null then
                            Slot.Picture_Resource.Image_View_Binding /=
                            No_Image_View));
end Vulkan.Extensions.KHR_Video_Decode_Queue;


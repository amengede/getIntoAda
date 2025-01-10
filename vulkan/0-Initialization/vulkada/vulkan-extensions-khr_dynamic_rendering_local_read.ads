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

-- Operations for the dynamic rendering local read extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Dynamic_Rendering_Local_Read is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdSetRenderingAttachmentLocationsKHR
    procedure Set_Rendering_Attachment_Locations
        (Command_Buffer: in Vulkan.Command_Buffer;
         Location_Info: in KHR.Rendering_Attachment_Location_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetRenderingInputAttachmentIndicesKHR
    procedure Set_Rendering_Input_Attachment_Indices
        (Command_Buffer: in Vulkan.Command_Buffer;
         Input_Attachment_Index_Info:
            in KHR.Rendering_Input_Attachment_Index_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;
end Vulkan.Extensions.KHR_Dynamic_Rendering_Local_Read;


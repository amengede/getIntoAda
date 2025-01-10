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

with Vulkan.Core;
with Vulkan.C_KHR;

package body Vulkan.Extensions.KHR_Dynamic_Rendering_Local_Read is
    -- Loaded extension functions.
    type vkCmdSetRenderingAttachmentLocationsKHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Location_Info: in C_KHR.Rendering_Attachment_Location_Info_C)
        with Convention => C;

    vkCmdSetRenderingAttachmentLocationsKHR:
        vkCmdSetRenderingAttachmentLocationsKHR_Access;

    type vkCmdSetRenderingInputAttachmentIndicesKHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Input_Attachment_Index_Info:
                in C_KHR.Rendering_Input_Attachment_Index_Info_C)
        with Convention => C;

    vkCmdSetRenderingInputAttachmentIndicesKHR:
        vkCmdSetRenderingInputAttachmentIndicesKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer
            (vkCmdSetRenderingAttachmentLocationsKHR_Access);
        procedure Load is new Load_Pointer
            (vkCmdSetRenderingInputAttachmentIndicesKHR_Access);
    begin
        Load(vkCmdSetRenderingAttachmentLocationsKHR,
             "vkCmdSetRenderingAttachmentLocationsKHR");
        Load(vkCmdSetRenderingInputAttachmentIndicesKHR,
             "vkCmdSetRenderingInputAttachmentIndicesKHR");
    end Load_Extension;

    procedure Set_Rendering_Attachment_Locations
        (Command_Buffer: in Vulkan.Command_Buffer;
         Location_Info: in KHR.Rendering_Attachment_Location_Info) is
        C_Info: C_KHR.Rendering_Attachment_Location_Info_C :=
            C_KHR.To_C(Location_Info);
    begin
        vkCmdSetRenderingAttachmentLocationsKHR(Command_Buffer, C_Info);
        C_KHR.Free(C_Info);
    end Set_Rendering_Attachment_Locations;

    procedure Set_Rendering_Input_Attachment_Indices
        (Command_Buffer: in Vulkan.Command_Buffer;
         Input_Attachment_Index_Info:
            in KHR.Rendering_Input_Attachment_Index_Info) is
        C_Info: C_KHR.Rendering_Input_Attachment_Index_Info_C :=
            C_KHR.To_C(Input_Attachment_Index_Info);
    begin
        vkCmdSetRenderingInputAttachmentIndicesKHR(Command_Buffer, C_Info);
        C_KHR.Free(C_Info);
    end Set_Rendering_Input_Attachment_Indices;
end Vulkan.Extensions.KHR_Dynamic_Rendering_Local_Read;


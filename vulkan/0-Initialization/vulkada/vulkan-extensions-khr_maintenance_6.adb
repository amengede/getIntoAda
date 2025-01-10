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

-- Operations for the maintenance 5 extension

with Vulkan.Core;
with Vulkan.C_KHR;

package body Vulkan.Extensions.KHR_Maintenance_6 is
    -- Loaded extension functions.
    type vkCmdBindDescriptorSets2KHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Bind_Descriptor_Sets_Info: in C_KHR.Bind_Descriptor_Sets_Info_C)
        with Convention => C;

    vkCmdBindDescriptorSets2KHR: vkCmdBindDescriptorSets2KHR_Access;

    type vkCmdPushConstants2KHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Push_Constants_Info: in C_KHR.Push_Constants_Info_C)
        with Convention => C;

    vkCmdPushConstants2KHR: vkCmdPushConstants2KHR_Access;
    
    type vkCmdPushDescriptorSet2KHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Push_Descriptor_Set_Info: in C_KHR.Push_Descriptor_Set_Info_C)
        with Convention => C;

    vkCmdPushDescriptorSet2KHR: vkCmdPushDescriptorSet2KHR_Access;

    type vkCmdPushDescriptorSetWithTemplate2KHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Push_Descriptor_SetWithTemplate_Info:
                in C_KHR.Push_Descriptor_Set_With_Template_Info_C)
        with Convention => C;

    vkCmdPushDescriptorSetWithTemplate2KHR:
        vkCmdPushDescriptorSetWithTemplate2KHR_Access;

    type vkCmdSetDescriptorBufferOffsets2EXT_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Set_Descriptor_Buffer_Offsets_Info:
                in C_KHR.Set_Descriptor_Buffer_Offsets_Info_C)
        with Convention => C;

    vkCmdSetDescriptorBufferOffsets2EXT:
        vkCmdSetDescriptorBufferOffsets2EXT_Access;

    type vkCmdBindDescriptorBufferEmbeddedSamplers2EXT_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Bind_Descriptor_Buffer_Embedded_Samplers_Info: in
                C_KHR.Bind_Descriptor_Buffer_Embedded_Samplers_Info_C)
        with Convention => C;

    vkCmdBindDescriptorBufferEmbeddedSamplers2EXT:
        vkCmdBindDescriptorBufferEmbeddedSamplers2EXT_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdBindDescriptorSets2KHR_Access);
        procedure Load is new Load_Pointer(vkCmdPushConstants2KHR_Access);
        procedure Load is new Load_Pointer(vkCmdPushDescriptorSet2KHR_Access);
        procedure Load is
            new Load_Pointer(vkCmdPushDescriptorSetWithTemplate2KHR_Access);
        procedure Load is
            new Load_Pointer(vkCmdSetDescriptorBufferOffsets2EXT_Access);
        procedure Load is new Load_Pointer
            (vkCmdBindDescriptorBufferEmbeddedSamplers2EXT_Access);
    begin
        Load(vkCmdBindDescriptorSets2KHR, "vkCmdBindDescriptorSets2KHR");
        Load(vkCmdPushConstants2KHR, "vkCmdPushConstants2KHR");
        Load(vkCmdPushDescriptorSet2KHR, "vkCmdPushDescriptorSet2KHR");
        Load(vkCmdPushDescriptorSetWithTemplate2KHR,
             "vkCmdPushDescriptorSetWithTemplate2KHR");
        Load(vkCmdSetDescriptorBufferOffsets2EXT,
             "vkCmdSetDescriptorBufferOffsets2EXT");
        Load(vkCmdBindDescriptorBufferEmbeddedSamplers2EXT,
             "vkCmdBindDescriptorBufferEmbeddedSamplers2EXT");
    end Load_Extension;

    procedure Bind
        (Command_Buffer: in Vulkan.Command_Buffer;
         Bind_Descriptor_Sets_Info: in KHR.Bind_Descriptor_Sets_Info) is
        Info_C: C_KHR.Bind_Descriptor_Sets_Info_C :=
            C_KHR.To_C(Bind_Descriptor_Sets_Info);
    begin
        vkCmdBindDescriptorSets2KHR(Command_Buffer, Info_C);
        C_KHR.Free(Info_C);
    end Bind;

    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Push_Constants_Info: in KHR.Push_Constants_Info) is
        Info_C: C_KHR.Push_Constants_Info_C := C_KHR.To_C(Push_Constants_Info);
    begin
        vkCmdPushConstants2KHR(Command_Buffer, Info_C);
        C_KHR.Free(Info_C);
    end Push;

    procedure Push
        (Command_Buffer: in Vulkan.Command_Buffer;
         Push_Descriptor_Set_Info: in KHR.Push_Descriptor_Set_Info) is
        Info_C: C_KHR.Push_Descriptor_Set_Info_C :=
            C_KHR.To_C(Push_Descriptor_Set_Info);
    begin
        vkCmdPushDescriptorSet2KHR(Command_Buffer, Info_C);
        C_KHR.Free(Info_C);
    end Push;

    procedure Push
        (Command_Buffer: in Vulkan.Command_Buffer;
         Push_Descriptor_Set_With_Template_Info:
            in KHR.Push_Descriptor_Set_With_Template_Info) is
        Info_C: C_KHR.Push_Descriptor_Set_With_Template_Info_C :=
            C_KHR.To_C(Push_Descriptor_Set_With_Template_Info);
    begin
        vkCmdPushDescriptorSetWithTemplate2KHR(Command_Buffer, Info_C);
        C_KHR.Free(Info_C);
    end Push;

    procedure Set_Descriptor_Buffer_Offsets
        (Command_Buffer: in Vulkan.Command_Buffer;
         Set_Descriptor_Buffer_Offsets_Info:
            in KHR.Set_Descriptor_Buffer_Offsets_Info) is
        Info_C: C_KHR.Set_Descriptor_Buffer_Offsets_Info_C :=
            C_KHR.To_C(Set_Descriptor_Buffer_Offsets_Info);
    begin
        vkCmdSetDescriptorBufferOffsets2EXT(Command_Buffer, Info_C);
        C_KHR.Free(Info_C);
    end Set_Descriptor_Buffer_Offsets;

    procedure Bind
        (Command_Buffer: in Vulkan.Command_Buffer;
         Bind_Descriptor_Buffer_Embedded_Samplers_Info:
            in KHR.Bind_Descriptor_Buffer_Embedded_Samplers_Info) is
        Info_C:
            C_KHR.Bind_Descriptor_Buffer_Embedded_Samplers_Info_C :=
                C_KHR.To_C(Bind_Descriptor_Buffer_Embedded_Samplers_Info);
    begin
        vkCmdBindDescriptorBufferEmbeddedSamplers2EXT(Command_Buffer, Info_C);
        C_KHR.Free(Info_C);
    end Bind;
end Vulkan.Extensions.KHR_Maintenance_6;


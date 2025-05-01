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

-- Operations for the maintenance 6 extension

package Vulkan.Maintenance_6 is
    use type Ada.Containers.Count_Type;
    use type System.Address;
    use type Interfaces.Unsigned_32;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdBindDescriptorSets2KHR
    procedure Bind
        (Command_Buffer: in Vulkan.Command_Buffer;
         Bind_Descriptor_Sets_Info: in Vulkan.Bind_Descriptor_Sets_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Bind_Descriptor_Sets_Info.Stage_Flags /=
                    Shader_Stage_No_Bit and
                    not Bind_Descriptor_Sets_Info.Descriptor_Sets.Is_Empty and
                    (for all Set of Bind_Descriptor_Sets_Info.Descriptor_Sets =>
                        Set /= No_Descriptor_Set);

    -- vkPushConstants2KHR
    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Push_Constants_Info: in Vulkan.Push_Constants_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Push_Constants_Info.Stage_Flags /= Shader_Stage_No_Bit and
                    Push_Constants_Info.Size > 0 and
                    Push_Constants_Info.Values /= System.Null_Address;

    -- vkCmdPushDescriptorSet2KHR
    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Push_Descriptor_Set_Info: in Vulkan.Push_Descriptor_Set_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Push_Descriptor_Set_Info.Stage_Flags /=
                    Shader_Stage_No_Bit and
                    not Push_Descriptor_Set_Info.Descriptor_Writes.Is_Empty and
                    (for all Write of
                        Push_Descriptor_Set_Info.Descriptor_Writes =>
                            Write.Dst_Set /= No_Descriptor_Set and
                            Write.Descriptor_Count /= 0);

    -- vkCmdPushDescriptorSetWithTemplate2KHR
    procedure Push
        (Command_Buffer: in Vulkan.Command_Buffer;
         Push_Descriptor_Set_With_Template_Info:
            in Vulkan.Push_Descriptor_Set_With_Template_Info)
        with Pre =>
            Command_Buffer /= No_Command_Buffer and
            Push_Descriptor_Set_With_Template_Info.Descriptor_Update_Template /=
            No_Descriptor_Update_Template and
            Push_Descriptor_Set_With_Template_Info.Layout /=
            No_Pipeline_Layout and
            Push_Descriptor_Set_With_Template_Info.Data /= System.Null_Address;

    -- vkCmdSetDescriptorBufferOffsets2EXT
    procedure Set_Descriptor_Buffer_Offsets
        (Command_Buffer: in Vulkan.Command_Buffer;
         Set_Descriptor_Buffer_Offsets_Info:
            in Vulkan.Set_Descriptor_Buffer_Offsets_Info)
        with Pre =>
            Command_Buffer /= No_Command_Buffer and
            Set_Descriptor_Buffer_Offsets_Info.Stage_Flags /=
            Shader_Stage_No_Bit and
            not Set_Descriptor_Buffer_Offsets_Info.Buffer_Indices.Is_Empty and
            Set_Descriptor_Buffer_Offsets_Info.Buffer_Indices.Length =
            Set_Descriptor_Buffer_Offsets_Info.Offsets.Length;

    -- vkCmdBindDescriptorBufferEmbeddedSamplers2EXT
    procedure Bind
        (Command_Buffer: in Vulkan.Command_Buffer;
         Bind_Descriptor_Buffer_Embedded_Samplers_Info:
            in Vulkan.Bind_Descriptor_Buffer_Embedded_Samplers_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Bind_Descriptor_Buffer_Embedded_Samplers_Info.Stage_Flags /=
                    Shader_Stage_No_Bit;
end Vulkan.Maintenance_6;


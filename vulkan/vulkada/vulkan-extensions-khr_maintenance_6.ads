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

-- Operations for the maintenance 6 extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Maintenance_6 is
    use type Ada.Containers.Count_Type;
    use type System.Address;
    use type Interfaces.Unsigned_32;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdSetDescriptorBufferOffsets2EXT
    procedure Set_Descriptor_Buffer_Offsets
        (Command_Buffer: in Vulkan.Command_Buffer;
         Set_Descriptor_Buffer_Offsets_Info:
            in KHR.Set_Descriptor_Buffer_Offsets_Info)
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
            in KHR.Bind_Descriptor_Buffer_Embedded_Samplers_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Bind_Descriptor_Buffer_Embedded_Samplers_Info.Stage_Flags /=
                    Shader_Stage_No_Bit;
end Vulkan.Extensions.KHR_Maintenance_6;


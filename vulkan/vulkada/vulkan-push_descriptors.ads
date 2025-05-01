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

-- Operation for the push descriptor extension

package Vulkan.Push_Descriptors is
    use type System.Address;
    use type Interfaces.Unsigned_32;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdPushDescriptorSetKHR
    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Bind_Point: in Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   Set: in Interfaces.Unsigned_32;
                   Descriptor_Writes: in Write_Descriptor_Set_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Layout /= No_Pipeline_Layout and
                    not Descriptor_Writes.Is_Empty and
                    (for all Write of Descriptor_Writes =>
                        Write.Dst_Set /= No_Descriptor_Set and
                        Write.Descriptor_Count > 0);

    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Bind_Point: in Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   Set: in Interfaces.Unsigned_32;
                   Descriptor_Write: in Write_Descriptor_Set)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Layout /= No_Pipeline_Layout and
                    Descriptor_Write.Dst_Set /= No_Descriptor_Set and
                    Descriptor_Write.Descriptor_Count > 0;

    -- vkCmdPushDescriptorSetWithTemplateKHR
    procedure Push
        (Command_Buffer: in Vulkan.Command_Buffer;
         Descriptor_Update_Template: in Vulkan.Descriptor_Update_Template;
         Layout: in Pipeline_Layout;
         Set: in Interfaces.Unsigned_32;
         Data: in Interfaces.C.Extensions.void_ptr)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Descriptor_Update_Template /=
                    No_Descriptor_Update_Template and
                    Layout /= No_Pipeline_Layout and
                    Data /= System.Null_Address;
end Vulkan.Push_Descriptors;


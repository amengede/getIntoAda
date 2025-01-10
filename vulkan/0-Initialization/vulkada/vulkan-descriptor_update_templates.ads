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

-- Descriptor update template subprograms

private with Vulkan.Objects_Common_Access;
private with Vulkan.C_V1_1;

package Vulkan.Descriptor_Update_Templates is
    -- Vulkan 1.1
    -- vkCreateDescriptorUpdateTemplate
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Update_Template_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Template: out Descriptor_Update_Template)
        return Result
        with Pre => Device /= No_Device and
                    Create_Info.Flags =
                        Descriptor_Update_Template_Create_No_Bit and
                    not Create_Info.Descriptor_Update_Entries.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Template /= No_Descriptor_Update_Template);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Update_Template_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Descriptor_Update_Template
        with Pre => Device /= No_Device and
                    Create_Info.Flags =
                        Descriptor_Update_Template_Create_No_Bit and
                    not Create_Info.Descriptor_Update_Entries.Is_Empty,
             Post => Create'Result /= No_Descriptor_Update_Template;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Update_Template_Create_Info;
                    Template: out Descriptor_Update_Template)
        return Result
        with Pre => Device /= No_Device and
                    Create_Info.Flags =
                        Descriptor_Update_Template_Create_No_Bit and
                    not Create_Info.Descriptor_Update_Entries.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Template /= No_Descriptor_Update_Template);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Update_Template_Create_Info)
        return Descriptor_Update_Template
        with Pre => Device /= No_Device and
                    Create_Info.Flags =
                        Descriptor_Update_Template_Create_No_Bit and
                    not Create_Info.Descriptor_Update_Entries.Is_Empty,
             Post => Create'Result /= No_Descriptor_Update_Template;

    -- vkDestroyDescriptorUpdateTemplate
    procedure Destroy(Device: in Vulkan.Device;
                      Template: in out Descriptor_Update_Template;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Template = No_Descriptor_Update_Template;

    procedure Destroy(Device: in Vulkan.Device;
                      Template: in out Descriptor_Update_Template)
        with Inline,
             Pre => Device /= No_Device,
             Post => Template = No_Descriptor_Update_Template;

private
    package Descriptor_Update_Templates_Common is
        new Objects_Common_Access
            (Descriptor_Update_Template_Create_Info,
             C_V1_1.Descriptor_Update_Template_Create_Info_C,
             Descriptor_Update_Template,
             No_Descriptor_Update_Template,
             C_V1_1.To_C,
             C_V1_1.Free,
             C_V1_1.vkCreateDescriptorUpdateTemplate_Access,
             C_V1_1.vkDestroyDescriptorUpdateTemplate_Access,
             C_V1_1.vkCreateDescriptorUpdateTemplate,
             C_V1_1.vkDestroyDescriptorUpdateTemplate);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Update_Template_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Template: out Descriptor_Update_Template)
        return Result
        renames Descriptor_Update_Templates_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Update_Template_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Descriptor_Update_Template
        renames Descriptor_Update_Templates_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Update_Template_Create_Info;
                    Template: out Descriptor_Update_Template)
        return Result
        renames Descriptor_Update_Templates_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Update_Template_Create_Info)
        return Descriptor_Update_Template
        renames Descriptor_Update_Templates_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Template: in out Descriptor_Update_Template;
                      Allocator: aliased in Allocation_Callbacks)
        renames Descriptor_Update_Templates_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Template: in out Descriptor_Update_Template)
        renames Descriptor_Update_Templates_Common.Destroy;
end Vulkan.Descriptor_Update_Templates;


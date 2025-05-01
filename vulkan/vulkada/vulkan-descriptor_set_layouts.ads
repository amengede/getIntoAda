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

-- Descriptor set layout related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Descriptor_Set_Layouts is
    -- vkCreateDescriptorSetLayout
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Set_Layout_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Descriptor_Set_Layout: out Vulkan.Descriptor_Set_Layout)
        return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Descriptor_Set_Layout /= No_Descriptor_Set_Layout);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Set_Layout_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Descriptor_Set_Layout
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Descriptor_Set_Layout;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Set_Layout_Create_Info;
                    Descriptor_Set_Layout: out Vulkan.Descriptor_Set_Layout)
        return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Descriptor_Set_Layout /= No_Descriptor_Set_Layout);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Set_Layout_Create_Info)
        return Descriptor_Set_Layout
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Descriptor_Set_Layout;

    -- vkDestroyDescriptorSetLayout
    procedure Destroy
        (Device: in Vulkan.Device;
         Descriptor_Set_Layout: in out Vulkan.Descriptor_Set_Layout;
         Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Descriptor_Set_Layout = No_Descriptor_Set_Layout;

    procedure Destroy
        (Device: in Vulkan.Device;
         Descriptor_Set_Layout: in out Vulkan.Descriptor_Set_Layout)
        with Inline,
             Pre => Device /= No_Device,
             Post => Descriptor_Set_Layout = No_Descriptor_Set_Layout;

private
    package Descriptor_Set_Layouts_Common is
        new Objects_Common(Descriptor_Set_Layout_Create_Info,
                           C.Descriptor_Set_Layout_Create_Info_C,
                           Descriptor_Set_Layout,
                           No_Descriptor_Set_Layout,
                           C.To_C,
                           C.Free,
                           C.vkCreateDescriptorSetLayout,
                           C.vkDestroyDescriptorSetLayout);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Set_Layout_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Descriptor_Set_Layout: out Vulkan.Descriptor_Set_Layout)
                        return Result
        renames Descriptor_Set_Layouts_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Set_Layout_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Descriptor_Set_Layout
        renames Descriptor_Set_Layouts_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Set_Layout_Create_Info;
                    Descriptor_Set_Layout: out Vulkan.Descriptor_Set_Layout)
                        return Result
        renames Descriptor_Set_Layouts_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Set_Layout_Create_Info)
                        return Descriptor_Set_Layout
        renames Descriptor_Set_Layouts_Common.Create;

    procedure Destroy
        (Device: in Vulkan.Device;
         Descriptor_Set_Layout: in out Vulkan.Descriptor_Set_Layout;
         Allocator: aliased in Allocation_Callbacks)
        renames Descriptor_Set_Layouts_Common.Destroy;

    procedure Destroy
        (Device: in Vulkan.Device;
         Descriptor_Set_Layout: in out Vulkan.Descriptor_Set_Layout)
        renames Descriptor_Set_Layouts_Common.Destroy;
end Vulkan.Descriptor_Set_Layouts;


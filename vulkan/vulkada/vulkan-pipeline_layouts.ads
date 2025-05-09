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

-- Pipeline layout related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Pipeline_Layouts is
    -- vkCreatePipelineLayout
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Layout_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Pipeline_Layout: out Vulkan.Pipeline_Layout) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Pipeline_Layout /= No_Pipeline_Layout);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Layout_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Pipeline_Layout
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Pipeline_Layout;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Layout_Create_Info;
                    Pipeline_Layout: out Vulkan.Pipeline_Layout) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Pipeline_Layout /= No_Pipeline_Layout);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Layout_Create_Info)
        return Pipeline_Layout
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Pipeline_Layout;

    -- vkDestroyPipelineLayout
    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline_Layout: in out Vulkan.Pipeline_Layout;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Pipeline_Layout = No_Pipeline_Layout;

    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline_Layout: in out Vulkan.Pipeline_Layout)
        with Inline,
             Pre => Device /= No_Device,
             Post => Pipeline_Layout = No_Pipeline_Layout;

private
    package Pipeline_Layout_Common is
        new Objects_Common(Pipeline_Layout_Create_Info,
                           C.Pipeline_Layout_Create_Info_C,
                           Pipeline_Layout,
                           No_Pipeline_Layout,
                           C.To_C,
                           C.Free,
                           C.vkCreatePipelineLayout,
                           C.vkDestroyPipelineLayout);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Layout_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Pipeline_Layout: out Vulkan.Pipeline_Layout) return Result
        renames Pipeline_Layout_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Layout_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Pipeline_Layout
        renames Pipeline_Layout_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Layout_Create_Info;
                    Pipeline_Layout: out Vulkan.Pipeline_Layout) return Result
        renames Pipeline_Layout_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Layout_Create_Info)
                        return Pipeline_Layout
        renames Pipeline_Layout_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline_Layout: in out Vulkan.Pipeline_Layout;
                      Allocator: aliased in Allocation_Callbacks)
        renames Pipeline_Layout_Common.Destroy;


    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline_Layout: in out Vulkan.Pipeline_Layout)
        renames Pipeline_Layout_Common.Destroy;
end Vulkan.Pipeline_Layouts;


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

-- Render pass related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;
private with Vulkan.C_V1_2;

package Vulkan.Render_Passes is
    -- vkCreateRenderPass
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Render_Pass: out Vulkan.Render_Pass) return Result
        with Pre => Device /= No_Device and
                    not Create_Info.Subpasses.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Render_Pass /= No_Render_Pass);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Render_Pass
        with Pre => Device /= No_Device and
                    not Create_Info.Subpasses.Is_Empty,
             Post => Create'Result /= No_Render_Pass;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info;
                    Render_Pass: out Vulkan.Render_Pass) return Result
        with Pre => Device /= No_Device and
                    not Create_Info.Subpasses.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Render_Pass /= No_Render_Pass);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info)
        return Render_Pass
        with Pre => Device /= No_Device and
                    not Create_Info.Subpasses.Is_Empty,
             Post => Create'Result /= No_Render_Pass;

    -- vkDestroyRenderPass
    procedure Destroy(Device: in Vulkan.Device;
                      Render_Pass: in out Vulkan.Render_Pass;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Render_Pass = No_Render_Pass;

    procedure Destroy(Device: in Vulkan.Device;
                      Render_Pass: in out Vulkan.Render_Pass)
        with Inline,
             Pre => Device /= No_Device,
             Post => Render_Pass = No_Render_Pass;

    -- vkGetRenderAreaGranularity
    function Get_Granularity(Device: in Vulkan.Device;
                             Render_Pass: in Vulkan.Render_Pass)
        return Extent_2D
        with Inline,
             Pre => Device /= No_Device and
                    Render_Pass /= No_Render_Pass;

    -- Vulkan 1.2
    -- vkCreateRenderPass2
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info_2;
                    Allocator: aliased in Allocation_Callbacks;
                    Render_Pass: out Vulkan.Render_Pass) return Result
        with Pre => Device /= No_Device and
                    not Create_Info.Subpasses.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Render_Pass /= No_Render_Pass);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info_2;
                    Allocator: aliased in Allocation_Callbacks)
        return Render_Pass
        with Pre => Device /= No_Device and
                    not Create_Info.Subpasses.Is_Empty,
             Post => Create'Result /= No_Render_Pass;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info_2;
                    Render_Pass: out Vulkan.Render_Pass) return Result
        with Pre => Device /= No_Device and
                    not Create_Info.Subpasses.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Render_Pass /= No_Render_Pass);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info_2)
        return Render_Pass
        with Pre => Device /= No_Device and
                    not Create_Info.Subpasses.Is_Empty,
             Post => Create'Result /= No_Render_Pass;

private
    package Render_Passes_Common is
        new Objects_Common(Render_Pass_Create_Info,
                           C.Render_Pass_Create_Info_C,
                           Render_Pass,
                           No_Render_Pass,
                           C.To_C,
                           C.Free,
                           C.vkCreateRenderPass,
                           C.vkDestroyRenderPass);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Render_Pass: out Vulkan.Render_Pass) return Result
        renames Render_Passes_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Render_Pass
        renames Render_Passes_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info;
                    Render_Pass: out Vulkan.Render_Pass) return Result
        renames Render_Passes_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info)
        return Render_Pass
        renames Render_Passes_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Render_Pass: in out Vulkan.Render_Pass;
                      Allocator: aliased in Allocation_Callbacks)
        renames Render_Passes_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Render_Pass: in out Vulkan.Render_Pass)
        renames Render_Passes_Common.Destroy;
end Vulkan.Render_Passes;


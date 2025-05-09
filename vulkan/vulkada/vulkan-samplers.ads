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

-- Sampler related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Samplers is
    use type Interfaces.C.C_float;

    -- vkCreateSampler
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Sampler: out Vulkan.Sampler) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Max_Lod >= Create_Info.Min_Lod,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Sampler /= No_Sampler);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Sampler
        with Pre => Device /= No_Device and
                    Create_Info.Max_Lod >= Create_Info.Min_Lod,
             Post => Create'Result /= No_Sampler;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_Create_Info;
                    Sampler: out Vulkan.Sampler) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Max_Lod >= Create_Info.Min_Lod,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Sampler /= No_Sampler);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_Create_Info) return Sampler
        with Pre => Device /= No_Device and
                    Create_Info.Max_Lod >= Create_Info.Min_Lod,
             Post => Create'Result /= No_Sampler;

    -- vkDestroySampler
    procedure Destroy(Device: in Vulkan.Device;
                      Sampler: in out Vulkan.Sampler;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Sampler = No_Sampler;

    procedure Destroy(Device: in Vulkan.Device;
                      Sampler: in out Vulkan.Sampler)
        with Inline,
             Pre => Device /= No_Device,
             Post => Sampler = No_Sampler;

private
    package Sampler_Common is
        new Objects_Common(Sampler_Create_Info,
                           C.Sampler_Create_Info_C,
                           Sampler,
                           No_Sampler,
                           C.To_C,
                           C.Free,
                           C.vkCreateSampler,
                           C.vkDestroySampler);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Sampler: out Vulkan.Sampler) return Result
        renames Sampler_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Sampler
        renames Sampler_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_Create_Info;
                    Sampler: out Vulkan.Sampler) return Result
        renames Sampler_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_Create_Info) return Sampler
        renames Sampler_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Sampler: in out Vulkan.Sampler;
                      Allocator: aliased in Allocation_Callbacks)
        renames Sampler_Common.Destroy;


    procedure Destroy(Device: in Vulkan.Device;
                      Sampler: in out Vulkan.Sampler)
        renames Sampler_Common.Destroy;
end Vulkan.Samplers;


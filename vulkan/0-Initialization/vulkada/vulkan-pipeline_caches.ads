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

-- Pipeline cache related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Pipeline_Caches is
    -- vkCreatePipelineCache
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Cache_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Pipeline_Cache: out Vulkan.Pipeline_Cache) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Next = null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Pipeline_Cache /= No_Pipeline_Cache);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Cache_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Pipeline_Cache
        with Pre => Device /= No_Device and
                    Create_Info.Next = null,
             Post => Create'Result /= No_Pipeline_Cache;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Cache_Create_Info;
                    Pipeline_Cache: out Vulkan.Pipeline_Cache) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Next = null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Pipeline_Cache /= No_Pipeline_Cache);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Cache_Create_Info)
        return Pipeline_Cache
        with Pre => Device /= No_Device and
                    Create_Info.Next = null,
             Post => Create'Result /= No_Pipeline_Cache;

    -- vkDestroyPipelineCache
    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline_Cache: in out Vulkan.Pipeline_Cache;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Pipeline_Cache = No_Pipeline_Cache;

    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline_Cache: in out Vulkan.Pipeline_Cache)
        with Inline,
             Pre => Device /= No_Device,
             Post => Pipeline_Cache = No_Pipeline_Cache;

    -- vkGetPipelineCacheData
    function Get_Available_Data_Size(Device: in Vulkan.Device;
                                     Pipeline_Cache: in Vulkan.Pipeline_Cache;
                                     Data_Size: in out Interfaces.C.size_t) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Pipeline_Cache /= No_Pipeline_Cache,
             Post => Get_Available_Data_Size'Result in Success |
                                                       Incomplete |
                                                       Out_Of_Host_Memory |
                                                       Out_Of_Device_Memory;

    generic
        type Element is limited private;
        type Index is (<>);
        type Element_Array is array (Index range <>) of aliased Element;
    function Get_Data(Device: in Vulkan.Device;
                      Pipeline_Cache: in Vulkan.Pipeline_Cache;
                      Data_Size: in out Interfaces.C.size_t;
                      Data: aliased out Element_Array) return Result
        with Pre => Device /= No_Device and
                    Pipeline_Cache /= No_Pipeline_Cache,
             Post => Get_Data'Result in Success |
                                        Incomplete |
                                        Out_Of_Host_Memory |
                                        Out_Of_Device_Memory;

    -- vkMergePipelineCaches
    function Merge(Device: in Vulkan.Device;
                   Destination_Cache: in Pipeline_Cache;
                   Source_Caches: in Pipeline_Cache_Vectors.Vector) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Destination_Cache /= No_Pipeline_Cache and
                    not Source_Caches.Is_Empty and
                    (for all Cache of Source_Caches => Cache /= Destination_Cache),
             Post => Merge'Result in Success |
                                     Out_Of_Host_Memory |
                                     Out_Of_Device_Memory;

    procedure Merge(Device: in Vulkan.Device;
                    Destination_Cache: in Pipeline_Cache;
                    Source_Caches: in Pipeline_Cache_Vectors.Vector)
        with Inline,
             Pre => Device /= No_Device and
                    Destination_Cache /= No_Pipeline_Cache and
                    not Source_Caches.Is_Empty and
                    (for all Cache of Source_Caches => Cache /= Destination_Cache);

    function Merge(Device: in Vulkan.Device;
                   Destination_Cache: in Pipeline_Cache;
                   Source_Cache: in Pipeline_Cache) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Destination_Cache /= No_Pipeline_Cache and
                    Source_Cache /= Destination_Cache,
             Post => Merge'Result in Success |
                                     Out_Of_Host_Memory |
                                     Out_Of_Device_Memory;

    procedure Merge(Device: in Vulkan.Device;
                    Destination_Cache: in Pipeline_Cache;
                    Source_Cache: in Pipeline_Cache)
        with Inline,
             Pre => Device /= No_Device and
                    Destination_Cache /= No_Pipeline_Cache and
                    Source_Cache /= Destination_Cache;

private
    package Pipeline_Caches_Common is
        new Objects_Common(Pipeline_Cache_Create_Info,
                           C.Pipeline_Cache_Create_Info_C,
                           Pipeline_Cache,
                           No_Pipeline_Cache,
                           C.To_C,
                           C.Free,
                           C.vkCreatePipelineCache,
                           C.vkDestroyPipelineCache);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Cache_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Pipeline_Cache: out Vulkan.Pipeline_Cache) return Result
        renames Pipeline_Caches_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Cache_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Pipeline_Cache
        renames Pipeline_Caches_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Cache_Create_Info;
                    Pipeline_Cache: out Vulkan.Pipeline_Cache) return Result
        renames Pipeline_Caches_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Pipeline_Cache_Create_Info)
                        return Pipeline_Cache
        renames Pipeline_Caches_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline_Cache: in out Vulkan.Pipeline_Cache;
                      Allocator: aliased in Allocation_Callbacks)
        renames Pipeline_Caches_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline_Cache: in out Vulkan.Pipeline_Cache)
        renames Pipeline_Caches_Common.Destroy;
end Vulkan.Pipeline_Caches;


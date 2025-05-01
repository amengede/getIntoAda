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

-- Descriptor pool related subprograms

with Interfaces.C.Pointers;

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Descriptor_Pools is
    use type Ada.Containers.Count_Type;
    use type Interfaces.Unsigned_32;

    -- vkCreateDescriptorPool
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Descriptor_Pool: out Vulkan.Descriptor_Pool) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Max_Sets > 0 and
                    not Create_Info.Pool_Sizes.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Fragmentation and
                     (if Create'Result = Success then
                        Descriptor_Pool /= No_Descriptor_Pool);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Descriptor_Pool
        with Pre => Device /= No_Device and
                    Create_Info.Max_Sets > 0 and
                    not Create_Info.Pool_Sizes.Is_Empty,
             Post => Create'Result /= No_Descriptor_Pool;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Pool_Create_Info;
                    Descriptor_Pool: out Vulkan.Descriptor_Pool) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Max_Sets > 0 and
                    not Create_Info.Pool_Sizes.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Fragmentation and
                     (if Create'Result = Success then
                        Descriptor_Pool /= No_Descriptor_Pool);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Pool_Create_Info)
        return Descriptor_Pool
        with Pre => Device /= No_Device and
                    Create_Info.Max_Sets > 0 and
                    not Create_Info.Pool_Sizes.Is_Empty,
             Post => Create'Result /= No_Descriptor_Pool;

    -- vkDestroyDescriptorPool
    procedure Destroy(Device: in Vulkan.Device;
                      Descriptor_Pool: in out Vulkan.Descriptor_Pool;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Descriptor_Pool = No_Descriptor_Pool;

    procedure Destroy(Device: in Vulkan.Device;
                      Descriptor_Pool: in out Vulkan.Descriptor_Pool)
        with Inline,
             Pre => Device /= No_Device,
             Post => Descriptor_Pool = No_Descriptor_Pool;

    -- vkResetDescriptorPool
    procedure Reset(Device: in Vulkan.Device;
                    Descriptor_Pool: in Vulkan.Descriptor_Pool)
        with Inline,
             Pre => Device /= No_Device and
                    Descriptor_Pool /= No_Descriptor_Pool;

    -- vkAllocateDescriptorSets
    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Descriptor_Set_Allocate_Info;
                      Descriptor_Sets: out Descriptor_Set_Vectors.Vector) return Result
        with Pre => Device /= No_Device and
                    Allocate_Info.Descriptor_Pool /= No_Descriptor_Pool and
                    not Allocate_Info.Set_Layouts.Is_Empty,
             Post => Allocate'Result in Success |
                                        Out_Of_Host_Memory |
                                        Out_Of_Device_Memory |
                                        Fragmented_Pool |
                                        Out_Of_Pool_Memory;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Descriptor_Set_Allocate_Info;
                      Descriptor_Set: out Vulkan.Descriptor_Set) return Result
        with Pre => Device /= No_Device and
                    Allocate_Info.Descriptor_Pool /= No_Descriptor_Pool and
                    Allocate_Info.Set_Layouts.Length = 1,
             Post => Allocate'Result in Success |
                                        Out_Of_Host_Memory |
                                        Out_Of_Device_Memory |
                                        Fragmented_Pool |
                                        Out_Of_Pool_Memory;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Descriptor_Set_Allocate_Info)
                        return Descriptor_Set_Vectors.Vector
        with Inline,
             Pre => Device /= No_Device and
                    Allocate_Info.Descriptor_Pool /= No_Descriptor_Pool and
                    not Allocate_Info.Set_Layouts.Is_Empty,
             Post => Allocate'Result.Length = Allocate_Info.Set_Layouts.Length;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Descriptor_Set_Allocate_Info)
                        return Descriptor_Set
        with Inline,
             Pre => Device /= No_Device and
                    Allocate_Info.Descriptor_Pool /= No_Descriptor_Pool and
                    Allocate_Info.Set_Layouts.Length = 1;

    -- vkFreeDescriptorSets
    procedure Free(Device: in Vulkan.Device;
                   Descriptor_Pool: in Vulkan.Descriptor_Pool;
                   Descriptor_Sets: in out Descriptor_Set_Vectors.Vector)
        with Pre => Device /= No_Device and
                    Descriptor_Pool /= No_Descriptor_Pool,
             Post => Descriptor_Sets.Is_Empty;

    procedure Free(Device: in Vulkan.Device;
                   Descriptor_Pool: in Vulkan.Descriptor_Pool;
                   Descriptor_Set: in out Vulkan.Descriptor_Set)
        with Inline,
             Pre => Device /= No_Device and
                    Descriptor_Pool /= No_Descriptor_Pool,
             Post => Descriptor_Set = No_Descriptor_Set;

    -- vkUpdateDescriptorSets
    procedure Update(Device: in Vulkan.Device;
                     Descriptor_Writes: in Write_Descriptor_Set_Vectors.Vector
                        := Write_Descriptor_Set_Vectors.Empty_Vector;
                     Descriptor_Copies: in Copy_Descriptor_Set_Vectors.Vector
                        := Copy_Descriptor_Set_Vectors.Empty_Vector)
        with Pre => Device /= No_Device;

    -- Vulkan 1.1
    -- vkUpdateDescriptorSetWithTemplate
    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    procedure Update_With_Template
        (Device: in Vulkan.Device;
         Descriptor_Set: in Vulkan.Descriptor_Set;
         Descriptor_Update_Template: in Vulkan.Descriptor_Update_Template;
         Data: in Pointers.Pointer)
        with Inline,
             Pre => Device /= No_Device and
                    Descriptor_Set /= No_Descriptor_Set and
                    Descriptor_Update_Template /= No_Descriptor_Update_Template;

private
    package Descriptor_Pools_Common is
        new Objects_Common(Descriptor_Pool_Create_Info,
                           C.Descriptor_Pool_Create_Info_C,
                           Descriptor_Pool,
                           No_Descriptor_Pool,
                           C.To_C,
                           C.Free,
                           C.vkCreateDescriptorPool,
                           C.vkDestroyDescriptorPool);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Descriptor_Pool: out Vulkan.Descriptor_Pool) return Result
        renames Descriptor_Pools_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Descriptor_Pool
        renames Descriptor_Pools_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Pool_Create_Info;
                    Descriptor_Pool: out Vulkan.Descriptor_Pool) return Result
        renames Descriptor_Pools_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Descriptor_Pool_Create_Info)
                        return Descriptor_Pool
        renames Descriptor_Pools_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Descriptor_Pool: in out Vulkan.Descriptor_Pool;
                      Allocator: aliased in Allocation_Callbacks)
        renames Descriptor_Pools_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Descriptor_Pool: in out Vulkan.Descriptor_Pool)
        renames Descriptor_Pools_Common.Destroy;
end Vulkan.Descriptor_Pools;


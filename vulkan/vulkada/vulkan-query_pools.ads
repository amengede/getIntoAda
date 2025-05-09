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

-- Query pool related subprograms

with Interfaces.C.Pointers;

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Query_Pools is
    use type Interfaces.Unsigned_32;

    -- vkCreateQueryPool
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Query_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Query_Pool: out Vulkan.Query_Pool) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Query_Count > 0,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Query_Pool /= No_Query_Pool);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Query_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Query_Pool
        with Pre => Device /= No_Device and
                    Create_Info.Query_Count > 0,
             Post => Create'Result /= No_Query_Pool;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Query_Pool_Create_Info;
                    Query_Pool: out Vulkan.Query_Pool) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Query_Count > 0,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Query_Pool /= No_Query_Pool);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Query_Pool_Create_Info) return Query_Pool
        with Pre => Device /= No_Device and
                    Create_Info.Query_Count > 0,
             Post => Create'Result /= No_Query_Pool;

    -- vkDestroyQueryPool
    procedure Destroy(Device: in Vulkan.Device;
                      Query_Pool: in out Vulkan.Query_Pool;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Query_Pool = No_Query_Pool;

    procedure Destroy(Device: in Vulkan.Device;
                      Query_Pool: in out Vulkan.Query_Pool)
        with Inline,
             Pre => Device /= No_Device,
             Post => Query_Pool = No_Query_Pool;

    -- vkGetQueryPoolResults
    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    function Get_Results(Device: in Vulkan.Device;
                         Query_Pool: in Vulkan.Query_Pool;
                         First_Query, Query_Count: in Interfaces.Unsigned_32;
                         Data: out Pointers.Element_Array;
                         Stride: in Device_Size;
                         Flags: in Query_Result_Flags) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Query_Pool /= No_Query_Pool and
                    (if (Flags and Query_Result_64_Bit) = Query_Result_No_Bit then
                         Stride mod 4 = 0
                     else
                         Stride mod 8 = 0),
             Post => Get_Results'Result in Success |
                                           Not_Ready |
                                           Out_Of_Host_Memory |
                                           Out_Of_Device_Memory |
                                           Device_Lost;

    -- Vulkan 1.2
    -- vkResetQueryPool
    procedure Reset(Device: in Vulkan.Device;
                    Query_Pool: in Vulkan.Query_Pool;
                    First_Query, Query_Count: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Device /= No_Device and Query_Pool /= No_Query_Pool;

private
    package Query_Pools_Common is
        new Objects_Common(Query_Pool_Create_Info,
                           C.Query_Pool_Create_Info_C,
                           Query_Pool,
                           No_Query_Pool,
                           C.To_C,
                           C.Free,
                           C.vkCreateQueryPool,
                           C.vkDestroyQueryPool);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Query_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Query_Pool: out Vulkan.Query_Pool) return Result
        renames Query_Pools_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Query_Pool_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Query_Pool
        renames Query_Pools_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Query_Pool_Create_Info;
                    Query_Pool: out Vulkan.Query_Pool) return Result
        renames Query_Pools_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Query_Pool_Create_Info) return Query_Pool
        renames Query_Pools_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Query_Pool: in out Vulkan.Query_Pool;
                      Allocator: aliased in Allocation_Callbacks)
        renames Query_Pools_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Query_Pool: in out Vulkan.Query_Pool)
        renames Query_Pools_Common.Destroy;
end Vulkan.Query_Pools;


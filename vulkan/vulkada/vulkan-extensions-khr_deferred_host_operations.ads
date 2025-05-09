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

-- Operations for the deferred host operations extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Deferred_Host_Operations is
    use type KHR.Deferred_Operation;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCreateDeferredOperationKHR
    function Create(Device: in Vulkan.Device;
                    Allocator: aliased in Allocation_Callbacks;
                    Deferred_Operation: out KHR.Deferred_Operation)
        return Result
        with Inline,
             Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory and
                     (if Create'Result = Success then
                        Deferred_Operation /= KHR.No_Deferred_Operation);

    function Create(Device: in Vulkan.Device;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Deferred_Operation
        with Inline,
             Pre => Device /= No_Device,
             Post => Create'Result /= KHR.No_Deferred_Operation;

    function Create(Device: in Vulkan.Device;
                    Deferred_Operation: out KHR.Deferred_Operation)
        return Result
        with Inline,
             Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory and
                     (if Create'Result = Success then
                        Deferred_Operation /= KHR.No_Deferred_Operation);

    function Create(Device: in Vulkan.Device) return KHR.Deferred_Operation
        with Inline,
             Pre => Device /= No_Device,
             Post => Create'Result /= KHR.No_Deferred_Operation;

    -- vkDestroyDeferredOperationKHR
    procedure Destroy(Device: in Vulkan.Device;
                      Operation: in out KHR.Deferred_Operation;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Operation = KHR.No_Deferred_Operation;

    procedure Destroy(Device: in Vulkan.Device;
                      Operation: in out KHR.Deferred_Operation)
        with Inline,
             Pre => Device /= No_Device,
             Post => Operation = KHR.No_Deferred_Operation;

    -- vkGetDeferredOperationMaxConcurrencyKHR
    function Get_Max_Concurrency(Device: in Vulkan.Device;
                                 Operation: in KHR.Deferred_Operation)
        return Interfaces.Unsigned_32
        with Inline,
             Pre => Device /= No_Device and
                    Operation /= KHR.No_Deferred_Operation;

    -- vkGetDeferredOperationResultKHR
    function Get_Result(Device: in Vulkan.Device;
                        Operation: in KHR.Deferred_Operation) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Operation /= KHR.No_Deferred_Operation,
             Post => Get_Result'Result in Success |
                                          Not_Ready;

    -- vkDeferredOperationJoinKHR
    function Join(Device: in Vulkan.Device;
                  Operation: in KHR.Deferred_Operation) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Operation /= KHR.No_Deferred_Operation,
             Post => Join'Result in Success |
                                    Thread_Done |
                                    Thread_Idle |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory;
end Vulkan.Extensions.KHR_Deferred_Host_Operations;


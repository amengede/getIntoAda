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

-- Operations for the deferred host operations extension

with Vulkan.Core;
with Vulkan.Exceptions;

package body Vulkan.Extensions.KHR_Deferred_Host_Operations is
    -- Loaded extension functions.
    type vkCreateDeferredOperationKHR_Access is
        access function(Device: in Vulkan.Device;
                        Allocator: access constant Allocation_Callbacks;
                        Deferred_Operation: out KHR.Deferred_Operation)
        return Result
        with Convention => C;

    vkCreateDeferredOperationKHR: vkCreateDeferredOperationKHR_Access;

    type vkDestroyDeferredOperationKHR_Access is
        access procedure(Device: in Vulkan.Device;
                         Operation: in KHR.Deferred_Operation;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroyDeferredOperationKHR: vkDestroyDeferredOperationKHR_Access;

    type vkGetDeferredOperationMaxConcurrencyKHR_Access is
        access function(Device: in Vulkan.Device;
                        Operation: in KHR.Deferred_Operation)
        return Interfaces.Unsigned_32
        with Convention => C;

    vkGetDeferredOperationMaxConcurrencyKHR:
        vkGetDeferredOperationMaxConcurrencyKHR_Access;

    type vkGetDeferredOperationResultKHR_Access is
        access function(Device: in Vulkan.Device;
                        Operation: in KHR.Deferred_Operation) return Result
        with Convention => C;

    vkGetDeferredOperationResultKHR: vkGetDeferredOperationResultKHR_Access;

    type vkDeferredOperationJoinKHR_Access is
        access function(Device: in Vulkan.Device;
                        Operation: in KHR.Deferred_Operation) return Result
        with Convention => C;

    vkDeferredOperationJoinKHR: vkDeferredOperationJoinKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCreateDeferredOperationKHR_Access);
        procedure Load is new Load_Pointer
            (vkDestroyDeferredOperationKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetDeferredOperationMaxConcurrencyKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetDeferredOperationResultKHR_Access);
        procedure Load is new Load_Pointer(vkDeferredOperationJoinKHR_Access);
    begin
        Load(vkCreateDeferredOperationKHR, "vkCreateDeferredOperationKHR");
        Load(vkDestroyDeferredOperationKHR, "vkDestroyDeferredOperationKHR");
        Load(vkGetDeferredOperationMaxConcurrencyKHR,
             "vkGetDeferredOperationMaxConcurrencyKHR");
        Load(vkGetDeferredOperationResultKHR,
             "vkGetDeferredOperationResultKHR");
        Load(vkDeferredOperationJoinKHR, "vkDeferredOperationJoinKHR");
    end Load_Extension;

    function Create(Device: in Vulkan.Device;
                    Allocator: aliased in Allocation_Callbacks;
                    Deferred_Operation: out KHR.Deferred_Operation)
        return Result is
    begin
        return vkCreateDeferredOperationKHR(Device,
                                            Allocator'Access,
                                            Deferred_Operation);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Deferred_Operation is
        Operation: KHR.Deferred_Operation;
    begin
        Exceptions.Check(Create(Device, Allocator, Operation));

        return Operation;
    end Create;

    function Create(Device: in Vulkan.Device;
                    Deferred_Operation: out KHR.Deferred_Operation)
        return Result is
    begin
        return vkCreateDeferredOperationKHR(Device, null, Deferred_Operation);
    end Create;

    function Create(Device: in Vulkan.Device) return KHR.Deferred_Operation is
        Operation: KHR.Deferred_Operation;
    begin
        Exceptions.Check(Create(Device, Operation));

        return Operation;
    end Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Operation: in out KHR.Deferred_Operation;
                      Allocator: aliased in Allocation_Callbacks) is
    begin
        vkDestroyDeferredOperationKHR(Device, Operation, Allocator'Access);
        Operation := KHR.No_Deferred_Operation;
    end Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Operation: in out KHR.Deferred_Operation) is
    begin
        vkDestroyDeferredOperationKHR(Device, Operation, null);
        Operation := KHR.No_Deferred_Operation;
    end Destroy;

    function Get_Max_Concurrency(Device: in Vulkan.Device;
                                 Operation: in KHR.Deferred_Operation)
        return Interfaces.Unsigned_32 is
    begin
        return vkGetDeferredOperationMaxConcurrencyKHR(Device, Operation);
    end Get_Max_Concurrency;

    function Get_Result(Device: in Vulkan.Device;
                        Operation: in KHR.Deferred_Operation) return Result is
    begin
        return vkGetDeferredOperationResultKHR(Device, Operation);
    end Get_Result;

    function Join(Device: in Vulkan.Device;
                  Operation: in KHR.Deferred_Operation) return Result is
    begin
        return vkDeferredOperationJoinKHR(Device, Operation);
    end Join;
end Vulkan.Extensions.KHR_Deferred_Host_Operations;


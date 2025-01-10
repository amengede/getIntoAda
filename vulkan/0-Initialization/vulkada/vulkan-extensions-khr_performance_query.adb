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

-- Operations for the performance query extension

with Vulkan.C_KHR;
with Vulkan.Core;
with Vulkan.Extension_Records;
with Vulkan.Exceptions;

package body Vulkan.Extensions.KHR_Performance_Query is
    -- Loaded extension functions.
    type vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR_Access
        is access function
            (Physical_Device: in Vulkan.Physical_Device;
             Queue_Family_Index: in Vulkan.Queue_Family_Index;
             Counter_Count: in out Interfaces.Unsigned_32;
             Counters: access C_KHR.Performance_Counter_C;
             Counter_Description:
                access C_KHR.Performance_Counter_Description_C)
        return Result
        with Convention => C;

    vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR:
        vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR_Access;

    type vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR_Access is
        access procedure
            (Physical_Device: in Vulkan.Physical_Device;
             Peformance_Query_Create_Info:
                in C_KHR.Query_Pool_Performance_Create_Info_C;
             Num_Passes: out Interfaces.Unsigned_32)
        with Convention => C;

    vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR:
        vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR_Access;

    type vkAcquireProfilingLockKHR_Access is
        access function(Device: in Vulkan.Device;
                        Info: in C_KHR.Acquire_Profiling_Lock_Info_C)
        return Result
        with Convention => C;

    vkAcquireProfilingLockKHR: vkAcquireProfilingLockKHR_Access;

    type vkReleaseProfilingLockKHR_Access is
        access procedure(Device: in Vulkan.Device)
        with Convention => C;

    vkReleaseProfilingLockKHR: vkReleaseProfilingLockKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer
       (vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR_Access);
        procedure Load is new Load_Pointer(vkAcquireProfilingLockKHR_Access);
        procedure Load is new Load_Pointer(vkReleaseProfilingLockKHR_Access);
    begin
        Load(vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR,
             "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR");
        Load(vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR,
             "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR");
        Load(vkAcquireProfilingLockKHR, "vkAcquireProfilingLockKHR");
        Load(vkReleaseProfilingLockKHR, "vkReleaseProfilingLockKHR");
    end Load_Extension;

    function Performance_Query_Counter_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check
            (vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
                (Physical_Device, Queue_Family_Index, Count, null, null));

        return Count;
    end Performance_Query_Counter_Count;

    function Get_Performance_Query_Counters
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Counters: in out KHR.Performance_Counter_Vectors.Vector;
         Counter_Descriptions:
            in out KHR.Performance_Counter_Description_Vectors.Vector)
         return Result is
        Counters_C: array (1 .. Positive(Counters.Length)) of aliased
            C_KHR.Performance_Counter_C;
        Counter_Descriptions_C: array (Counters_C'Range) of aliased
            C_KHR.Performance_Counter_Description_C;
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Counters.Length);
        Result: Vulkan.Result;
    begin
        for X in Counters_C'Range loop
            Counters_C(X).Next := Extension_Records.To_C(Counters(X).Next);
            Counter_Descriptions_C(X).Next :=
                Extension_Records.To_C(Counter_Descriptions(X).Next);
        end loop;

        Result :=
            vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
                (Physical_Device,
                 Queue_Family_Index,
                 Count,
                 Counters_C(1)'Access,
                 Counter_Descriptions_C(1)'Access);

        for X in Counters_C'Range loop
            C_KHR.To_Ada(Counters(X), Counters_C(X));
            C_KHR.To_Ada(Counter_Descriptions(X),
                                         Counter_Descriptions_C(X));
            Extension_Records.Free(Counters_C(X).Next);
            Extension_Records.Free(Counter_Descriptions_C(X).Next);
        end loop;

        return Result;
    end Get_Performance_Query_Counters;

    procedure Get_Performance_Query_Counters
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Counters: in out KHR.Performance_Counter_Vectors.Vector;
         Counter_Descriptions:
            in out KHR.Performance_Counter_Description_Vectors.Vector) is
    begin
        Exceptions.Check(Get_Performance_Query_Counters(Physical_Device,
                                                        Queue_Family_Index,
                                                        Counters,
                                                        Counter_Descriptions));
    end Get_Performance_Query_Counters;
    
    function Get_Performance_Query_Passes
        (Physical_Device: in Vulkan.Physical_Device;
         Performance_Query_Create_Info:
            in KHR.Query_Pool_Performance_Create_Info)
        return Interfaces.Unsigned_32 is
        Performance_Query_Create_Info_C:
            C_KHR.Query_Pool_Performance_Create_Info_C :=
                C_KHR.To_C(Performance_Query_Create_Info);
        Count: Interfaces.Unsigned_32;
    begin
        vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
            (Physical_Device,
             Performance_Query_Create_Info_C,
             Count);
        C_KHR.Free(Performance_Query_Create_Info_C);

        return Count;
    end Get_Performance_Query_Passes;

    function Acquire_Profiling_Lock(Device: in Vulkan.Device;
                                    Info: in KHR.Acquire_Profiling_Lock_Info)
        return Result is
        Info_C: C_KHR.Acquire_Profiling_Lock_Info_C := C_KHR.To_C(Info);
        Result: Vulkan.Result;
    begin
        Result := vkAcquireProfilingLockKHR(Device, Info_C);
        C_KHR.Free(Info_C);

        return Result;
    end Acquire_Profiling_Lock;

    procedure Release_Profiling_Lock(Device: in Vulkan.Device) is
    begin
        vkReleaseProfilingLockKHR(Device);
    end Release_Profiling_Lock;
end Vulkan.Extensions.KHR_Performance_Query;


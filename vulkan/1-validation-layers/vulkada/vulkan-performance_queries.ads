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

package Vulkan.Performance_Queries is
    use type Ada.Containers.Count_Type;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
    function Performance_Query_Counter_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index)
        return Interfaces.Unsigned_32
        with Inline,
             Pre => Physical_Device /= No_Physical_Device;

    function Get_Performance_Query_Counters
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Counters: in out Performance_Counter_Vectors.Vector;
         Counter_Descriptions:
            in out Performance_Counter_Description_Vectors.Vector)
         return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    not Counters.Is_Empty and
                    Counters.Length = Counter_Descriptions.Length,
             Post => Get_Performance_Query_Counters'Result in
                Success |
                Incomplete |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory |
                Initialization_Failed;

    procedure Get_Performance_Query_Counters
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Counters: in out Performance_Counter_Vectors.Vector;
         Counter_Descriptions:
            in out Performance_Counter_Description_Vectors.Vector)
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    not Counters.Is_Empty and
                    Counters.Length = Counter_Descriptions.Length;

    -- vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
    function Get_Performance_Query_Passes
        (Physical_Device: in Vulkan.Physical_Device;
         Performance_Query_Create_Info: in Query_Pool_Performance_Create_Info)
        return Interfaces.Unsigned_32
        with Pre => Physical_Device /= No_Physical_Device and
                    not Performance_Query_Create_Info.Counter_Indices.Is_Empty;

    -- vkAcquireProfilingLockKHR
    function Acquire_Profiling_Lock(Device: in Vulkan.Device;
                                    Info: in Acquire_Profiling_Lock_Info)
        return Result
        with Pre => Device /= No_Device,
             Post => Acquire_Profiling_Lock'Result in Success |
                                                      Out_Of_Host_Memory |
                                                      Timeout;

    -- vkReleaseProfilingLockKHR
    procedure Release_Profiling_Lock(Device: in Vulkan.Device)
        with Inline,
             Pre => Device /= No_Device;
end Vulkan.Performance_Queries;


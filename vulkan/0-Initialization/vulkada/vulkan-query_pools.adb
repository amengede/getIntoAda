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

-- Query pool related subprograms

with Vulkan.C_V1_2;

package body Vulkan.Query_Pools is
    function Get_Results(Device: in Vulkan.Device;
                         Query_Pool: in Vulkan.Query_Pool;
                         First_Query, Query_Count: in Interfaces.Unsigned_32;
                         Data: out Pointers.Element_Array;
                         Stride: in Device_Size;
                         Flags: in Query_Result_Flags) return Result is
        use type Interfaces.C.size_t;
    begin
        return C.vkGetQueryPoolResults(Device,
                                       Query_Pool,
                                       First_Query,
                                       Query_Count,
                                       Data'Length * Pointers.Element'Size / 8,
                                       Data'Address,
                                       Stride,
                                       Flags);
    end Get_Results;

    procedure Reset(Device: in Vulkan.Device;
                    Query_Pool: in Vulkan.Query_Pool;
                    First_Query, Query_Count: in Interfaces.Unsigned_32) is
    begin
        C_V1_2.vkResetQueryPool(Device, Query_Pool, First_Query, Query_Count);
    end Reset;
end Vulkan.Query_Pools;


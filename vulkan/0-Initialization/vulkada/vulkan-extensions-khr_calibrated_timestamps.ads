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

-- Operations for the calibrated timestamps extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Calibrated_Timestamps is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceCalibrateableTimeDomainsKHR
    function Physical_Device_Calibrateable_Time_Domains_Count
        (Physical_Device: in Vulkan.Physical_Device)
        return Interfaces.Unsigned_32
        with Inline,
             Pre => Physical_Device /= No_Physical_Device;

    function Get_Physical_Device_Calibrateable_Time_Domains
        (Physical_Device: in Vulkan.Physical_Device;
         Time_Domains: out KHR.Time_Domain_Vectors.Vector) return Result
        with Pre => Physical_Device /= No_Physical_Device,
             Post => Get_Physical_Device_Calibrateable_Time_Domains'Result in
                Success |
                Incomplete |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Physical_Device_Calibrateable_Time_Domains
        (Physical_Device: in Vulkan.Physical_Device)
        return KHR.Time_Domain_Vectors.Vector
        with Inline,
             Pre => Physical_Device /= No_Physical_Device;

    -- vkGetCalibratedTimestampsKHR
    function Get_Calibrated_Timestamps
        (Device: in Vulkan.Device;
         Timestamp_Infos: in KHR.Calibrated_Timestamp_Info_Vectors.Vector;
         Timestamps: out KHR.Unsigned_64_Vectors.Vector;
         Max_Deviation: out Interfaces.Unsigned_64) return Result
        with Pre => Device /= No_Device and not Timestamp_Infos.Is_Empty,
             Post => Get_Calibrated_Timestamps'Result in Success |
                                                         Out_Of_Host_Memory |
                                                         Out_Of_Device_Memory;
end Vulkan.Extensions.KHR_Calibrated_Timestamps;


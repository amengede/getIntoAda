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

-- Operations for the fragment shading rate extension

package Vulkan.Fragment_Shading_Rates is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceFragmentShadingRateKHR
    function Fragment_Shading_Rates_Count
        (Physical_Device: in Vulkan.Physical_Device)
        return Interfaces.Unsigned_32
        with Inline,
             Pre => Physical_Device /= No_Physical_Device;

    function Get_Fragment_Shading_Rates
        (Physical_Device: in Vulkan.Physical_Device;
         Fragment_Shading_Rates:
            in out Physical_Device_Fragment_Shading_Rate_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    not Fragment_Shading_Rates.Is_Empty,
             Post => Get_Fragment_Shading_Rates'Result in Success |
                                                          Incomplete |
                                                          Out_Of_Host_Memory;

    function Get_Fragment_Shading_Rates
        (Physical_Device: in Vulkan.Physical_Device)
        return Physical_Device_Fragment_Shading_Rate_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkCmdSetFragmentShadingRateKHR
    procedure Set_Fragment_Shading_Rate
        (Command_Buffer: in Vulkan.Command_Buffer;
         Fragment_Size: in Extent_2D;
         Combiner_Ops: in Fragment_Shading_Rate_Combiner_Op_Array)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
             Fragment_Size.Width in 1 | 2 | 4 and
             Fragment_Size.Height in 1 | 2 | 4;
end Vulkan.Fragment_Shading_Rates;


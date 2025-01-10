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

-- Operations for the pipeline executable properties extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Pipeline_Executable_Properties is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPipelineExecutablePropertiesKHR
    function Properties_Count(Device: in Vulkan.Device;
                              Pipeline_Info: in KHR.Pipeline_Info)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Device and
                    Pipeline_Info.Pipeline /= No_Pipeline;

    function Get_Properties
        (Device: in Vulkan.Device;
         Pipeline_Info: in KHR.Pipeline_Info;
         Properties: in out KHR.Pipeline_Executable_Properties_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    Pipeline_Info.Pipeline /= No_Pipeline,
             Post => Get_Properties'Result in Success |
                                              Incomplete |
                                              Out_Of_Host_Memory |
                                              Out_Of_Device_Memory;

    function Get_Properties(Device: in Vulkan.Device;
                            Pipeline_Info: in KHR.Pipeline_Info)
        return KHR.Pipeline_Executable_Properties_Vectors.Vector
        with Pre => Device /= No_Device and
                    Pipeline_Info.Pipeline /= No_Pipeline;

    -- vkGetPipelineExecutableStatisticsKHR
    function Statistics_Count(Device: in Vulkan.Device;
                              Executable_Info: in KHR.Pipeline_Executable_Info)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Device and
                    Executable_Info.Pipeline /= No_Pipeline;

    function Get_Statistics
        (Device: in Vulkan.Device;
         Executable_Info: in KHR.Pipeline_Executable_Info;
         Statistics: in out KHR.Pipeline_Executable_Statistic_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    Executable_Info.Pipeline /= No_Pipeline,
             Post => Get_Statistics'Result in Success |
                                              Incomplete |
                                              Out_Of_Host_Memory |
                                              Out_Of_Device_Memory;

    function Get_Statistics(Device: in Vulkan.Device;
                            Executable_Info: in KHR.Pipeline_Executable_Info)
        return KHR.Pipeline_Executable_Statistic_Vectors.Vector
        with Pre => Device /= No_Device and
                    Executable_Info.Pipeline /= No_Pipeline;

    -- vkGetPipelineExecutableInternalRepresentationsKHR
    function Internal_Representations_Count
        (Device: in Vulkan.Device;
         Executable_Info: in KHR.Pipeline_Executable_Info)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Device and
                    Executable_Info.Pipeline /= No_Pipeline;

    function Get_Internal_Representations
        (Device: in Vulkan.Device;
         Executable_Info: in KHR.Pipeline_Executable_Info;
         Internal_Representations: in out
            KHR.Pipeline_Executable_Internal_Representation_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    Executable_Info.Pipeline /= No_Pipeline,
             Post => Get_Internal_Representations'Result in
                Success |
                Incomplete |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Internal_Representations
        (Device: in Vulkan.Device;
         Executable_Info: in KHR.Pipeline_Executable_Info)
        return KHR.Pipeline_Executable_Internal_Representation_Vectors.Vector
        with Pre => Device /= No_Device and
                    Executable_Info.Pipeline /= No_Pipeline;
end Vulkan.Extensions.KHR_Pipeline_Executable_Properties;


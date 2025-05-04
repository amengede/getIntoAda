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

-- Operations for the shader info extension

with Interfaces.C.Pointers;

package Vulkan.Shader_Infos is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetShaderInfoAMD
    function Get_Statistics(Device: in Vulkan.Device;
                            Pipeline: in Vulkan.Pipeline;
                            Shader_Stage: in Shader_Stage_Flags;
                            Statistics: out Shader_Statistics_Info)
        return Result
        with Inline,
             Pre => Device /= No_Device and Pipeline /= No_Pipeline,
             Post => Get_Statistics'Result in Success |
                                              Incomplete |
                                              Feature_Not_Present |
                                              Out_Of_Host_Memory;

    function Get_Statistics(Device: in Vulkan.Device;
                            Pipeline: in Vulkan.Pipeline;
                            Shader_Stage: in Shader_Stage_Flags)
        return Shader_Statistics_Info
        with Inline,
             Pre => Device /= No_Device and Pipeline /= No_Pipeline;

    function Get_Binary_Size(Device: in Vulkan.Device;
                             Pipeline: in Vulkan.Pipeline;
                             Shader_Stage: in Shader_Stage_Flags)
        return Interfaces.C.size_t
        with Inline,
             Pre => Device /= No_Device and Pipeline /= No_Pipeline;

    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    function Get_Binary(Device: in Vulkan.Device;
                        Pipeline: in Vulkan.Pipeline;
                        Shader_Stage: in Shader_Stage_Flags;
                        Info: out Pointers.Element_Array) return Result
        with Inline,
             Pre => Device /= No_Device and Pipeline /= No_Pipeline,
             Post => Get_Binary'Result in Success |
                                          Incomplete |
                                          Feature_Not_Present |
                                          Out_Of_Host_Memory;

    function Get_Disassembly
        (Device: in Vulkan.Device;
         Pipeline: in Vulkan.Pipeline;
         Shader_Stage: in Shader_Stage_Flags;
         Disassembly: out Ada.Strings.Unbounded.Unbounded_String) return Result
        with Pre => Device /= No_Device and Pipeline /= No_Pipeline,
             Post => Get_Disassembly'Result in Success |
                                               Incomplete |
                                               Feature_Not_Present |
                                               Out_Of_Host_Memory;

    function Get_Disassembly(Device: in Vulkan.Device;
                             Pipeline: in Vulkan.Pipeline;
                             Shader_Stage: in Shader_Stage_Flags)
        return Ada.Strings.Unbounded.Unbounded_String
        with Inline,
             Pre => Device /= No_Device and Pipeline /= No_Pipeline;
end Vulkan.Shader_Infos;


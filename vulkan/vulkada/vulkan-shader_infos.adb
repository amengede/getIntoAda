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

with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with Vulkan.Core;
with Vulkan.Exceptions;

package body Vulkan.Shader_Infos is
    -- Loaded extension functions.
    type vkGetShaderInfoAMD_Access is
        access function(Device: in Vulkan.Device;
                        Pipeline: in Vulkan.Pipeline;
                        Shader_Stage: in Shader_Stage_Flags;
                        Info_Type: in Shader_Info_Type;
                        Info_Size: in out Interfaces.C.size_t;
                        Info: Interfaces.C.Extensions.void_ptr) return Result
        with Convention => C;

    vkGetShaderInfoAMD: vkGetShaderInfoAMD_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkGetShaderInfoAMD_Access);
    begin
        Load(vkGetShaderInfoAMD, "vkGetShaderInfoAMD");
    end Load_Extension;
    
    function Get_Statistics(Device: in Vulkan.Device;
                            Pipeline: in Vulkan.Pipeline;
                            Shader_Stage: in Shader_Stage_Flags;
                            Statistics: out Shader_Statistics_Info)
        return Result is
        use type Interfaces.C.size_t;
        Size: Interfaces.C.size_t := Statistics'Size / System.Storage_Unit;
    begin
        return vkGetShaderInfoAMD(Device,
                                  Pipeline,
                                  Shader_Stage,
                                  Vulkan.Statistics,
                                  Size,
                                  Statistics'Address);
    end Get_Statistics;

    function Get_Statistics(Device: in Vulkan.Device;
                            Pipeline: in Vulkan.Pipeline;
                            Shader_Stage: in Shader_Stage_Flags)
        return Shader_Statistics_Info is
        Statistics: Shader_Statistics_Info;
    begin
        Exceptions.Check(Get_Statistics(Device,
                                        Pipeline,
                                        Shader_Stage,
                                        Statistics));

        return Statistics;
    end Get_Statistics;

    function Get_Binary_Size(Device: in Vulkan.Device;
                             Pipeline: in Vulkan.Pipeline;
                             Shader_Stage: in Shader_Stage_Flags)
        return Interfaces.C.size_t is
        Size: Interfaces.C.size_t := 0;
    begin
        Exceptions.Check(vkGetShaderInfoAMD(Device,
                                            Pipeline,
                                            Shader_Stage,
                                            Binary,
                                            Size,
                                            System.Null_Address));

        return Size;
    end Get_Binary_Size;

    function Get_Binary(Device: in Vulkan.Device;
                        Pipeline: in Vulkan.Pipeline;
                        Shader_Stage: in Shader_Stage_Flags;
                        Info: out Pointers.Element_Array) return Result is
        use type Interfaces.C.size_t;

        Size: Interfaces.C.size_t := Info'Size / System.Storage_Unit;
    begin
        return vkGetShaderInfoAMD(Device,
                                  Pipeline,
                                  Shader_Stage,
                                  Binary,
                                  Size,
                                  Info(Info'First)'Address);
    end Get_Binary;
        
    function Get_Disassembly
        (Device: in Vulkan.Device;
         Pipeline: in Vulkan.Pipeline;
         Shader_Stage: in Shader_Stage_Flags;
         Disassembly: out Ada.Strings.Unbounded.Unbounded_String)
        return Result is
        procedure Free is new Ada.Unchecked_Deallocation
            (Interfaces.C.char_array, Interfaces.C.Strings.char_array_access);

        Size: Interfaces.C.size_t := 0;
        Result: Vulkan.Result;
        C_String: Interfaces.C.Strings.char_array_access;
    begin
        Result := vkGetShaderInfoAMD(Device,
                                     Pipeline,
                                     Shader_Stage,
                                     Vulkan.Disassembly,
                                     Size,
                                     System.Null_Address);

        if Result /= Success then
            return Result;
        end if;

        C_String := new Interfaces.C.char_array(1 .. Size);
        Result := vkGetShaderInfoAMD(Device,
                                     Pipeline,
                                     Shader_Stage,
                                     Vulkan.Disassembly,
                                     Size,
                                     C_String(1)'Address);

        if Result = Success then
            Ada.Strings.Unbounded.Set_Unbounded_String
                (Disassembly, Interfaces.C.To_Ada(C_String.all));
        end if;

        Free(C_String);

        return Result;
    end Get_Disassembly;

    function Get_Disassembly(Device: in Vulkan.Device;
                             Pipeline: in Vulkan.Pipeline;
                             Shader_Stage: in Shader_Stage_Flags)
        return Ada.Strings.Unbounded.Unbounded_String is
        Disassembly: Ada.Strings.Unbounded.Unbounded_String;
    begin
        Exceptions.Check(Get_Disassembly(Device,
                                         Pipeline,
                                         Shader_Stage,
                                         Disassembly));

        return Disassembly;
    end Get_Disassembly;
end Vulkan.Shader_Infos;


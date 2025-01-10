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

-- Operations for the external memory FD extension

with Vulkan.C_KHR;
with Vulkan.Core;
with Vulkan.Exceptions;
with Vulkan.Extension_Records;

package body Vulkan.Extensions.KHR_External_Memory_FD is
    -- Loaded extension functions.
    type vkGetMemoryFdKHR_Access is
        access function(Device: in Vulkan.Device;
                        Get_FD_Info: C_KHR.Memory_Get_FD_Info_C;
                        FD: out File_Descriptor) return Result
        with Convention => C;

    vkGetMemoryFdKHR: vkGetMemoryFdKHR_Access;

    type vkGetMemoryFdPropertiesKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Handle_Type: in External_Memory_Handle_Type_Flags;
             FD: in File_Descriptor;
             Memory_FD_Properties: out C_KHR.Memory_FD_Properties_C)
            return Result
        with Convention => C;

    vkGetMemoryFdPropertiesKHR: vkGetMemoryFdPropertiesKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkGetMemoryFdKHR_Access);
        procedure Load is new Load_Pointer(vkGetMemoryFdPropertiesKHR_Access);
    begin
        Load(vkGetMemoryFdKHR, "vkGetMemoryFdKHR");
        Load(vkGetMemoryFdPropertiesKHR, "vkGetMemoryFdPropertiesKHR");
    end Load_Extension;
    
    function Get_Memory_FD(Device: in Vulkan.Device;
                           Get_FD_Info: in KHR.Memory_Get_FD_Info;
                           FD: out File_Descriptor) return Result is
        Get_FD_Info_C: C_KHR.Memory_Get_FD_Info_C := C_KHR.To_C(Get_FD_Info);
        Result: Vulkan.Result;
    begin
        Result := vkGetMemoryFdKHR(Device, Get_FD_Info_C, FD);
        C_KHR.Free(Get_FD_Info_C);

        return Result;
    end Get_Memory_FD;

    function Get_Memory_FD(Device: in Vulkan.Device;
                           Get_FD_Info: in KHR.Memory_Get_FD_Info)
        return File_Descriptor is
        FD: File_Descriptor;
    begin
        Exceptions.Check(Get_Memory_FD(Device, Get_FD_Info, FD));

        return FD;
    end Get_Memory_FD;

    function Get_Memory_FD_Properties
        (Device: in Vulkan.Device;
         Handle_Type: in External_Memory_Handle_Type_Flags;
         FD: in File_Descriptor;
         Memory_FD_Properties: out KHR.Memory_FD_Properties) return Result is
        Memory_FD_Properties_C: C_KHR.Memory_FD_Properties_C;
        Result: Vulkan.Result;
    begin
        Memory_FD_Properties_C.Next :=
            Extension_Records.To_C(Memory_FD_Properties.Next);
        Result := vkGetMemoryFdPropertiesKHR(Device,
                                             Handle_Type,
                                             FD,
                                             Memory_FD_Properties_C);
        C_KHR.To_Ada(Memory_FD_Properties, Memory_FD_Properties_C);
        Extension_Records.Free(Memory_FD_Properties_C.Next);

        return Result;
    end Get_Memory_FD_Properties;
    
    function Get_Memory_FD_Properties
        (Device: in Vulkan.Device;
         Handle_Type: in External_Memory_Handle_Type_Flags;
         FD: in File_Descriptor) return KHR.Memory_FD_Properties is
        Properties: KHR.Memory_FD_Properties;
    begin
        Exceptions.Check(Get_Memory_FD_Properties(Device,
                                                  Handle_Type,
                                                  FD,
                                                  Properties));

        return Properties;
    end Get_Memory_FD_Properties;
end Vulkan.Extensions.KHR_External_Memory_FD;


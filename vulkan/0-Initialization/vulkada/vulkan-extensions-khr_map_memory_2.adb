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

-- Operations for the map memory 2 extension

with Vulkan.Core;
with Vulkan.C_KHR;
with Vulkan.Exceptions;

package body Vulkan.Extensions.KHR_Map_Memory_2 is
    -- Loaded extension functions.
    type vkMapMemory2KHR_Access is
        access function(Device: in Vulkan.Device;
                        Memory_Map_Info: in C_KHR.Memory_Map_Info_C;
                        Data: in Interfaces.C.Extensions.void_ptr)
        return Result
        with Convention => C;

    vkMapMemory2KHR: vkMapMemory2KHR_Access;

    type vkUnmapMemory2KHR_Access is
        access function
            (Device: in Vulkan.Device;
             Memory_Unmap_Info: in C_KHR.Memory_Unmap_Info_C)
        return Result
        with Convention => C;

    vkUnmapMemory2KHR: vkUnmapMemory2KHR_Access;
                        
    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkMapMemory2KHR_Access);
        procedure Load is new Load_Pointer(vkUnmapMemory2KHR_Access);
    begin
        Load(vkMapMemory2KHR, "vkMapMemory2KHR");
        Load(vkUnmapMemory2KHR, "vkUnmapMemory2KHR");
    end Load_Extension;

    function Map_2_With_Result(Device: in Vulkan.Device;
                               Memory_Map_Info: in KHR.Memory_Map_Info;
                               Data: out Pointers.Pointer) return Result is
        C_Info: C_KHR.Memory_Map_Info_C := C_KHR.To_C(Memory_Map_Info);
        Result: Vulkan.Result;
    begin
        Result := vkMapMemory2KHR(Device, C_Info, Data'Address);
        C_KHR.Free(C_Info);

        return Result;
    end Map_2_With_Result;
    
    function Map_2_With_Exception(Device: in Vulkan.Device;
                                  Memory_Map_Info: in KHR.Memory_Map_Info)
        return Pointers.Pointer is
        function Map_2 is new Map_2_With_Result(Pointers);

        Data: Pointers.Pointer;
    begin
        Exceptions.Check(Map_2(Device, Memory_Map_Info, Data));

        return Data;
    end Map_2_With_Exception;

    procedure Unmap(Device: in Vulkan.Device;
                    Memory_Unmap_Info: in KHR.Memory_Unmap_Info) is
        C_Info: C_KHR.Memory_Unmap_Info_C := C_KHR.To_C(Memory_Unmap_Info);
        Result: Vulkan.Result;
    begin
        Result := vkUnmapMemory2KHR(Device, C_Info);
        C_KHR.Free(C_Info);
    end Unmap;
end Vulkan.Extensions.KHR_Map_Memory_2;


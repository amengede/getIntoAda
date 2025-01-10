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

-- Operations for the external fence FD extension

with Vulkan.C_KHR;
with Vulkan.Core;
with Vulkan.Exceptions;

package body Vulkan.Extensions.KHR_External_Fence_FD is
    -- Loaded extension functions.
    type vkImportFenceFdKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Import_Fence_FD_Info: in C_KHR.Import_Fence_FD_Info_C)
        return Result
        with Convention => C;

    vkImportFenceFdKHR: vkImportFenceFdKHR_Access;

    type vkGetFenceFdKHR_Access is
        access function(Device: in Vulkan.Device;
                        Get_Fd_Info: in C_KHR.Fence_Get_FD_Info_C;
                        FD: out File_Descriptor) return Result
        with Convention => C;

    vkGetFenceFdKHR: vkGetFenceFdKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkImportFenceFdKHR_Access);
        procedure Load is new Load_Pointer(vkGetFenceFdKHR_Access);
    begin
        Load(vkImportFenceFdKHR, "vkImportFenceFdKHR");
        Load(vkGetFenceFdKHR, "vkGetFenceFdKHR");
    end Load_Extension;

    function Import_Fence_FD
        (Device: in Vulkan.Device;
         Import_Fence_FD_Info: in KHR.Import_Fence_FD_Info) return Result is
        Import_Fence_FD_Info_C: C_KHR.Import_Fence_FD_Info_C :=
            C_KHR.To_C(Import_Fence_FD_Info);
        Result: Vulkan.Result;
    begin
        Result := vkImportFenceFdKHR(Device, Import_Fence_FD_Info_C);
        C_KHR.Free(Import_Fence_FD_Info_C);

        return Result;
    end Import_Fence_FD;
    
    procedure Import_Fence_FD
        (Device: in Vulkan.Device;
         Import_Fence_FD_Info: in KHR.Import_Fence_FD_Info) is
    begin
        Exceptions.Check(Import_Fence_FD(Device, Import_Fence_FD_Info));
    end Import_Fence_FD;

    function Get_Fence_FD(Device: in Vulkan.Device;
                          Get_FD_Info: in KHR.Fence_Get_FD_Info;
                          FD: out File_Descriptor) return Result is
        Get_FD_Info_C: C_KHR.Fence_Get_FD_Info_C := C_KHR.To_C(Get_FD_Info);
        Result: Vulkan.Result;
    begin
        Result := vkGetFenceFdKHR(Device, Get_FD_Info_C, FD);
        C_KHR.Free(Get_FD_Info_C);

        return Result;
    end Get_Fence_FD;

    function Get_Fence_FD(Device: in Vulkan.Device;
                          Get_FD_Info: in KHR.Fence_Get_FD_Info)
        return File_Descriptor is
        FD: File_Descriptor;
    begin
        Exceptions.Check(Get_Fence_FD(Device, Get_FD_Info, FD));

        return FD;
    end Get_Fence_FD;
end Vulkan.Extensions.KHR_External_Fence_FD;


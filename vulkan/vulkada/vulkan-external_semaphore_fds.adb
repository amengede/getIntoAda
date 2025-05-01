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

-- Operations for the external semaphore FD extension

with Vulkan.Core;
with Vulkan.External_Semaphore_FDs_C;
with Vulkan.Exceptions;

package body Vulkan.External_Semaphore_FDs is
    -- Loaded extension functions.
    type vkImportSemaphoreFdKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Import_Semaphore_FD_Info:
                in External_Semaphore_FDs_C.Import_Semaphore_FD_Info_C)
        return Result
        with Convention => C;

    vkImportSemaphoreFdKHR: vkImportSemaphoreFdKHR_Access;

    type vkGetSemaphoreFdKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Get_FD_Info: External_Semaphore_FDs_C.Semaphore_Get_FD_Info_C;
             FD: out File_Descriptor) return Result
        with Convention => C;

    vkGetSemaphoreFdKHR: vkGetSemaphoreFdKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkImportSemaphoreFdKHR_Access);
        procedure Load is new Load_Pointer(vkGetSemaphoreFdKHR_Access);
    begin
        Load(vkImportSemaphoreFdKHR, "vkImportSemaphoreFdKHR");
        Load(vkGetSemaphoreFdKHR, "vkGetSemaphoreFdKHR");
    end Load_Extension;

    function Import_Semaphore_FD
        (Device: in Vulkan.Device;
         Import_Semaphore_FD_Info: in Vulkan.Import_Semaphore_FD_Info)
        return Result is
        Import_Semaphore_FD_Info_C:
            External_Semaphore_FDs_C.Import_Semaphore_FD_Info_C :=
                External_Semaphore_FDs_C.To_C(Import_Semaphore_FD_Info);
        Result: Vulkan.Result;
    begin
        Result := vkImportSemaphoreFdKHR(Device, Import_Semaphore_FD_Info_C);
        External_Semaphore_FDs_C.Free(Import_Semaphore_FD_Info_C);

        return Result;
    end Import_Semaphore_FD;
    
    procedure Import_Semaphore_FD
        (Device: in Vulkan.Device;
         Import_Semaphore_FD_Info: in Vulkan.Import_Semaphore_FD_Info) is
    begin
        Exceptions.Check(Import_Semaphore_FD(Device, Import_Semaphore_FD_Info));
    end Import_Semaphore_FD;

    function Get_Semaphore_FD(Device: in Vulkan.Device;
                              Get_FD_Info: in Semaphore_Get_FD_Info;
                              FD: out File_Descriptor) return Result is
        Get_FD_Info_C: External_Semaphore_FDs_C.Semaphore_Get_FD_Info_C :=
            External_Semaphore_FDs_C.To_C(Get_FD_Info);
        Result: Vulkan.Result;
    begin
        Result := vkGetSemaphoreFdKHR(Device, Get_FD_Info_C, FD);
        External_Semaphore_FDs_C.Free(Get_FD_Info_C);

        return Result;
    end Get_Semaphore_FD;

    function Get_Semaphore_FD(Device: in Vulkan.Device;
                              Get_FD_Info: in Semaphore_Get_FD_Info)
        return File_Descriptor is
        FD: File_Descriptor;
    begin
        Exceptions.Check(Get_Semaphore_FD(Device, Get_FD_Info, FD));

        return FD;
    end Get_Semaphore_FD;
end Vulkan.External_Semaphore_FDs;


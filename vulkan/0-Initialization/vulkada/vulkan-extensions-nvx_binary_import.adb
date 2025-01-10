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

-- Operations for the binary import extension

with Vulkan.Core;
with Vulkan.C_NVX;
with Vulkan.Objects_Common_Access;

package body Vulkan.Extensions.NVX_Binary_Import is
    -- Loaded extension functions.
    type vkCreateCuModuleNVX_Access is
        access function(Device: in Vulkan.Device;
                        Create_Info: in C_NVX.Cu_Module_Create_Info_C;
                        Allocator: access constant Allocation_Callbacks;
                        Module: out NVX.Cu_Module) return Result
        with Convention => C;

    vkCreateCuModuleNVX: vkCreateCuModuleNVX_Access;

    type vkCreateCuFunctionNVX_Access is
        access function(Device: in Vulkan.Device;
                        Create_Info: in C_NVX.Cu_Function_Create_Info_C;
                        Allocator: access constant Allocation_Callbacks;
                        Cu_Function: out NVX.Cu_Function) return Result
        with Convention => C;

    vkCreateCuFunctionNVX: vkCreateCuFunctionNVX_Access;

    type vkDestroyCuModuleNVX_Access is
        access procedure(Device: in Vulkan.Device;
                         Module: in NVX.Cu_Module;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroyCuModuleNVX: vkDestroyCuModuleNVX_Access;

    type vkDestroyCuFunctionNVX_Access is
        access procedure(Device: in Vulkan.Device;
                         Cu_Function: in NVX.Cu_Function;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroyCuFunctionNVX: vkDestroyCuFunctionNVX_Access;

    type vkCmdCuLaunchKernelNVX_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Launch_Info: in C_NVX.Cu_Launch_Info_C)
        with Convention => C;

    vkCmdCuLaunchKernelNVX: vkCmdCuLaunchKernelNVX_Access;

    package Cu_Modules_Common is new Objects_Common_Access
        (NVX.Cu_Module_Create_Info,
         C_NVX.Cu_Module_Create_Info_C,
         NVX.Cu_Module,
         NVX.No_Cu_Module,
         C_NVX.To_C,
         C_NVX.Free,
         vkCreateCuModuleNVX_Access,
         vkDestroyCuModuleNVX_Access,
         vkCreateCuModuleNVX,
         vkDestroyCuModuleNVX);

    package Cu_Functions_Common is new Objects_Common_Access
        (NVX.Cu_Function_Create_Info,
         C_NVX.Cu_Function_Create_Info_C,
         NVX.Cu_Function,
         NVX.No_Cu_Function,
         C_NVX.To_C,
         C_NVX.Free,
         vkCreateCuFunctionNVX_Access,
         vkDestroyCuFunctionNVX_Access,
         vkCreateCuFunctionNVX,
         vkDestroyCuFunctionNVX);

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCreateCuModuleNVX_Access);
        procedure Load is new Load_Pointer(vkCreateCuFunctionNVX_Access);
        procedure Load is new Load_Pointer(vkDestroyCuModuleNVX_Access);
        procedure Load is new Load_Pointer(vkDestroyCuFunctionNVX_Access);
        procedure Load is new Load_Pointer(vkCmdCuLaunchKernelNVX_Access);
    begin
        Load(vkCreateCuModuleNVX, "vkCreateCuModuleNVX");
        Load(vkCreateCuFunctionNVX, "vkCreateCuFunctionNVX");
        Load(vkDestroyCuModuleNVX, "vkDestroyCuModuleNVX");
        Load(vkDestroyCuFunctionNVX, "vkDestroyCuFunctionNVX");
        Load(vkCmdCuLaunchKernelNVX, "vkCmdCuLaunchKernelNVX");
    end Load_Extension;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Module_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Module: out NVX.Cu_Module) return Result
        renames Cu_Modules_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Module_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return NVX.Cu_Module
        renames Cu_Modules_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Module_Create_Info;
                    Module: out NVX.Cu_Module) return Result
        renames Cu_Modules_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Module_Create_Info)
        return NVX.Cu_Module
        renames Cu_Modules_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Function_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Cu_Function: out NVX.Cu_Function) return Result
        renames Cu_Functions_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Function_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return NVX.Cu_Function
        renames Cu_Functions_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Function_Create_Info;
                    Cu_Function: out NVX.Cu_Function) return Result
        renames Cu_Functions_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Function_Create_Info)
        return NVX.Cu_Function
        renames Cu_Functions_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Module: in out NVX.Cu_Module;
                      Allocator: aliased in Allocation_Callbacks)
        renames Cu_Modules_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device; Module: in out NVX.Cu_Module)
        renames Cu_Modules_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Cu_Function: in out NVX.Cu_Function;
                      Allocator: aliased in Allocation_Callbacks)
        renames Cu_Functions_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Cu_Function: in out NVX.Cu_Function)
        renames Cu_Functions_Common.Destroy;

    procedure Launch_Kernel(Command_Buffer: in Vulkan.Command_Buffer;
                            Launch_Info: in NVX.Cu_Launch_Info) is
        Info_C: C_NVX.Cu_Launch_Info_C := C_NVX.To_C(Launch_Info);
    begin
        vkCmdCuLaunchKernelNVX(Command_Buffer, Info_C);
        C_NVX.Free(Info_C);
    end Launch_Kernel;
end Vulkan.Extensions.NVX_Binary_Import;


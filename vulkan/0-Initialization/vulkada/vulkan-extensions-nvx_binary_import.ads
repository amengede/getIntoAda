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

with Vulkan.Extensions.NVX;

package Vulkan.Extensions.NVX_Binary_Import is
    use type System.Address;
    use type Interfaces.C.size_t;
    use type NVX.Cu_Module;
    use type NVX.Cu_Function;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCreateCuModuleNVX
    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Module_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Module: out NVX.Cu_Module) return Result
        with Pre => Device /= No_Device and
                    (if Create_Info.Data_Size > 0 then
                        Create_Info.Data /= System.Null_Address),
             Post => Create'Result in Success |
                     Out_Of_Host_Memory |
                     Initialization_Failed and
                     (if Create'Result = Success then
                        Module /= NVX.No_Cu_Module);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Module_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return NVX.Cu_Module
        with Pre => Device /= No_Device and
                    (if Create_Info.Data_Size > 0 then
                        Create_Info.Data /= System.Null_Address),
             Post => Create'Result /= NVX.No_Cu_Module;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Module_Create_Info;
                    Module: out NVX.Cu_Module) return Result
        with Pre => Device /= No_Device and
                    (if Create_Info.Data_Size > 0 then
                        Create_Info.Data /= System.Null_Address),
             Post => Create'Result in Success |
                     Out_Of_Host_Memory |
                     Initialization_Failed and
                     (if Create'Result = Success then
                        Module /= NVX.No_Cu_Module);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Module_Create_Info)
        return NVX.Cu_Module
        with Pre => Device /= No_Device and
                    (if Create_Info.Data_Size > 0 then
                        Create_Info.Data /= System.Null_Address),
             Post => Create'Result /= NVX.No_Cu_Module;

    -- vkCreateCuFunctionNVX
    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Function_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Cu_Function: out NVX.Cu_Function) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Module /= NVX.No_Cu_Module,
             Post => Create'Result in Success |
                     Out_Of_Host_Memory |
                     Initialization_Failed and
                     (if Create'Result = Success then
                        Cu_Function /= NVX.No_Cu_Function);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Function_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return NVX.Cu_Function
        with Pre => Device /= No_Device and
                    Create_Info.Module /= NVX.No_Cu_Module,
             Post => Create'Result /= NVX.No_Cu_Function;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Function_Create_Info;
                    Cu_Function: out NVX.Cu_Function) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Module /= NVX.No_Cu_Module,
             Post => Create'Result in Success |
                     Out_Of_Host_Memory |
                     Initialization_Failed and
                     (if Create'Result = Success then
                        Cu_Function /= NVX.No_Cu_Function);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in NVX.Cu_Function_Create_Info)
        return NVX.Cu_Function
        with Pre => Device /= No_Device and
                    Create_Info.Module /= NVX.No_Cu_Module,
             Post => Create'Result /= NVX.No_Cu_Function;

    -- vkDestroyCuModuleNVX
    procedure Destroy(Device: in Vulkan.Device;
                      Module: in out NVX.Cu_Module;
                      Allocator: aliased in Allocation_Callbacks)
        with Pre => Device /= No_Device,
             Post => Module = NVX.No_Cu_Module;

    procedure Destroy(Device: in Vulkan.Device; Module: in out NVX.Cu_Module)
        with Pre => Device /= No_Device,
             Post => Module = NVX.No_Cu_Module;

    -- vkDestroyCuFunctionNVX
    procedure Destroy(Device: in Vulkan.Device;
                      Cu_Function: in out NVX.Cu_Function;
                      Allocator: aliased in Allocation_Callbacks)
        with Pre => Device /= No_Device,
             Post => Cu_Function = NVX.No_Cu_Function;

    procedure Destroy(Device: in Vulkan.Device;
                      Cu_Function: in out NVX.Cu_Function)
        with Pre => Device /= No_Device,
             Post => Cu_Function = NVX.No_Cu_Function;

    -- vkCmdCuLaunchKernelNVX
    procedure Launch_Kernel(Command_Buffer: in Vulkan.Command_Buffer;
                            Launch_Info: in NVX.Cu_Launch_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Launch_Info.Cu_Function /= NVX.No_Cu_Function;
end Vulkan.Extensions.NVX_Binary_Import;


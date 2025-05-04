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

-- Shader module related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Shader_Modules is
    -- vkCreateShaderModule
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Shader_Module_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Shader_Module: out Vulkan.Shader_Module) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then
                        Shader_Module /= No_Shader_Module);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Shader_Module_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Shader_Module
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Shader_Module;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Shader_Module_Create_Info;
                    Shader_Module: out Vulkan.Shader_Module) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then
                        Shader_Module /= No_Shader_Module);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Shader_Module_Create_Info)
        return Shader_Module
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Shader_Module;

    -- vkDestroyShaderModule
    procedure Destroy(Device: in Vulkan.Device;
                      Shader_Module: in out Vulkan.Shader_Module;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Shader_Module = No_Shader_Module;

    procedure Destroy(Device: in Vulkan.Device;
                      Shader_Module: in out Vulkan.Shader_Module)
        with Inline,
             Pre => Device /= No_Device,
             Post => Shader_Module = No_Shader_Module;

private
    package Shader_Modules_Common is
        new Objects_Common(Shader_Module_Create_Info,
                           C.Shader_Module_Create_Info_C,
                           Shader_Module,
                           No_Shader_Module,
                           C.To_C,
                           C.Free,
                           C.vkCreateShaderModule,
                           C.vkDestroyShaderModule);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Shader_Module_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Shader_Module: out Vulkan.Shader_Module) return Result
        renames Shader_Modules_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Shader_Module_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Shader_Module
        renames Shader_Modules_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Shader_Module_Create_Info;
                    Shader_Module: out Vulkan.Shader_Module) return Result
        renames Shader_Modules_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Shader_Module_Create_Info)
        return Shader_Module
        renames Shader_Modules_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Shader_Module: in out Vulkan.Shader_Module;
                      Allocator: aliased in Allocation_Callbacks)
        renames Shader_Modules_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Shader_Module: in out Vulkan.Shader_Module)
        renames Shader_Modules_Common.Destroy;
end Vulkan.Shader_Modules;


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

-- Core Vulkan subprograms

package Vulkan.Core is
    -- vkCreateInstance
    function Create(Create_Info: in Instance_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Instance: out Vulkan.Instance)
        return Result
        with Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed |
                                      Layer_Not_Present |
                                      Extension_Not_Present |
                                      Incompatible_Driver and
                     (if Create'Result = Success then Instance /= No_Instance);
    
    function Create(Create_Info: in Instance_Create_Info;
                    Allocator: aliased in Allocation_Callbacks) return Instance
        with Post => Create'Result /= No_Instance;

    function Create(Create_Info: in Instance_Create_Info;
                    Instance: out Vulkan.Instance) return Result
        with Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed |
                                      Layer_Not_Present |
                                      Extension_Not_Present |
                                      Incompatible_Driver and
                     (if Create'Result = Success then Instance /= No_Instance);

    function Create(Create_Info: in Instance_Create_Info) return Instance
        with Post => Create'Result /= No_Instance;

    -- vkDestroyInstance
    procedure Destroy(Instance: in out Vulkan.Instance;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Post => Instance = No_Instance;

    procedure Destroy(Instance: in out Vulkan.Instance)
        with Inline,
             Post => Instance = No_Instance;

    -- vkGetInstanceProcAddr
    generic
        type Proc(<>) is private;
    function Get_Proc_Addr(Instance: in Vulkan.Instance;
                           Name: in String) return Proc;

    -- vkEnumerateInstanceExtensionProperties
    function Enumerate_Instance_Extension_Properties(Layer_Name: in String)
        return Extension_Properties_Vectors.Vector;

    function Enumerate_Instance_Extension_Properties
        return Extension_Properties_Vectors.Vector;

    -- vkEnumerateInstanceLayerProperties
    function Enumerate_Instance_Layer_Properties
        return Layer_Properties_Vectors.Vector;

    -- Vulkan 1.1
    -- vkEnumerateInstanceVersion
    function Enumerate_Instance_Version return Version_Number
        with Inline;
end Vulkan.Core;


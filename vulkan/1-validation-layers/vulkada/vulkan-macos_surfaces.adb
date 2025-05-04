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

-- Operations for the MacOS surface extension

with Vulkan.Core;
with Vulkan.MacOS_Surfaces_C;
with Vulkan.Platform_Surfaces;

package body Vulkan.MacOS_Surfaces is
    -- Loaded extension functions.
    type vkCreateMacOSSurfaceMVK_Access is
        access function
            (Instance: in Vulkan.Instance;
             Create_Info: in MacOS_Surfaces_C.MacOS_Surface_Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Surface: out Vulkan.Surface) return Result
        with Convention => C;

    vkCreateMacOSSurfaceMVK: vkCreateMacOSSurfaceMVK_Access;

    -- Common surface implementation.
    package Surfaces_Common is
        new Platform_Surfaces(Create_Info,
                              MacOS_Surfaces_C.MacOS_Surface_Create_Info_C,
                              MacOS_Surfaces_C.To_C,
                              MacOS_Surfaces_C.Free,
                              vkCreateMacOSSurfaceMVK_Access,
                              vkCreateMacOSSurfaceMVK);

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCreateMacOSSurfaceMVK_Access);
    begin
        Load(vkCreateMacOSSurfaceMVK, "vkCreateMacOSSurfaceMVK");
    end Load_Extension;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in MacOS_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out Vulkan.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in MacOS_Surfaces.Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Surface renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in MacOS_Surfaces.Create_Info;
                    Surface: out Vulkan.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in MacOS_Surfaces.Create_Info) return Surface
        renames Surfaces_Common.Create;
end Vulkan.MacOS_Surfaces;


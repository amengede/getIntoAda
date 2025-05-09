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

-- Copyright 2025 Phaser Cat Games LLC

-- Operations for the Metal surface extension

with Vulkan.Core;
with Vulkan.C_EXT;
with Vulkan.Platform_Surfaces;

package body Vulkan.Extensions.EXT_Metal_Surface is
    -- Loaded extension functions.
    type vkCreateMetalSurfaceEXT_Access is
        access function
            (Instance: in Vulkan.Instance;
             Create_Info: in C_EXT.Metal_Surface_Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Surface: out KHR.Surface) return Result
        with Convention => C;

    vkCreateMetalSurfaceEXT: vkCreateMetalSurfaceEXT_Access;

    -- Common surface implementation.
    package Surfaces_Common is
        new Platform_Surfaces(EXT.Metal_Surface_Create_Info,
                              C_EXT.Metal_Surface_Create_Info_C,
                              C_EXT.To_C,
                              C_EXT.Free,
                              vkCreateMetalSurfaceEXT_Access,
                              vkCreateMetalSurfaceEXT);

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCreateMetalSurfaceEXT_Access);
    begin
        Load(vkCreateMetalSurfaceEXT, "vkCreateMetalSurfaceEXT");
    end Load_Extension;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Metal_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out KHR.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Metal_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Surface renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Metal_Surface_Create_Info;
                    Surface: out KHR.Surface) return Result
        renames Surfaces_Common.Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Metal_Surface_Create_Info)
        return KHR.Surface renames Surfaces_Common.Create;
end Vulkan.Extensions.EXT_Metal_Surface;


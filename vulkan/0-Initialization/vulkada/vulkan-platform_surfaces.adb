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

-- Common creation code for platform surfaces

with Vulkan.Exceptions;

package body Vulkan.Platform_Surfaces is
    -- Common creation logic.
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type;
                    Allocator: access constant Allocation_Callbacks;
                    Surface: out Extensions.KHR.Surface) return Result;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out Extensions.KHR.Surface) return Result is
    begin
        return Create(Instance, Create_Info, Allocator'Access, Surface);
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type;
                    Allocator: aliased in Allocation_Callbacks)
        return Extensions.KHR.Surface is
        Surface: Extensions.KHR.Surface;
    begin
        Exceptions.Check(Create(Instance, 
                                Create_Info,
                                Allocator'Access,
                                Surface));

        return Surface;
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type;
                    Surface: out Extensions.KHR.Surface) return Result is
    begin
        return Create(Instance, Create_Info, null, Surface);
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type)
        return Extensions.KHR.Surface is
        Surface: Extensions.KHR.Surface;
    begin
        Exceptions.Check(Create(Instance, Create_Info, null, Surface));

        return Surface;
    end Create;
    
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type;
                    Allocator: access constant Allocation_Callbacks;
                    Surface: out Extensions.KHR.Surface) return Result is
        Info_C: Create_Info_Type_C := To_C(Create_Info);
        Result: Vulkan.Result;
    begin
        Result := C_Create(Instance, Info_C, Allocator, Surface);
        Free(Info_C);

        return Result;
    end Create;
end Vulkan.Platform_Surfaces;


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

-- Common creation code for platform surfaces

with Vulkan.Extensions.KHR;

private generic
    type Create_Info_Type(<>) is limited private;
    type Create_Info_Type_C(<>) is limited private;
    with function To_C(Struct: in Create_Info_Type) return Create_Info_Type_C;
    with procedure Free(Struct: in out Create_Info_Type_C);
    type C_Create_Access is
        access function(Instance: in Vulkan.Instance;
                        Create_Info: in Create_Info_Type_C;
                        Allocator: access constant Allocation_Callbacks;
                        Surface: out Extensions.KHR.Surface) return Result;
    C_Create: in out C_Create_Access;
package Vulkan.Platform_Surfaces is
    -- vkCreateWhateverSurfaceBLAH
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out Extensions.KHR.Surface) return Result;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type;
                    Allocator: aliased in Allocation_Callbacks)
        return Extensions.KHR.Surface;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type;
                    Surface: out Extensions.KHR.Surface) return Result;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Create_Info_Type)
        return Extensions.KHR.Surface;
end Vulkan.Platform_Surfaces;


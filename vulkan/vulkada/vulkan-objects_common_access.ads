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

-- Common create and destroy subprograms for objects added in 1.1+

private generic
    type Create_Info_Type(<>) is limited private;
    type Create_Info_Type_C(<>) is limited private;
    type Object is private;
    No_Object: in Object;
    with function To_C(Struct: in Create_Info_Type) return Create_Info_Type_C;
    with procedure Free(Struct: in out Create_Info_Type_C);
    type C_Create_Access is
        access function(Device: in Vulkan.Device;
                        Create_Info: in Create_Info_Type_C;
                        Allocator: access constant Allocation_Callbacks;
                        Handle: out Object) return Result;
    type C_Destroy_Access is
        access procedure(Device: in Vulkan.Device;
                         Handle: in Object;
                         Allocator: access constant Allocation_Callbacks);
    C_Create: in out C_Create_Access;
    C_Destroy: in out C_Destroy_Access;
package Vulkan.Objects_Common_Access is
    -- vkCreateWhatever
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type;
                    Allocator: aliased in Allocation_Callbacks;
                    Handle: out Object) return Result;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type;
                    Allocator: aliased in Allocation_Callbacks) return Object;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type;
                    Handle: out Object) return Result;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type) return Object;

    -- vkDestroyWhatever
    procedure Destroy(Device: in Vulkan.Device;
                      Handle: in out Object;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline;

    procedure Destroy(Device: in Vulkan.Device;
                      Handle: in out Object)
        with Inline;
end Vulkan.Objects_Common_Access;


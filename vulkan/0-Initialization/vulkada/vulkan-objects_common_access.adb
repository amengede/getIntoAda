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

with Vulkan.Exceptions;

package body Vulkan.Objects_Common_Access is
    -- Common creation logic.
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type;
                    Allocator: access constant Allocation_Callbacks;
                    Handle: out Object) return Result;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type;
                    Allocator: aliased in Allocation_Callbacks;
                    Handle: out Object) return Result is
    begin
        return Create(Device, Create_Info, Allocator'Access, Handle);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type;
                    Allocator: aliased in Allocation_Callbacks) return Object is
        O: Object;
    begin
        Exceptions.Check(Create(Device, Create_Info, Allocator'Access, O));

        return O;
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type;
                    Handle: out Object) return Result is
    begin
        return Create(Device, Create_Info, null, Handle);
    end Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type) return Object is
        O: Object;
    begin
        Exceptions.Check(Create(Device, Create_Info, null, O));

        return O;
    end Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Handle: in out Object;
                      Allocator: aliased in Allocation_Callbacks) is
    begin
        C_Destroy(Device, Handle, Allocator'Access);
        Handle := No_Object;
    end Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Handle: in out Object) is
    begin
        C_Destroy(Device, Handle, null);
        Handle := No_Object;
    end Destroy;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Create_Info_Type;
                    Allocator: access constant Allocation_Callbacks;
                    Handle: out Object) return Result is
        Create_Info_C: Create_Info_Type_C := To_C(Create_Info);
        Result: Vulkan.Result;
    begin
        Result := C_Create(Device,
                           Create_Info_C,
                           Allocator,
                           Handle);
        Free(Create_Info_C);

        return Result;
    end Create;
end Vulkan.Objects_Common_Access;


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

-- Operations for the display swapchain extension

with Vulkan.Core;
with Vulkan.Exceptions;
with Vulkan.C_KHR;

package body Vulkan.Extensions.KHR_Display_Swapchain is
    -- Common creation function.
    function Create(Device: in Vulkan.Device;
                    Create_Infos: in KHR.Swapchain_Create_Info_Vectors.Vector;
                    Allocator: access constant Allocation_Callbacks;
                    Swapchains: out KHR.Swapchain_Vectors.Vector) return Result;

    -- Loaded extension functions.
    type vkCreateSharedSwapchainsKHR_Access is
        access function(Device: in Vulkan.Device;
                        Swapchain_Count: in Interfaces.Unsigned_32;
                        Create_Infos: access C_KHR.Swapchain_Create_Info_C;
                        Allocator: access constant Allocation_Callbacks;
                        Swapchains: access KHR.Swapchain) return Result
        with Convention => C;

    vkCreateSharedSwapchainsKHR: vkCreateSharedSwapchainsKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCreateSharedSwapchainsKHR_Access);
    begin
        Load(vkCreateSharedSwapchainsKHR, "vkCreateSharedSwapchainsKHR");
    end Load_Extension;

    function Create(Device: in Vulkan.Device;
                    Create_Infos: in KHR.Swapchain_Create_Info_Vectors.Vector;
                    Allocator: aliased in Allocation_Callbacks;
                    Swapchains: out KHR.Swapchain_Vectors.Vector)
        return Result is
    begin
        return Create(Device, Create_Infos, Allocator'Access, Swapchains);
    end Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Infos: in KHR.Swapchain_Create_Info_Vectors.Vector;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Swapchain_Vectors.Vector is
        Swapchains: KHR.Swapchain_Vectors.Vector;
    begin
        Exceptions.Check(Create(Device,
                                Create_Infos,
                                Allocator'Access,
                                Swapchains));

        return Swapchains;
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Infos: in KHR.Swapchain_Create_Info_Vectors.Vector;
                    Swapchains: out KHR.Swapchain_Vectors.Vector)
        return Result is
    begin
        return Create(Device, Create_Infos, null, Swapchains);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Infos: in KHR.Swapchain_Create_Info_Vectors.Vector)
        return KHR.Swapchain_Vectors.Vector is
        Swapchains: KHR.Swapchain_Vectors.Vector;
    begin
        Exceptions.Check(Create(Device, Create_Infos, null, Swapchains));

        return Swapchains;
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Swapchain: out KHR.Swapchain) return Result is
        Swapchains: KHR.Swapchain_Vectors.Vector;
        Result: Vulkan.Result;
    begin
        Result :=
            Create(Device,
                   KHR.Swapchain_Create_Info_Vectors.To_Vector(Create_Info, 1),
                   Allocator,
                   Swapchains);

        if Result = Success then
            Swapchain := Swapchains(1);
        end if;

        return Result;
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Swapchain is
    begin
        return Create(Device,
                      KHR.Swapchain_Create_Info_Vectors.To_Vector
                          (Create_Info, 1),
                      Allocator)(1);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info;
                    Swapchain: out KHR.Swapchain) return Result is
        Swapchains: KHR.Swapchain_Vectors.Vector;
        Result: Vulkan.Result;
    begin
        Result :=
            Create(Device,
                   KHR.Swapchain_Create_Info_Vectors.To_Vector(Create_Info, 1),
                   Swapchains);

        if Result = Success then
            Swapchain := Swapchains(1);
        end if;

        return Result;
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in KHR.Swapchain_Create_Info)
        return KHR.Swapchain is
    begin
        return Create(Device,
                      KHR.Swapchain_Create_Info_Vectors.To_Vector(Create_Info,
                                                                  1))(1);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Infos: in KHR.Swapchain_Create_Info_Vectors.Vector;
                    Allocator: access constant Allocation_Callbacks;
                    Swapchains: out KHR.Swapchain_Vectors.Vector)
        return Result is
        Create_Infos_C: array (1 .. Positive(Create_Infos.Length))
            of aliased C_KHR.Swapchain_Create_Info_C;
        New_Swapchains: array (Create_Infos_C'Range) of aliased KHR.Swapchain;
        Result: Vulkan.Result;
    begin
        for X in Create_Infos_C'Range loop
            Create_Infos_C(X) := C_KHR.To_C(Create_Infos(X));
        end loop;

        Swapchains.Set_Length(Create_Infos.Length);

        Result := vkCreateSharedSwapchainsKHR
            (Device,
             Interfaces.Unsigned_32(Create_Infos.Length),
             Create_Infos_C(1)'Access,
             Allocator,
             New_Swapchains(1)'Access);

        for X in Create_Infos_C'Range loop
            C_KHR.Free(Create_Infos_C(X));
            Swapchains(X) := New_Swapchains(X);
        end loop;

        return Result;
    end Create;
end Vulkan.Extensions.KHR_Display_Swapchain;


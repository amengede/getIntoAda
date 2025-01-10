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

-- Operations for the surface extension

with Vulkan.Core;
with Vulkan.C_KHR;
with Vulkan.Utilities;
with Vulkan.Exceptions;

package body Vulkan.Extensions.KHR_Surface is
    -- Loaded extension functions.
    type vkDestroySurfaceKHR_Access is
        access procedure(Instance: in Vulkan.Instance;
                         Surface: in KHR.Surface;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroySurfaceKHR: vkDestroySurfaceKHR_Access;

    type vkGetPhysicalDeviceSurfaceSupportKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Queue_Family_Index: in Vulkan.Queue_Family_Index;
                        Surface: in KHR.Surface;
                        Supported: out Interfaces.Unsigned_32) return Result
        with Convention => C;

    vkGetPhysicalDeviceSurfaceSupportKHR:
        vkGetPhysicalDeviceSurfaceSupportKHR_Access;

    type vkGetPhysicalDeviceSurfaceCapabilitiesKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Surface: in KHR.Surface;
                        Surface_Capabilities: out KHR.Surface_Capabilities)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceSurfaceCapabilitiesKHR:
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR_Access;

    type vkGetPhysicalDeviceSurfaceFormatsKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Surface: in KHR.Surface;
                        Surface_Format_Count: in out Interfaces.Unsigned_32;
                        Surface_Formats: access KHR.Surface_Format)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceSurfaceFormatsKHR:
        vkGetPhysicalDeviceSurfaceFormatsKHR_Access;

    type vkGetPhysicalDeviceSurfacePresentModesKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Surface: in KHR.Surface;
                        Present_Mode_Count: in out Interfaces.Unsigned_32;
                        Present_Modes: access KHR.Present_Mode) return Result
        with Convention => C;

    vkGetPhysicalDeviceSurfacePresentModesKHR:
        vkGetPhysicalDeviceSurfacePresentModesKHR_Access;

    -- Common query logic for formats and present modes.
    generic
        type Object is private;
        with package Object_Vectors is new Ada.Containers.Vectors(Positive,
                                                                  Object);
        type Function_Type is
            access function(Physical_Device: in Vulkan.Physical_Device;
                            Surface: in KHR.Surface;
                            Count: in out Interfaces.Unsigned_32;
                            Objects: access Object) return Result;
        Function_Pointer: in out Function_Type;
    package Queries_Common is
        function Get_Count(Physical_Device: in Vulkan.Physical_Device;
                           Surface: in KHR.Surface)
            return Interfaces.Unsigned_32;

        function Get_Objects(Physical_Device: in Vulkan.Physical_Device;
                             Surface: in KHR.Surface;
                             Objects: in out Object_Vectors.Vector)
            return Result;

        function Get_Objects(Physical_Device: in Vulkan.Physical_Device;
                             Surface: in KHR.Surface)
            return Object_Vectors.Vector;
    end Queries_Common;

    package body Queries_Common is
        function Get_Count(Physical_Device: in Vulkan.Physical_Device;
                           Surface: in KHR.Surface)
            return Interfaces.Unsigned_32 is
            Count: Interfaces.Unsigned_32 := 0;
        begin
           Exceptions.Check(Function_Pointer(Physical_Device,
                                             Surface,
                                             Count,
                                             null));

           return Count;
        end Get_Count;

        function Get_Objects(Physical_Device: in Vulkan.Physical_Device;
                             Surface: in KHR.Surface;
                             Objects: in out Object_Vectors.Vector)
            return Result is
            use type Interfaces.Unsigned_32;

            Count: Interfaces.Unsigned_32 := Get_Count(Physical_Device,
                                                       Surface);
            C_Objects: array (1 .. Count) of aliased Object;
            Result: Vulkan.Result;
        begin
            if Count = 0 then
                Objects.Clear;

                return Success;
            end if;

            Result := Function_Pointer(Physical_Device,
                                       Surface,
                                       Count,
                                       C_Objects(1)'Access);

            if Result = Success then
                Objects.Clear;

                for Object of C_Objects loop
                    Objects.Append(Object);
                end loop;
            end if;

            return Result;
        end Get_Objects;

        function Get_Objects(Physical_Device: in Vulkan.Physical_Device;
                             Surface: in KHR.Surface)
            return Object_Vectors.Vector is
            Objects: Object_Vectors.Vector;
        begin
            Exceptions.Check(Get_Objects(Physical_Device, Surface, Objects));

            return Objects;
        end Get_Objects;
    end Queries_Common;

    package Surface_Format_Queries is
        new Queries_Common(KHR.Surface_Format,
                           KHR.Surface_Format_Vectors,
                           vkGetPhysicalDeviceSurfaceFormatsKHR_Access,
                           vkGetPhysicalDeviceSurfaceFormatsKHR);

    package Present_Mode_Queries is
        new Queries_Common(KHR.Present_Mode,
                           KHR.Present_Mode_Vectors,
                           vkGetPhysicalDeviceSurfacePresentModesKHR_Access,
                           vkGetPhysicalDeviceSurfacePresentModesKHR);

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkDestroySurfaceKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceSurfaceSupportKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceSurfaceCapabilitiesKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceSurfaceFormatsKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceSurfacePresentModesKHR_Access);
    begin
        Load(vkDestroySurfaceKHR, "vkDestroySurfaceKHR");
        Load(vkGetPhysicalDeviceSurfaceSupportKHR,
             "vkGetPhysicalDeviceSurfaceSupportKHR");
        Load(vkGetPhysicalDeviceSurfaceCapabilitiesKHR,
             "vkGetPhysicalDeviceSurfaceCapabilitiesKHR");
        Load(vkGetPhysicalDeviceSurfaceFormatsKHR,
             "vkGetPhysicalDeviceSurfaceFormatsKHR");
        Load(vkGetPhysicalDeviceSurfacePresentModesKHR,
             "vkGetPhysicalDeviceSurfacePresentModesKHR");
    end Load_Extension;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Surface: in out KHR.Surface;
                      Allocator: aliased in Allocation_Callbacks) is
    begin
        vkDestroySurfaceKHR(Instance, Surface, Allocator'Access);
        Surface := KHR.No_Surface;
    end Destroy;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Surface: in out KHR.Surface) is
    begin
        vkDestroySurfaceKHR(Instance, Surface, null);
        Surface := KHR.No_Surface;
    end Destroy;

    function Get_Physical_Device_Surface_Support
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Surface: in KHR.Surface;
         Supported: out Boolean) return Result is
        C_Supported: Interfaces.Unsigned_32;
        Result: Vulkan.Result;
    begin
        Result := vkGetPhysicalDeviceSurfaceSupportKHR(Physical_Device,
                                                       Queue_Family_Index,
                                                       Surface,
                                                       C_Supported);

        if Result = Success then
            Supported := Utilities.To_Ada(C_Supported);
        end if;

        return Result;
    end Get_Physical_Device_Surface_Support;

    function Get_Physical_Device_Surface_Support
        (Physical_Device: in Vulkan.Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Surface: in KHR.Surface) return Boolean is
        Support: Boolean;
    begin
        Exceptions.Check
            (Get_Physical_Device_Surface_Support(Physical_Device,
                                                 Queue_Family_Index,
                                                 Surface,
                                                 Support));

        return Support;
    end Get_Physical_Device_Surface_Support;

    function Get_Physical_Device_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Capabilities: out KHR.Surface_Capabilities) return Result is
    begin
        return vkGetPhysicalDeviceSurfaceCapabilitiesKHR(Physical_Device,
                                                         Surface,
                                                         Surface_Capabilities);
    end Get_Physical_Device_Surface_Capabilities;

    function Get_Physical_Device_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return KHR.Surface_Capabilities is
        Capabilities: KHR.Surface_Capabilities;
    begin
        Exceptions.Check
            (Get_Physical_Device_Surface_Capabilities(Physical_Device,
                                                      Surface,
                                                      Capabilities));

        return Capabilities;
    end Get_Physical_Device_Surface_Capabilities;

    function Surface_Format_Count(Physical_Device: in Vulkan.Physical_Device;
                                  Surface: in KHR.Surface)
        return Interfaces.Unsigned_32 renames Surface_Format_Queries.Get_Count;

    function Get_Physical_Device_Surface_Formats
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Formats: in out KHR.Surface_Format_Vectors.Vector)
        return Result renames Surface_Format_Queries.Get_Objects;

    function Get_Physical_Device_Surface_Formats
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return KHR.Surface_Format_Vectors.Vector
        renames Surface_Format_Queries.Get_Objects;

    function Surface_Present_Modes_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return Interfaces.Unsigned_32
        renames Present_Mode_Queries.Get_Count;

    function Get_Physical_Device_Surface_Present_Modes
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Formats: in out KHR.Present_Mode_Vectors.Vector)
        return Result renames Present_Mode_Queries.Get_Objects;

    function Get_Physical_Device_Surface_Present_Modes
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return KHR.Present_Mode_Vectors.Vector
        renames Present_Mode_Queries.Get_Objects;
end Vulkan.Extensions.KHR_Surface;


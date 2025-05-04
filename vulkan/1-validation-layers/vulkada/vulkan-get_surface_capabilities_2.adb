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

-- Operations for the get surface capabilities 2 extension

with Vulkan.Get_Surface_Capabilities_2_C;
with Vulkan.Core;
with Vulkan.Extension_Records;
with Vulkan.Exceptions;

package body Vulkan.Get_Surface_Capabilities_2 is
    -- Loaded extension functions.
    type vkGetPhysicalDeviceSurfaceCapabilities2KHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Surface_Info: in
                Get_Surface_Capabilities_2_C.Physical_Device_Surface_Info_2_C;
             Surface_Capabilities: out
                Get_Surface_Capabilities_2_C.Surface_Capabilities_2_C)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceSurfaceCapabilities2KHR:
        vkGetPhysicalDeviceSurfaceCapabilities2KHR_Access;

    type vkGetPhysicalDeviceSurfaceFormats2KHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Surface_Info: in
                Get_Surface_Capabilities_2_C.Physical_Device_Surface_Info_2_C;
             Surface_Format_Count: in out Interfaces.Unsigned_32;
             Surface_Formats:
                access Get_Surface_Capabilities_2_C.Surface_Format_2_C)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceSurfaceFormats2KHR:
        vkGetPhysicalDeviceSurfaceFormats2KHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceSurfaceCapabilities2KHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceSurfaceFormats2KHR_Access);
    begin
        Load(vkGetPhysicalDeviceSurfaceCapabilities2KHR,
             "vkGetPhysicalDeviceSurfaceCapabilities2KHR");
        Load(vkGetPhysicalDeviceSurfaceFormats2KHR,
             "vkGetPhysicalDeviceSurfaceFormats2KHR");
    end Load_Extension;

    function Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in Physical_Device_Surface_Info_2;
         Surface_Capabilities: in out Surface_Capabilities_2) return Result is
        Surface_Info_C:
            Get_Surface_Capabilities_2_C.Physical_Device_Surface_Info_2_C :=
                Get_Surface_Capabilities_2_C.To_C(Surface_Info);
        Surface_Capabilities_C:
            Get_Surface_Capabilities_2_C.Surface_Capabilities_2_C;
        Result: Vulkan.Result;
    begin
        Surface_Capabilities_C.Next :=
            Extension_Records.To_C(Surface_Capabilities.Next);
        Result := vkGetPhysicalDeviceSurfaceCapabilities2KHR
            (Physical_Device, Surface_Info_C, Surface_Capabilities_C);
        Get_Surface_Capabilities_2_C.Free(Surface_Info_C);
        Get_Surface_Capabilities_2_C.To_Ada(Surface_Capabilities,
                                            Surface_Capabilities_C);
        Extension_Records.Free(Surface_Capabilities_C.Next);

        return Result;
    end Get_Surface_Capabilities;

    function Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in Physical_Device_Surface_Info_2)
        return Surface_Capabilities_2 is
        Capabilities: Surface_Capabilities_2;
    begin
        Exceptions.Check(Get_Surface_Capabilities(Physical_Device,
                                                  Surface_Info,
                                                  Capabilities));

        return Capabilities;
    end Get_Surface_Capabilities;

    function Surface_Format_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in Physical_Device_Surface_Info_2)
        return Interfaces.Unsigned_32 is
        Surface_Info_C:
            Get_Surface_Capabilities_2_C.Physical_Device_Surface_Info_2_C :=
                Get_Surface_Capabilities_2_C.To_C(Surface_Info);
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check
            (vkGetPhysicalDeviceSurfaceFormats2KHR(Physical_Device,
                                                   Surface_Info_C,
                                                   Count,
                                                   null));
        Get_Surface_Capabilities_2_C.Free(Surface_Info_C);

        return Count;
    end Surface_Format_Count;

    function Get_Surface_Formats
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in Physical_Device_Surface_Info_2;
         Surface_Formats: in out Surface_Format_2_Vectors.Vector)
        return Result is
        Surface_Info_C:
            Get_Surface_Capabilities_2_C.Physical_Device_Surface_Info_2_C :=
                Get_Surface_Capabilities_2_C.To_C(Surface_Info);
        Surface_Formats_C: array (1 .. Positive(Surface_Formats.Length)) of
            aliased Get_Surface_Capabilities_2_C.Surface_Format_2_C;
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Surface_Formats.Length);
        Result: Vulkan.Result;
    begin
        for X in Surface_Formats_C'Range loop
            Surface_Formats_C(X).Next :=
                Extension_Records.To_C(Surface_Formats(X).Next);
        end loop;

        Result :=
            vkGetPhysicalDeviceSurfaceFormats2KHR(Physical_Device,
                                                  Surface_Info_C,
                                                  Count,
                                                  Surface_Formats_C(1)'Access);
        Get_Surface_Capabilities_2_C.Free(Surface_Info_C);

        for X in Surface_Formats_C'Range loop
            Get_Surface_Capabilities_2_C.To_Ada(Surface_Formats(X),
                                                Surface_Formats_C(X));
            Extension_Records.Free(Surface_Formats_C(X).Next);
        end loop;

        return Result;
    end Get_Surface_Formats;

    function Get_Surface_Formats
        (Physical_Device: in Vulkan.Physical_Device;
         Surface_Info: in Physical_Device_Surface_Info_2)
        return Surface_Format_2_Vectors.Vector is
        Format: Surface_Format_2;
        Count: Ada.Containers.Count_Type :=
            Ada.Containers.Count_Type(Surface_Format_Count(Physical_Device,
                                                           Surface_Info));
        Formats: Surface_Format_2_Vectors.Vector :=
            Surface_Format_2_Vectors.To_Vector(Format, Count);
    begin
        Exceptions.Check(Get_Surface_Formats(Physical_Device,
                                             Surface_Info,
                                             Formats));

        return Formats;
    end Get_Surface_Formats;
end Vulkan.Get_Surface_Capabilities_2;


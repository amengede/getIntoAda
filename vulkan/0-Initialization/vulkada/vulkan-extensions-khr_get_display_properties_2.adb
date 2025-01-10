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

-- Operations for the get display properties 2 extension

with Vulkan.C_KHR;
with Vulkan.Core;
with Vulkan.Extension_Records;
with Vulkan.Exceptions;

package body Vulkan.Extensions.KHR_Get_Display_Properties_2 is
    -- Loaded extension functions.
    type vkGetPhysicalDeviceDisplayProperties2KHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Property_Count: in out Interfaces.Unsigned_32;
                        Properties: access C_KHR.Display_Properties_2_C)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceDisplayProperties2KHR:
        vkGetPhysicalDeviceDisplayProperties2KHR_Access;

    type vkGetPhysicalDeviceDisplayPlaneProperties2KHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Property_Count: in out Interfaces.Unsigned_32;
             Properties: access C_KHR.Display_Plane_Properties_2_C)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceDisplayPlaneProperties2KHR:
        vkGetPhysicalDeviceDisplayPlaneProperties2KHR_Access;

    type vkGetDisplayModeProperties2KHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Display: in KHR.Display;
             Property_Count: in out Interfaces.Unsigned_32;
             Properties: access C_KHR.Display_Mode_Properties_2_C)
        return Result
        with Convention => C;

    vkGetDisplayModeProperties2KHR: vkGetDisplayModeProperties2KHR_Access;

    type vkGetDisplayPlaneCapabilities2KHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Display_Plane_Info: in C_KHR.Display_Plane_Info_2_C;
             Capabilities: out C_KHR.Display_Plane_Capabilities_2_C)
        return Result
        with Convention => C;

    vkGetDisplayPlaneCapabilities2KHR: vkGetDisplayPlaneCapabilities2KHR_Access;

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
            (vkGetPhysicalDeviceDisplayProperties2KHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceDisplayPlaneProperties2KHR_Access);
        procedure Load is new Load_Pointer
            (vkGetDisplayModeProperties2KHR_Access);
        procedure Load is new Load_Pointer
            (vkGetDisplayPlaneCapabilities2KHR_Access);
    begin
        Load(vkGetPhysicalDeviceDisplayProperties2KHR,
             "vkGetPhysicalDeviceDisplayProperties2KHR");
        Load(vkGetPhysicalDeviceDisplayPlaneProperties2KHR,
             "vkGetPhysicalDeviceDisplayPlaneProperties2KHR");
        Load(vkGetDisplayModeProperties2KHR, "vkGetDisplayModeProperties2KHR");
        Load(vkGetDisplayPlaneCapabilities2KHR,
             "vkGetDisplayPlaneCapabilities2KHR");
    end Load_Extension;

    function Display_Properties_Count
        (Physical_Device: in Vulkan.Physical_Device)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(vkGetPhysicalDeviceDisplayProperties2KHR
                            (Physical_Device, Count, null));

        return Count;
    end Display_Properties_Count;

    function Get_Display_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out KHR.Display_Properties_2_Vectors.Vector)
        return Result is
        C_Properties: array (1 .. Positive(Properties.Length)) of
            aliased C_KHR.Display_Properties_2_C;
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Properties.Length);
        Result: Vulkan.Result;
    begin
        for X in C_Properties'Range loop
            C_Properties(X).Next := Extension_Records.To_C(Properties(X).Next);
        end loop;

        Result := vkGetPhysicalDeviceDisplayProperties2KHR
                    (Physical_Device, Count, C_Properties(1)'Access);

        for X in C_Properties'Range loop
            C_KHR.To_Ada(Properties(X), C_Properties(X));
            Extension_Records.Free(C_Properties(X).Next);
        end loop;

        return Result;
    end Get_Display_Properties;
    
    function Get_Display_Properties(Physical_Device: in Vulkan.Physical_Device)
        return KHR.Display_Properties_2_Vectors.Vector is
        Properties: KHR.Display_Properties_2_Vectors.Vector;
        Property: KHR.Display_Properties_2;
    begin
        Properties := KHR.Display_Properties_2_Vectors.To_Vector
            (Property, Ada.Containers.Count_Type(Display_Properties_Count
                            (Physical_Device)));
        Exceptions.Check(Get_Display_Properties(Physical_Device, Properties));

        return Properties;
    end Get_Display_Properties;

    function Display_Plane_Properties_Count
        (Physical_Device: in Vulkan.Physical_Device)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(vkGetPhysicalDeviceDisplayPlaneProperties2KHR
                            (Physical_Device, Count, null));

        return Count;
    end Display_Plane_Properties_Count;

    function Get_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out KHR.Display_Plane_Properties_2_Vectors.Vector)
        return Result is
        C_Properties: array (1 .. Positive(Properties.Length)) of
            aliased C_KHR.Display_Plane_Properties_2_C;
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Properties.Length);
        Result: Vulkan.Result;
    begin
        for X in C_Properties'Range loop
            C_Properties(X).Next := Extension_Records.To_C(Properties(X).Next);
        end loop;

        Result := vkGetPhysicalDeviceDisplayPlaneProperties2KHR
                    (Physical_Device, Count, C_Properties(1)'Access);

        for X in C_Properties'Range loop
            C_KHR.To_Ada(Properties(X), C_Properties(X));
            Extension_Records.Free(C_Properties(X).Next);
        end loop;

        return Result;
    end Get_Display_Plane_Properties;
    
    function Get_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device)
        return KHR.Display_Plane_Properties_2_Vectors.Vector is
        Properties: KHR.Display_Plane_Properties_2_Vectors.Vector;
        Property: KHR.Display_Plane_Properties_2;
    begin
        Properties := KHR.Display_Plane_Properties_2_Vectors.To_Vector
            (Property, Ada.Containers.Count_Type(Display_Plane_Properties_Count
                            (Physical_Device)));
        Exceptions.Check(Get_Display_Plane_Properties(Physical_Device, 
                                                      Properties));

        return Properties;
    end Get_Display_Plane_Properties;

    function Display_Mode_Properties_Count
        (Physical_Device: in Vulkan.Physical_Device; Display: in KHR.Display)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(vkGetDisplayModeProperties2KHR
                            (Physical_Device, Display, Count, null));

        return Count;
    end Display_Mode_Properties_Count;

    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in KHR.Display;
         Properties: in out KHR.Display_Mode_Properties_2_Vectors.Vector)
        return Result is
        C_Properties: array (1 .. Positive(Properties.Length)) of
            aliased C_KHR.Display_Mode_Properties_2_C;
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Properties.Length);
        Result: Vulkan.Result;
    begin
        for X in C_Properties'Range loop
            C_Properties(X).Next := Extension_Records.To_C(Properties(X).Next);
        end loop;

        Result := vkGetDisplayModeProperties2KHR
                    (Physical_Device, Display, Count, C_Properties(1)'Access);

        for X in C_Properties'Range loop
            C_KHR.To_Ada(Properties(X), C_Properties(X));
            Extension_Records.Free(C_Properties(X).Next);
        end loop;

        return Result;
    end Get_Display_Mode_Properties;

    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in KHR.Display)
        return KHR.Display_Mode_Properties_2_Vectors.Vector is
        Properties: KHR.Display_Mode_Properties_2_Vectors.Vector;
        Property: KHR.Display_Mode_Properties_2;
    begin
        Properties := KHR.Display_Mode_Properties_2_Vectors.To_Vector
            (Property, Ada.Containers.Count_Type(Display_Mode_Properties_Count
                            (Physical_Device, Display)));
        Exceptions.Check(Get_Display_Mode_Properties(Physical_Device,
                                                     Display,
                                                     Properties));

        return Properties;
    end Get_Display_Mode_Properties;

    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Display_Plane_Info: in KHR.Display_Plane_Info_2;
         Capabilities: in out KHR.Display_Plane_Capabilities_2) return Result is
        C_Info: C_KHR.Display_Plane_Info_2_C := C_KHR.To_C(Display_Plane_Info);
        C_Capabilities: C_KHR.Display_Plane_Capabilities_2_C;
        Result: Vulkan.Result;
    begin
        C_Capabilities.Next := Extension_Records.To_C(Capabilities.Next);

        Result := vkGetDisplayPlaneCapabilities2KHR(Physical_Device,
                                                    C_Info,
                                                    C_Capabilities);

        C_KHR.Free(C_Info);
        C_KHR.To_Ada(Capabilities, C_Capabilities);
        Extension_Records.Free(C_Capabilities.Next);

        return Result;
    end Get_Display_Plane_Capabilities;

    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Display_Plane_Info: in KHR.Display_Plane_Info_2)
        return KHR.Display_Plane_Capabilities_2 is
        Capabilities: KHR.Display_Plane_Capabilities_2;
    begin
        Exceptions.Check(Get_Display_Plane_Capabilities(Physical_Device,
                                                        Display_Plane_Info,
                                                        Capabilities));

        return Capabilities;
    end Get_Display_Plane_Capabilities;
end Vulkan.Extensions.KHR_Get_Display_Properties_2;


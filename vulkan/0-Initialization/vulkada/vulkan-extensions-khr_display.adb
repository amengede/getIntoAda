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

-- Operations for the display extension

with Interfaces.C.Strings;
with Vulkan.Core;
with Vulkan.Exceptions;
with Vulkan.C_KHR;

package body Vulkan.Extensions.KHR_Display is
    -- Common creation functions.
    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Mode: out KHR.Display_Mode) return Result;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Surface: out KHR.Surface) return Result;

    -- Loaded extension functions.
    type vkGetPhysicalDeviceDisplayPropertiesKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Property_Count: in out Interfaces.Unsigned_32;
                        Properties: access C_KHR.Display_Properties_C)
            return Result
        with Convention => C;

    vkGetPhysicalDeviceDisplayPropertiesKHR:
        vkGetPhysicalDeviceDisplayPropertiesKHR_Access;

    type vkGetPhysicalDeviceDisplayPlanePropertiesKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Property_Count: in out Interfaces.Unsigned_32;
                        Properties: access KHR.Display_Plane_Properties)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceDisplayPlanePropertiesKHR:
        vkGetPhysicalDeviceDisplayPlanePropertiesKHR_Access;

    type vkGetDisplayPlaneSupportedDisplaysKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Plane_Index: in Interfaces.Unsigned_32;
                        Display_Count: in out Interfaces.Unsigned_32;
                        Displays: access KHR.Display) return Result
        with Convention => C;

    vkGetDisplayPlaneSupportedDisplaysKHR:
        vkGetDisplayPlaneSupportedDisplaysKHR_Access;

    type vkGetDisplayModePropertiesKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Display: in KHR.Display;
                        Property_Count: in out Interfaces.Unsigned_32;
                        Properties: access KHR.Display_Mode_Properties)
        return Result
        with Convention => C;

    vkGetDisplayModePropertiesKHR: vkGetDisplayModePropertiesKHR_Access;

    type vkCreateDisplayModeKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Display: in KHR.Display;
                        Create_Info: in out C_KHR.Display_Mode_Create_Info_C;
                        Allocator: access constant Allocation_Callbacks;
                        Mode: out KHR.Display_Mode) return Result
        with Convention => C;

    vkCreateDisplayModeKHR: vkCreateDisplayModeKHR_Access;

    type vkGetDisplayPlaneCapabilitiesKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Mode: in KHR.Display_Mode;
                        Plane_Index: in Interfaces.Unsigned_32;
                        Capabilities: out KHR.Display_Plane_Capabilities)
        return Result
        with Convention => C;

    vkGetDisplayPlaneCapabilitiesKHR: vkGetDisplayPlaneCapabilitiesKHR_Access;

    type vkCreateDisplayPlaneSurfaceKHR_Access is
        access function(Instance: in Vulkan.Instance;
                        Create_Info: in C_KHR.Display_Surface_Create_Info_C;
                        Allocator: access constant Allocation_Callbacks;
                        Surface: out KHR.Surface) return Result
        with Convention => C;

    vkCreateDisplayPlaneSurfaceKHR: vkCreateDisplayPlaneSurfaceKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceDisplayPropertiesKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceDisplayPlanePropertiesKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetDisplayPlaneSupportedDisplaysKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetDisplayModePropertiesKHR_Access);
        procedure Load is new Load_Pointer(vkCreateDisplayModeKHR_Access);
        procedure Load is
            new Load_Pointer(vkGetDisplayPlaneCapabilitiesKHR_Access);
        procedure Load is
            new Load_Pointer(vkCreateDisplayPlaneSurfaceKHR_Access);
    begin
        Load(vkGetPhysicalDeviceDisplayPropertiesKHR,
             "vkGetPhysicalDeviceDisplayPropertiesKHR");
        Load(vkGetPhysicalDeviceDisplayPlanePropertiesKHR,
             "vkGetPhysicalDeviceDisplayPlanePropertiesKHR");
        Load(vkGetDisplayPlaneSupportedDisplaysKHR,
             "vkGetDisplayPlaneSupportedDisplaysKHR");
        Load(vkGetDisplayModePropertiesKHR, "vkGetDisplayModePropertiesKHR");
        Load(vkCreateDisplayModeKHR, "vkCreateDisplayModeKHR");
        Load(vkGetDisplayPlaneCapabilitiesKHR,
             "vkGetDisplayPlaneCapabilitiesKHR");
        Load(vkCreateDisplayPlaneSurfaceKHR, "vkCreateDisplayPlaneSurfaceKHR");
    end Load_Extension;

    function Get_Physical_Device_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out KHR.Display_Properties_Vectors.Vector)
        return Result is
        Count: Interfaces.Unsigned_32 := 0;
        Result: Vulkan.Result;
    begin
        Properties.Clear;
        Result := vkGetPhysicalDeviceDisplayPropertiesKHR(Physical_Device,
                                                          Count,
                                                          null);

        if Result /= Success then
            return Result;
        end if;
        
        declare
            C_Properties: array (1 .. Count) of
                aliased C_KHR.Display_Properties_C
                with Convention => C;
        begin
            Result := vkGetPhysicalDeviceDisplayPropertiesKHR
                (Physical_Device, Count, C_Properties(1)'Access);

            if Result /= Success then
                return Result;
            end if;

            Properties.Reserve_Capacity(Ada.Containers.Count_Type(Count));

            for C_Property of C_Properties loop
                Properties.Append(C_KHR.To_Ada(C_Property));
            end loop;
        end;

        return Success;
    end Get_Physical_Device_Properties;

    function Get_Physical_Device_Properties
        (Physical_Device: in Vulkan.Physical_Device)
        return KHR.Display_Properties_Vectors.Vector is
        Properties: KHR.Display_Properties_Vectors.Vector;
    begin
        Exceptions.Check(Get_Physical_Device_Properties(Physical_Device,
                                                        Properties));

        return Properties;
    end Get_Physical_Device_Properties;
 
    function Get_Physical_Device_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out KHR.Display_Plane_Properties_Vectors.Vector)
        return Result is
        Count: Interfaces.Unsigned_32 := 0;
        Result: Vulkan.Result;
    begin
        Properties.Clear;
        Result := vkGetPhysicalDeviceDisplayPlanePropertiesKHR(Physical_Device,
                                                               Count,
                                                               null);

        if Result /= Success then
            return Result;
        end if;

        declare
            Properties_Array: array (1 .. Count)
                of aliased KHR.Display_Plane_Properties;
        begin
            Result := vkGetPhysicalDeviceDisplayPlanePropertiesKHR
                (Physical_Device, Count, Properties_Array(1)'Access);

            if Result /= Success then
                return Result;
            end if;

            Properties.Reserve_Capacity(Ada.Containers.Count_Type(Count));

            for Prop of Properties_Array loop
                Properties.Append(Prop);
            end loop;
        end;
        
        return Success;
    end Get_Physical_Device_Display_Plane_Properties;

    function Get_Physical_Device_Display_Plane_Properties
        (Physical_Device: in Vulkan.Physical_Device)
        return KHR.Display_Plane_Properties_Vectors.Vector is
        Properties: KHR.Display_Plane_Properties_Vectors.Vector;
    begin
        Exceptions.Check
            (Get_Physical_Device_Display_Plane_Properties(Physical_Device,
                                                          Properties));

        return Properties;
    end Get_Physical_Device_Display_Plane_Properties;
   
    function Get_Display_Plane_Supported_Displays
        (Physical_Device: in Vulkan.Physical_Device;
         Plane_Index: in Interfaces.Unsigned_32;
         Displays: in out KHR.Display_Vectors.Vector) return Result is
        Count: Interfaces.Unsigned_32 := 0;
        Result: Vulkan.Result;
    begin
        Displays.Clear;
        Result := vkGetDisplayPlaneSupportedDisplaysKHR(Physical_Device,
                                                        Plane_Index,
                                                        Count,
                                                        null);

        if Result /= Success then
            return Result;
        end if;

        declare
            Displays_Array: array (1 .. Count) of aliased KHR.Display;
        begin
            Result :=
                vkGetDisplayPlaneSupportedDisplaysKHR(Physical_Device,
                                                      Plane_Index,
                                                      Count,
                                                      Displays_Array(1)'Access);

            if Result /= Success then
                return Result;
            end if;

            Displays.Reserve_Capacity(Ada.Containers.Count_Type(Count));

            for Display of Displays_Array loop
                Displays.Append(Display);
            end loop;
        end;

        return Success;
    end Get_Display_Plane_Supported_Displays;

    function Get_Display_Plane_Supported_Displays
        (Physical_Device: in Vulkan.Physical_Device;
         Plane_Index: in Interfaces.Unsigned_32)
        return KHR.Display_Vectors.Vector is
        Displays: KHR.Display_Vectors.Vector;
    begin
        Exceptions.Check(Get_Display_Plane_Supported_Displays(Physical_Device,
                                                              Plane_Index,
                                                              Displays));

        return Displays;
    end Get_Display_Plane_Supported_Displays;

    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in KHR.Display;
         Properties: in out KHR.Display_Mode_Properties_Vectors.Vector)
        return Result is
        Count: Interfaces.Unsigned_32 := 0;
        Result: Vulkan.Result;
    begin
        Properties.Clear;
        Result := vkGetDisplayModePropertiesKHR(Physical_Device,
                                                Display,
                                                Count,
                                                null);

        if Result /= Success then
            return Result;
        end if;

        declare
            Properties_Array:
                array (1 .. Count) of aliased KHR.Display_Mode_Properties;
        begin
            Result := vkGetDisplayModePropertiesKHR(Physical_Device,
                                                    Display,
                                                    Count,
                                                    Properties_Array(1)'Access);

            if Result /= Success then
                return Result;
            end if;

            Properties.Reserve_Capacity(Ada.Containers.Count_Type(Count));

            for Prop of Properties_Array loop
                Properties.Append(Prop);
            end loop;
        end;

        return Success;
    end Get_Display_Mode_Properties;

    function Get_Display_Mode_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Display: in KHR.Display)
        return KHR.Display_Mode_Properties_Vectors.Vector is
        Properties: KHR.Display_Mode_Properties_Vectors.Vector;
    begin
        Exceptions.Check(Get_Display_Mode_Properties(Physical_Device,
                                                     Display,
                                                     Properties));

        return Properties;
    end Get_Display_Mode_Properties;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Mode: out KHR.Display_Mode) return Result is
    begin
        return Create(Physical_Device,
                      Display,
                      Create_Info,
                      Allocator'Access,
                      Mode);
    end Create;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Display_Mode is
        Mode: KHR.Display_Mode;
    begin
        Exceptions.Check(Create(Physical_Device,
                                Display,
                                Create_Info,
                                Allocator'Access,
                                Mode));

        return Mode;
    end Create;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info;
                    Mode: out KHR.Display_Mode) return Result is
    begin
        return Create(Physical_Device, Display, Create_Info, null, Mode);
    end Create;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info)
        return KHR.Display_Mode is
        Mode: KHR.Display_Mode;
    begin
        Exceptions.Check(Create(Physical_Device,
                                Display,
                                Create_Info,
                                null,
                                Mode));

        return Mode;
    end Create;

    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Mode: in KHR.Display_Mode;
         Plane_Index: in Interfaces.Unsigned_32;
         Capabilities: out KHR.Display_Plane_Capabilities) return Result is
    begin
        return vkGetDisplayPlaneCapabilitiesKHR(Physical_Device,
                                                Mode,
                                                Plane_Index,
                                                Capabilities);
    end Get_Display_Plane_Capabilities;

    function Get_Display_Plane_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Mode: in KHR.Display_Mode;
         Plane_Index: in Interfaces.Unsigned_32)
        return KHR.Display_Plane_Capabilities is
        Capabilities: KHR.Display_Plane_Capabilities;
    begin
        Exceptions.Check(Get_Display_Plane_Capabilities(Physical_Device,
                                                        Mode,
                                                        Plane_Index,
                                                        Capabilities));

        return Capabilities;
    end Get_Display_Plane_Capabilities;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Surface: out KHR.Surface) return Result is
    begin
        return Create(Instance, Create_Info, Allocator'Access, Surface);
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return KHR.Surface is
        Surface: KHR.Surface;
    begin
        Exceptions.Check(Create(Instance,
                                Create_Info,
                                Allocator'Access,
                                Surface));

        return Surface;
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info;
                    Surface: out KHR.Surface) return Result is
    begin
        return Create(Instance, Create_Info, null, Surface);
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info)
        return KHR.Surface is
        Surface: KHR.Surface;
    begin
        Exceptions.Check(Create(Instance, Create_Info, null, Surface));

        return Surface;
    end Create;

    function Create(Physical_Device: in Vulkan.Physical_Device;
                    Display: in KHR.Display;
                    Create_Info: in KHR.Display_Mode_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Mode: out KHR.Display_Mode) return Result is
        Create_Info_C: C_KHR.Display_Mode_Create_Info_C :=
                        C_KHR.To_C(Create_Info);
        Result: Vulkan.Result;
    begin
        Result := vkCreateDisplayModeKHR(Physical_Device,
                                         Display,
                                         Create_Info_C,
                                         Allocator,
                                         Mode);
        C_KHR.Free(Create_Info_C);

        return Result;
    end Create;
    
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in KHR.Display_Surface_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Surface: out KHR.Surface) return Result is
        Create_Info_C: C_KHR.Display_Surface_Create_Info_C :=
            C_KHR.To_C(Create_Info);
        Result: Vulkan.Result;
    begin
        Result := vkCreateDisplayPlaneSurfaceKHR(Instance,
                                                 Create_Info_C,
                                                 Allocator,
                                                 Surface);
        C_KHR.Free(Create_Info_C);

        return Result;
    end Create;
end Vulkan.Extensions.KHR_Display;


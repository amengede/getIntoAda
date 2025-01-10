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

-- Operations for the display surface counter extension

with Vulkan.Core;
with Vulkan.C_EXT;
with Vulkan.Extension_Records;
with Vulkan.Exceptions;

package body Vulkan.Extensions.EXT_Display_Surface_Counter is
    -- Loaded extension functions.
    type vkGetPhysicalDeviceSurfaceCapabilities2EXT_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Surface: in KHR.Surface;
             Surface_Capabilities: in out C_EXT.Surface_Capabilities_2_C)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceSurfaceCapabilities2EXT:
        vkGetPhysicalDeviceSurfaceCapabilities2EXT_Access;

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
            (vkGetPhysicalDeviceSurfaceCapabilities2EXT_Access);
    begin
        Load(vkGetPhysicalDeviceSurfaceCapabilities2EXT,
             "vkGetPhysicalDeviceSurfaceCapabilities2EXT");
    end Load_Extension;

    function Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Capabilities: in out EXT.Surface_Capabilities_2)
        return Result is
        Capabilities_C: C_EXT.Surface_Capabilities_2_C;
        Result: Vulkan.Result;
    begin
        Capabilities_C.Next :=
            Extension_Records.To_C(Surface_Capabilities.Next);

        Result := vkGetPhysicalDeviceSurfaceCapabilities2EXT(Physical_Device,
                                                             Surface,
                                                             Capabilities_C);

        if Result = Success then
            C_EXT.To_Ada(Surface_Capabilities, Capabilities_C);
        end if;

        Extension_Records.Free(Capabilities_C.Next);

        return Result;
    end Get_Surface_Capabilities;

    procedure Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface;
         Surface_Capabilities: in out EXT.Surface_Capabilities_2) is
    begin
        Exceptions.Check(Get_Surface_Capabilities(Physical_Device,
                                                  Surface,
                                                  Surface_Capabilities));
    end Get_Surface_Capabilities;

    function Get_Surface_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Surface: in KHR.Surface) return EXT.Surface_Capabilities_2 is
        Capabilities: EXT.Surface_Capabilities_2;
    begin
        Exceptions.Check(Get_Surface_Capabilities(Physical_Device,
                                                  Surface,
                                                  Capabilities));

        return Capabilities;
    end Get_Surface_Capabilities;
end Vulkan.Extensions.EXT_Display_Surface_Counter;


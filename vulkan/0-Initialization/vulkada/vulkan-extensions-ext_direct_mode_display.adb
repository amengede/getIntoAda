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

-- Operations for the direct mode display extension

with Vulkan.Core;
with Vulkan.Exceptions;

package body Vulkan.Extensions.EXT_Direct_Mode_Display is
    -- Loaded extension functions.
    type vkReleaseDisplayEXT_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Display: in KHR.Display) return Result
        with Convention => C;

    vkReleaseDisplayEXT: vkReleaseDisplayEXT_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkReleaseDisplayEXT_Access);
    begin
        Load(vkReleaseDisplayEXT, "vkReleaseDisplayEXT");
    end Load_Extension;

    function Release(Physical_Device: in Vulkan.Physical_Device;
                     Display: in KHR.Display) return Result is
    begin
        return vkReleaseDisplayEXT(Physical_Device, Display);
    end Release;

    procedure Release(Physical_Device: in Vulkan.Physical_Device;
                      Display: in KHR.Display) is
    begin
        Exceptions.Check(Release(Physical_Device, Display));
    end Release;
end Vulkan.Extensions.EXT_Direct_Mode_Display;


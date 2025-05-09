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

-- Operations for the ray tracing maintenance 1 extension

with Vulkan.Core;

package body Vulkan.Extensions.KHR_Ray_Tracing_Maintenance_1 is
    -- Loaded extension functions.
    type vkCmdTraceRaysIndirect2KHR_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Indirect_Device_Address: in Device_Address)
        with Convention => C;

    vkCmdTraceRaysIndirect2KHR: vkCmdTraceRaysIndirect2KHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdTraceRaysIndirect2KHR_Access);
    begin
        Load(vkCmdTraceRaysIndirect2KHR, "vkCmdTraceRaysIndirect2KHR");
    end Load_Extension;

    procedure Trace_Rays_Indirect_2
        (Command_Buffer: in Vulkan.Command_Buffer;
         Indirect_Device_Address: in Device_Address) is
    begin
        vkCmdTraceRaysIndirect2KHR(Command_Buffer, Indirect_Device_Address);
    end Trace_Rays_Indirect_2;
end Vulkan.Extensions.KHR_Ray_Tracing_Maintenance_1;


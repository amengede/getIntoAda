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

-- Operations for the clip space W scaling extension

with Vulkan.Core;

package body Vulkan.Extensions.NV_Clip_Space_W_Scaling is
    -- Loaded extension functions.
    type vkCmdSetViewportWScalingNV_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             First_Viewport,
             Viewport_Count: in Interfaces.Unsigned_32;
             Viewport_Scalings: access constant NV.Viewport_W_Scaling)
        with Convention => C;

    vkCmdSetViewportWScalingNV: vkCmdSetViewportWScalingNV_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdSetViewportWScalingNV_Access);
    begin
        Load(vkCmdSetViewportWScalingNV, "vkCmdSetViewportWScalingNV");
    end Load_Extension;

    procedure Set_Viewport_W_Scaling
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Viewport: in Interfaces.Unsigned_32;
         Viewport_W_Scaling: in NV.Viewport_W_Scaling) is
        Scaling: aliased NV.Viewport_W_Scaling := Viewport_W_Scaling;
    begin
        vkCmdSetViewportWScalingNV(Command_Buffer,
                                   First_Viewport,
                                   1,
                                   Scaling'Access);
    end Set_Viewport_W_Scaling;

    procedure Set_Viewport_W_Scaling
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Viewport: in Interfaces.Unsigned_32;
         Viewport_W_Scalings: in NV.Viewport_W_Scaling_Vectors.Vector) is
        Scalings: array (1 .. Positive(Viewport_W_Scalings.Length))
                    of aliased NV.Viewport_W_Scaling;
    begin
        for X in Scalings'Range loop
            Scalings(X) := Viewport_W_Scalings(X);
        end loop;

        vkCmdSetViewportWScalingNV(Command_Buffer,
                                   First_Viewport,
                                   Scalings'Length,
                                   Scalings(1)'Access);
    end Set_Viewport_W_Scaling;
end Vulkan.Extensions.NV_Clip_Space_W_Scaling;


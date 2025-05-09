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

-- Fence related subprograms

with Vulkan.Exceptions;

package body Vulkan.Fences is
    function Reset(Device: in Vulkan.Device;
                   Fences: in Fence_Vectors.Vector) return Result is
        C_Fences: array (1 .. Positive(Fences.Length)) of aliased Fence
            with Convention => C;
    begin
        for X in C_Fences'Range loop
            C_Fences(X) := Fences(X);
        end loop;

        return C.vkResetFences(Device,
                               Interfaces.Unsigned_32(Fences.Length),
                               C_Fences(1)'Access);
    end Reset;

    procedure Reset(Device: in Vulkan.Device;
                    Fences: in Fence_Vectors.Vector) is
    begin
        Exceptions.Check(Reset(Device, Fences));
    end Reset;

    function Reset(Device: in Vulkan.Device;
                   Fence: in Vulkan.Fence) return Result is
        Local_Fence: aliased Vulkan.Fence := Fence;
    begin
        return C.vkResetFences(Device, 1, Local_Fence'Access);
    end Reset;

    procedure Reset(Device: in Vulkan.Device;
                    Fence: in Vulkan.Fence) is
    begin
        Exceptions.Check(Reset(Device, Fence));
    end Reset;

    function Wait(Device: in Vulkan.Device;
                  Fences: in Fence_Vectors.Vector;
                  Wait_All: in Boolean;
                  Timeout: in Interfaces.Unsigned_64) return Result is
        C_Fences: array (1 .. Positive(Fences.Length)) of aliased Fence
            with Convention => C;
    begin
        for X in C_Fences'Range loop
            C_Fences(X) := Fences(X);
        end loop;

        return C.vkWaitForFences(Device,
                                 Interfaces.Unsigned_32(Fences.Length),
                                 C_Fences(1)'Access,
                                 (if Wait_All then 1 else 0),
                                 Timeout);
    end Wait;

    function Wait(Device: in Vulkan.Device;
                  Fence: in Vulkan.Fence;
                  Timeout: in Interfaces.Unsigned_64) return Result is
        Local_Fence: aliased Vulkan.Fence := Fence;
    begin
        return C.vkWaitForFences(Device,
                                 1,
                                 Local_Fence'Access,
                                 1,
                                 Timeout);
    end Wait;
end Vulkan.Fences;


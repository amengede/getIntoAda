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

-- Render pass related subprograms

with Vulkan.Exceptions;

package body Vulkan.Render_Passes is
    -- Common Create_2 implementation.
    function Create_2(Device: in Vulkan.Device;
                      Create_Info: in Render_Pass_Create_Info_2;
                      Allocator: access constant Allocation_Callbacks;
                      Render_Pass: out Vulkan.Render_Pass) return Result;

    function Get_Granularity(Device: in Vulkan.Device;
                             Render_Pass: in Vulkan.Render_Pass)
        return Extent_2D is
        Granularity: Extent_2D;
    begin
        C.vkGetRenderAreaGranularity(Device, Render_Pass, Granularity);

        return Granularity;
    end Get_Granularity;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info_2;
                    Allocator: aliased in Allocation_Callbacks;
                    Render_Pass: out Vulkan.Render_Pass) return Result is
    begin
        return Create_2(Device, Create_Info, Allocator'Access, Render_Pass);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info_2;
                    Allocator: aliased in Allocation_Callbacks)
        return Render_Pass is
        RP: Render_Pass;
    begin
        Exceptions.Check(Create_2(Device, Create_Info, Allocator'Access, RP));

        return RP;
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info_2;
                    Render_Pass: out Vulkan.Render_Pass) return Result is
    begin
        return Create_2(Device, Create_Info, null, Render_Pass);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Render_Pass_Create_Info_2)
        return Render_Pass is
        RP: Render_Pass;
    begin
        Exceptions.Check(Create_2(Device, Create_Info, null, RP));

        return RP;
    end Create;

    function Create_2(Device: in Vulkan.Device;
                      Create_Info: in Render_Pass_Create_Info_2;
                      Allocator: access constant Allocation_Callbacks;
                      Render_Pass: out Vulkan.Render_Pass) return Result is
        Create_Info_C: C_V1_2.Render_Pass_Create_Info_2_C :=
            C_V1_2.To_C(Create_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_2.vkCreateRenderPass2(Device,
                                             Create_Info_C,
                                             Allocator,
                                             Render_Pass);
        C_V1_2.Free(Create_Info_C);

        return Result;
    end Create_2;
end Vulkan.Render_Passes;


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

-- Operations for the HDR metadata extension

with Vulkan.Core;
with Vulkan.C_EXT;

package body Vulkan.Extensions.EXT_HDR_Metadata is
    -- Loaded extension functions.
    type vkSetHdrMetadataEXT_Access is
        access procedure(Device: in Vulkan.Device;
                         Swapchain_Count: in Interfaces.Unsigned_32;
                         Swapchains: access constant KHR.Swapchain;
                         Metadata: access constant C_EXT.HDR_Metadata_C)
        with Convention => C;

    vkSetHdrMetadataEXT: vkSetHdrMetadataEXT_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkSetHdrMetadataEXT_Access);
    begin
        Load(vkSetHdrMetadataEXT, "vkSetHdrMetadataEXT");
    end Load_Extension;

    procedure Set_Metadata(Device: in Vulkan.Device;
                           Swapchains: in KHR.Swapchain_Vectors.Vector;
                           Metadata: in EXT.HDR_Metadata_Vectors.Vector) is
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Swapchains.Length);
        C_Swapchains: array (1 .. Positive(Count)) of aliased KHR.Swapchain;
        C_Metadata: array (1 .. Positive(Count)) of
                    aliased C_EXT.HDR_Metadata_C;
    begin
        for X in C_Swapchains'Range loop
            C_Swapchains(X) := Swapchains(X);
            C_Metadata(X) := C_EXT.To_C(Metadata(X));
        end loop;

        vkSetHdrMetadataEXT(Device,
                            Count,
                            C_Swapchains(1)'Access,
                            C_Metadata(1)'Access);

        for Metadata of C_Metadata loop
            C_EXT.Free(Metadata);
        end loop;
    end Set_Metadata;

    procedure Set_Metadata(Device: in Vulkan.Device;
                           Swapchain: in KHR.Swapchain;
                           Metadata: in EXT.HDR_Metadata) is
    begin
        Set_Metadata(Device,
                     KHR.Swapchain_Vectors.To_Vector(Swapchain, 1),
                     EXT.HDR_Metadata_Vectors.To_Vector(Metadata, 1));
    end Set_Metadata;
end Vulkan.Extensions.EXT_HDR_Metadata;


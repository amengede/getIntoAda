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

with Vulkan.Extensions.EXT;
with Vulkan.Extensions.KHR;

package Vulkan.Extensions.EXT_HDR_Metadata is
    use type Ada.Containers.Count_Type;
    use type KHR.Swapchain;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkSetHdrMetadataEXT
    procedure Set_Metadata(Device: in Vulkan.Device;
                           Swapchains: in KHR.Swapchain_Vectors.Vector;
                           Metadata: in EXT.HDR_Metadata_Vectors.Vector)
        with Pre => Device /= No_Device and
                    not Swapchains.Is_Empty and
                    Swapchains.Length = Metadata.Length and
                    (for all Swapchain of Swapchains =>
                        Swapchain /= KHR.No_Swapchain);

    procedure Set_Metadata(Device: in Vulkan.Device;
                           Swapchain: in KHR.Swapchain;
                           Metadata: in EXT.HDR_Metadata)
        with Inline,
             Pre => Device /= No_Device and Swapchain /= KHR.No_Swapchain;
end Vulkan.Extensions.EXT_HDR_Metadata;


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

-- Operations for the maintenance 5 extension

package Vulkan.Maintenance_5 is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdBindIndexBuffer2KHR
    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Buffer: in Vulkan.Buffer;
                   Offset, Size: in Device_Size;
                   Index_Type: in Vulkan.Index_Type)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    (if Buffer = No_Buffer then Offset = 0);

    -- vkGetRenderingAreaGranularityKHR
    function Get_Granularity(Device: in Vulkan.Device;
                             Rendering_Area_Info: in Vulkan.Rendering_Area_Info)
        return Extent_2D
        with Pre => Device /= No_Device;

    -- vkGetDeviceImageSubresourceLayoutKHR
    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Info: in Device_Image_Subresource_Info;
                                     Layout: in out Subresource_Layout_2)
        with Pre => Device /= No_Device;

    function Get_Subresource_Layout(Device: in Vulkan.Device;
                                    Info: in Device_Image_Subresource_Info)
        return Subresource_Layout_2
        with Inline,
             Pre => Device /= No_Device;

    -- vkGetImageSubresourceLayout2KHR
    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Image: in Vulkan.Image;
                                     Info: in Image_Subresource_2;
                                     Layout: in out Subresource_Layout_2)
        with Pre => Device /= No_Device and
                    Image /= No_Image;

    function Get_Subresource_Layout(Device: in Vulkan.Device;
                                    Image: in Vulkan.Image;
                                    Info: in Image_Subresource_2)
        return Subresource_Layout_2
        with Inline,
             Pre => Device /= No_Device and
                    Image /= No_Image;
end Vulkan.Maintenance_5;


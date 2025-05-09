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

-- Operations for the image view handle extension

with Vulkan.Extensions.NVX;

package Vulkan.Extensions.NVX_Image_View_Handle is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetImageViewHandleNVX
    function Get_Image_View_Handle(Device: in Vulkan.Device;
                                   Info: in NVX.Image_View_Handle_Info)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Device and
                    Info.Descriptor_Type in Sampled_Image |
                                            Storage_Image |
                                            Combined_Image_Sampler and
                    Info.Image_View /= No_Image_View and
                    (if Info.Descriptor_Type = Combined_Image_Sampler then
                        Info.Sampler /= No_Sampler);

    -- vkGetImageViewAddressNVX
    function Get_Image_View_Address
        (Device: in Vulkan.Device;
         Image_View: in Vulkan.Image_View;
         Properties: in out NVX.Image_View_Address_Properties) return Result
        with Pre => Device /= No_Device and Image_View /= No_Image_View,
             Post => Get_Image_View_Address'Result in Success |
                                                      Out_Of_Host_Memory;

    procedure Get_Image_View_Address
        (Device: in Vulkan.Device;
         Image_View: in Vulkan.Image_View;
         Properties: in out NVX.Image_View_Address_Properties)
        with Inline,
             Pre => Device /= No_Device and Image_View /= No_Image_View;

    function Get_Image_View_Address(Device: in Vulkan.Device;
                                    Image_View: in Vulkan.Image_View)
        return NVX.Image_View_Address_Properties
        with Pre => Device /= No_Device and Image_View /= No_Image_View;
end Vulkan.Extensions.NVX_Image_View_Handle;


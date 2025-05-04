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

-- Operations for the external memory capabilities extension

package Vulkan.External_Memory_Capabilities is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceExternalImageFormatPropertiesNV
    function Get_Physical_Device_External_Image_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Tiling: in Image_Tiling;
         Usage: in Image_Usage_Flags;
         Flags: in Image_Create_Flags;
         External_Handle_Type: in External_Memory_Handle_Type_Flags_NV;
         External_Image_Format_Properties:
            out External_Image_Format_Properties_NV) return Result
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Usage /= Image_Usage_No_Bit,
             Post =>
                Get_Physical_Device_External_Image_Format_Properties'Result in
                    Success |
                    Out_Of_Host_Memory |
                    Out_Of_Device_Memory |
                    Format_Not_Supported;

    procedure Get_Physical_Device_External_Image_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Tiling: in Image_Tiling;
         Usage: in Image_Usage_Flags;
         Flags: in Image_Create_Flags;
         External_Handle_Type: in External_Memory_Handle_Type_Flags_NV;
         External_Image_Format_Properties:
            out External_Image_Format_Properties_NV)
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Usage /= Image_Usage_No_Bit;

    function Get_Physical_Device_External_Image_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Tiling: in Image_Tiling;
         Usage: in Image_Usage_Flags;
         Flags: in Image_Create_Flags;
         External_Handle_Type: in External_Memory_Handle_Type_Flags_NV)
        return External_Image_Format_Properties_NV
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Usage /= Image_Usage_No_Bit;
end Vulkan.External_Memory_Capabilities;


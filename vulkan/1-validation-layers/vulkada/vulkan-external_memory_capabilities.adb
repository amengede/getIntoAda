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

with Vulkan.Core;
with Vulkan.Exceptions;

package body Vulkan.External_Memory_Capabilities is
    -- Loaded extension functions.
    type vkGetPhysicalDeviceExternalImageFormatPropertiesNV_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Format: in Vulkan.Format;
             Image_Type: in Vulkan.Image_Type;
             Tiling: in Image_Tiling;
             Usage: in Image_Usage_Flags;
             Flags: in Image_Create_Flags;
             External_Handle_Type: in External_Memory_Handle_Type_Flags_NV;
             External_Image_Format_Properties:
                out External_Image_Format_Properties_NV) return Result
        with Convention => C;

    vkGetPhysicalDeviceExternalImageFormatPropertiesNV:
        vkGetPhysicalDeviceExternalImageFormatPropertiesNV_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceExternalImageFormatPropertiesNV_Access);
    begin
        Load(vkGetPhysicalDeviceExternalImageFormatPropertiesNV,
             "vkGetPhysicalDeviceExternalImageFormatPropertiesNV");
    end Load_Extension;

    function Get_Physical_Device_External_Image_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Tiling: in Image_Tiling;
         Usage: in Image_Usage_Flags;
         Flags: in Image_Create_Flags;
         External_Handle_Type: in External_Memory_Handle_Type_Flags_NV;
         External_Image_Format_Properties:
            out External_Image_Format_Properties_NV) return Result is
    begin
        return vkGetPhysicalDeviceExternalImageFormatPropertiesNV
                (Physical_Device,
                 Format,
                 Image_Type,
                 Tiling,
                 Usage,
                 Flags,
                 External_Handle_Type,
                 External_Image_Format_Properties);
    end Get_Physical_Device_External_Image_Format_Properties;

    procedure Get_Physical_Device_External_Image_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Tiling: in Image_Tiling;
         Usage: in Image_Usage_Flags;
         Flags: in Image_Create_Flags;
         External_Handle_Type: in External_Memory_Handle_Type_Flags_NV;
         External_Image_Format_Properties:
            out External_Image_Format_Properties_NV) is
    begin
        Exceptions.Check(Get_Physical_Device_External_Image_Format_Properties
                            (Physical_Device,
                             Format,
                             Image_Type,
                             Tiling,
                             Usage,
                             Flags,
                             External_Handle_Type,
                             External_Image_Format_Properties));
    end Get_Physical_Device_External_Image_Format_Properties;

    function Get_Physical_Device_External_Image_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Tiling: in Image_Tiling;
         Usage: in Image_Usage_Flags;
         Flags: in Image_Create_Flags;
         External_Handle_Type: in External_Memory_Handle_Type_Flags_NV)
        return External_Image_Format_Properties_NV is
        Properties: External_Image_Format_Properties_NV;
    begin
        Exceptions.Check(Get_Physical_Device_External_Image_Format_Properties
                            (Physical_Device,
                             Format,
                             Image_Type,
                             Tiling,
                             Usage,
                             Flags,
                             External_Handle_Type,
                             Properties));

        return Properties;
    end Get_Physical_Device_External_Image_Format_Properties;
end Vulkan.External_Memory_Capabilities;


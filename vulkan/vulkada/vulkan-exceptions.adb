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

-- Various Vulkan errors expressed as exceptions

package body Vulkan.Exceptions is
    procedure Check(Result: in Vulkan.Result) is
    begin
        case Result is
            when Vulkan.Out_Of_Host_Memory =>
                raise Out_Of_Host_Memory;
            when Vulkan.Out_Of_Device_Memory =>
                raise Out_Of_Device_Memory;
            when Vulkan.Invalid_Opaque_Capture_Address =>
                raise Invalid_Opaque_Capture_Address;
            when Vulkan.Fragmentation =>
                raise Fragmentation;
            when Vulkan.Out_Of_Pool_Memory =>
                raise Out_Of_Pool_Memory;
            when Vulkan.Initialization_Failed =>
                raise Initialization_Failed;
            when Vulkan.Incompatible_Driver =>
                raise Incompatible_Driver;
            when Vulkan.Extension_Not_Present =>
                raise Extension_Not_Present;
            when Vulkan.Layer_Not_Present =>
                raise Layer_Not_Present;
            when Vulkan.Feature_Not_Present =>
                raise Feature_Not_Present;
            when Vulkan.Too_Many_Objects =>
                raise Too_Many_Objects;
            when Vulkan.Device_Lost =>
                raise Device_Lost;
            when Vulkan.Invalid_External_Handle =>
                raise Invalid_External_Handle;
            when Vulkan.Memory_Map_Failed =>
                raise Memory_Map_Failed;
            when Vulkan.Format_Not_Supported =>
                raise Format_Not_Supported;
            when Vulkan.Invalid_Shader =>
                raise Invalid_Shader;
            when Vulkan.Surface_Lost =>
                raise Surface_Lost;
            when Vulkan.Incompatible_Display =>
                raise Incompatible_Display;
            when Vulkan.Native_Window_In_Use =>
                raise Native_Window_In_Use;
            when Vulkan.Video_Profile_Operation_Not_Supported =>
                raise Video_Profile_Operation_Not_Supported;
            when Vulkan.Video_Profile_Format_Not_Supported =>
                raise Video_Profile_Format_Not_Supported;
            when Vulkan.Video_Picture_Layout_Not_Supported =>
                raise Video_Picture_Layout_Not_Supported;
            when Vulkan.Video_Profile_Codec_Not_Supported =>
                raise Video_Profile_Codec_Not_Supported;
            when Vulkan.Image_Usage_Not_Supported =>
                raise Image_Usage_Not_Supported;
            when Vulkan.Video_Std_Version_Not_Supported =>
                raise Video_Std_Version_Not_Supported;
            when Vulkan.Out_Of_Date =>
                raise Out_Of_Date;
            when Vulkan.Full_Screen_Exclusive_Mode_Lost =>
                raise Full_Screen_Exclusive_Mode_Lost;
            when others =>
                null;
        end case;
    end Check;
end Vulkan.Exceptions;


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

-- Various Vulkan errors expressed as exceptions

package Vulkan.Exceptions is
    -- Raise an exception from a Result if necessary.
    procedure Check(Result: in Vulkan.Result);

    -- Exception forms for errors.
    Out_Of_Host_Memory: exception;
    Out_Of_Device_Memory: exception;
    Invalid_Opaque_Capture_Address: exception;
    Fragmentation: exception;
    Out_Of_Pool_Memory: exception;
    Initialization_Failed: exception;
    Incompatible_Driver: exception;
    Extension_Not_Present: exception;
    Layer_Not_Present: exception;
    Feature_Not_Present: exception;
    Too_Many_Objects: exception;
    Device_Lost: exception;
    Invalid_External_Handle: exception;
    Memory_Map_Failed: exception;
    Format_Not_Supported: exception;
    Invalid_Shader: exception;
    Surface_Lost: exception;
    Incompatible_Display: exception;
    Native_Window_In_Use: exception;
    Video_Profile_Operation_Not_Supported: exception;
    Video_Profile_Format_Not_Supported: exception;
    Video_Picture_Layout_Not_Supported: exception;
    Video_Profile_Codec_Not_Supported: exception;
    Image_Usage_Not_Supported: exception;
    Video_Std_Version_Not_Supported: exception;
    Out_Of_Date: exception;
    Full_Screen_Exclusive_Mode_Lost: exception;
end Vulkan.Exceptions;


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

-- Operations for the video encode queue extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.KHR_Video_Encode_Queue is
    use type Interfaces.C.size_t;
    use type Interfaces.C.Extensions.void_ptr;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceVideoEncodeQualityLevelPropertiesKHR
    function Get_Physical_Device_Video_Encode_Quality_Level_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Quality_Level_Info:
            in KHR.Physical_Device_Video_Encode_Quality_Level_Info;
         Quality_Level_Properties:
            out KHR.Video_Encode_Quality_Level_Properties) return Result
        with Pre => Physical_Device /= No_Physical_Device,
             Post =>
                Get_Physical_Device_Video_Encode_Quality_Level_Properties'Result
                    in Success |
                       Out_Of_Host_Memory |
                       Out_Of_Device_Memory |
                       Video_Profile_Operation_Not_Supported |
                       Video_Profile_Format_Not_Supported |
                       Video_Picture_Layout_Not_Supported |
                       Video_Profile_Codec_Not_Supported;

    function Get_Physical_Device_Video_Encode_Quality_Level_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Quality_Level_Info:
            in KHR.Physical_Device_Video_Encode_Quality_Level_Info)
        return KHR.Video_Encode_Quality_Level_Properties
        with Pre => Physical_Device /= No_Physical_Device;

    -- vkGetEncodedVideoSessionParametersKHR
    function Encode_Video_Session_Parameters_Size
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info;
         Feedback_Info:
            in out KHR.Video_Encode_Session_Parameters_Feedback_Info)
        return Interfaces.C.size_t
        with Pre => Device /= No_Device;

    function Encode_Video_Session_Parameters_Size
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info)
        return Interfaces.C.size_t
        with Pre => Device /= No_Device;

    function Get_Encoded_Video_Session_Parameters
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info;
         Feedback_Info:
            in out KHR.Video_Encode_Session_Parameters_Feedback_Info)
        return Result
        with Pre => Device /= No_Device,
             Post => Get_Encoded_Video_Session_Parameters'Result in
                Success |
                Incomplete |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Encoded_Video_Session_Parameters
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info)
        return KHR.Video_Encode_Session_Parameters_Feedback_Info
        with Pre => Device /= No_Device;

    function Get_Encoded_Video_Session_Parameters
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info;
         Data_Size: in out Interfaces.C.size_t;
         Data: in Interfaces.C.Extensions.void_ptr) return Result
        with Pre => Device /= No_Device and
                    (Data_Size /= 0 or Data = System.Null_Address),
             Post => Get_Encoded_Video_Session_Parameters'Result in
                Success |
                Incomplete |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    function Get_Encoded_Video_Session_Parameters
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info;
         Feedback_Info:
            in out KHR.Video_Encode_Session_Parameters_Feedback_Info;
         Data_Size: in out Interfaces.C.size_t;
         Data: in Interfaces.C.Extensions.void_ptr) return Result
        with Pre => Device /= No_Device and
                    (Data_Size /= 0 or Data = System.Null_Address),
             Post => Get_Encoded_Video_Session_Parameters'Result in
                Success |
                Incomplete |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory;

    -- vkCmdEncodeVideoKHR
    procedure Encode_Video(Command_Buffer: in Vulkan.Command_Buffer;
                           Encode_Info: in KHR.Video_Encode_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;
end Vulkan.Extensions.KHR_Video_Encode_Queue;


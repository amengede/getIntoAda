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

with Vulkan.C_KHR;
with Vulkan.Core;
with Vulkan.Extension_Records;
with Vulkan.Exceptions;

package body Vulkan.Extensions.KHR_Video_Encode_Queue is
    -- Loaded extension functions.
    type vkGetPhysicalDeviceVideoEncodeQualityLevelPropertiesKHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Quality_Level_Info:
                in C_KHR.Physical_Device_Video_Encode_Quality_Level_Info_C;
             Quality_Level_Properties:
                out C_KHR.Video_Encode_Quality_Level_Properties_C)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceVideoEncodeQualityLevelPropertiesKHR:
        vkGetPhysicalDeviceVideoEncodeQualityLevelPropertiesKHR_Access;

    type vkGetEncodedVideoSessionParametersKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Video_Session_Parameters_Info:
                in C_KHR.Video_Encode_Session_Parameters_Get_Info_C;
             Feedback_Info:
                access C_KHR.Video_Encode_Session_Parameters_Feedback_Info_C;
             Data_Size: in out Interfaces.C.size_t;
             Data: in Interfaces.C.Extensions.void_ptr) return Result
        with Convention => C;

    vkGetEncodedVideoSessionParametersKHR:
        vkGetEncodedVideoSessionParametersKHR_Access;

    type vkCmdEncodeVideoKHR_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Encode_Info: in C_KHR.Video_Encode_Info_C)
        with Convention => C;

    vkCmdEncodeVideoKHR: vkCmdEncodeVideoKHR_Access;

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
            (vkGetPhysicalDeviceVideoEncodeQualityLevelPropertiesKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetEncodedVideoSessionParametersKHR_Access);
        procedure Load is new Load_Pointer(vkCmdEncodeVideoKHR_Access);
    begin
        Load(vkGetPhysicalDeviceVideoEncodeQualityLevelPropertiesKHR,
             "vkGetPhysicalDeviceVideoEncodeQualityLevelPropertiesKHR");
        Load(vkGetEncodedVideoSessionParametersKHR,
             "vkGetEncodedVideoSessionParametersKHR");
        Load(vkCmdEncodeVideoKHR, "vkCmdEncodeVideoKHR");
    end Load_Extension;

    function Get_Physical_Device_Video_Encode_Quality_Level_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Quality_Level_Info:
            in KHR.Physical_Device_Video_Encode_Quality_Level_Info;
         Quality_Level_Properties:
            out KHR.Video_Encode_Quality_Level_Properties) return Result is
        C_Info: C_KHR.Physical_Device_Video_Encode_Quality_Level_Info_C :=
            C_KHR.To_C(Quality_Level_Info);
        C_Properties: C_KHR.Video_Encode_Quality_Level_Properties_C;
        Result: Vulkan.Result;
    begin
        C_Properties.Next :=
            Extension_Records.To_C(Quality_Level_Properties.Next);
        Result := vkGetPhysicalDeviceVideoEncodeQualityLevelPropertiesKHR
            (Physical_Device, C_Info, C_Properties);
        C_KHR.Free(C_Info);

        if Result = Success then
            C_KHR.To_Ada(Quality_Level_Properties, C_Properties);
        end if;

        Extension_Records.Free(C_Properties.Next);

        return Result;
    end Get_Physical_Device_Video_Encode_Quality_Level_Properties;

    function Get_Physical_Device_Video_Encode_Quality_Level_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Quality_Level_Info:
            in KHR.Physical_Device_Video_Encode_Quality_Level_Info)
        return KHR.Video_Encode_Quality_Level_Properties is
        Properties: KHR.Video_Encode_Quality_Level_Properties;
    begin
        Exceptions.Check
            (Get_Physical_Device_Video_Encode_Quality_Level_Properties
                (Physical_Device, Quality_Level_Info, Properties));

        return Properties;
    end Get_Physical_Device_Video_Encode_Quality_Level_Properties;

    function Encode_Video_Session_Parameters_Size
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info;
         Feedback_Info:
            in out KHR.Video_Encode_Session_Parameters_Feedback_Info)
        return Interfaces.C.size_t is
        C_Info: C_KHR.Video_Encode_Session_Parameters_Get_Info_C :=
            C_KHR.To_C(Video_Session_Parameters_Info);
        C_Feedback:
            aliased C_KHR.Video_Encode_Session_Parameters_Feedback_Info_C;
        Count: Interfaces.C.size_t := 0;
    begin
        C_Feedback.Next := Extension_Records.To_C(Feedback_Info.Next);
        Exceptions.Check(vkGetEncodedVideoSessionParametersKHR
            (Device, C_Info, C_Feedback'Access, Count, System.Null_Address));
        C_KHR.Free(C_Info);
        C_KHR.To_Ada(Feedback_Info, C_Feedback);
        Extension_Records.Free(C_Feedback.Next);

        return Count;
    end Encode_Video_Session_Parameters_Size;

    function Encode_Video_Session_Parameters_Size
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info)
        return Interfaces.C.size_t is
        C_Info: C_KHR.Video_Encode_Session_Parameters_Get_Info_C :=
            C_KHR.To_C(Video_Session_Parameters_Info);
        Count: Interfaces.C.size_t := 0;
    begin
        Exceptions.Check(vkGetEncodedVideoSessionParametersKHR
            (Device, C_Info, null, Count, System.Null_Address));
        C_KHR.Free(C_Info);

        return Count;
    end Encode_Video_Session_Parameters_Size;

    function Get_Encoded_Video_Session_Parameters
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info;
         Feedback_Info:
            in out KHR.Video_Encode_Session_Parameters_Feedback_Info)
        return Result is
        C_Info: C_KHR.Video_Encode_Session_Parameters_Get_Info_C :=
            C_KHR.To_C(Video_Session_Parameters_Info);
        C_Feedback:
            aliased C_KHR.Video_Encode_Session_Parameters_Feedback_Info_C;
        Count: Interfaces.C.size_t := 0;
        Result: Vulkan.Result;
    begin
        C_Feedback.Next := Extension_Records.To_C(Feedback_Info.Next);
        Result := vkGetEncodedVideoSessionParametersKHR(Device,
                                                        C_Info,
                                                        C_Feedback'Access,
                                                        Count,
                                                        System.Null_Address);
        C_KHR.Free(C_Info);

        if Result = Success then
            C_KHR.To_Ada(Feedback_Info, C_Feedback);
        end if;

        Extension_Records.Free(C_Feedback.Next);

        return Result;
    end Get_Encoded_Video_Session_Parameters;

    function Get_Encoded_Video_Session_Parameters
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info)
        return KHR.Video_Encode_Session_Parameters_Feedback_Info is
        Feedback: KHR.Video_Encode_Session_Parameters_Feedback_Info;
    begin
        Exceptions.Check(Get_Encoded_Video_Session_Parameters
            (Device, Video_Session_Parameters_Info, Feedback));

        return Feedback;
    end Get_Encoded_Video_Session_Parameters;

    function Get_Encoded_Video_Session_Parameters
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info;
         Data_Size: in out Interfaces.C.size_t;
         Data: in Interfaces.C.Extensions.void_ptr) return Result is
        C_Info: C_KHR.Video_Encode_Session_Parameters_Get_Info_C :=
            C_KHR.To_C(Video_Session_Parameters_Info);
        Result: Vulkan.Result;
    begin
        Result := vkGetEncodedVideoSessionParametersKHR
            (Device, C_Info, null, Data_Size, Data);
        C_KHR.Free(C_Info);

        return Result;
    end Get_Encoded_Video_Session_Parameters;

    function Get_Encoded_Video_Session_Parameters
        (Device: in Vulkan.Device;
         Video_Session_Parameters_Info:
            in KHR.Video_Encode_Session_Parameters_Get_Info;
         Feedback_Info:
            in out KHR.Video_Encode_Session_Parameters_Feedback_Info;
         Data_Size: in out Interfaces.C.size_t;
         Data: in Interfaces.C.Extensions.void_ptr) return Result is
        C_Info: C_KHR.Video_Encode_Session_Parameters_Get_Info_C :=
            C_KHR.To_C(Video_Session_Parameters_Info);
        C_Feedback:
            aliased C_KHR.Video_Encode_Session_Parameters_Feedback_Info_C;
        Result: Vulkan.Result;
    begin
        C_Feedback.Next := Extension_Records.To_C(Feedback_Info.Next);
        Result := vkGetEncodedVideoSessionParametersKHR
            (Device, C_Info, C_Feedback'Access, Data_Size, Data);
        C_KHR.Free(C_Info);

        if Result = Success then
            C_KHR.To_Ada(Feedback_Info, C_Feedback);
        end if;

        Extension_Records.Free(C_Feedback.Next);

        return Result;
    end Get_Encoded_Video_Session_Parameters;

    procedure Encode_Video(Command_Buffer: in Vulkan.Command_Buffer;
                           Encode_Info: in KHR.Video_Encode_Info) is
        C_Info: C_KHR.Video_Encode_Info_C := C_KHR.To_C(Encode_Info);
    begin
        vkCmdEncodeVideoKHR(Command_Buffer, C_Info);
        C_KHR.Free(C_Info);
    end Encode_Video;
end Vulkan.Extensions.KHR_Video_Encode_Queue;


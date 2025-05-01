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

-- Operations for the video queues extension

package Vulkan.Video_Queues is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceVideoCapabilitiesKHR
    function Get_Physical_Device_Video_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Profile: in Video_Profile_Info;
         Capabilities: in out Video_Capabilities) return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Video_Profile.Chroma_Subsampling /=
                        Video_Chroma_Subsampling_Invalid_Bit and
                    Video_Profile.Luma_Bit_Depth /=
                        Video_Component_Bit_Depth_Invalid_Bit,
             Post => Get_Physical_Device_Video_Capabilities'Result in
                        Success |
                        Out_Of_Host_Memory |
                        Out_Of_Device_Memory |
                        Video_Profile_Operation_Not_Supported |
                        Video_Profile_Format_Not_Supported |
                        Video_Picture_Layout_Not_Supported |
                        Video_Profile_Codec_Not_Supported;

    function Get_Physical_Device_Video_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Profile: in Video_Profile_Info) return Video_Capabilities
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Video_Profile.Chroma_Subsampling /=
                        Video_Chroma_Subsampling_Invalid_Bit and
                    Video_Profile.Luma_Bit_Depth /=
                        Video_Component_Bit_Depth_Invalid_Bit;

    -- vkGetPhysicalDeviceVideoFormatPropertiesKHR
    function Physical_Device_Video_Format_Properties_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Format_Info: in Physical_Device_Video_Format_Info)
        return Interfaces.Unsigned_32
        with Pre => Physical_Device /= No_Physical_Device and
                    Video_Format_Info.Image_Usage /= Image_Usage_No_Bit;

    function Get_Physical_Device_Video_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Format_Info: in Physical_Device_Video_Format_Info;
         Video_Format_Properties: in out Video_Format_Properties_Vectors.Vector)
        return Result
        with Pre => Physical_Device /= No_Physical_Device and
                    Video_Format_Info.Image_Usage /= Image_Usage_No_Bit,
             Post => Get_Physical_Device_Video_Format_Properties'Result in
                Success |
                Incomplete |
                Out_Of_Host_Memory |
                Out_Of_Device_Memory |
                Image_Usage_Not_Supported |
                Video_Profile_Operation_Not_Supported |
                Video_Profile_Format_Not_Supported |
                Video_Picture_Layout_Not_Supported |
                Video_Profile_Codec_Not_Supported;

    function Get_Physical_Device_Video_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Format_Info: in Physical_Device_Video_Format_Info)
        return Video_Format_Properties_Vectors.Vector
        with Pre => Physical_Device /= No_Physical_Device and
                    Video_Format_Info.Image_Usage /= Image_Usage_No_Bit;

    -- vkCreateVideoSessionKHR
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Video_Session: out Vulkan.Video_Session) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Create_Info.Video_Profile /= null and
                    Create_Info.Std_Header_Version /= null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Video_Std_Version_Not_Supported and
                     (if Create'Result = Success then
                        Video_Session /= No_Video_Session);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Video_Session
        with Inline,
             Pre => Device /= No_Device and
                    Create_Info.Video_Profile /= null and
                    Create_Info.Std_Header_Version /= null,
             Post => Create'Result /= No_Video_Session;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Create_Info;
                    Video_Session: out Vulkan.Video_Session) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Create_Info.Video_Profile /= null and
                    Create_Info.Std_Header_Version /= null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Video_Std_Version_Not_Supported and
                     (if Create'Result = Success then
                        Video_Session /= No_Video_Session);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Create_Info)
        return Video_Session
        with Inline,
             Pre => Device /= No_Device and
                    Create_Info.Video_Profile /= null and
                    Create_Info.Std_Header_Version /= null,
             Post => Create'Result /= No_Video_Session;

    -- vkDestroyVideoSessionKHR
    procedure Destroy(Device: in Vulkan.Device;
                      Video_Session: in out Vulkan.Video_Session;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Video_Session = No_Video_Session;

    procedure Destroy(Device: in Vulkan.Device;
                      Video_Session: in out Vulkan.Video_Session)
        with Inline,
             Pre => Device /= No_Device,
             Post => Video_Session = No_Video_Session;

    -- vkGetVideoSessionMemoryRequirementsKHR
    function Video_Session_Memory_Requirements_Count
        (Device: in Vulkan.Device;
         Video_Session: in Vulkan.Video_Session) return Interfaces.Unsigned_32
        with Pre => Device /= No_Device and Video_Session /= No_Video_Session;

    function Get_Video_Session_Memory_Requirements
        (Device: in Vulkan.Device;
         Video_Session: in Vulkan.Video_Session;
         Requirements: in out Video_Session_Memory_Requirements_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and Video_Session /= No_Video_Session,
             Post => Get_Video_Session_Memory_Requirements'Result in
                Success |
                Incomplete;

    function Get_Video_Session_Memory_Requirements
        (Device: in Vulkan.Device;
         Video_Session: in Vulkan.Video_Session)
        return Video_Session_Memory_Requirements_Vectors.Vector
        with Pre => Device /= No_Device and Video_Session /= No_Video_Session;

    -- vkBindVideoSessionMemoryKHR
    function Bind(Device: in Vulkan.Device;
                  Video_Session: in Vulkan.Video_Session;
                  Bind_Session_Memory_Infos:
                    in Bind_Video_Session_Memory_Info_Vectors.Vector)
        return Result
        with Pre => Device /= No_Device and
                    Video_Session /= No_Video_Session and
                    not Bind_Session_Memory_Infos.Is_Empty and
                    (for all Info of Bind_Session_Memory_Infos =>
                        Info.Memory /= No_Device_Memory),
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory;

    procedure Bind(Device: in Vulkan.Device;
                   Video_Session: in Vulkan.Video_Session;
                   Bind_Session_Memory_Infos:
                    in Bind_Video_Session_Memory_Info_Vectors.Vector)
        with Inline,
             Pre => Device /= No_Device and
                    Video_Session /= No_Video_Session and
                    not Bind_Session_Memory_Infos.Is_Empty and
                    (for all Info of Bind_Session_Memory_Infos =>
                        Info.Memory /= No_Device_Memory);

    function Bind(Device: in Vulkan.Device;
                  Video_Session: in Vulkan.Video_Session;
                  Bind_Session_Memory_Info: in Bind_Video_Session_Memory_Info)
        return Result
        with Inline,
             Pre => Device /= No_Device and
                    Video_Session /= No_Video_Session and
                    Bind_Session_Memory_Info.Memory /= No_Device_Memory,
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory;

    procedure Bind(Device: in Vulkan.Device;
                   Video_Session: in Vulkan.Video_Session;
                   Bind_Session_Memory_Info: in Bind_Video_Session_Memory_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Video_Session /= No_Video_Session and
                    Bind_Session_Memory_Info.Memory /= No_Device_Memory;

    -- vkCreateVideoSessionParametersKHR
    function Create
        (Device: in Vulkan.Device;
         Create_Info: in Video_Session_Parameters_Create_Info;
         Allocator: aliased in Allocation_Callbacks;
         Video_Session_Parameters: out Vulkan.Video_Session_Parameters)
        return Result
        with Inline,
             Pre => Device /= No_Device and
                    Create_Info.Video_Session /= No_Video_Session,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed |
                                      Video_Std_Version_Not_Supported and
                     (if Create'Result = Success then
                        Video_Session_Parameters /=
                        No_Video_Session_Parameters);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Parameters_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Video_Session_Parameters
        with Inline,
             Pre => Device /= No_Device and
                    Create_Info.Video_Session /= No_Video_Session,
             Post => Create'Result /= No_Video_Session_Parameters;

    function Create
        (Device: in Vulkan.Device;
         Create_Info: in Video_Session_Parameters_Create_Info;
         Video_Session_Parameters: out Vulkan.Video_Session_Parameters)
        return Result
        with Inline,
             Pre => Device /= No_Device and
                    Create_Info.Video_Session /= No_Video_Session,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Initialization_Failed |
                                      Video_Std_Version_Not_Supported and
                     (if Create'Result = Success then
                        Video_Session_Parameters /=
                        No_Video_Session_Parameters);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Parameters_Create_Info)
        return Video_Session_Parameters
        with Inline,
             Pre => Device /= No_Device and
                    Create_Info.Video_Session /= No_Video_Session,
             Post => Create'Result /= No_Video_Session_Parameters;

    -- vkUpdateVideoSessionParametersKHR
    function Update
        (Device: in Vulkan.Device;
         Video_Session_Parameters: in Vulkan.Video_Session_Parameters;
         Update_Info: in Video_Session_Parameters_Update_Info) return Result
        with Pre => Device /= No_Device and
                    Video_Session_Parameters /= No_Video_Session_Parameters,
             Post => Update'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory;
    procedure Update
        (Device: in Vulkan.Device;
         Video_Session_Parameters: in Vulkan.Video_Session_Parameters;
         Update_Info: in Video_Session_Parameters_Update_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Video_Session_Parameters /= No_Video_Session_Parameters;

    -- vkDestroyVideoSessionParametersKHR
    procedure Destroy
        (Device: in Vulkan.Device;
         Video_Session_Parameters: in out Vulkan.Video_Session_Parameters;
         Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Video_Session_Parameters = No_Video_Session_Parameters;

    procedure Destroy
        (Device: in Vulkan.Device;
         Video_Session_Parameters: in out Vulkan.Video_Session_Parameters)
        with Inline,
             Pre => Device /= No_Device,
             Post => Video_Session_Parameters = No_Video_Session_Parameters;

    -- vkCmdBeginVideoCodingKHR
    procedure Begin_Video_Coding(Command_Buffer: in Vulkan.Command_Buffer;
                                 Begin_Info: in Video_Begin_Coding_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Begin_Info.Video_Session /= No_Video_Session;

    -- vkCmdEndVideoCodingKHR
    procedure End_Video_Coding(Command_Buffer: in Vulkan.Command_Buffer;
                               End_Coding_Info: in Video_End_Coding_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdControlVideoCodingKHR
    procedure Control_Video_Coding
        (Command_Buffer: in Vulkan.Command_Buffer;
         Coding_Control_Info: in Video_Coding_Control_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Coding_Control_Info.Flags /= Video_Coding_Control_No_Bit;
end Vulkan.Video_Queues;


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

-- Operations for the debug utils extension

package Vulkan.Debug_Utils is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkSetDebugUtilsObjectNameEXT
    function Set_Object_Name(Device: in Vulkan.Device;
                             Name_Info: in Debug_Utils_Object_Name_Info)
        return Result
        with Pre => Device /= No_Device and
                    Name_Info.Object_Type /= Unknown_Object_Type and
                    Name_Info.Object_Handle /= No_Object_Handle,
             Post => Set_Object_Name'Result in Success |
                                               Out_Of_Host_Memory |
                                               Out_Of_Device_Memory;

    procedure Set_Object_Name(Device: in Vulkan.Device;
                              Name_Info: in Debug_Utils_Object_Name_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Name_Info.Object_Type /= Unknown_Object_Type and
                    Name_Info.Object_Handle /= No_Object_Handle;

    -- vkSetDebugUtilsObjectTagEXT
    function Set_Object_Tag(Device: in Vulkan.Device;
                            Tag_Info: in Debug_Utils_Object_Tag_Info)
        return Result
        with Pre => Device /= No_Device,
             Post => Set_Object_Tag'Result in Success |
                                              Out_Of_Host_Memory |
                                              Out_Of_Device_Memory;

    procedure Set_Object_Tag(Device: in Vulkan.Device;
                             Tag_Info: in Debug_Utils_Object_Tag_Info)
        with Inline,
             Pre => Device /= No_Device;

    -- vkQueueBeginDebugUtilsLabelEXT
    procedure Queue_Begin_Label(Queue: in Vulkan.Queue;
                                Label_Info: in Debug_Utils_Label)
        with Pre => Queue /= No_Queue;

    -- vkQueueEndDebugUtilsLabelEXT
    procedure Queue_End_Label(Queue: in Vulkan.Queue)
        with Inline,
             Pre => Queue /= No_Queue;

    -- vkQueueInsertDebugUtilsLabelEXT
    procedure Queue_Insert_Label(Queue: in Vulkan.Queue;
                                 Label_Info: in Debug_Utils_Label)
        with Pre => Queue /= No_Queue;

    -- vkCmdBeginDebugUtilsLabelEXT
    procedure Begin_Label(Command_Buffer: in Vulkan.Command_Buffer;
                          Label_Info: in Debug_Utils_Label)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdEndDebugUtilsLabelEXT
    procedure End_Label(Command_Buffer: in Vulkan.Command_Buffer)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdInsertDebugUtilsLabelEXT
    procedure Insert_Label(Command_Buffer: in Vulkan.Command_Buffer;
                           Label_Info: in Debug_Utils_Label)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCreateDebugUtilsMessengerEXT
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Utils_Messenger_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Messenger: out Debug_Utils_Messenger) return Result
        with Pre => Instance /= No_Instance and
                    Create_Info.Message_Severity /=
                    Debug_Utils_Message_Severity_No_Bit and
                    Create_Info.Message_Type /=
                    Debug_Utils_Message_Type_No_Bit and
                    Create_Info.User_Callback /= null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory and
                     (if Create'Result = Success then
                        Messenger /= No_Debug_Utils_Messenger);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Utils_Messenger_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Debug_Utils_Messenger
        with Pre => Instance /= No_Instance and
                    Create_Info.Message_Severity /=
                    Debug_Utils_Message_Severity_No_Bit and
                    Create_Info.Message_Type /=
                    Debug_Utils_Message_Type_No_Bit and
                    Create_Info.User_Callback /= null,
             Post => Create'Result /= No_Debug_Utils_Messenger;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Utils_Messenger_Create_Info;
                    Messenger: out Debug_Utils_Messenger) return Result
        with Pre => Instance /= No_Instance and
                    Create_Info.Message_Severity /=
                    Debug_Utils_Message_Severity_No_Bit and
                    Create_Info.Message_Type /=
                    Debug_Utils_Message_Type_No_Bit and
                    Create_Info.User_Callback /= null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory and
                     (if Create'Result = Success then
                        Messenger /= No_Debug_Utils_Messenger);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Utils_Messenger_Create_Info)
        return Debug_Utils_Messenger
        with Pre => Instance /= No_Instance and
                    Create_Info.Message_Severity /=
                    Debug_Utils_Message_Severity_No_Bit and
                    Create_Info.Message_Type /=
                    Debug_Utils_Message_Type_No_Bit and
                    Create_Info.User_Callback /= null,
             Post => Create'Result /= No_Debug_Utils_Messenger;

    -- vkDestroyDebugUtilsMessengerEXT
    procedure Destroy(Instance: in Vulkan.Instance;
                      Messenger: in out Debug_Utils_Messenger;
                      Allocator: aliased in Allocation_Callbacks)
        with Pre => Instance /= No_Instance,
             Post => Messenger = No_Debug_Utils_Messenger;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Messenger: in out Debug_Utils_Messenger)
        with Pre => Instance /= No_Instance,
             Post => Messenger = No_Debug_Utils_Messenger;

    -- vkSubmitDebugUtilsMessageEXT
    procedure Submit_Message
        (Instance: in Vulkan.Instance;
         Message_Severity: in Debug_Utils_Message_Severity_Flags;
         Message_Types: in Debug_Utils_Message_Type_Flags;
         Callback_Data: in Debug_Utils_Messenger_Callback_Data)
        with Pre => Instance /= No_Instance and
                    Message_Types /= Debug_Utils_Message_Type_No_Bit and
                    (for all Object of Callback_Data.Objects =>
                        Object.Object_Type /= Unknown_Object_Type);
end Vulkan.Debug_Utils;


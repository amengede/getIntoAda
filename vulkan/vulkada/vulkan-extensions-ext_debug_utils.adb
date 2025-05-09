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

-- Operations for the debug utils extension

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Vulkan.Core;
with Vulkan.Exceptions;
with Vulkan.C_EXT;
with Vulkan.Utilities;
with Vulkan.C_Arrays;
with Vulkan.Callback_Marshallers;

package body Vulkan.Extensions.EXT_Debug_Utils is
    -- Loaded extension functions.
    type vkSetDebugUtilsObjectNameEXT_Access is
        access function
            (Device: in Vulkan.Device;
             Name_Info: in C_EXT.Debug_Utils_Object_Name_Info_C)
            return Result
        with Convention => C;

    vkSetDebugUtilsObjectNameEXT: vkSetDebugUtilsObjectNameEXT_Access;

    type vkSetDebugUtilsObjectTagEXT_Access is
        access function
            (Device: in Vulkan.Device;
             Tag_Info: in C_EXT.Debug_Utils_Object_Tag_Info_C)
            return Result
        with Convention => C;

    vkSetDebugUtilsObjectTagEXT: vkSetDebugUtilsObjectTagEXT_Access;

    type vkQueueBeginDebugUtilsLabelEXT_Access is
        access procedure(Queue: in Vulkan.Queue;
                         Label_Info: in C_EXT.Debug_Utils_Label_C)
        with Convention => C;

    vkQueueBeginDebugUtilsLabelEXT: vkQueueBeginDebugUtilsLabelEXT_Access;

    type vkQueueEndDebugUtilsLabelEXT_Access is
        access procedure(Queue: in Vulkan.Queue)
        with Convention => C;

    vkQueueEndDebugUtilsLabelEXT: vkQueueEndDebugUtilsLabelEXT_Access;

    type vkQueueInsertDebugUtilsLabelEXT_Access is
        access procedure(Queue: in Vulkan.Queue;
                         Label_Info: in C_EXT.Debug_Utils_Label_C)
        with Convention => C;

    vkQueueInsertDebugUtilsLabelEXT: vkQueueInsertDebugUtilsLabelEXT_Access;

    type vkCmdBeginDebugUtilsLabelEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Label_Info: in C_EXT.Debug_Utils_Label_C)
        with Convention => C;

    vkCmdBeginDebugUtilsLabelEXT: vkCmdBeginDebugUtilsLabelEXT_Access;

    type vkCmdEndDebugUtilsLabelEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer)
        with Convention => C;

    vkCmdEndDebugUtilsLabelEXT: vkCmdEndDebugUtilsLabelEXT_Access;

    type vkCmdInsertDebugUtilsLabelEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Label_Info: in C_EXT.Debug_Utils_Label_C)
        with Convention => C;

    vkCmdInsertDebugUtilsLabelEXT: vkCmdInsertDebugUtilsLabelEXT_Access;

    type vkCreateDebugUtilsMessengerEXT_Access is
        access function
            (Instance: in Vulkan.Instance;
             Create_Info: in C_EXT.Debug_Utils_Messenger_Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Messenger: in out EXT.Debug_Utils_Messenger) return Result
        with Convention => C;

    vkCreateDebugUtilsMessengerEXT: vkCreateDebugUtilsMessengerEXT_Access;

    type vkDestroyDebugUtilsMessengerEXT_Access is
        access procedure(Instance: in Vulkan.Instance;
                         Messenger: in EXT.Debug_Utils_Messenger;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroyDebugUtilsMessengerEXT: vkDestroyDebugUtilsMessengerEXT_Access;

    type vkSubmitDebugUtilsMessageEXT_Access is
        access procedure
            (Instance: in Vulkan.Instance;
             Message_Severity: in EXT.Debug_Utils_Message_Severity_Flags;
             Message_Types: in EXT.Debug_Utils_Message_Type_Flags;
             Callback_Data: in C_EXT.Debug_Utils_Messenger_Callback_Data_C)
        with Convention => C;

    vkSubmitDebugUtilsMessageEXT: vkSubmitDebugUtilsMessageEXT_Access;

    -- Storage for the current marshallers.
    package Debug_Marshallers is
        new Callback_Marshallers(EXT.Debug_Utils_Messenger,
                                 Interfaces.C.Extensions.void_ptr,
                                 EXT.Debug_Messenger_Callback);

    Marshallers: Debug_Marshallers.Marshallers;

    -- Common creation logic.
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Debug_Utils_Messenger_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Messenger: out EXT.Debug_Utils_Messenger) return Result;

    -- Proxy dispatcher.
    function Callback_Proxy
        (Message_Severity: in EXT.Debug_Utils_Message_Severity_Flags;
         Message_Types: in EXT.Debug_Utils_Message_Type_Flags;
         Callback_Data: in C_EXT.Debug_Utils_Messenger_Callback_Data_C;
         User_Data: in Interfaces.C.Extensions.void_ptr)
        return Interfaces.Unsigned_32
        with Convention => C;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is
            new Load_Pointer(vkCreateDebugUtilsMessengerEXT_Access);
        procedure Load is
            new Load_Pointer(vkDestroyDebugUtilsMessengerEXT_Access);
        procedure Load is
            new Load_Pointer(vkSetDebugUtilsObjectNameEXT_Access);
        procedure Load is
            new Load_Pointer(vkSetDebugUtilsObjectTagEXT_Access);
        procedure Load is
            new Load_Pointer(vkQueueBeginDebugUtilsLabelEXT_Access);
        procedure Load is
            new Load_Pointer(vkQueueEndDebugUtilsLabelEXT_Access);
        procedure Load is
            new Load_Pointer(vkQueueInsertDebugUtilsLabelEXT_Access);
        procedure Load is
            new Load_Pointer(vkCmdBeginDebugUtilsLabelEXT_Access);
        procedure Load is
            new Load_Pointer(vkCmdEndDebugUtilsLabelEXT_Access);
        procedure Load is
            new Load_Pointer(vkCmdInsertDebugUtilsLabelEXT_Access);
        procedure Load is
            new Load_Pointer(vkSubmitDebugUtilsMessageEXT_Access);
    begin
        Load(vkSetDebugUtilsObjectNameEXT, "vkSetDebugUtilsObjectNameEXT");
        Load(vkSetDebugUtilsObjectTagEXT, "vkSetDebugUtilsObjectTagEXT");
        Load(vkQueueBeginDebugUtilsLabelEXT, "vkQueueBeginDebugUtilsLabelEXT");
        Load(vkQueueEndDebugUtilsLabelEXT, "vkQueueEndDebugUtilsLabelEXT");
        Load(vkQueueInsertDebugUtilsLabelEXT,
             "vkQueueInsertDebugUtilsLabelEXT");
        Load(vkCmdBeginDebugUtilsLabelEXT, "vkCmdBeginDebugUtilsLabelEXT");
        Load(vkCmdEndDebugUtilsLabelEXT, "vkCmdEndDebugUtilsLabelEXT");
        Load(vkCmdInsertDebugUtilsLabelEXT, "vkCmdInsertDebugUtilsLabelEXT");
        Load(vkCreateDebugUtilsMessengerEXT, "vkCreateDebugUtilsMessengerEXT");
        Load(vkDestroyDebugUtilsMessengerEXT,
             "vkDestroyDebugUtilsMessengerEXT");
        Load(vkSubmitDebugUtilsMessageEXT, "vkSubmitDebugUtilsMessageEXT");
    end Load_Extension;

    function Set_Object_Name(Device: in Vulkan.Device;
                             Name_Info: in EXT.Debug_Utils_Object_Name_Info)
        return Result is
        Name_Info_C: C_EXT.Debug_Utils_Object_Name_Info_C :=
            C_EXT.To_C(Name_Info);
        Result: Vulkan.Result;
    begin
        Result := vkSetDebugUtilsObjectNameEXT(Device, Name_Info_C);
        C_EXT.Free(Name_Info_C);

        return Result;
    end Set_Object_Name;

    procedure Set_Object_Name(Device: in Vulkan.Device;
                              Name_Info: in EXT.Debug_Utils_Object_Name_Info) is
    begin
        Exceptions.Check(Set_Object_Name(Device, Name_Info));
    end Set_Object_Name;

    function Set_Object_Tag(Device: in Vulkan.Device;
                            Tag_Info: in EXT.Debug_Utils_Object_Tag_Info)
        return Result is
        Tag_Info_C: C_EXT.Debug_Utils_Object_Tag_Info_C := C_EXT.To_C(Tag_Info);
        Result: Vulkan.Result;
    begin
        Result := vkSetDebugUtilsObjectTagEXT(Device, Tag_Info_C);
        C_EXT.Free(Tag_Info_C);

        return Result;
    end Set_Object_Tag;

    procedure Set_Object_Tag(Device: in Vulkan.Device;
                             Tag_Info: in EXT.Debug_Utils_Object_Tag_Info) is
    begin
        Exceptions.Check(Set_Object_Tag(Device, Tag_Info));
    end Set_Object_Tag;

    procedure Queue_Begin_Label(Queue: in Vulkan.Queue;
                                Label_Info: in EXT.Debug_Utils_Label) is
        Label_Info_C: C_EXT.Debug_Utils_Label_C := C_EXT.To_C(Label_Info);
    begin
        vkQueueBeginDebugUtilsLabelEXT(Queue, Label_Info_C);
        C_EXT.Free(Label_Info_C);
    end Queue_Begin_Label;

    procedure Queue_End_Label(Queue: in Vulkan.Queue) is
    begin
        vkQueueEndDebugUtilsLabelEXT(Queue);
    end Queue_End_Label;

    procedure Queue_Insert_Label(Queue: in Vulkan.Queue;
                                 Label_Info: in EXT.Debug_Utils_Label) is
        Label_Info_C: C_EXT.Debug_Utils_Label_C := C_EXT.To_C(Label_Info);
    begin
        vkQueueInsertDebugUtilsLabelEXT(Queue, Label_Info_C);
        C_EXT.Free(Label_Info_C);
    end Queue_Insert_Label;

    procedure Begin_Label(Command_Buffer: in Vulkan.Command_Buffer;
                          Label_Info: in EXT.Debug_Utils_Label) is
        Label_Info_C: C_EXT.Debug_Utils_Label_C := C_EXT.To_C(Label_Info);
    begin
        vkCmdBeginDebugUtilsLabelEXT(Command_Buffer, Label_Info_C);
        C_EXT.Free(Label_Info_C);
    end Begin_Label;

    procedure End_Label(Command_Buffer: in Vulkan.Command_Buffer) is
    begin
        vkCmdEndDebugUtilsLabelEXT(Command_Buffer);
    end End_Label;

    procedure Insert_Label(Command_Buffer: in Vulkan.Command_Buffer;
                          Label_Info: in EXT.Debug_Utils_Label) is
        Label_Info_C: C_EXT.Debug_Utils_Label_C := C_EXT.To_C(Label_Info);
    begin
        vkCmdInsertDebugUtilsLabelEXT(Command_Buffer, Label_Info_C);
        C_EXT.Free(Label_Info_C);
    end Insert_Label;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Debug_Utils_Messenger_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Messenger: out EXT.Debug_Utils_Messenger) return Result is
    begin
        return Create(Instance, Create_Info, Allocator'Access, Messenger);
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Debug_Utils_Messenger_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return EXT.Debug_Utils_Messenger is
        Messenger: EXT.Debug_Utils_Messenger;
    begin
        Exceptions.Check(Create(Instance,
                                Create_Info,
                                Allocator'Access,
                                Messenger));

        return Messenger;
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Debug_Utils_Messenger_Create_Info;
                    Messenger: out EXT.Debug_Utils_Messenger) return Result is
    begin
        return Create(Instance, Create_Info, null, Messenger);
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Debug_Utils_Messenger_Create_Info)
        return EXT.Debug_Utils_Messenger is
        Messenger: EXT.Debug_Utils_Messenger;
    begin
        Exceptions.Check(Create(Instance, Create_Info, null, Messenger));

        return Messenger;
    end Create;
    
    procedure Destroy(Instance: in Vulkan.Instance;
                      Messenger: in out EXT.Debug_Utils_Messenger;
                      Allocator: aliased in Allocation_Callbacks) is
    begin
        if Messenger /= EXT.No_Debug_Utils_Messenger then
            Marshallers.Remove(Messenger);
            vkDestroyDebugUtilsMessengerEXT(Instance,
                                            Messenger,
                                            Allocator'Access);
            Messenger := EXT.No_Debug_Utils_Messenger;
        end if;
    end Destroy;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Messenger: in out EXT.Debug_Utils_Messenger) is
    begin
        if Messenger /= EXT.No_Debug_Utils_Messenger then
            Marshallers.Remove(Messenger);
            vkDestroyDebugUtilsMessengerEXT(Instance, Messenger, null);
            Messenger := EXT.No_Debug_Utils_Messenger;
        end if;
    end Destroy;

    procedure Submit_Message
        (Instance: in Vulkan.Instance;
         Message_Severity: in EXT.Debug_Utils_Message_Severity_Flags;
         Message_Types: in EXT.Debug_Utils_Message_Type_Flags;
         Callback_Data: in EXT.Debug_Utils_Messenger_Callback_Data) is
        Callback_Data_C: C_EXT.Debug_Utils_Messenger_Callback_Data_C :=
            C_EXT.To_C(Callback_Data);
    begin
        vkSubmitDebugUtilsMessageEXT(Instance,
                                     Message_Severity,
                                     Message_Types,
                                     Callback_Data_C);
        C_EXT.Free(Callback_Data_C);
    end Submit_Message;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in EXT.Debug_Utils_Messenger_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Messenger: out EXT.Debug_Utils_Messenger) return Result is
        Create_Info_C: C_EXT.Debug_Utils_Messenger_Create_Info_C :=
            C_EXT.To_C(Create_Info);
        Result: Vulkan.Result;
        Marshaller: Debug_Marshallers.Marshaller_Access :=
            new Debug_Marshallers.Marshaller;
    begin
        Marshaller.Data := Create_Info.User_Data;
        Marshaller.Callback := Create_Info.User_Callback;
        Create_Info_C.User_Callback := Callback_Proxy'Access;
        Create_Info_C.User_Data := Marshaller.all'Address;

        Result := vkCreateDebugUtilsMessengerEXT(Instance,
                                                 Create_Info_C,
                                                 Allocator,
                                                 Messenger);
        C_EXT.Free(Create_Info_C);

        if Result = Success then
            Marshallers.Register(Messenger, Marshaller);
        else
            Debug_Marshallers.Free(Marshaller);
        end if;

        return Result;
    end Create;

    function Callback_Proxy
        (Message_Severity: in EXT.Debug_Utils_Message_Severity_Flags;
         Message_Types: in EXT.Debug_Utils_Message_Type_Flags;
         Callback_Data: in C_EXT.Debug_Utils_Messenger_Callback_Data_C;
         User_Data: in Interfaces.C.Extensions.void_ptr)
        return Interfaces.Unsigned_32 is
        use type Interfaces.Unsigned_32;

        package Debug_Utils_Label_C_Pointers is
            new Interfaces.C.Pointers
                (Positive,
                 C_EXT.Debug_Utils_Label_C,
                 C_EXT.Debug_Utils_Label_C_Arrays.Element_Array,
                 (Next => null,
                  Label_Name => Interfaces.C.Strings.Null_Ptr,
                  Color => (others => 0.0),
                  others => <>));

        package Debug_Utils_Object_Name_Info_C_Pointers is
            new Interfaces.C.Pointers
            (Positive,
             C_EXT.Debug_Utils_Object_Name_Info_C,
             C_EXT.Debug_Utils_Object_Name_Info_C_Arrays.Element_Array,
             (Next => null,
              Object_Type => Unknown_Object_Type,
              Object_Handle => No_Object_Handle,
              Object_Name => Interfaces.C.Strings.Null_Ptr,
              others => <>));

        function To_Ada(Label: in C_EXT.Debug_Utils_Label_C)
            return EXT.Debug_Utils_Label is
            Label_Ada: EXT.Debug_Utils_Label;
        begin
            Label_Ada.Label_Name :=
                Utilities.To_Unbounded_String(Label.Label_Name);
            Label_Ada.Color := Label.Color;

            return Label_Ada;
        end To_Ada;

        function To_Ada(Name_Info: in C_EXT.Debug_Utils_Object_Name_Info_C)
            return EXT.Debug_Utils_Object_Name_Info is
            Name_Info_Ada: EXT.Debug_Utils_Object_Name_Info;
        begin
            Name_Info_Ada.Object_Type := Name_Info.Object_Type;
            Name_Info_Ada.Object_Handle := Name_Info.Object_Handle;
            Name_Info_Ada.Object_Name := Utilities.To_Unbounded_String
                (Name_Info.Object_Name);

            return Name_Info_Ada;
        end To_Ada;

        function To_Pointer is new Ada.Unchecked_Conversion
            (C_EXT.Debug_Utils_Label_C_Arrays.Pointer,
             Debug_Utils_Label_C_Pointers.Pointer);

        function To_Pointer is new Ada.Unchecked_Conversion
            (C_EXT.Debug_Utils_Object_Name_Info_C_Arrays.Pointer,
             Debug_Utils_Object_Name_Info_C_Pointers.Pointer);

        Marshaller: Debug_Marshallers.Marshaller_Access :=
            Debug_Marshallers.To_Marshaller(User_Data);
        Callback_Data_Ada: EXT.Debug_Utils_Messenger_Callback_Data;
    begin
        Callback_Data_Ada.Flags := Callback_Data.Flags;
        Callback_Data_Ada.Message_ID_Name := Utilities.To_Unbounded_String
            (Callback_Data.Message_ID_Name);
        Callback_Data_Ada.Message_ID_Number := Callback_Data.Message_ID_Number;
        Callback_Data_Ada.Message := Utilities.To_Unbounded_String
            (Callback_Data.Message);

        if Callback_Data.Queue_Label_Count /= 0 then
            declare
                Label: Debug_Utils_Label_C_Pointers.Pointer :=
                    To_Pointer(Callback_Data.Queue_Labels);
            begin
                for X in 1 .. Callback_Data.Queue_Label_Count loop
                    Callback_Data_Ada.Queue_Labels.Append(To_Ada(Label.all));
                    Debug_Utils_Label_C_Pointers.Increment(Label);
                end loop;
            end;
        end if;

        if Callback_Data.Cmd_Buf_Label_Count /= 0 then
            declare
                Label: Debug_Utils_Label_C_Pointers.Pointer :=
                    To_Pointer(Callback_Data.Cmd_Buf_Labels);
            begin
                for X in 1 .. Callback_Data.Cmd_Buf_Label_Count loop
                    Callback_Data_Ada.Cmd_Buf_Labels.Append(To_Ada(Label.all));
                    Debug_Utils_Label_C_Pointers.Increment(Label);
                end loop;
            end;
        end if;

        if Callback_Data.Object_Count /= 0 then
            declare
                Name_Info: Debug_Utils_Object_Name_Info_C_Pointers.Pointer :=
                    To_Pointer(Callback_Data.Objects);
            begin
                for X in 1 .. Callback_Data.Object_Count loop
                    Callback_Data_Ada.Objects.Append(To_Ada(Name_Info.all));
                    Debug_Utils_Object_Name_Info_C_Pointers.Increment
                        (Name_Info);
                end loop;
            end;
        end if;

        return Utilities.To_C(Marshaller.Callback(Message_Severity,
                                                  Message_Types,
                                                  Callback_Data_Ada,
                                                  Marshaller.Data));
    end Callback_Proxy;
end Vulkan.Extensions.EXT_Debug_Utils;


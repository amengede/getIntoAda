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

-- Operations for the debug reports extension

with Interfaces.C.Strings;
with Vulkan.Core;
with Vulkan.Debug_Reports_C;
with Vulkan.Callback_Marshallers;
with Vulkan.Utilities;
with Vulkan.Exceptions;

package body Vulkan.Debug_Reports is 
    -- Loaded extension functions.
    type vkCreateDebugReportCallbackEXT_Access is
        access function
            (Instance: in Vulkan.Instance;
             Create_Info: in
                Debug_Reports_C.Debug_Report_Callback_Create_Info_C;
             Allocation: access constant Allocation_Callbacks;
             Callback: out Debug_Report_Callback) return Result
        with Convention => C;

    vkCreateDebugReportCallbackEXT: vkCreateDebugReportCallbackEXT_Access;

    type vkDestroyDebugReportCallbackEXT_Access is
        access procedure(Instance: in Vulkan.Instance;
                         Callback: in Debug_Report_Callback;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroyDebugReportCallbackEXT: vkDestroyDebugReportCallbackEXT_Access;

    type vkDebugReportMessageEXT_Access is
        access procedure(Instance: in Vulkan.Instance;
                         Flags: in Debug_Report_Flags;
                         Object_Type: in Debug_Report_Object_Type;
                         Object: in Object_Handle;
                         Location: in Interfaces.C.size_t;
                         Message_Code: in Interfaces.Integer_32;
                         Layer_Prefix,
                         Message: in Interfaces.C.Strings.chars_ptr)
        with Convention => C;

    vkDebugReportMessageEXT: vkDebugReportMessageEXT_Access;

    -- Storage for the current marshallers.
    package Debug_Marshallers is
        new Callback_Marshallers(Debug_Report_Callback,
                                 Interfaces.C.Extensions.void_ptr,
                                 Debug_Report_Callback_Function);

    Marshallers: Debug_Marshallers.Marshallers;

    -- Common creation logic.
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Callback: out Debug_Report_Callback) return Result;

    -- Proxy dispatcher.
    function Callback_Proxy
        (Flags: in Debug_Report_Flags;
         Object_Type: in Debug_Report_Object_Type;
         Object: in Object_Handle;
         Location: in Interfaces.C.size_t;
         Message_Code: in Interfaces.Integer_32;
         Layer_Prefix, Message: in Interfaces.C.Strings.chars_ptr;
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
            new Load_Pointer(vkCreateDebugReportCallbackEXT_Access);
        procedure Load is
            new Load_Pointer(vkDestroyDebugReportCallbackEXT_Access);
        procedure Load is new Load_Pointer(vkDebugReportMessageEXT_Access);
    begin
        Load(vkCreateDebugReportCallbackEXT, "vkCreateDebugReportCallbackEXT");
        Load(vkDestroyDebugReportCallbackEXT,
             "vkDestroyDebugReportCallbackEXT");
        Load(vkDebugReportMessageEXT, "vkDebugReportMessageEXT");
    end Load_Extension;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Callback: out Debug_Report_Callback) return Result is
    begin
        return Create(Instance, Create_Info, Allocator'Access, Callback);
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Debug_Report_Callback is
        Callback: Debug_Report_Callback;
    begin
        Exceptions.Check(Create(Instance,
                                Create_Info,
                                Allocator'Access,
                                Callback));

        return Callback;
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info;
                    Callback: out Debug_Report_Callback) return Result is
    begin
        return Create(Instance, Create_Info, null, Callback);
    end Create;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info)
        return Debug_Report_Callback is
        Callback: Debug_Report_Callback;
    begin
        Exceptions.Check(Create(Instance, Create_Info, null, Callback));

        return Callback;
    end Create;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Callback: in out Debug_Report_Callback;
                      Allocator: aliased in Allocation_Callbacks) is
    begin
        vkDestroyDebugReportCallbackEXT(Instance, Callback, Allocator'Access);
    end Destroy;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Callback: in out Debug_Report_Callback) is
    begin
        vkDestroyDebugReportCallbackEXT(Instance, Callback, null);
    end Destroy;

    procedure Message(Instance: in Vulkan.Instance;
                      Flags: in Debug_Report_Flags;
                      Object_Type: in Debug_Report_Object_Type := Debug_Unknown;
                      Object: in Object_Handle := No_Object_Handle;
                      Location: in Interfaces.C.size_t := 0;
                      Message_Code: in Interfaces.Integer_32 := 0;
                      Layer_Prefix, Message: in String := "") is
        Prefix_C: aliased Interfaces.C.char_array :=
            Interfaces.C.To_C(Layer_Prefix);
        Message_C: aliased Interfaces.C.char_array :=
            Interfaces.C.To_C(Message);
    begin
        vkDebugReportMessageEXT
            (Instance,
             Flags,
             Object_Type,
             Object,
             Location,
             Message_Code,
             Interfaces.C.Strings.To_Chars_Ptr(Prefix_C'Unchecked_Access),
             Interfaces.C.Strings.To_Chars_Ptr(Message_C'Unchecked_Access));
    end Message;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Callback: out Debug_Report_Callback) return Result is
        Info_C: Debug_Reports_C.Debug_Report_Callback_Create_Info_C :=
            Debug_Reports_C.To_C(Create_Info);
        Result: Vulkan.Result;
        Marshaller: Debug_Marshallers.Marshaller_Access :=
            new Debug_Marshallers.Marshaller;
    begin
        Marshaller.Data := Create_Info.User_Data;
        Marshaller.Callback := Create_Info.Callback;
        Info_C.Callback := Callback_Proxy'Access;
        Info_C.User_Data := Marshaller.all'Address;

        Result := vkCreateDebugReportCallbackEXT(Instance,
                                                 Info_C,
                                                 Allocator,
                                                 Callback);
        Debug_Reports_C.Free(Info_C);

        if Result = Success then
            Marshallers.Register(Callback, Marshaller);
        else
            Debug_Marshallers.Free(Marshaller);
        end if;

        return Result;
    end Create;

    function Callback_Proxy
        (Flags: in Debug_Report_Flags;
         Object_Type: in Debug_Report_Object_Type;
         Object: in Object_Handle;
         Location: in Interfaces.C.size_t;
         Message_Code: in Interfaces.Integer_32;
         Layer_Prefix, Message: in Interfaces.C.Strings.chars_ptr;
         User_Data: in Interfaces.C.Extensions.void_ptr)
        return Interfaces.Unsigned_32 is
        Marshaller: Debug_Marshallers.Marshaller_Access :=
            Debug_Marshallers.To_Marshaller(User_Data);
    begin
        return Utilities.To_C
            (Marshaller.Callback(Flags,
                                 Object_Type,
                                 Object,
                                 Location,
                                 Message_Code,
                                 Interfaces.C.Strings.Value(Layer_Prefix),
                                 Interfaces.C.Strings.Value(Message),
                                 Marshaller.Data));
    end Callback_Proxy;
end Vulkan.Debug_Reports;


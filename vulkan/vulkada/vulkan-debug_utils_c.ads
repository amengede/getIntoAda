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

-- C interface for the debug utils extension

with Interfaces.C.Strings;
with Vulkan.C_Arrays;
with Vulkan.C;

private package Vulkan.Debug_Utils_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Debug_Utils_Label_Type |
            Debug_Utils_Object_Name_Info_Type |
            Debug_Utils_Messenger_Callback_Data_Type |
            Debug_Utils_Messenger_Create_Info_Type |
            Debug_Utils_Object_Tag_Info_Type;
                         
    -- C interface records.
    type Debug_Utils_Label_C is
    record
        Record_Type: In_Structure_Type := Debug_Utils_Label_Type;
        Next: C.In_Structure_C_Access;
        Label_Name: Interfaces.C.Strings.chars_ptr;
        Color: Debug_Color;
    end record
        with Convention => C;

    type Debug_Utils_Label_C_Access is access Debug_Utils_Label_C
        with Convention => C;

    type Debug_Utils_Object_Name_Info_C is
    record
        Record_Type: In_Structure_Type := Debug_Utils_Object_Name_Info_Type;
        Next: C.In_Structure_C_Access;
        Object_Type: Vulkan.Object_Type;
        Object_Handle: Vulkan.Object_Handle;
        Object_Name: Interfaces.C.Strings.chars_ptr;
    end record
        with Convention => C;

    type Debug_Utils_Object_Name_Info_C_Access is
        access Debug_Utils_Object_Name_Info_C
        with Convention => C;

    package Debug_Utils_Label_C_Arrays is new C_Arrays(Debug_Utils_Label_C);

    package Debug_Utils_Object_Name_Info_C_Arrays is
        new C_Arrays(Debug_Utils_Object_Name_Info_C);

    type Debug_Utils_Messenger_Callback_Data_C is
    record
        Record_Type: In_Structure_Type :=
            Debug_Utils_Messenger_Callback_Data_Type;
        Next: C.In_Structure_C_Access;
        Flags: Debug_Utils_Messenger_Callback_Data_Flags;
        Message_ID_Name: Interfaces.C.Strings.chars_ptr;
        Message_ID_Number: Interfaces.Integer_32;
        Message: Interfaces.C.Strings.chars_ptr;
        Queue_Label_Count: Interfaces.Unsigned_32;
        Queue_Labels: Debug_Utils_Label_C_Arrays.Pointer;
        Cmd_Buf_Label_Count: Interfaces.Unsigned_32;
        Cmd_Buf_Labels: Debug_Utils_Label_C_Arrays.Pointer;
        Object_Count: Interfaces.Unsigned_32;
        Objects: Debug_Utils_Object_Name_Info_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Debug_Utils_Messenger_Callback_Data_C_Access is
        access Debug_Utils_Messenger_Callback_Data_C
        with Convention => C;

    type Messenger_Callback_C is
        access function(Message_Severity: in Debug_Utils_Message_Severity_Flags;
                        Message_Types: in Debug_Utils_Message_Type_Flags;
                        Callback_Data: in Debug_Utils_Messenger_Callback_Data_C;
                        User_Data: in Interfaces.C.Extensions.void_ptr)
        return Interfaces.Unsigned_32
        with Convention => C;

    type Debug_Utils_Messenger_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Debug_Utils_Messenger_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Debug_Utils_Messenger_Create_Flags;
        Message_Severity: Debug_Utils_Message_Severity_Flags;
        Message_Type: Debug_Utils_Message_Type_Flags;
        User_Callback: Messenger_Callback_C;
        User_Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Debug_Utils_Messenger_Create_Info_C_Access is
        access all Debug_Utils_Messenger_Create_Info_C
        with Convention => C;

    type Debug_Utils_Object_Tag_Info_C is
    record
        Record_Type: In_Structure_Type := Debug_Utils_Object_Tag_Info_Type;
        Next: C.In_Structure_C_Access;
        Object_Type: Vulkan.Object_Type;
        Object_Handle: Vulkan.Object_Handle;
        Tag_Name: Interfaces.Unsigned_64;
        Tag_Size: Interfaces.C.size_t;
        Tag: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Debug_Utils_Object_Tag_Info_C_Access is
        access all Debug_Utils_Object_Tag_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Debug_Utils_Label) return Debug_Utils_Label_C;
    procedure Free(Struct: in out Debug_Utils_Label_C);

    function To_C(Struct: in Debug_Utils_Object_Name_Info)
        return Debug_Utils_Object_Name_Info_C;
    procedure Free(Struct: in out Debug_Utils_Object_Name_Info_C);

    function To_C(Struct: in Debug_Utils_Messenger_Callback_Data)
        return Debug_Utils_Messenger_Callback_Data_C;
    procedure Free(Struct: in out Debug_Utils_Messenger_Callback_Data_C);

    function To_C(Struct: in Debug_Utils_Messenger_Create_Info)
        return Debug_Utils_Messenger_Create_Info_C;
    procedure Free(Struct: in out Debug_Utils_Messenger_Create_Info_C);

    function To_C(Struct: in Debug_Utils_Object_Tag_Info)
        return Debug_Utils_Object_Tag_Info_C;
    procedure Free(Struct: in out Debug_Utils_Object_Tag_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Debug_Utils_C;


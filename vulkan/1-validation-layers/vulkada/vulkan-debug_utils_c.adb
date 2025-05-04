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

with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Debug_Utils_C is
    function To_C(Struct: in Debug_Utils_Label) return Debug_Utils_Label_C is
        DULC: Debug_Utils_Label_C;
    begin
        DULC.Next := Extension_Records.To_C(Struct.Next);
        DULC.Label_Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Label_Name));
        DULC.Color := Struct.Color;

        return DULC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Label_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Label_Name);
    end Free;

    function To_C(Struct: in Debug_Utils_Object_Name_Info)
        return Debug_Utils_Object_Name_Info_C is
        DUONIC: Debug_Utils_Object_Name_Info_C;
    begin
        DUONIC.Next := Extension_Records.To_C(Struct.Next);
        DUONIC.Object_Type := Struct.Object_Type;
        DUONIC.Object_Handle := Struct.Object_Handle;
        DUONIC.Object_Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Object_Name));

        return DUONIC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Object_Name_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Object_Name);
    end Free;

    function To_C(Struct: in Debug_Utils_Messenger_Callback_Data)
        return Debug_Utils_Messenger_Callback_Data_C is

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Debug_Utils_Label_C_Arrays, Debug_Utils_Label_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Debug_Utils_Object_Name_Info_C_Arrays,
             Debug_Utils_Object_Name_Info_Vectors);

        DUMCDC: Debug_Utils_Messenger_Callback_Data_C;
    begin
        DUMCDC.Next := Extension_Records.To_C(Struct.Next);
        DUMCDC.Flags := Struct.Flags;
        DUMCDC.Message_ID_Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Message_ID_Name));
        DUMCDC.Message_ID_Number := Struct.Message_ID_Number;
        DUMCDC.Message := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Message));
        To_C_Array(DUMCDC.Queue_Label_Count,
                   Struct.Queue_Labels,
                   DUMCDC.Queue_Labels);
        To_C_Array(DUMCDC.Cmd_Buf_Label_Count,
                   Struct.Cmd_Buf_Labels,
                   DUMCDC.Cmd_Buf_Labels);
        To_C_Array(DUMCDC.Object_Count, Struct.Objects, DUMCDC.Objects);

        return DUMCDC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Messenger_Callback_Data_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Message_ID_Name);
        Interfaces.C.Strings.Free(Struct.Message);
        Debug_Utils_Label_C_Arrays.Free(Struct.Queue_Labels, Free'Access);
        Debug_Utils_Label_C_Arrays.Free(Struct.Cmd_Buf_Labels, Free'Access);
        Debug_Utils_Object_Name_Info_C_Arrays.Free(Struct.Objects, Free'Access);
    end Free;

    function To_C(Struct: in Debug_Utils_Messenger_Create_Info)
        return Debug_Utils_Messenger_Create_Info_C is
        DUMCIC: Debug_Utils_Messenger_Create_Info_C;
    begin
        DUMCIC.Next := Extension_Records.To_C(Struct.Next);
        DUMCIC.Flags := Struct.Flags;
        DUMCIC.Message_Severity := Struct.Message_Severity;
        DUMCIC.Message_Type := Struct.Message_Type;

        return DUMCIC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Messenger_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Debug_Utils_Object_Tag_Info)
        return Debug_Utils_Object_Tag_Info_C is
        DUOTIC: Debug_Utils_Object_Tag_Info_C;
    begin
        DUOTIC.Next := Extension_Records.To_C(Struct.Next);
        DUOTIC.Object_Type := Struct.Object_Type;
        DUOTIC.Object_Handle := Struct.Object_Handle;
        DUOTIC.Tag_Name := Struct.Tag_Name;
        DUOTIC.Tag_Size := Struct.Tag_Size;
        DUOTIC.Tag := Struct.Tag;

        return DUOTIC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Object_Tag_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free; 
    
    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Debug_Utils_Label_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Debug_Utils_Label,
                         Debug_Utils_Label_C,
                         Debug_Utils_Label_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Utils_Object_Name_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Debug_Utils_Object_Name_Info,
                         Debug_Utils_Object_Name_Info_C,
                         Debug_Utils_Object_Name_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Utils_Messenger_Callback_Data_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Debug_Utils_Messenger_Callback_Data,
                         Debug_Utils_Messenger_Callback_Data_C,
                         Debug_Utils_Messenger_Callback_Data_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Utils_Messenger_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Debug_Utils_Messenger_Create_Info,
                         Debug_Utils_Messenger_Create_Info_C,
                         Debug_Utils_Messenger_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Utils_Object_Tag_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Debug_Utils_Object_Tag_Info,
                         Debug_Utils_Object_Tag_Info_C,
                         Debug_Utils_Object_Tag_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Structure(Next.Record_Type) is
            when Debug_Utils_Label_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Label_C,
                         Debug_Utils_Label_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Utils_Object_Name_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Object_Name_Info_C,
                         Debug_Utils_Object_Name_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Utils_Messenger_Callback_Data_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Messenger_Callback_Data_C,
                         Debug_Utils_Messenger_Callback_Data_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Utils_Messenger_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Messenger_Create_Info_C,
                         Debug_Utils_Messenger_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Utils_Object_Tag_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Object_Tag_Info_C,
                         Debug_Utils_Object_Tag_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Debug_Utils_C;


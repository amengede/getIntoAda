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

-- C interface for the video decode H264 extension

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Video_Decode_H264_C is
    function To_C(Struct: in Video_Decode_H264_Profile_Info)
        return Video_Decode_H264_Profile_Info_C is
        VDHPIC: Video_Decode_H264_Profile_Info_C;
    begin
        VDHPIC.Next := Extension_Records.To_C(Struct.Next);
        VDHPIC.Std_Profile := Struct.Std_Profile;
        VDHPIC.Picture_Layout := Struct.Picture_Layout;

        return VDHPIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_H264_Profile_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Video_Decode_H264_Capabilities;
                     C_Struct: in Video_Decode_H264_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Level := C_Struct.Max_Level;
        Ada_Struct.Field_Offset_Granularity :=
            C_Struct.Field_Offset_Granularity;
    end To_Ada;

    function To_C(Struct: in Video_Decode_H264_Session_Parameters_Add_Info)
        return Video_Decode_H264_Session_Parameters_Add_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Std_Video_H264_Sequence_Parameter_Set_Arrays,
             Std_Video_H264_Sequence_Parameter_Set_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (Std_Video_H264_Picture_Parameter_Set_Arrays,
             Std_Video_H264_Picture_Parameter_Set_Vectors);

        VDHSPAIC: Video_Decode_H264_Session_Parameters_Add_Info_C;
    begin
        VDHSPAIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VDHSPAIC.Std_SPS_Count, Struct.Std_SPSs, VDHSPAIC.Std_SPSs);
        To_C_Array(VDHSPAIC.Std_PPS_Count, Struct.Std_PPSs, VDHSPAIC.Std_PPSs);

        return VDHSPAIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Decode_H264_Session_Parameters_Add_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Std_Video_H264_Sequence_Parameter_Set_Arrays.Free(Struct.Std_SPSs);
        Std_Video_H264_Picture_Parameter_Set_Arrays.Free(Struct.Std_PPSs);
    end Free;

    function To_C(Struct: in Video_Decode_H264_Session_Parameters_Create_Info)
        return Video_Decode_H264_Session_Parameters_Create_Info_C is
        VDHSPCIC: Video_Decode_H264_Session_Parameters_Create_Info_C;
    begin
        VDHSPCIC.Next := Extension_Records.To_C(Struct.Next);
        VDHSPCIC.Max_Std_SPS_Count := Struct.Max_Std_SPS_Count;
        VDHSPCIC.Max_Std_PPS_Count := Struct.Max_Std_PPS_Count;

        if Struct.Parameters_Add_Info /= null then
            VDHSPCIC.Parameters_Add_Info :=
                new Video_Decode_H264_Session_Parameters_Add_Info_C'
                    (To_C(Struct.Parameters_Add_Info.all));
        end if;

        return VDHSPCIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Decode_H264_Session_Parameters_Create_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Decode_H264_Session_Parameters_Add_Info_C,
             Video_Decode_H264_Session_Parameters_Add_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Parameters_Add_Info /= null then
            Free(Struct.Parameters_Add_Info.all);
            Free(Struct.Parameters_Add_Info);
        end if;
    end Free;

    function To_C(Struct: in Video_Decode_H264_Picture_Info)
        return Video_Decode_H264_Picture_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        VDHPIC: Video_Decode_H264_Picture_Info_C;
    begin
        VDHPIC.Next := Extension_Records.To_C(Struct.Next);
        VDHPIC.Std_Picture_Info := Struct.Std_Picture_Info;
        To_C_Array(VDHPIC.Slice_Count,
                   Struct.Slice_Offsets,
                   VDHPIC.Slice_Offsets);

        return VDHPIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_H264_Picture_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Slice_Offsets);
    end Free;

    function To_C(Struct: in Video_Decode_H264_DPB_Slot_Info)
        return Video_Decode_H264_DPB_Slot_Info_C is
        VDHDSIC: Video_Decode_H264_DPB_Slot_Info_C;
    begin
        VDHDSIC.Next := Extension_Records.To_C(Struct.Next);
        VDHDSIC.Std_Reference_Info := Struct.Std_Reference_Info;

        return VDHDSIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_H264_DPB_Slot_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Video_Decode_H264_Profile_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Decode_H264_Profile_Info,
                         Video_Decode_H264_Profile_Info_C,
                         Video_Decode_H264_Profile_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_Session_Parameters_Add_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                       (Video_Decode_H264_Session_Parameters_Add_Info,
                        Video_Decode_H264_Session_Parameters_Add_Info_C,
                        Video_Decode_H264_Session_Parameters_Add_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_Session_Parameters_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                    (Video_Decode_H264_Session_Parameters_Create_Info,
                     Video_Decode_H264_Session_Parameters_Create_Info_C,
                     Video_Decode_H264_Session_Parameters_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_Picture_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Decode_H264_Picture_Info,
                         Video_Decode_H264_Picture_Info_C,
                         Video_Decode_H264_Picture_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_DPB_Slot_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Decode_H264_DPB_Slot_Info,
                         Video_Decode_H264_DPB_Slot_Info_C,
                         Video_Decode_H264_DPB_Slot_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Video_Decode_H264_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Video_Decode_H264_Capabilities,
                         Video_Decode_H264_Capabilities_C,
                         Video_Decode_H264_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Video_Decode_H264_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Decode_H264_Capabilities_C_Access);
                begin
                    To_Ada(Video_Decode_H264_Capabilities(Ada_Struct),
                           To_Access(Next).all);
                end;
        end case;
    end To_Ada;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case In_Structure(Next.Record_Type) is
            when Video_Decode_H264_Profile_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H264_Profile_Info_C,
                         Video_Decode_H264_Profile_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_Session_Parameters_Add_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Video_Decode_H264_Session_Parameters_Add_Info_C,
                        Video_Decode_H264_Session_Parameters_Add_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_Session_Parameters_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Video_Decode_H264_Session_Parameters_Create_Info_C,
                     Video_Decode_H264_Session_Parameters_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_Picture_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H264_Picture_Info_C,
                         Video_Decode_H264_Picture_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_DPB_Slot_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H264_DPB_Slot_Info_C,
                         Video_Decode_H264_DPB_Slot_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Video_Decode_H264_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Decode_H264_Capabilities_C,
                         Video_Decode_H264_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Video_Decode_H264_C;


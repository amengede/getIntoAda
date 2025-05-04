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

-- C interfaces for the video queues extension

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Video_Queues_C is
    procedure To_Ada
        (Ada_Struct: in out Queue_Family_Query_Result_Status_Properties;
         C_Struct: in Queue_Family_Query_Result_Status_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Query_Result_Status_Support :=
            Utilities.To_Ada(C_Struct.Query_Result_Status_Support);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Queue_Family_Video_Properties;
                     C_Struct: in Queue_Family_Video_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Video_Codec_Operations := C_Struct.Video_Codec_Operations;
    end To_Ada;

    function To_C(Struct: in Video_Profile_Info) return Video_Profile_Info_C is
        VPIC: Video_Profile_Info_C;
    begin
        VPIC.Next := Extension_Records.To_C(Struct.Next);
        VPIC.Video_Codec_Operation := Struct.Video_Codec_Operation;
        VPIC.Chroma_Subsampling := Struct.Chroma_Subsampling;
        VPIC.Luma_Bit_Depth := Struct.Luma_Bit_Depth;
        VPIC.Chroma_Bit_Depth := Struct.Chroma_Bit_Depth;

        return VPIC;
    end To_C;

    procedure Free(Struct: in out Video_Profile_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Video_Profile_List_Info)
        return Video_Profile_List_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Video_Profile_Info_C_Arrays, Video_Profile_Info_Vectors);

        VPLIC: Video_Profile_List_Info_C;
    begin
        VPLIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VPLIC.Profile_Count,
                   Struct.Profiles,
                   VPLIC.Profiles);

        return VPLIC;
    end To_C;

    procedure Free(Struct: in out Video_Profile_List_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Video_Profile_Info_C_Arrays.Free(Struct.Profiles, Free'Access);
    end Free;

    procedure To_Ada(Ada_Struct: in out Video_Capabilities;
                     C_Struct: in Video_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Flags := C_Struct.Flags;
        Ada_Struct.Min_Bitstream_Buffer_Offset_Alignment :=
            C_Struct.Min_Bitstream_Buffer_Offset_Alignment;
        Ada_Struct.Min_Bitstream_Buffer_Size_Alignment :=
            C_Struct.Min_Bitstream_Buffer_Size_Alignment;
        Ada_Struct.Picture_Access_Granularity :=
            C_Struct.Picture_Access_Granularity;
        Ada_Struct.Min_Coded_Extent := C_Struct.Min_Coded_Extent;
        Ada_Struct.Max_Coded_Extent := C_Struct.Max_Coded_Extent;
        Ada_Struct.Max_DPB_Slots := C_Struct.Max_DPB_Slots;
        Ada_Struct.Max_Active_Reference_Pictures :=
            C_Struct.Max_Active_Reference_Pictures;
        Ada_Struct.Std_Header_Version.Name :=
            Ada.Strings.Unbounded.To_Unbounded_String
                (Interfaces.C.To_Ada
                    (C_Struct.Std_Header_Version.Extension_Name));
        Ada_Struct.Std_Header_Version.Spec_Version :=
            C_Struct.Std_Header_Version.Spec_Version;
    end To_Ada;

    function To_C(Struct: in Physical_Device_Video_Format_Info)
        return Physical_Device_Video_Format_Info_C is
        PDVDIC: Physical_Device_Video_Format_Info_C;
    begin
        PDVDIC.Next := Extension_Records.To_C(Struct.Next);
        PDVDIC.Image_Usage := Struct.Image_Usage;

        return PDVDIC;
    end To_C;

    procedure Free(Struct: in out Physical_Device_Video_Format_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Video_Format_Properties;
                     C_Struct: in Video_Format_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Format := C_Struct.Format;
        Ada_Struct.Component_Mapping := C_Struct.Component_Mapping;
        Ada_Struct.Image_Create_Flags := C_Struct.Image_Create_Flags;
        Ada_Struct.Image_Type := C_Struct.Image_Type;
        Ada_Struct.Image_Tiling := C_Struct.Image_Tiling;
        Ada_Struct.Image_Usage_Flags := C_Struct.Image_Usage_Flags;
    end To_Ada;

    function To_C(Struct: in Video_Picture_Resource_Info)
        return Video_Picture_Resource_Info_C is
        VPRIC: Video_Picture_Resource_Info_C;
    begin
        VPRIC.Next := Extension_Records.To_C(Struct.Next);
        VPRIC.Coded_Offset := Struct.Coded_Offset;
        VPRIC.Coded_Extent := Struct.Coded_Extent;
        VPRIC.Base_Array_Layer := Struct.Base_Array_Layer;
        VPRIC.Image_View_Binding := Struct.Image_View_Binding;

        return VPRIC;
    end To_C;

    procedure Free(Struct: in out Video_Picture_Resource_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Video_Reference_Slot_Info)
        return Video_Reference_Slot_Info_C is
        VRSIC: Video_Reference_Slot_Info_C;
    begin
        VRSIC.Next := Extension_Records.To_C(Struct.Next);
        VRSIC.Slot_Index := Struct.Slot_Index;

        if Struct.Picture_Resource /= null then
            VRSIC.Picture_Resource := new Video_Picture_Resource_Info_C'
                (To_C(Struct.Picture_Resource.all));
        end if;

        return VRSIC;
    end To_C;

    procedure Free(Struct: in out Video_Reference_Slot_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Picture_Resource_Info_C,
             Video_Picture_Resource_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        Free(Struct.Picture_Resource.all);
        Free(Struct.Picture_Resource);
    end Free;

    procedure To_Ada(Ada_Struct: in out Video_Session_Memory_Requirements;
                     C_Struct: in Video_Session_Memory_Requirements_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Memory_Bind_Index := C_Struct.Memory_Bind_Index;
        Ada_Struct.Memory_Requirements := C_Struct.Memory_Requirements;
    end To_Ada;

    function To_C(Struct: in Bind_Video_Session_Memory_Info)
        return Bind_Video_Session_Memory_Info_C is
        BVSMIC: Bind_Video_Session_Memory_Info_C;
    begin
        BVSMIC.Next := Extension_Records.To_C(Struct.Next);
        BVSMIC.Memory_Bind_Index := Struct.Memory_Bind_Index;
        BVSMIC.Memory := Struct.Memory;
        BVSMIC.Memory_Offset := Struct.Memory_Offset;
        BVSMIC.Memory_Size := Struct.Memory_Size;

        return BVSMIC;
    end To_C;

    procedure Free(Struct: in out Bind_Video_Session_Memory_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Video_Session_Create_Info)
        return Video_Session_Create_Info_C is
        VSCIC: Video_Session_Create_Info_C;
    begin
        VSCIC.Next := Extension_Records.To_C(Struct.Next);
        VSCIC.Queue_Family_Index := Struct.Queue_Family_Index;
        VSCIC.Flags := Struct.Flags;
        
        if Struct.Video_Profile /= null then
            VSCIC.Video_Profile :=
                new Video_Profile_Info_C'(To_C(Struct.Video_Profile.all));
        end if;

        VSCIC.Picture_Format := Struct.Picture_Format;
        VSCIC.Max_Coded_Extent := Struct.Max_Coded_Extent;
        VSCIC.Reference_Picture_Format := Struct.Reference_Picture_Format;
        VSCIC.Max_DPB_Slots := Struct.Max_DPB_Slots;
        VSCIC.Max_Active_Reference_Pictures :=
            Struct.Max_Active_Reference_Pictures;

        if Struct.Std_Header_Version /= null then
            VSCIC.Std_Header_Version := new C.Extension_Properties_C;
            VSCIC.Std_Header_Version.Extension_Name :=
                Interfaces.C.To_C
                    (Ada.Strings.Unbounded.To_String
                        (Struct.Std_Header_Version.Name));
            VSCIC.Std_Header_Version.Spec_Version :=
                Struct.Std_Header_Version.Spec_Version;
        end if;

        return VSCIC;
    end To_C;

    procedure Free(Struct: in out Video_Session_Create_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Profile_Info_C, Video_Profile_Info_C_Access);
        procedure Free is new Ada.Unchecked_Deallocation
            (C.Extension_Properties_C, C.Extension_Properties_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        Free(Struct.Video_Profile);
        Free(Struct.Std_Header_Version);
    end Free;

    function To_C(Struct: in Video_Session_Parameters_Create_Info)
        return Video_Session_Parameters_Create_Info_C is
        VSPCIC: Video_Session_Parameters_Create_Info_C;
    begin
        VSPCIC.Next := Extension_Records.To_C(Struct.Next);
        VSPCIC.Flags := Struct.Flags;
        VSPCIC.Video_Session_Parameters_Template :=
            Struct.Video_Session_Parameters_Template;
        VSPCIC.Video_Session := Struct.Video_Session;

        return VSPCIC;
    end To_C;

    procedure Free(Struct: in out Video_Session_Parameters_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Video_Session_Parameters_Update_Info)
        return Video_Session_Parameters_Update_Info_C is
        VSPUIC: Video_Session_Parameters_Update_Info_C;
    begin
        VSPUIC.Next := Extension_Records.To_C(Struct.Next);
        VSPUIC.Update_Sequence_Count := Struct.Update_Sequence_Count;

        return VSPUIC;
    end To_C;

    procedure Free(Struct: in out Video_Session_Parameters_Update_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Video_Begin_Coding_Info)
        return Video_Begin_Coding_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Video_Reference_Slot_Info_C_Arrays,
             Video_Reference_Slot_Info_Vectors);

        VBCIC: Video_Begin_Coding_Info_C;
    begin
        VBCIC.Next := Extension_Records.To_C(Struct.Next);
        VBCIC.Flags := Struct.Flags;
        VBCIC.Video_Session := Struct.Video_Session;
        VBCIC.Video_Session_Parameters := Struct.Video_Session_Parameters;
        To_C_Array(VBCIC.Reference_Slot_Count,
                   Struct.Reference_Slots,
                   VBCIC.Reference_Slots);

        return VBCIC;
    end To_C;

    procedure Free(Struct: in out Video_Begin_Coding_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Video_Reference_Slot_Info_C_Arrays.Free(Struct.Reference_Slots,
                                                Free'Access);
    end Free;

    function To_C(Struct: in Video_End_Coding_Info)
        return Video_End_Coding_Info_C is
        VECIC: Video_End_Coding_Info_C;
    begin
        VECIC.Next := Extension_Records.To_C(Struct.Next);
        VECIC.Flags := Struct.Flags;

        return VECIC;
    end To_C;

    procedure Free(Struct: in out Video_End_Coding_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Video_Coding_Control_Info)
        return Video_Coding_Control_Info_C is
        VCCIC: Video_Coding_Control_Info_C;
    begin
        VCCIC.Next := Extension_Records.To_C(Struct.Next);
        VCCIC.Flags := Struct.Flags;

        return VCCIC;
    end To_C;

    procedure Free(Struct: in out Video_Coding_Control_Info_C) is
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
            when Video_Profile_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Profile_Info,
                         Video_Profile_Info_C,
                         Video_Profile_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Profile_List_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Profile_List_Info,
                         Video_Profile_List_Info_C,
                         Video_Profile_List_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Video_Format_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Physical_Device_Video_Format_Info,
                         Physical_Device_Video_Format_Info_C,
                         Physical_Device_Video_Format_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Picture_Resource_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Picture_Resource_Info,
                         Video_Picture_Resource_Info_C,
                         Video_Picture_Resource_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Reference_Slot_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Reference_Slot_Info,
                         Video_Reference_Slot_Info_C,
                         Video_Reference_Slot_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Video_Session_Memory_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Video_Session_Memory_Info,
                         Bind_Video_Session_Memory_Info_C,
                         Bind_Video_Session_Memory_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Session_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Session_Create_Info,
                         Video_Session_Create_Info_C,
                         Video_Session_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Session_Parameters_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Session_Parameters_Create_Info,
                         Video_Session_Parameters_Create_Info_C,
                         Video_Session_Parameters_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Session_Parameters_Update_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Session_Parameters_Update_Info,
                         Video_Session_Parameters_Update_Info_C,
                         Video_Session_Parameters_Update_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Begin_Coding_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Begin_Coding_Info,
                         Video_Begin_Coding_Info_C,
                         Video_Begin_Coding_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_End_Coding_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_End_Coding_Info,
                         Video_End_Coding_Info_C,
                         Video_End_Coding_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Coding_Control_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Coding_Control_Info,
                         Video_Coding_Control_Info_C,
                         Video_Coding_Control_Info_C_Access);
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
            when Queue_Family_Query_Result_Status_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Queue_Family_Query_Result_Status_Properties,
                         Queue_Family_Query_Result_Status_Properties_C,
                         Queue_Family_Query_Result_Status_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Queue_Family_Video_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Queue_Family_Video_Properties,
                         Queue_Family_Video_Properties_C,
                         Queue_Family_Video_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Video_Capabilities,
                         Video_Capabilities_C,
                         Video_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Format_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Video_Format_Properties,
                         Video_Format_Properties_C,
                         Video_Format_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Session_Memory_Requirements_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Video_Session_Memory_Requirements,
                         Video_Session_Memory_Requirements_C,
                         Video_Session_Memory_Requirements_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Queue_Family_Query_Result_Status_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Queue_Family_Query_Result_Status_Properties_C_Access);
                begin
                    To_Ada
                       (Queue_Family_Query_Result_Status_Properties(Ada_Struct),
                        To_Access(Next).all);
                end;
            when Queue_Family_Video_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Queue_Family_Video_Properties_C_Access);
                begin
                    To_Ada(Queue_Family_Video_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Capabilities_C_Access);
                begin
                    To_Ada(Video_Capabilities(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Format_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Format_Properties_C_Access);
                begin
                    To_Ada(Video_Format_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Session_Memory_Requirements_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Session_Memory_Requirements_C_Access);
                begin
                    To_Ada(Video_Session_Memory_Requirements(Ada_Struct),
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
            when Video_Profile_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Profile_Info_C,
                         Video_Profile_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Profile_List_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Profile_List_Info_C,
                         Video_Profile_List_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Video_Format_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_Video_Format_Info_C,
                         Physical_Device_Video_Format_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Picture_Resource_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Picture_Resource_Info_C,
                         Video_Picture_Resource_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Reference_Slot_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Reference_Slot_Info_C,
                         Video_Reference_Slot_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Video_Session_Memory_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Video_Session_Memory_Info_C,
                         Bind_Video_Session_Memory_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Session_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Session_Create_Info_C,
                         Video_Session_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Session_Parameters_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Session_Parameters_Create_Info_C,
                         Video_Session_Parameters_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Session_Parameters_Update_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Session_Parameters_Update_Info_C,
                         Video_Session_Parameters_Update_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Begin_Coding_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Begin_Coding_Info_C,
                         Video_Begin_Coding_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_End_Coding_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_End_Coding_Info_C,
                         Video_End_Coding_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Coding_Control_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Coding_Control_Info_C,
                         Video_Coding_Control_Info_C_Access);
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
            when Queue_Family_Query_Result_Status_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Queue_Family_Query_Result_Status_Properties_C,
                         Queue_Family_Query_Result_Status_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Queue_Family_Video_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Queue_Family_Video_Properties_C,
                         Queue_Family_Video_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Capabilities_C,
                         Video_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Format_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Format_Properties_C,
                         Video_Format_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Session_Memory_Requirements_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Session_Memory_Requirements_C,
                         Video_Session_Memory_Requirements_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Video_Queues_C;


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

-- C interface for the video decode H265 extension

with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.Video_Decode_H265_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Video_Decode_H265_Profile_Info_Type |
            Video_Decode_H265_Capabilities_Type |
            Video_Decode_H265_Session_Parameters_Add_Info_Type |
            Video_Decode_H265_Session_Parameters_Create_Info_Type |
            Video_Decode_H265_Picture_Info_Type |
            Video_Decode_H265_DPB_Slot_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Video_Decode_H265_Profile_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_H265_Profile_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Profile: Std_Video_H265_Profile;
    end record
        with Convention => C;

    type Video_Decode_H265_Profile_Info_C_Access is
        access Video_Decode_H265_Profile_Info_C
        with Convention => C;

    type Video_Decode_H265_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Decode_H265_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Max_Level: Std_Video_H265_Level;
    end record
        with Convention => C;

    type Video_Decode_H265_Capabilities_C_Access is
        access Video_Decode_H265_Capabilities_C
        with Convention => C;

    package Std_Video_H265_Video_Parameter_Set_Arrays is new C_Arrays
        (Std_Video_H265_Video_Parameter_Set);

    package Std_Video_H265_Sequence_Parameter_Set_Arrays is new C_Arrays
        (Std_Video_H265_Sequence_Parameter_Set);

    package Std_Video_H265_Picture_Parameter_Set_Arrays is new C_Arrays
        (Std_Video_H265_Picture_Parameter_Set);

    type Video_Decode_H265_Session_Parameters_Add_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Decode_H265_Session_Parameters_Add_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_VPS_Count: Interfaces.Unsigned_32;
        Std_VPSs: Std_Video_H265_Video_Parameter_Set_Arrays.Pointer;
        Std_SPS_Count: Interfaces.Unsigned_32;
        Std_SPSs: Std_Video_H265_Sequence_Parameter_Set_Arrays.Pointer;
        Std_PPS_Count: Interfaces.Unsigned_32;
        Std_PPSs: Std_Video_H265_Picture_Parameter_Set_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Decode_H265_Session_Parameters_Add_Info_C_Access is
        access Video_Decode_H265_Session_Parameters_Add_Info_C
        with Convention => C;

    type Video_Decode_H265_Session_Parameters_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Decode_H265_Session_Parameters_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Max_Std_VPS_Count: Interfaces.Unsigned_32;
        Max_Std_SPS_Count: Interfaces.Unsigned_32;
        Max_Std_PPS_Count: Interfaces.Unsigned_32;
        Parameters_Add_Info:
            Video_Decode_H265_Session_Parameters_Add_Info_C_Access;
    end record
        with Convention => C;

    type Video_Decode_H265_Session_Parameters_Create_Info_C_Access is
        access Video_Decode_H265_Session_Parameters_Create_Info_C
        with Convention => C;

    type Video_Decode_H265_Picture_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_H265_Picture_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Picture_Info: Std_Video_Decode_H265_Picture_Info_Access;
        Slice_Segment_Count: Interfaces.Unsigned_32;
        Slice_Segment_Offsets: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Decode_H265_Picture_Info_C_Access is
        access Video_Decode_H265_Picture_Info_C
        with Convention => C;

    type Video_Decode_H265_DPB_Slot_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_H265_DPB_Slot_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Reference_Info: Std_Video_Decode_H265_Reference_Info_Access;
    end record
        with Convention => C;

    type Video_Decode_H265_DPB_Slot_Info_C_Access is
        access Video_Decode_H265_DPB_Slot_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Video_Decode_H265_Profile_Info)
        return Video_Decode_H265_Profile_Info_C;
    procedure Free(Struct: in out Video_Decode_H265_Profile_Info_C);

    procedure To_Ada(Ada_Struct: in out Video_Decode_H265_Capabilities;
                     C_Struct: in Video_Decode_H265_Capabilities_C);

    function To_C(Struct: in Video_Decode_H265_Session_Parameters_Add_Info)
        return Video_Decode_H265_Session_Parameters_Add_Info_C;
    procedure Free
        (Struct: in out Video_Decode_H265_Session_Parameters_Add_Info_C);

    function To_C(Struct: in Video_Decode_H265_Session_Parameters_Create_Info)
        return Video_Decode_H265_Session_Parameters_Create_Info_C;
    procedure Free
        (Struct: in out Video_Decode_H265_Session_Parameters_Create_Info_C);

    function To_C(Struct: in Video_Decode_H265_Picture_Info)
        return Video_Decode_H265_Picture_Info_C;
    procedure Free(Struct: in out Video_Decode_H265_Picture_Info_C);

    function To_C(Struct: in Video_Decode_H265_DPB_Slot_Info)
        return Video_Decode_H265_DPB_Slot_Info_C;
    procedure Free(Struct: in out Video_Decode_H265_DPB_Slot_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Video_Decode_H265_C;


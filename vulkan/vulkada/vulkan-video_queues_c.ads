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

-- C interface for the video queues extension

with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.Video_Queues_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Queue_Family_Query_Result_Status_Properties_Type |
            Queue_Family_Video_Properties_Type |
            Video_Profile_Info_Type |
            Video_Profile_List_Info_Type |
            Video_Capabilities_Type |
            Physical_Device_Video_Format_Info_Type |
            Video_Format_Properties_Type |
            Video_Picture_Resource_Info_Type |
            Video_Reference_Slot_Info_Type |
            Video_Session_Memory_Requirements_Type |
            Bind_Video_Session_Memory_Info_Type |
            Video_Session_Create_Info_Type |
            Video_Session_Parameters_Create_Info_Type |
            Video_Session_Parameters_Update_Info_Type |
            Video_Begin_Coding_Info_Type |
            Video_End_Coding_Info_Type |
            Video_Coding_Control_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Queue_Family_Query_Result_Status_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Queue_Family_Query_Result_Status_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Query_Result_Status_Support: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Queue_Family_Query_Result_Status_Properties_C_Access is
        access Queue_Family_Query_Result_Status_Properties_C
        with Convention => C;

    type Queue_Family_Video_Properties_C is
    record
        Record_Type: Out_Structure_Type := Queue_Family_Video_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Video_Codec_Operations: Video_Codec_Operation_Flags;
    end record
        with Convention => C;

    type Queue_Family_Video_Properties_C_Access is
        access Queue_Family_Video_Properties_C
        with Convention => C;

    type Video_Profile_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Profile_Info_Type;
        Next: C.In_Structure_C_Access;
        Video_Codec_Operation: Video_Codec_Operation_Flags;
        Chroma_Subsampling: Video_Chroma_Subsampling_Flags;
        Luma_Bit_Depth: Video_Component_Bit_Depth_Flags;
        Chroma_Bit_Depth: Video_Component_Bit_Depth_Flags;
    end record
        with Convention => C;

    type Video_Profile_Info_C_Access is access Video_Profile_Info_C
        with Convention => C;

    package Video_Profile_Info_C_Arrays is new C_Arrays(Video_Profile_Info_C);

    type Video_Profile_List_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Profile_List_Info_Type;
        Next: C.In_Structure_C_Access;
        Profile_Count: Interfaces.Unsigned_32;
        Profiles: Video_Profile_Info_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Profile_List_Info_C_Access is access Video_Profile_List_Info_C
        with Convention => C;

    type Video_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Flags: Video_Capability_Flags;
        Min_Bitstream_Buffer_Offset_Alignment: Device_Size;
        Min_Bitstream_Buffer_Size_Alignment: Device_Size;
        Picture_Access_Granularity: Extent_2D;
        Min_Coded_Extent: Extent_2D;
        Max_Coded_Extent: Extent_2D;
        Max_DPB_Slots: Interfaces.Unsigned_32;
        Max_Active_Reference_Pictures: Interfaces.Unsigned_32;
        Std_Header_Version: C.Extension_Properties_C;
    end record
        with Convention => C;

    type Video_Capabilities_C_Access is access Video_Capabilities_C
        with Convention => C;

    type Physical_Device_Video_Format_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Physical_Device_Video_Format_Info_Type;
        Next: C.In_Structure_C_Access;
        Image_Usage: Image_Usage_Flags;
    end record
        with Convention => C;

    type Physical_Device_Video_Format_Info_C_Access is
        access Physical_Device_Video_Format_Info_C
        with Convention => C;

    type Video_Format_Properties_C is
    record
        Record_Type: Out_Structure_Type := Video_Format_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Format: Vulkan.Format;
        Component_Mapping: Vulkan.Component_Mapping;
        Image_Create_Flags: Vulkan.Image_Create_Flags;
        Image_Type: Vulkan.Image_Type;
        Image_Tiling: Vulkan.Image_Tiling;
        Image_Usage_Flags: Vulkan.Image_Usage_Flags;
    end record
        with Convention => C;

    type Video_Format_Properties_C_Access is access Video_Format_Properties_C
        with Convention => C;

    type Video_Picture_Resource_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Picture_Resource_Info_Type;
        Next: C.In_Structure_C_Access;
        Coded_Offset: Offset_2D;
        Coded_Extent: Extent_2D;
        Base_Array_Layer: Array_Layers;
        Image_View_Binding: Image_View;
    end record
        with Convention => C;

    type Video_Picture_Resource_Info_C_Access is
        access Video_Picture_Resource_Info_C
    with Convention => C;

    type Video_Reference_Slot_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Reference_Slot_Info_Type;
        Next: C.In_Structure_C_Access;
        Slot_Index: Interfaces.Unsigned_32;
        Picture_Resource: Video_Picture_Resource_Info_C_Access;
    end record
        with Convention => C;

    type Video_Reference_Slot_Info_C_Access is
        access Video_Reference_Slot_Info_C
        with Convention => C;

    type Video_Session_Memory_Requirements_C is
    record
        Record_Type: Out_Structure_Type :=
            Video_Session_Memory_Requirements_Type;
        Next: C.Out_Structure_C_Access;
        Memory_Bind_Index: Interfaces.Unsigned_32;
        Memory_Requirements: Vulkan.Memory_Requirements;
    end record
        with Convention => C;

    type Video_Session_Memory_Requirements_C_Access is
        access Video_Session_Memory_Requirements_C
        with Convention => C;

    type Bind_Video_Session_Memory_Info_C is
    record
        Record_Type: In_Structure_Type := Bind_Video_Session_Memory_Info_Type;
        Next: C.In_Structure_C_Access;
        Memory_Bind_Index: Interfaces.Unsigned_32;
        Memory: Device_Memory;
        Memory_Offset: Device_Size;
        Memory_Size: Device_Size;
    end record
        with Convention => C;

    type Bind_Video_Session_Memory_Info_C_Access is
        access Bind_Video_Session_Memory_Info_C
        with Convention => C;

    type Video_Session_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Session_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Queue_Family_Index: Vulkan.Queue_Family_Index;
        Flags: Video_Session_Create_Flags;
        Video_Profile: Video_Profile_Info_C_Access;
        Picture_Format: Format;
        Max_Coded_Extent: Extent_2D;
        Reference_Picture_Format: Format;
        Max_DPB_Slots: Interfaces.Unsigned_32;
        Max_Active_Reference_Pictures: Interfaces.Unsigned_32;
        Std_Header_Version: C.Extension_Properties_C_Access;
    end record
        with Convention => C;

    type Video_Session_Create_Info_C_Access is
        access Video_Session_Create_Info_C
        with Convention => C;

    type Video_Session_Parameters_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Session_Parameters_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Video_Session_Parameters_Create_Flags;
        Video_Session_Parameters_Template: Video_Session_Parameters;
        Video_Session: Vulkan.Video_Session;
    end record
        with Convention => C;

    type Video_Session_Parameters_Create_Info_C_Access is
        access Video_Session_Parameters_Create_Info_C
        with Convention => C;

    type Video_Session_Parameters_Update_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Session_Parameters_Update_Info_Type;
        Next: C.In_Structure_C_Access;
        Update_Sequence_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Session_Parameters_Update_Info_C_Access is
        access Video_Session_Parameters_Update_Info_C
        with Convention => C;

    package Video_Reference_Slot_Info_C_Arrays is
        new C_Arrays(Video_Reference_Slot_Info_C);

    type Video_Begin_Coding_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Begin_Coding_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Video_Begin_Coding_Flags;
        Video_Session: Vulkan.Video_Session;
        Video_Session_Parameters: Vulkan.Video_Session_Parameters;
        Reference_Slot_Count: Interfaces.Unsigned_32;
        Reference_Slots: Video_Reference_Slot_Info_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Begin_Coding_Info_C_Access is access Video_Begin_Coding_Info_C
        with Convention => C;

    type Video_End_Coding_Info_C is
    record
        Record_Type: Structure_Type := Video_End_Coding_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Video_End_Coding_Flags;
    end record
        with Convention => C;

    type Video_End_Coding_Info_C_Access is access Video_End_Coding_Info_C
        with Convention => C;

    type Video_Coding_Control_Info_C is
    record
        Record_Type: Structure_Type := Video_Coding_Control_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Video_Coding_Control_Flags;
    end record
        with Convention => C;

    type Video_Coding_Control_Info_C_Access is
        access Video_Coding_Control_Info_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada
        (Ada_Struct: in out Queue_Family_Query_Result_Status_Properties;
         C_Struct: in Queue_Family_Query_Result_Status_Properties_C);

    procedure To_Ada(Ada_Struct: in out Queue_Family_Video_Properties;
                     C_Struct: in Queue_Family_Video_Properties_C);

    function To_C(Struct: in Video_Profile_Info) return Video_Profile_Info_C;
    procedure Free(Struct: in out Video_Profile_Info_C);

    function To_C(Struct: in Video_Profile_List_Info)
        return Video_Profile_List_Info_C;
    procedure Free(Struct: in out Video_Profile_List_Info_C);

    procedure To_Ada(Ada_Struct: in out Video_Capabilities;
                     C_Struct: in Video_Capabilities_C);

    function To_C(Struct: in Physical_Device_Video_Format_Info)
        return Physical_Device_Video_Format_Info_C;
    procedure Free(Struct: in out Physical_Device_Video_Format_Info_C);

    procedure To_Ada(Ada_Struct: in out Video_Format_Properties;
                     C_Struct: in Video_Format_Properties_C);

    function To_C(Struct: in Video_Picture_Resource_Info)
        return Video_Picture_Resource_Info_C;
    procedure Free(Struct: in out Video_Picture_Resource_Info_C);

    function To_C(Struct: in Video_Reference_Slot_Info)
        return Video_Reference_Slot_Info_C;
    procedure Free(Struct: in out Video_Reference_Slot_Info_C);

    procedure To_Ada(Ada_Struct: in out Video_Session_Memory_Requirements;
                     C_Struct: in Video_Session_Memory_Requirements_C);

    function To_C(Struct: in Bind_Video_Session_Memory_Info)
        return Bind_Video_Session_Memory_Info_C;
    procedure Free(Struct: in out Bind_Video_Session_Memory_Info_C);

    function To_C(Struct: in Video_Session_Create_Info)
        return Video_Session_Create_Info_C;
    procedure Free(Struct: in out Video_Session_Create_Info_C);

    function To_C(Struct: in Video_Session_Parameters_Create_Info)
        return Video_Session_Parameters_Create_Info_C;
    procedure Free(Struct: in out Video_Session_Parameters_Create_Info_C);

    function To_C(Struct: in Video_Session_Parameters_Update_Info)
        return Video_Session_Parameters_Update_Info_C;
    procedure Free(Struct: in out Video_Session_Parameters_Update_Info_C);

    function To_C(Struct: in Video_Begin_Coding_Info)
        return Video_Begin_Coding_Info_C;
    procedure Free(Struct: in out Video_Begin_Coding_Info_C);

    function To_C(Struct: in Video_End_Coding_Info)
        return Video_End_Coding_Info_C;
    procedure Free(Struct: in out Video_End_Coding_Info_C);

    function To_C(Struct: in Video_Coding_Control_Info)
        return Video_Coding_Control_Info_C;
    procedure Free(Struct: in out Video_Coding_Control_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Video_Queues_C;


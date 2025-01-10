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

-- C interface for KHR records

with Interfaces.C.Strings;
with Vulkan.C;
with Vulkan.C_V1_2;
with Vulkan.C_Arrays;
with Vulkan.Extensions.KHR;
with Vulkan.Extensions.Std_Video.H264.Encode;
with Vulkan.Extensions.Std_Video.H264.Decode;
with Vulkan.Extensions.Std_Video.H265.Encode;
with Vulkan.Extensions.Std_Video.H265.Decode;
with Vulkan.Extensions.Std_Video.AV1.Decode;
with Vulkan.Xlib;
with Vulkan.Xcb;
with Vulkan.Wayland;
with Vulkan.Win32;

private package Vulkan.C_KHR is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Swapchain_Create_Info_Type |
            Present_Info_Type |
            Image_Swapchain_Create_Info_Type |
            Bind_Image_Memory_Swapchain_Info_Type |
            Acquire_Next_Image_Info_Type |
            Device_Group_Present_Capabilities_Type |
            Device_Group_Present_Info_Type |
            Device_Group_Swapchain_Create_Info_Type |
            Display_Mode_Create_Info_Type |
            Display_Surface_Create_Info_Type |
            Display_Present_Info_Type |
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
            Video_Coding_Control_Info_Type |
            Video_Decode_Capabilities_Type |
            Video_Decode_Usage_Info_Type |
            Video_Decode_Info_Type |
            Video_Encode_H264_Capabilities_Type |
            Video_Encode_H264_Quality_Level_Properties_Type |
            Video_Encode_H264_Session_Create_Info_Type |
            Video_Encode_H264_Session_Parameters_Add_Info_Type |
            Video_Encode_H264_Session_Parameters_Create_Info_Type |
            Video_Encode_H264_Session_Parameters_Get_Info_Type |
            Video_Encode_H264_Session_Parameters_Feedback_Info_Type |
            Video_Encode_H264_Nalu_Slice_Info_Type |
            Video_Encode_H264_Picture_Info_Type |
            Video_Encode_H264_DPB_Slot_Info_Type |
            Video_Encode_H264_Profile_Info_Type |
            Video_Encode_H264_Rate_Control_Info_Type |
            Video_Encode_H264_Rate_Control_Layer_Info_Type |
            Video_Encode_H264_GOP_Remaining_Frame_Info_Type |
            Video_Encode_H265_Capabilities_Type |
            Video_Encode_H265_Session_Create_Info_Type |
            Video_Encode_H265_Quality_Level_Properties_Type |
            Video_Encode_H265_Session_Parameters_Add_Info_Type |
            Video_Encode_H265_Session_Parameters_Create_Info_Type |
            Video_Encode_H265_Session_Parameters_Get_Info_Type |
            Video_Encode_H265_Session_Parameters_Feedback_Info_Type |
            Video_Encode_H265_Nalu_Slice_Segment_Info_Type |
            Video_Encode_H265_Picture_Info_Type |
            Video_Encode_H265_DPB_Slot_Info_Type |
            Video_Encode_H265_Profile_Info_Type |
            Video_Encode_H265_Rate_Control_Info_Type |
            Video_Encode_H265_Rate_Control_Layer_Info_Type |
            Video_Encode_H265_GOP_Remaining_Frame_Info_Type |
            Video_Decode_H264_Profile_Info_Type |
            Video_Decode_H264_Capabilities_Type |
            Video_Decode_H264_Session_Parameters_Add_Info_Type |
            Video_Decode_H264_Session_Parameters_Create_Info_Type |
            Video_Decode_H264_Picture_Info_Type |
            Video_Decode_H264_DPB_Slot_Info_Type |
            Rendering_Fragment_Shading_Rate_Attachment_Info_Type |
            Import_Memory_FD_Info_Type |
            Memory_FD_Properties_Type |
            Memory_Get_FD_Info_Type |
            Import_Semaphore_FD_Info_Type |
            Semaphore_Get_FD_Info_Type |
            Physical_Device_Push_Descriptor_Properties_Type |
            Present_Regions_Type |
            Shared_Present_Surface_Capabilities_Type |
            Import_Fence_FD_Info_Type |
            Fence_Get_FD_Info_Type |
            Physical_Device_Performance_Query_Features_Type |
            Physical_Device_Performance_Query_Properties_Type |
            Performance_Counter_Type |
            Performance_Counter_Description_Type |
            Query_Pool_Performance_Create_Info_Type |
            Acquire_Profiling_Lock_Info_Type |
            Performance_Query_Submit_Info_Type |
            Physical_Device_Surface_Info_2_Type |
            Surface_Capabilities_2_Type |
            Surface_Format_2_Type |
            Display_Properties_2_Type |
            Display_Plane_Properties_2_Type |
            Display_Mode_Properties_2_Type |
            Display_Plane_Info_2_Type |
            Display_Plane_Capabilities_2_Type |
            Physical_Device_Shader_Clock_Features_Type |
            Video_Decode_H265_Profile_Info_Type |
            Video_Decode_H265_Capabilities_Type |
            Video_Decode_H265_Session_Parameters_Add_Info_Type |
            Video_Decode_H265_Session_Parameters_Create_Info_Type |
            Video_Decode_H265_Picture_Info_Type |
            Video_Decode_H265_DPB_Slot_Info_Type |
            Device_Queue_Global_Priority_Create_Info_Type |
            Physical_Device_Global_Priority_Query_Features_Type |
            Queue_Family_Global_Priority_Properties_Type |
            Fragment_Shading_Rate_Attachment_Info_Type |
            Pipeline_Fragment_Shading_Rate_State_Create_Info_Type |
            Physical_Device_Fragment_Shading_Rate_Features_Type |
            Physical_Device_Fragment_Shading_Rate_Properties_Type |
            Physical_Device_Fragment_Shading_Rate_Type |
            Physical_Device_Dynamic_Rendering_Local_Read_Features_Type |
            Rendering_Attachment_Location_Info_Type |
            Rendering_Input_Attachment_Index_Info_Type |
            Physical_Device_Shader_Quad_Control_Features_Type |
            Surface_Protected_Capabilities_Type |
            Physical_Device_Present_Wait_Features_Type |
            Physical_Device_Pipeline_Executable_Properties_Features_Type |
            Pipeline_Info_Type |
            Pipeline_Executable_Properties_Type |
            Pipeline_Executable_Info_Type |
            Pipeline_Executable_Statistic_Type |
            Pipeline_Executable_Internal_Representation_Type |
            Memory_Map_Info_Type |
            Memory_Unmap_Info_Type |
            Pipeline_Library_Create_Info_Type |
            Present_ID_Type |
            Physical_Device_Present_ID_Features_Type |
            Video_Encode_Info_Type |
            Video_Encode_Capabilities_Type |
            Query_Pool_Video_Encode_Feedback_Create_Info_Type |
            Video_Encode_Usage_Info_Type |
            Video_Encode_Rate_Control_Layer_Info_Type |
            Video_Encode_Rate_Control_Info_Type |
            Physical_Device_Video_Encode_Quality_Level_Info_Type |
            Video_Encode_Quality_Level_Properties_Type |
            Video_Encode_Quality_Level_Info_Type |
            Video_Encode_Session_Parameters_Get_Info_Type |
            Video_Encode_Session_Parameters_Feedback_Info_Type |
            Physical_Device_Fragment_Shader_Barycentric_Features_Type |
            Physical_Device_Fragment_Shader_Barycentric_Properties_Type |
            Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_Type |
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type |
            Physical_Device_Ray_Tracing_Maintenance_1_Features_Type |
            Physical_Device_Shader_Maximal_Reconvergence_Features_Type |
            Physical_Device_Maintenance_5_Features_Type |
            Physical_Device_Maintenance_5_Properties_Type |
            Rendering_Area_Info_Type |
            Image_Subresource_2_Type |
            Device_Image_Subresource_Info_Type |
            Subresource_Layout_2_Type |
            Pipeline_Create_Flags_2_Create_Info_Type |
            Buffer_Usage_Flags_2_Create_Info_Type |
            Physical_Device_Ray_Tracing_Position_Fetch_Features_Type |
            Cooperative_Matrix_Properties_Type |
            Physical_Device_Cooperative_Matrix_Features_Type |
            Physical_Device_Cooperative_Matrix_Properties_Type |
            Video_Decode_AV1_Profile_Info_Type |
            Video_Decode_AV1_Capabilities_Type |
            Video_Decode_AV1_Session_Parameters_Create_Info_Type |
            Video_Decode_AV1_Picture_Info_Type |
            Video_Decode_AV1_DPB_Slot_Info_Type |
            Physical_Device_Video_Maintenance_1_Features_Type |
            Video_Inline_Query_Info_Type |
            Physical_Device_Vertex_Attribute_Divisor_Properties_Type |
            Pipeline_Vertex_Input_Divisor_State_Create_Info_Type |
            Physical_Device_Vertex_Attribute_Divisor_Features_Type |
            Calibrated_Timestamp_Info_Type |
            Physical_Device_Maintenance_6_Features_Type |
            Physical_Device_Maintenance_6_Properties_Type |
            Bind_Memory_Status_Type |
            Bind_Descriptor_Sets_Info_Type |
            Push_Constants_Info_Type |
            Push_Descriptor_Set_Info_Type |
            Push_Descriptor_Set_With_Template_Info_Type |
            Set_Descriptor_Buffer_Offsets_Info_Type |
            Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type |
            Xlib_Surface_Create_Info_Type |
            Xcb_Surface_Create_Info_Type |
            Wayland_Surface_Create_Info_Type |
            Win32_Surface_Create_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Swapchain_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Swapchain_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Swapchain_Create_Flags;
        Surface: Extensions.KHR.Surface;
        Min_Image_Count: Interfaces.Unsigned_32;
        Image_Format: Format;
        Image_Color_Space: Extensions.KHR.Color_Space;
        Image_Extent: Extent_2D;
        Image_Array_Layers: Interfaces.Unsigned_32;
        Image_Usage: Image_Usage_Flags;
        Image_Sharing_Mode: Sharing_Mode;
        Queue_Family_Index_Count: Interfaces.Unsigned_32;
        Queue_Family_Indices: C.Queue_Family_Index_Arrays.Pointer;
        Pre_Transform: Extensions.KHR.Surface_Transform_Flags;
        Composite_Alpha: Extensions.KHR.Composite_Alpha_Flags;
        Present_Mode: Extensions.KHR.Present_Mode;
        Clipped: Interfaces.Unsigned_32;
        Old_Swapchain: Extensions.KHR.Swapchain;
    end record
        with Convention => C;

    type Swapchain_Create_Info_C_Access is access Swapchain_Create_Info_C
        with Convention => C;

    package Swapchain_Arrays is new C_Arrays(Extensions.KHR.Swapchain);
    package Result_Arrays is new C_Arrays(Result);

    type Present_Info_C is
    record
        Record_Type: In_Structure_Type := Present_Info_Type;
        Next: C.In_Structure_C_Access;
        Wait_Semaphore_Count: Interfaces.Unsigned_32;
        Wait_Semaphores: C.Semaphore_Arrays.Pointer;
        Swapchain_Count: Interfaces.Unsigned_32;
        Swapchains: Swapchain_Arrays.Pointer;
        Image_Indices: C.Uint32_t_Arrays.Pointer;
        Results: Result_Arrays.Pointer;
    end record
        with Convention => C;

    type Present_Info_C_Access is access Present_Info_C
        with Convention => C;

    type Image_Swapchain_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Image_Swapchain_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Swapchain: Extensions.KHR.Swapchain;
    end record
        with Convention => C;

    type Image_Swapchain_Create_Info_C_Access is
        access Image_Swapchain_Create_Info_C
        with Convention => C;

    type Bind_Image_Memory_Swapchain_Info_C is
    record
        Record_Type: In_Structure_Type := Bind_Image_Memory_Swapchain_Info_Type;
        Next: C.In_Structure_C_Access;
        Swapchain: Extensions.KHR.Swapchain;
        Image_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Bind_Image_Memory_Swapchain_Info_C_Access is
        access Bind_Image_Memory_Swapchain_Info_C
        with Convention => C;

    type Acquire_Next_Image_Info_C is
    record
        Record_Type: In_Structure_Type := Acquire_Next_Image_Info_Type;
        Next: C.In_Structure_C_Access;
        Swapchain: Extensions.KHR.Swapchain;
        Timeout: Interfaces.Unsigned_64;
        Semaphore: Vulkan.Semaphore;
        Fence: Vulkan.Fence;
        Device_Mask: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Acquire_Next_Image_Info_C_Access is
        access Acquire_Next_Image_Info_C
        with Convention => C;

    type Device_Group_Present_Capabilities_C is
    record
        Record_Type: Out_Structure_Type :=
            Device_Group_Present_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Present_Mask: Extensions.KHR.Present_Mask_Array;
        Modes: Extensions.KHR.Device_Group_Present_Mode_Flags;
    end record
        with Convention => C;

    type Device_Group_Present_Capabilities_C_Access is
        access Device_Group_Present_Capabilities_C
        with Convention => C;

    type Device_Group_Present_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Group_Present_Info_Type;
        Next: C.In_Structure_C_Access;
        Swapchain_Count: Interfaces.Unsigned_32;
        Device_Masks: C.Uint32_t_Arrays.Pointer;
        Mode: Extensions.KHR.Device_Group_Present_Mode_Flags;
    end record
        with Convention => C;

    type Device_Group_Present_Info_C_Access is
        access Device_Group_Present_Info_C
        with Convention => C;

    type Device_Group_Swapchain_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Device_Group_Swapchain_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Modes: Extensions.KHR.Device_Group_Present_Mode_Flags;
    end record
        with Convention => C;

    type Device_Group_Swapchain_Create_Info_C_Access is
        access Device_Group_Swapchain_Create_Info_C
        with Convention => C;

    type Display_Mode_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Display_Mode_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Display_Mode_Create_Flags;
        Parameters: Extensions.KHR.Display_Mode_Parameters;
    end record
        with Convention => C;

    type Display_Mode_Create_Info_C_Access is access Display_Mode_Create_Info_C
        with Convention => C;

    type Display_Properties_C is
    record
        Display: Extensions.KHR.Display;
        Display_Name: Interfaces.C.Strings.chars_ptr;
        Physical_Dimensions: Extent_2D;
        Physical_Resolution: Extent_2D;
        Supported_Transforms: Extensions.KHR.Surface_Transform_Flags;
        Plane_Reorder_Possible: Interfaces.Unsigned_32;
        Persistent_Content: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Display_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Display_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Display_Surface_Create_Flags;
        Display_Mode: Extensions.KHR.Display_Mode;
        Plane_Index: Interfaces.Unsigned_32;
        Plane_Stack_Index: Interfaces.Unsigned_32;
        Transform: Extensions.KHR.Surface_Transform_Flags;
        Global_Alpha: Interfaces.C.C_float;
        Alpha_Mode: Extensions.KHR.Display_Plane_Alpha_Flags;
        Image_Extent: Extent_2D;
    end record
        with Convention => C;

    type Display_Surface_Create_Info_C_Access is
        access Display_Surface_Create_Info_C
        with Convention => C;

    type Display_Present_Info_C is
    record
        Record_Type: In_Structure_Type := Display_Present_Info_Type;
        Next: C.In_Structure_C_Access;
        Src_Rect: Rect_2D;
        Dst_Rect: Rect_2D;
        Persistent: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Display_Present_Info_C_Access is access Display_Present_Info_C
        with Convention => C;

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
        Video_Codec_Operations: Extensions.KHR.Video_Codec_Operation_Flags;
    end record
        with Convention => C;

    type Queue_Family_Video_Properties_C_Access is
        access Queue_Family_Video_Properties_C
        with Convention => C;

    type Video_Profile_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Profile_Info_Type;
        Next: C.In_Structure_C_Access;
        Video_Codec_Operation: Extensions.KHR.Video_Codec_Operation_Flags;
        Chroma_Subsampling: Extensions.KHR.Video_Chroma_Subsampling_Flags;
        Luma_Bit_Depth: Extensions.KHR.Video_Component_Bit_Depth_Flags;
        Chroma_Bit_Depth: Extensions.KHR.Video_Component_Bit_Depth_Flags;
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
        Flags: Extensions.KHR.Video_Capability_Flags;
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
        Flags: Extensions.KHR.Video_Session_Create_Flags;
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
        Flags: Extensions.KHR.Video_Session_Parameters_Create_Flags;
        Video_Session_Parameters_Template:
            Extensions.KHR.Video_Session_Parameters;
        Video_Session: Extensions.KHR.Video_Session;
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
        Flags: Extensions.KHR.Video_Begin_Coding_Flags;
        Video_Session: Extensions.KHR.Video_Session;
        Video_Session_Parameters: Extensions.KHR.Video_Session_Parameters;
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
        Flags: Extensions.KHR.Video_End_Coding_Flags;
    end record
        with Convention => C;

    type Video_End_Coding_Info_C_Access is access Video_End_Coding_Info_C
        with Convention => C;

    type Video_Coding_Control_Info_C is
    record
        Record_Type: Structure_Type := Video_Coding_Control_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Video_Coding_Control_Flags;
    end record
        with Convention => C;

    type Video_Coding_Control_Info_C_Access is
        access Video_Coding_Control_Info_C
        with Convention => C;

    type Video_Decode_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Decode_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Flags: Extensions.KHR.Video_Decode_Capability_Flags;
    end record
        with Convention => C;

    type Video_Decode_Capabilities_C_Access is
        access Video_Decode_Capabilities_C
        with Convention => C;

    type Video_Decode_Usage_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_Usage_Info_Type;
        Next: C.In_Structure_C_Access;
        Video_Usage_Hints: Extensions.KHR.Video_Decode_Usage_Flags;
    end record
        with Convention => C;

    type Video_Decode_Usage_Info_C_Access is access Video_Decode_Usage_Info_C
        with Convention => C;

    type Video_Decode_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Video_Decode_Flags;
        Src_Buffer: Buffer;
        Src_Buffer_Offset: Device_Size;
        Src_Buffer_Range: Device_Size;
        Dst_Picture_Resource: Video_Picture_Resource_Info_C;
        Setup_Reference_Slot: Video_Reference_Slot_Info_C_Access;
        Reference_Slot_Count: Interfaces.Unsigned_32;
        Reference_Slots: Video_Reference_Slot_Info_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Decode_Info_C_Access is access Video_Decode_Info_C
        with Convention => C;

    type Video_Encode_H264_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Encode_H264_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Flags: Extensions.KHR.Video_Encode_H264_Capability_Flags;
        Max_Level_IDC: Extensions.Std_Video.H264.Level_IDC;
        Max_Slice_Count: Interfaces.Unsigned_32;
        Max_PPicture_L0_Reference_Count: Interfaces.Unsigned_32;
        Max_BPicture_L0_Reference_Count: Interfaces.Unsigned_32;
        Max_L1_Reference_Count: Interfaces.Unsigned_32;
        Max_Temporal_Layer_Count: Interfaces.Unsigned_32;
        Expect_Dyadic_Temporal_Layer_Pattern: Interfaces.Unsigned_32;
        Min_QP: Interfaces.Integer_32;
        Max_QP: Interfaces.Integer_32;
        Prefers_GOP_Remaining_Frames: Interfaces.Unsigned_32;
        Requires_GOP_Remaining_Frames: Interfaces.Unsigned_32;
        Std_Syntax_Flags: Extensions.KHR.Video_Encode_H264_Std_Flags;
    end record
        with Convention => C;

    type Video_Encode_H264_Capabilities_C_Access is
        access Video_Encode_H264_Capabilities_C
        with Convention => C;

    type Video_Encode_H264_Quality_Level_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Video_Encode_H264_Quality_Level_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Preferred_Rate_Control_Flags:
            Extensions.KHR.Video_Encode_H264_Rate_Control_Flags;
        Preferred_GOP_Frame_Count: Interfaces.Unsigned_32;
        Preferred_IDR_Period: Interfaces.Unsigned_32;
        Preferred_Consecutive_B_Frame_Count: Interfaces.Unsigned_32;
        Preferred_Temporal_Layer_Count: Interfaces.Unsigned_32;
        Preferred_Constant_QP: Extensions.KHR.Video_Encode_H264_QP;
        Preferred_Max_L0_Reference_Count: Interfaces.Unsigned_32;
        Preferred_Max_L1_Reference_Count: Interfaces.Unsigned_32;
        Preferred_Std_Entropy_Coding_Mode_Flag: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H264_Quality_Level_Properties_C_Access is
        access Video_Encode_H264_Quality_Level_Properties_C
        with Convention => C;

    type Video_Encode_H264_Session_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H264_Session_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Use_Max_Level_IDC: Interfaces.Unsigned_32;
        Max_Level_IDC: Extensions.Std_Video.H264.Level_IDC;
    end record
        with Convention => C;

    type Video_Encode_H264_Session_Create_Info_C_Access is
        access Video_Encode_H264_Session_Create_Info_C
        with Convention => C;

    package H264_Sequence_Parameter_Set_Arrays is new C_Arrays
        (Extensions.Std_Video.H264.Sequence_Parameter_Set);

    package H264_Picture_Parameter_Set_Arrays is new C_Arrays
        (Extensions.Std_Video.H264.Picture_Parameter_Set);

    type Video_Encode_H264_Session_Parameters_Add_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H264_Session_Parameters_Add_Info_Type;
        Next: C.In_Structure_C_Access;
        Sts_SPS_Count: Interfaces.Unsigned_32;
        Std_SPSs: H264_Sequence_Parameter_Set_Arrays.Pointer;
        Std_PPS_Count: Interfaces.Unsigned_32;
        Std_PPSs: H264_Picture_Parameter_Set_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Encode_H264_Session_Parameters_Add_Info_C_Access is
        access Video_Encode_H264_Session_Parameters_Add_Info_C
        with Convention => C;

    type Video_Encode_H264_Session_Parameters_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H264_Session_Parameters_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Max_Std_SPS_Count: Interfaces.Unsigned_32;
        Max_Std_PPS_Count: Interfaces.Unsigned_32;
        Parameters_Add_Info:
            Video_Encode_H264_Session_Parameters_Add_Info_C_Access;
    end record
        with Convention => C;

    type Video_Encode_H264_Session_Parameters_Create_Info_C_Access is
        access Video_Encode_H264_Session_Parameters_Create_Info_C
        with Convention => C;

    type Video_Encode_H264_Session_Parameters_Get_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H264_Session_Parameters_Get_Info_Type;
        Next: C.In_Structure_C_Access;
        Write_Std_SPS: Interfaces.Unsigned_32;
        Write_Std_PPS: Interfaces.Unsigned_32;
        Std_SPS_ID: Interfaces.Unsigned_32;
        Std_PPS_ID: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H264_Session_Parameters_Get_Info_C_Access is
        access Video_Encode_H264_Session_Parameters_Get_Info_C
        with Convention => C;

    type Video_Encode_H264_Session_Parameters_Feedback_Info_C is
    record
        Record_Type: Out_Structure_Type :=
            Video_Encode_H264_Session_Parameters_Feedback_Info_Type;
        Next: C.Out_Structure_C_Access;
        Has_Std_SPS_Overrides: Interfaces.Unsigned_32;
        Has_Std_PPS_Overrides: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H264_Session_Parameters_Feedback_Info_C_Access is
        access Video_Encode_H264_Session_Parameters_Feedback_Info_C
        with Convention => C;

    type Video_Encode_H264_Nalu_Slice_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H264_Nalu_Slice_Info_Type;
        Next: C.In_Structure_C_Access;
        Constant_QP: Interfaces.Integer_32;
        Std_Slice_Header: Extensions.Std_Video.H264.Encode.Slice_Header_Access;
    end record
        with Convention => C;

    type Video_Encode_H264_Nalu_Slice_Info_C_Access is
        access Video_Encode_H264_Nalu_Slice_Info_C
        with Convention => C;

    package Video_Encode_H264_Nalu_Slice_Info_C_Arrays is new C_Arrays
        (Video_Encode_H264_Nalu_Slice_Info_C);

    type Video_Encode_H264_Picture_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_H264_Picture_Info_Type;
        Next: C.In_Structure_C_Access;
        Nalu_Slice_Entry_Count: Interfaces.Unsigned_32;
        Nalu_Slice_Entries: Video_Encode_H264_Nalu_Slice_Info_C_Arrays.Pointer;
        Std_Picture_Info: Extensions.Std_Video.H264.Encode.Picture_Info_Access;
        Generate_Prefix_Nalu: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H264_Picture_Info_C_Access is
        access Video_Encode_H264_Picture_Info_C
        with Convention => C;

    type Video_Encode_H264_DPB_Slot_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_H264_DPB_Slot_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Reference_Info:
            Extensions.Std_Video.H264.Encode.Reference_Info_Access;
    end record
        with Convention => C;

    type Video_Encode_H264_DPB_Slot_Info_C_Access is
        access Video_Encode_H264_DPB_Slot_Info_C
        with Convention => C;

    type Video_Encode_H264_Profile_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_H264_Profile_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Profile_IDC: Extensions.Std_Video.H264.Profile_IDC;
    end record
        with Convention => C;

    type Video_Encode_H264_Profile_Info_C_Access is
        access Video_Encode_H264_Profile_Info_C
        with Convention => C;

    type Video_Encode_H264_Rate_Control_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H264_Rate_Control_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Video_Encode_H264_Rate_Control_Flags;
        GOP_Frame_Count: Interfaces.Unsigned_32;
        IDR_Period: Interfaces.Unsigned_32;
        Consecutive_B_Frame_Count: Interfaces.Unsigned_32;
        Temporal_Layer_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H264_Rate_Control_Info_C_Access is
        access Video_Encode_H264_Rate_Control_Info_C
        with Convention => C;

    type Video_Encode_H264_Rate_Control_Layer_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H264_Rate_Control_Layer_Info_Type;
        Next: C.In_Structure_C_Access;
        Use_Min_QP: Interfaces.Unsigned_32;
        Min_QP: Extensions.KHR.Video_Encode_H264_QP;
        Use_Max_QP: Interfaces.Unsigned_32;
        Max_QP: Extensions.KHR.Video_Encode_H264_QP;
        Use_Max_Frame_Size: Interfaces.Unsigned_32;
        Max_Frame_Size: Extensions.KHR.Video_Encode_H264_Frame_Size;
    end record
        with Convention => C;

    type Video_Encode_H264_Rate_Control_Layer_Info_C_Access is
        access Video_Encode_H264_Rate_Control_Layer_Info_C
        with Convention => C;

    type Video_Encode_H264_GOP_Remaining_Frame_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H264_GOP_Remaining_Frame_Info_Type;
        Next: C.In_Structure_C_Access;
        Use_GOP_Remaining_Frames: Interfaces.Unsigned_32;
        GOP_Remaining_I: Interfaces.Unsigned_32;
        GOP_Remaining_P: Interfaces.Unsigned_32;
        GOP_Remaining_B: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H264_GOP_Remaining_Frame_Info_C_Access is
        access Video_Encode_H264_GOP_Remaining_Frame_Info_C
        with Convention => C;

    type Video_Encode_H265_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Encode_H265_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Flags: Extensions.KHR.Video_Encode_H265_Capability_Flags;
        Max_Level_IDC: Extensions.Std_Video.H265.Level_IDC;
        Max_Slice_Segment_Count: Interfaces.Unsigned_32;
        Max_Tiles: Extent_2D;
        CTB_Sizes: Extensions.KHR.Video_Encode_H265_CTB_Size_Flags;
        Transform_Block_Sizes:
            Extensions.KHR.Video_Encode_H265_Transform_Block_Size_Flags;
        Max_P_Picture_L0_Reference_Count: Interfaces.Unsigned_32;
        Max_B_Picture_L0_Reference_Count: Interfaces.Unsigned_32;
        Max_L1_Reference_Count: Interfaces.Unsigned_32;
        Max_Sub_Layer_Count: Interfaces.Unsigned_32;
        Expect_Dyadic_Temporal_Sub_Layer_Pattern: Interfaces.Unsigned_32;
        Min_QP: Interfaces.Integer_32;
        Max_QP: Interfaces.Integer_32;
        Prefers_GOP_Remaining_Frames: Interfaces.Unsigned_32;
        Requires_GOP_Remaining_Frames: Interfaces.Unsigned_32;
        Std_Syntax_Flags: Extensions.KHR.Video_Encode_H265_Std_Flags;
    end record
        with Convention => C;

    type Video_Encode_H265_Capabilities_C_Access is
        access Video_Encode_H265_Capabilities_C
        with Convention => C;

    type Video_Encode_H265_Session_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H265_Session_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Use_Max_Level_IDC: Interfaces.Unsigned_32;
        Max_Level_IDC: Extensions.Std_Video.H265.Level_IDC;
    end record
        with Convention => C;

    type Video_Encode_H265_Session_Create_Info_C_Access is
        access Video_Encode_H265_Session_Create_Info_C
        with Convention => C;

    type Video_Encode_H265_Quality_Level_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Video_Encode_H265_Quality_Level_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Preferred_Rate_Control_Flags:
            Extensions.KHR.Video_Encode_H265_Rate_Control_Flags;
        Preferred_GOP_Frame_Count: Interfaces.Unsigned_32;
        Preferred_IDR_Period: Interfaces.Unsigned_32;
        Preferred_Consecutive_B_Frame_Count: Interfaces.Unsigned_32;
        Preferred_Sub_Layer_Count: Interfaces.Unsigned_32;
        Preferred_Constant_QP: Extensions.KHR.Video_Encode_H265_QP;
        Preferred_Max_L0_Reference_Count: Interfaces.Unsigned_32;
        Preferred_Max_L1_Reference_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H265_Quality_Level_Properties_C_Access is
        access Video_Encode_H265_Quality_Level_Properties_C
        with Convention => C;

    package H265_Video_Parameter_Set_Arrays is new C_Arrays
        (Extensions.Std_Video.H265.Video_Parameter_Set);

    package H265_Sequence_Parameter_Set_Arrays is new C_Arrays
        (Extensions.Std_Video.H265.Sequence_Parameter_Set);

    package H265_Picture_Parameter_Set_Arrays is new C_Arrays
        (Extensions.Std_Video.H265.Picture_Parameter_Set);

    type Video_Encode_H265_Session_Parameters_Add_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H265_Session_Parameters_Add_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_VPS_Count: Interfaces.Unsigned_32;
        Std_VPSs: H265_Video_Parameter_Set_Arrays.Pointer;
        Std_SPS_Count: Interfaces.Unsigned_32;
        Std_SPSs: H265_Sequence_Parameter_Set_Arrays.Pointer;
        Std_PPS_Count: Interfaces.Unsigned_32;
        Std_PPSs: H265_Picture_Parameter_Set_Arrays.Pointer;
    end record
        with Convention => C;
    
    type Video_Encode_H265_Session_Parameters_Add_Info_C_Access is
        access Video_Encode_H265_Session_Parameters_Add_Info_C
        with Convention => C;

    type Video_Encode_H265_Session_Parameters_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H265_Session_Parameters_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Max_Std_VPS_Count: Interfaces.Unsigned_32;
        Max_Std_SPS_Count: Interfaces.Unsigned_32;
        Max_Std_PPS_Count: Interfaces.Unsigned_32;
        Parameters_Add_Info:
            Video_Encode_H265_Session_Parameters_Add_Info_C_Access;
    end record
        with Convention => C;

    type Video_Encode_H265_Session_Parameters_Create_Info_C_Access is
        access Video_Encode_H265_Session_Parameters_Create_Info_C
        with Convention => C;

    type Video_Encode_H265_Session_Parameters_Get_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H265_Session_Parameters_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Write_Std_VPS: Interfaces.Unsigned_32;
        Write_Std_SPS: Interfaces.Unsigned_32;
        Write_Std_PPS: Interfaces.Unsigned_32;
        Std_VPS_ID: Interfaces.Unsigned_32;
        Std_SPS_ID: Interfaces.Unsigned_32;
        Std_PPS_ID: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H265_Session_Parameters_Get_Info_C_Access is
        access Video_Encode_H265_Session_Parameters_Get_Info_C
        with Convention => C;

    type Video_Encode_H265_Session_Parameters_Feedback_Info_C is
    record
        Record_Type: Out_Structure_Type :=
            Video_Encode_H265_Session_Parameters_Feedback_Info_Type;
        Next: C.Out_Structure_C_Access;
        Has_Std_VPS_Overrides: Interfaces.Unsigned_32;
        Has_Std_SPS_Overrides: Interfaces.Unsigned_32;
        Has_Std_PPS_Overrides: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H265_Session_Parameters_Feedback_Info_C_Access is
        access Video_Encode_H265_Session_Parameters_Feedback_Info_C
        with Convention => C;

    type Video_Encode_H265_Nalu_Slice_Segment_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H265_Nalu_Slice_Segment_Info_Type;
        Next: C.In_Structure_C_Access;
        Constant_QP: Interfaces.Integer_32;
        Std_Slice_Segment_Header:
            Extensions.Std_Video.H265.Encode.Slice_Segment_Header_Access;
    end record
        with Convention => C;

    type Video_Encode_H265_Nalu_Slice_Segment_Info_C_Access is
        access Video_Encode_H265_Nalu_Slice_Segment_Info_C
        with Convention => C;

    package Video_Encode_H265_Nalu_Slice_Segment_Info_C_Arrays is
        new C_Arrays(Video_Encode_H265_Nalu_Slice_Segment_Info_C);

    type Video_Encode_H265_Picture_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_H265_Picture_Info_Type;
        Next: C.In_Structure_C_Access;
        Nalu_Slice_Segment_Entry_Count: Interfaces.Unsigned_32;
        Nalu_Slice_Segment_Entries:
            Video_Encode_H265_Nalu_Slice_Segment_Info_C_Arrays.Pointer;
        Std_Picture_Info: Extensions.Std_Video.H265.Encode.Picture_Info_Access;
    end record
        with Convention => C;

    type Video_Encode_H265_Picture_Info_C_Access is
        access Video_Encode_H265_Picture_Info_C
        with Convention => C;

    type Video_Encode_H265_DPB_Slot_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_H265_DPB_Slot_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Reference_Info:
            Extensions.Std_Video.H265.Encode.Reference_Info_Access;
    end record
        with Convention => C;

    type Video_Encode_H265_DPB_Slot_Info_C_Access is
        access Video_Encode_H265_DPB_Slot_Info_C
        with Convention => C;

    type Video_Encode_H265_Profile_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_H265_Profile_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Profile_IDC: Extensions.Std_Video.H265.Profile_IDC;
    end record
        with Convention => C;

    type Video_Encode_H265_Profile_Info_C_Access is
        access Video_Encode_H265_Profile_Info_C
        with Convention => C;

    type Video_Encode_H265_Rate_Control_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H265_Rate_Control_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Video_Encode_H265_Rate_Control_Flags;
        GOP_Frame_Count: Interfaces.Unsigned_32;
        IDR_Period: Interfaces.Unsigned_32;
        Consecutive_B_Frame_Count: Interfaces.Unsigned_32;
        Sub_Layer_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H265_Rate_Control_Info_C_Access is
        access Video_Encode_H265_Rate_Control_Info_C
        with Convention => C;

    type Video_Encode_H265_Rate_Control_Layer_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H265_Rate_Control_Layer_Info_Type;
        Next: C.In_Structure_C_Access;
        Use_Min_QP: Interfaces.Unsigned_32;
        Min_QP: Extensions.KHR.Video_Encode_H265_QP;
        Use_Max_QP: Interfaces.Unsigned_32;
        Max_QP: Extensions.KHR.Video_Encode_H265_QP;
        Use_Max_Frame_Size: Interfaces.Unsigned_32;
        Max_Frame_Size: Extensions.KHR.Video_Encode_H265_Frame_Size;
    end record
        with Convention => C;

    type Video_Encode_H265_Rate_Control_Layer_Info_C_Access is
        access Video_Encode_H265_Rate_Control_Layer_Info_C
        with Convention => C;

    type Video_Encode_H265_GOP_Remaining_Frame_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_H265_GOP_Remaining_Frame_Info_Type;
        Next: C.In_Structure_C_Access;
        Use_GOP_Remaining_Frames: Interfaces.Unsigned_32;
        GOP_Remaining_I: Interfaces.Unsigned_32;
        GOP_Remaining_P: Interfaces.Unsigned_32;
        GOP_Remaining_B: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H265_GOP_Remaining_Frame_Info_C_Access is
        access Video_Encode_H265_GOP_Remaining_Frame_Info_C
        with Convention => C;

    type Video_Decode_H264_Profile_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_H264_Profile_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Profile_IDC: Extensions.Std_Video.H264.Profile_IDC;
        Picture_Layout: Extensions.KHR.Video_Decode_H264_Picture_Layout_Flags;
    end record
        with Convention => C;

    type Video_Decode_H264_Profile_Info_C_Access is
        access Video_Decode_H264_Profile_Info_C
        with Convention => C;

    type Video_Decode_H264_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Decode_H264_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Max_Level_IDC: Extensions.Std_Video.H264.Level_IDC;
        Field_Offset_Granularity: Offset_2D;
    end record
        with Convention => C;

    type Video_Decode_H264_Capabilities_C_Access is
        access Video_Decode_H264_Capabilities_C
        with Convention => C;
 
    type Video_Decode_H264_Session_Parameters_Add_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Decode_H264_Session_Parameters_Add_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_SPS_Count: Interfaces.Unsigned_32;
        Std_SPSs: H264_Sequence_Parameter_Set_Arrays.Pointer;
        Std_PPS_Count: Interfaces.Unsigned_32;
        Std_PPSs: H264_Picture_Parameter_Set_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Decode_H264_Session_Parameters_Add_Info_C_Access is
        access Video_Decode_H264_Session_Parameters_Add_Info_C
        with Convention => C;

    type Video_Decode_H264_Session_Parameters_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Decode_H264_Session_Parameters_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Max_Std_SPS_Count: Interfaces.Unsigned_32;
        Max_Std_PPS_Count: Interfaces.Unsigned_32;
        Parameters_Add_Info:
            Video_Decode_H264_Session_Parameters_Add_Info_C_Access;
    end record
        with Convention => C;

    type Video_Decode_H264_Session_Parameters_Create_Info_C_Access is
        access Video_Decode_H264_Session_Parameters_Create_Info_C
        with Convention => C;

    type Video_Decode_H264_Picture_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_H264_Picture_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Picture_Info: Extensions.Std_Video.H264.Decode.Picture_Info_Access;
        Slice_Count: Interfaces.Unsigned_32;
        Slice_Offsets: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Decode_H264_Picture_Info_C_Access is
        access Video_Decode_H264_Picture_Info_C
        with Convention => C;

    type Video_Decode_H264_DPB_Slot_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_H264_DPB_Slot_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Reference_Info:
            Extensions.Std_Video.H264.Decode.Reference_Info_Access;
    end record
        with Convention => C;

    type Video_Decode_H264_DPB_Slot_Info_C_Access is
        access Video_Decode_H264_DPB_Slot_Info_C
        with Convention => C;

    type Rendering_Fragment_Shading_Rate_Attachment_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Rendering_Fragment_Shading_Rate_Attachment_Info_Type;
        Next: C.In_Structure_C_Access;
        Image_View: Vulkan.Image_View;
        Image_Layout: Vulkan.Image_Layout;
        Shading_Rate_Attachment_Texel_Size: Extent_2D;
    end record
        with Convention => C;

    type Rendering_Fragment_Shading_Rate_Attachment_Info_C_Access is
        access Rendering_Fragment_Shading_Rate_Attachment_Info_C
        with Convention => C;

    type Import_Memory_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Import_Memory_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Type: External_Memory_Handle_Type_Flags;
        FD: File_Descriptor;
    end record
        with Convention => C;

    type Import_Memory_FD_Info_C_Access is access Import_Memory_FD_Info_C
        with Convention => C;

    type Memory_FD_Properties_C is
    record
        Record_Type: Out_Structure_Type := Memory_FD_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Memory_Type_Bits: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Memory_FD_Properties_C_Access is access Memory_FD_Properties_C
        with Convention => C;

    type Memory_Get_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Get_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Memory: Device_Memory;
        Handle_Type: External_Memory_Handle_Type_Flags;
    end record
        with Convention => C;

    type Memory_Get_FD_Info_C_Access is access Memory_Get_FD_Info_C
        with Convention => C;

    type Import_Semaphore_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Import_Semaphore_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Semaphore: Vulkan.Semaphore;
        Flags: Semaphore_Import_Flags;
        Handle_Type: External_Semaphore_Handle_Type_Flags;
        FD: File_Descriptor;
    end record
        with Convention => C;

    type Import_Semaphore_FD_Info_C_Access is access Import_Semaphore_FD_Info_C
        with Convention => C;

    type Semaphore_Get_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Semaphore_Get_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Semaphore: Vulkan.Semaphore;
        Handle_Type: External_Semaphore_Handle_Type_Flags;
    end record
        with Convention => C;

    type Semaphore_Get_FD_Info_C_Access is access Semaphore_Get_FD_Info_C
        with Convention => C;

    type Physical_Device_Push_Descriptor_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Push_Descriptor_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Push_Descriptors: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Push_Descriptor_Properties_C_Access is
        access Physical_Device_Push_Descriptor_Properties_C
        with Convention => C;

    package Rect_Layer_Arrays is new C_Arrays(Extensions.KHR.Rect_Layer);

    type Present_Region_C is
    record
        Rectangle_Count: Interfaces.Unsigned_32;
        Rectangles: Rect_Layer_Arrays.Pointer;
    end record
        with Convention => C;

    package Present_Region_C_Arrays is new C_Arrays(Present_Region_C);

    type Present_Regions_C is
    record
        Record_Type: In_Structure_Type := Present_Regions_Type;
        Next: C.In_Structure_C_Access;
        Swapchain_Count: Interfaces.Unsigned_32;
        Regions: Present_Region_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Present_Regions_C_Access is access Present_Regions_C
        with Convention => C;

    type Shared_Present_Surface_Capabilities_C is
    record
        Record_Type: Out_Structure_Type :=
            Shared_Present_Surface_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Shared_Present_Supported_Usage_Flags: Image_Usage_Flags;
    end record 
        with Convention => C;

    type Shared_Present_Surface_Capabilities_C_Access is
        access Shared_Present_Surface_Capabilities_C
        with Convention => C;

    type Import_Fence_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Import_Fence_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Fence: Vulkan.Fence;
        Flags: Fence_Import_Flags;
        Handle_Type: External_Fence_Handle_Type_Flags;
        FD: File_Descriptor;
    end record
        with Convention => C;

    type Import_Fence_FD_Info_C_Access is access Import_Fence_FD_Info_C
        with Convention => C;

    type Fence_Get_FD_Info_C is
    record
        Record_Type: In_Structure_Type := Fence_Get_FD_Info_Type;
        Next: C.In_Structure_C_Access;
        Fence: Vulkan.Fence;
        Handle_Type: External_Fence_Handle_Type_Flags;
    end record
        with Convention => C;

    type Fence_Get_FD_Info_C_Access is access Fence_Get_FD_Info_C
        with Convention => C;

    type Physical_Device_Performance_Query_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Performance_Query_Features_Type;
        Next: C.Out_Structure_C_Access;
        Performance_Counter_Query_Pools: Interfaces.Unsigned_32;
        Performance_Counter_Multiple_Query_Pools: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Performance_Query_Features_C_Access is
        access Physical_Device_Performance_Query_Features_C
        with Convention => C;

    type Physical_Device_Performance_Query_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Performance_Query_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Allow_Command_Buffer_Query_Copies: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Performance_Query_Properties_C_Access is
        access Physical_Device_Performance_Query_Properties_C
        with Convention => C;

    type Performance_Counter_C is
    record
        Record_Type: Out_Structure_Type := Performance_Counter_Type;
        Next: C.Out_Structure_C_Access;
        Unit: Extensions.KHR.Performance_Counter_Unit;
        Scope: Extensions.KHR.Performance_Counter_Scope;
        Storage: Extensions.KHR.Performance_Counter_Storage;
        UUID: Vulkan.UUID;
    end record
        with Convention => C;

    type Performance_Counter_C_Access is access Performance_Counter_C
        with Convention => C;

    type Performance_Counter_Description_C is
    record
        Record_Type: Out_Structure_Type := Performance_Counter_Description_Type;
        Next: C.Out_Structure_C_Access;
        Flags: Extensions.KHR.Performance_Counter_Description_Flags;
        Name: Interfaces.C.char_array(1 .. Max_Description_Size);
        Category: Interfaces.C.char_array(1 .. Max_Description_Size);
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
    end record
        with Convention => C;

    type Performance_Counter_Description_C_Access is
        access Performance_Counter_Description_C
        with Convention => C;

    type Query_Pool_Performance_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Query_Pool_Performance_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Queue_Family_Index: Vulkan.Queue_Family_Index;
        Counter_Index_Count: Interfaces.Unsigned_32;
        Counter_Indices: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Query_Pool_Performance_Create_Info_C_Access is
        access Query_Pool_Performance_Create_Info_C
        with Convention => C;

    type Acquire_Profiling_Lock_Info_C is
    record
        Record_Type: In_Structure_Type := Acquire_Profiling_Lock_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Acquire_Profiling_Lock_Flags;
        Timeout: Interfaces.Unsigned_64;
    end record
        with Convention => C;

    type Acquire_Profiling_Lock_Info_C_Access is
        access Acquire_Profiling_Lock_Info_C
        with Convention => C;

    type Performance_Query_Submit_Info_C is
    record
        Record_Type: In_Structure_Type := Performance_Query_Submit_Info_Type;
        Next: C.In_Structure_C_Access;
        Counter_Pass_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Performance_Query_Submit_Info_C_Access is
        access Performance_Query_Submit_Info_C
        with Convention => C;

    type Physical_Device_Surface_Info_2_C is
    record
        Record_Type: In_Structure_Type := Physical_Device_Surface_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Surface: Extensions.KHR.Surface;
    end record
        with Convention => C;

    type Physical_Device_Surface_Info_2_C_Access is
        access Physical_Device_Surface_Info_2_C
        with Convention => C;

    type Surface_Capabilities_2_C is
    record
        Record_Type: Out_Structure_Type := Surface_Capabilities_2_Type;
        Next: C.Out_Structure_C_Access;
        Surface_Capabilities: Extensions.KHR.Surface_Capabilities;
    end record
        with Convention => C;

    type Surface_Capabilities_2_C_Access is access Surface_Capabilities_2_C
        with Convention => C;

    type Surface_Format_2_C is
    record
        Record_Type: Out_Structure_Type := Surface_Format_2_Type;
        Next: C.Out_Structure_C_Access;
        Surface_Format: Extensions.KHR.Surface_Format;
    end record
        with Convention => C;

    type Surface_Format_2_C_Access is access Surface_Format_2_C
        with Convention => C;

    type Display_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Display_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Display_Properties: C_KHR.Display_Properties_C;
    end record
        with Convention => C;

    type Display_Properties_2_C_Access is access Display_Properties_2_C
        with Convention => C;

    type Display_Plane_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Display_Plane_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Display_Plane_Properties: Extensions.KHR.Display_Plane_Properties;
    end record
        with Convention => C;

    type Display_Plane_Properties_2_C_Access is
        access Display_Plane_Properties_2_C
        with Convention => C;

    type Display_Mode_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Display_Mode_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Display_Mode_Properties: Extensions.KHR.Display_Mode_Properties;
    end record
        with Convention => C;

    type Display_Mode_Properties_2_C_Access is
        access Display_Mode_Properties_2_C
        with Convention => C;

    type Display_Plane_Info_2_C is
    record
        Record_Type: In_Structure_Type := Display_Plane_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Mode: Extensions.KHR.Display_Mode;
        Plane_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Display_Plane_Info_2_C_Access is access Display_Plane_Info_2_C
        with Convention => C;

    type Display_Plane_Capabilities_2_C is
    record
        Record_Type: Out_Structure_Type := Display_Plane_Capabilities_2_Type;
        Next: C.Out_Structure_C_Access;
        Capabilities: Extensions.KHR.Display_Plane_Capabilities;
    end record
        with Convention => C;

    type Display_Plane_Capabilities_2_C_Access is
        access Display_Plane_Capabilities_2_C
        with Convention => C;

    type Physical_Device_Shader_Clock_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Clock_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Subgroup_Clock: Interfaces.Unsigned_32;
        Shader_Device_Clock: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Clock_Features_C_Access is
        access Physical_Device_Shader_Clock_Features_C
        with Convention => C;

    type Video_Decode_H265_Profile_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_H265_Profile_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Profile_IDC: Extensions.Std_Video.H265.Profile_IDC;
    end record
        with Convention => C;

    type Video_Decode_H265_Profile_Info_C_Access is
        access Video_Decode_H265_Profile_Info_C
        with Convention => C;

    type Video_Decode_H265_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Decode_H265_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Max_Level_IDC: Extensions.Std_Video.H265.Level_IDC;
    end record
        with Convention => C;

    type Video_Decode_H265_Capabilities_C_Access is
        access Video_Decode_H265_Capabilities_C
        with Convention => C;

    type Video_Decode_H265_Session_Parameters_Add_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Decode_H265_Session_Parameters_Add_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_VPS_Count: Interfaces.Unsigned_32;
        Std_VPSs: H265_Video_Parameter_Set_Arrays.Pointer;
        Std_SPS_Count: Interfaces.Unsigned_32;
        Std_SPSs: H265_Sequence_Parameter_Set_Arrays.Pointer;
        Std_PPS_Count: Interfaces.Unsigned_32;
        Std_PPSs: H265_Picture_Parameter_Set_Arrays.Pointer;
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
        Std_Picture_Info: Extensions.Std_Video.H265.Decode.Picture_Info_Access;
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
        Std_Reference_Info:
            Extensions.Std_Video.H265.Decode.Reference_Info_Access;
    end record
        with Convention => C;

    type Video_Decode_H265_DPB_Slot_Info_C_Access is
        access Video_Decode_H265_DPB_Slot_Info_C
        with Convention => C;

    type Device_Queue_Global_Priority_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Device_Queue_Global_Priority_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Global_Priority: Extensions.KHR.Queue_Global_Priority;
    end record
        with Convention => C;

    type Device_Queue_Global_Priority_Create_Info_C_Access is
        access Device_Queue_Global_Priority_Create_Info_C
        with Convention => C;

    type Physical_Device_Global_Priority_Query_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Global_Priority_Query_Features_Type;
        Next: C.Out_Structure_C_Access;
        Global_Priority_Query: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Global_Priority_Query_Features_C_Access is
        access Physical_Device_Global_Priority_Query_Features_C
        with Convention => C;

    type Queue_Family_Global_Priority_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Queue_Family_Global_Priority_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Priority_Count: Interfaces.Unsigned_32;
        Priorities: Extensions.KHR.Queue_Global_Priority_Array;
    end record
        with Convention => C;

    type Queue_Family_Global_Priority_Properties_C_Access is
        access Queue_Family_Global_Priority_Properties_C
        with Convention => C;

    type Fragment_Shading_Rate_Attachment_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Fragment_Shading_Rate_Attachment_Info_Type;
        Next: C.In_Structure_C_Access;
        Fragment_Shading_Rate_Attachment:
            C_V1_2.Attachment_Reference_2_C_Access;
        Shading_Rate_Attachment_Texel_Size: Extent_2D;
    end record
        with Convention => C;

    type Fragment_Shading_Rate_Attachment_Info_C_Access is
        access Fragment_Shading_Rate_Attachment_Info_C
        with Convention => C;

    type Pipeline_Fragment_Shading_Rate_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Fragment_Shading_Rate_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Fragment_Size: Extent_2D;
        Combiner_Ops: Extensions.KHR.Fragment_Shading_Rate_Combiner_Op_Array;
    end record
        with Convention => C;

    type Pipeline_Fragment_Shading_Rate_State_Create_Info_C_Access is
        access Pipeline_Fragment_Shading_Rate_State_Create_Info_C
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Fragment_Shading_Rate_Features_Type;
        Next: C.Out_Structure_C_Access;
        Pipeline_Fragment_Shading_Rate: Interfaces.Unsigned_32;
        Primitive_Fragment_Shading_Rate: Interfaces.Unsigned_32;
        Attachment_Fragment_Shading_Rate: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_Features_C_Access is
        access Physical_Device_Fragment_Shading_Rate_Features_C
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Fragment_Shading_Rate_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Min_Fragment_Shading_Rate_Attachment_Texel_Size: Extent_2D;
        Max_Fragment_Shading_Rate_Attachment_Texel_Size: Extent_2D;
        Max_Fragment_Shading_Rate_Attachment_Texel_Size_Aspect_Ratio:
            Interfaces.Unsigned_32;
        Primitive_Fragment_Shading_Rate_With_Multiple_Viewports:
            Interfaces.Unsigned_32;
        Layered_Shading_Rate_Attachments: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Non_Trivial_Combiner_Ops: Interfaces.Unsigned_32;
        Max_Fragment_Size: Extent_2D;
        Max_Fragment_Size_Aspect_Ratio: Interfaces.Unsigned_32;
        Max_Fragment_Shading_Rate_Coverage_Samples: Interfaces.Unsigned_32;
        Max_Fragment_Shading_Rate_Rasterization_Samples: Sample_Count_Flags;
        Fragment_Shading_Rate_With_Shader_Depth_Stencil_Writes:
            Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Sample_Mask: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Shader_Sample_Mask: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Conservative_Rasterization:
            Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Fragment_Shader_Interlock:
            Interfaces.Unsigned_32;
        Fragment_Shading_Rate_With_Custom_Sample_Locations:
            Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Strict_Multiply_Combiner: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_Properties_C_Access is
        access Physical_Device_Fragment_Shading_Rate_Properties_C
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Fragment_Shading_Rate_Type;
        Next: C.Out_Structure_C_Access;
        Sample_Counts: Sample_Count_Flags;
        Fragment_Size: Extent_2D;
    end record
        with Convention => C;

    type Physical_Device_Fragment_Shading_Rate_C_Access is
        access Physical_Device_Fragment_Shading_Rate_C
        with Convention => C;

    type Physical_Device_Dynamic_Rendering_Local_Read_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Dynamic_Rendering_Local_Read_Features_Type;
        Next: C.Out_Structure_C_Access;
        Dynamic_Rendering_Local_Read: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Dynamic_Rendering_Local_Read_Features_C_Access is
        access Physical_Device_Dynamic_Rendering_Local_Read_Features_C
        with Convention => C;

    type Rendering_Attachment_Location_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Rendering_Attachment_Location_Info_Type;
        Next: C.In_Structure_C_Access;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Locations: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Rendering_Attachment_Location_Info_C_Access is
        access Rendering_Attachment_Location_Info_C
        with Convention => C;

    type Rendering_Input_Attachment_Index_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Rendering_Input_Attachment_Index_Info_Type;
        Next: C.In_Structure_C_Access;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Input_Indices: C.Uint32_t_Arrays.Pointer;
        Depth_Input_Attachment_Index: Extensions.KHR.Unsigned_32_Access;
        Stencil_Input_Attachment_Index: Extensions.KHR.Unsigned_32_Access;
    end record
        with Convention => C;

    type Rendering_Input_Attachment_Index_Info_C_Access is
        access Rendering_Input_Attachment_Index_Info_C
        with Convention => C;

    type Physical_Device_Shader_Quad_Control_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Quad_Control_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Quad_Control: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Quad_Control_Features_C_Access is
        access Physical_Device_Shader_Quad_Control_Features_C
        with Convention => C;

    type Surface_Protected_Capabilities_C is
    record
        Record_Type: In_Structure_Type := Surface_Protected_Capabilities_Type;
        Next: C.In_Structure_C_Access;
        Supports_Protected: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Surface_Protected_Capabilities_C_Access is
        access Surface_Protected_Capabilities_C
        with Convention => C;

    type Physical_Device_Present_Wait_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Present_Wait_Features_Type;
        Next: C.Out_Structure_C_Access;
        Present_Wait: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Present_Wait_Features_C_Access is
        access Physical_Device_Present_Wait_Features_C
        with Convention => C;

    type Physical_Device_Pipeline_Executable_Properties_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Executable_Properties_Features_Type;
        Next: C.Out_Structure_C_Access;
        Pipeline_Executable_Info: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Executable_Properties_Features_C_Access is
        access Physical_Device_Pipeline_Executable_Properties_Features_C
        with Convention => C;

    type Pipeline_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Info_Type;
        Next: C.In_Structure_C_Access;
        Pipeline: Vulkan.Pipeline;
    end record
        with Convention => C;

    type Pipeline_Info_C_Access is access Pipeline_Info_C
        with Convention => C;

    type Pipeline_Executable_Properties_C is
    record
        Record_Type: Out_Structure_Type := Pipeline_Executable_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Stages: Shader_Stage_Flags;
        Name: Interfaces.C.char_array(1 .. Max_Description_Size);
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
        Subgroup_Size: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Executable_Properties_C_Access is
        access Pipeline_Executable_Properties_C
        with Convention => C;

    type Pipeline_Executable_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Executable_Info_Type;
        Next: C.In_Structure_C_Access;
        Pipeline: Vulkan.Pipeline;
        Executable_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Executable_Info_C_Access is access Pipeline_Executable_Info_C
        with Convention => C;

    type Pipeline_Executable_Statistic_C
        (Format_Type: Extensions.KHR.Pipeline_Executable_Statistic_Format) is
    record
        Record_Type: Out_Structure_Type := Pipeline_Executable_Statistic_Type;
        Next: C.Out_Structure_C_Access;
        Name: Interfaces.C.char_array(1 .. Max_Description_Size);
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
        Format: Extensions.KHR.Pipeline_Executable_Statistic_Format :=
            Format_Type;

        case Format_Type is
            when Extensions.KHR.Bool32 =>
                B32: Interfaces.Unsigned_32;
            when Extensions.KHR.Int64 =>
                I64: Interfaces.Integer_64;
            when Extensions.KHR.Uint64 =>
                U64: Interfaces.Unsigned_64;
            when Extensions.KHR.Float64 =>
                F64: Interfaces.C.double;
        end case;
    end record
        with Convention => C,
             Unchecked_Union;

    type Pipeline_Executable_Statistic_C_Access is
        access Pipeline_Executable_Statistic_C
        with Convention => C;

    type Pipeline_Executable_Internal_Representation_C is
    record
        Record_Type: Out_Structure_Type :=
            Pipeline_Executable_Internal_Representation_Type;
        Next: C.Out_Structure_C_Access;
        Name: Interfaces.C.char_array(1 .. Max_Description_Size);
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
        Is_Text: Interfaces.Unsigned_32;
        Data_Size: Interfaces.C.size_t;
        Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Pipeline_Executable_Internal_Representation_C_Access is
        access Pipeline_Executable_Internal_Representation_C
        with Convention => C;

    type Memory_Map_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Map_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Memory_Map_Flags;
        Memory: Device_Memory;
        Offset: Device_Size;
        Size: Device_Size;
    end record
        with Convention => C;

    type Memory_Map_Info_C_Access is access Memory_Map_Info_C
        with Convention => C;

    type Memory_Unmap_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Unmap_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Memory_Unmap_Flags;
        Memory: Device_Memory;
    end record
        with Convention => C;

    type Memory_Unmap_Info_C_Access is access Memory_Unmap_Info_C
        with Convention => C;

    package Pipeline_Arrays is new C_Arrays(Pipeline);

    type Pipeline_Library_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Library_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Library_Count: Interfaces.Unsigned_32;
        Libraries: Pipeline_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Library_Create_Info_C_Access is
        access Pipeline_Library_Create_Info_C
        with Convention => C;

    package Uint64_t_Arrays is new C_Arrays(Interfaces.Unsigned_64);

    type Present_ID_C is
    record
        Record_Type: In_Structure_Type := Present_ID_Type;
        Next: C.In_Structure_C_Access;
        Swapchain_Count: Interfaces.Unsigned_32;
        Present_IDs: Uint64_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Present_ID_C_Access is access Present_ID_C
        with Convention => C;
 
    type Physical_Device_Present_ID_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Present_ID_Features_Type;
        Next: C.Out_Structure_C_Access;
        Present_ID: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Present_ID_Features_C_Access is
        access Physical_Device_Present_ID_Features_C
        with Convention => C;

    type Video_Encode_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Video_Encode_Flags;
        Dst_Buffer: Buffer;
        Dst_Buffer_Offset: Device_Size;
        Dst_Buffer_Range: Device_Size;
        Src_Picture_Resource: Video_Picture_Resource_Info_C;
        Setup_Reference_Slot: Video_Reference_Slot_Info_C_Access;
        Reference_Slot_Count: Interfaces.Unsigned_32;
        Reference_Slots: Video_Reference_Slot_Info_C_Arrays.Pointer;
        Preceding_Externally_Encoded_Bytes: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_Info_C_Access is access Video_Encode_Info_C
        with Convention => C;

    type Video_Encode_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Encode_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Flags: Extensions.KHR.Video_Encode_Capability_Flags;
        Rate_Control_Modes: Extensions.KHR.Video_Encode_Rate_Control_Mode_Flags;
        Max_Rate_Control_Layers: Interfaces.Unsigned_32;
        Max_Bitrate: Interfaces.Unsigned_64;
        Max_Quality_Levels: Interfaces.Unsigned_32;
        Encode_Input_Picture_Granularity: Extent_2D;
        Supported_Encode_Feedback_Flags:
            Extensions.KHR.Video_Encode_Feedback_Flags;
    end record
        with Convention => C;

    type Video_Encode_Capabilities_C_Access is
        access Video_Encode_Capabilities_C
        with Convention => C;

    type Query_Pool_Video_Encode_Feedback_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Query_Pool_Video_Encode_Feedback_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Encode_Feedback_Flags: Extensions.KHR.Video_Encode_Feedback_Flags;
    end record
        with Convention => C;

    type Query_Pool_Video_Encode_Feedback_Create_Info_C_Access is
        access Query_Pool_Video_Encode_Feedback_Create_Info_C
        with Convention => C;

    type Video_Encode_Usage_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_Usage_Info_Type;
        Next: C.In_Structure_C_Access;
        Video_Usage_Hints: Extensions.KHR.Video_Encode_Usage_Flags;
        Video_Content_Hints: Extensions.KHR.Video_Encode_Content_Flags;
        Tuning_Mode: Extensions.KHR.Video_Encode_Tuning_Mode;
    end record
        with Convention => C;

    type Video_Encode_Usage_Info_C_Access is access Video_Encode_Usage_Info_C
        with Convention => C;

    type Video_Encode_Rate_Control_Layer_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_Rate_Control_Layer_Info_Type;
        Next: C.In_Structure_C_Access;
        Average_Bitrate: Interfaces.Unsigned_64;
        Max_Bitrate: Interfaces.Unsigned_64;
        Frame_Rate_Numerator: Interfaces.Unsigned_32;
        Frame_Rate_Denominator: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_Rate_Control_Layer_Info_C_Access is
        access Video_Encode_Rate_Control_Layer_Info_C
        with Convention => C;

    package Video_Encode_Rate_Control_Layer_Info_C_Arrays is
        new C_Arrays(Video_Encode_Rate_Control_Layer_Info_C);

    type Video_Encode_Rate_Control_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_Rate_Control_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Video_Encode_Rate_Control_Flags;
        Rate_Control_Mode: Extensions.KHR.Video_Encode_Rate_Control_Mode_Flags;
        Layer_Count: Interfaces.Unsigned_32;
        Layers: Video_Encode_Rate_Control_Layer_Info_C_Arrays.Pointer;
        Virtual_Buffer_Size_In_Ms: Interfaces.Unsigned_32;
        Initial_Virtual_Buffer_Size_In_Ms: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_Rate_Control_Info_C_Access is
        access Video_Encode_Rate_Control_Info_C
        with Convention => C;

    type Physical_Device_Video_Encode_Quality_Level_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Physical_Device_Video_Encode_Quality_Level_Info_Type;
        Next: C.In_Structure_C_Access;
        Video_Profile: Video_Profile_Info_C_Access;
        Quality_Level: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Video_Encode_Quality_Level_Info_C_Access is
        access Physical_Device_Video_Encode_Quality_Level_Info_C
        with Convention => C;

    type Video_Encode_Quality_Level_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Video_Encode_Quality_Level_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Preferred_Rate_Control_Mode:
            Extensions.KHR.Video_Encode_Rate_Control_Mode_Flags;
        Preferred_Rate_Control_Layer_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_Quality_Level_Properties_C_Access is
        access Video_Encode_Quality_Level_Properties_C
        with Convention => C;

    type Video_Encode_Quality_Level_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Encode_Quality_Level_Info_Type;
        Next: C.In_Structure_C_Access;
        Quality_Level: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_Quality_Level_Info_C_Access is
        access Video_Encode_Quality_Level_Info_C
        with Convention => C;

    type Video_Encode_Session_Parameters_Get_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Encode_Session_Parameters_Get_Info_Type;
        Next: C.In_Structure_C_Access;
        Video_Session_Parameters: Extensions.KHR.Video_Session_Parameters;
    end record
        with Convention => C;

    type Video_Encode_Session_Parameters_Get_Info_C_Access is
        access Video_Encode_Session_Parameters_Get_Info_C
        with Convention => C;

    type Video_Encode_Session_Parameters_Feedback_Info_C is
    record
        Record_Type: Out_Structure_Type :=
            Video_Encode_Session_Parameters_Feedback_Info_Type;
        Next: C.Out_Structure_C_Access;
        Has_Overrides: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_Session_Parameters_Feedback_Info_C_Access is
        access Video_Encode_Session_Parameters_Feedback_Info_C
        with Convention => C;

    type Physical_Device_Fragment_Shader_Barycentric_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Fragment_Shader_Barycentric_Features_Type;
        Next: C.Out_Structure_C_Access;
        Fragment_Shader_Barycentric: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Fragment_Shader_Barycentric_Features_C_Access is
        access Physical_Device_Fragment_Shader_Barycentric_Features_C
        with Convention => C;

    type Physical_Device_Fragment_Shader_Barycentric_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Fragment_Shader_Barycentric_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Tri_Strip_Vertex_Order_Independent_Of_Provoking_Vertex:
            Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Fragment_Shader_Barycentric_Properties_C_Access is
        access Physical_Device_Fragment_Shader_Barycentric_Properties_C
        with Convention => C;

    type Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Subgroup_Uniform_Control_Flow: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C_Access
        is access
            Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C
        with Convention => C;

    type Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type;
        Next: C.Out_Structure_C_Access;
        Workgroup_Memory_Explicit_Layout: Interfaces.Unsigned_32;
        Workgroup_Memory_Explicit_Layout_Scalar_Block_Layout:
            Interfaces.Unsigned_32;
        Workgroup_Memory_Explicit_Layout_8_Bit_Access: Interfaces.Unsigned_32;
        Workgroup_Memory_Explicit_Layout_16_Bit_Access: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C_Access is
        access Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C
        with Convention => C;

    type Physical_Device_Ray_Tracing_Maintenance_1_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Ray_Tracing_Maintenance_1_Features_Type;
        Next: C.Out_Structure_C_Access;
        Ray_Tracing_Maintenance_1: Interfaces.Unsigned_32;
        Ray_Tracing_Pipeline_Trace_Rays_Indirect_2: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Ray_Tracing_Maintenance_1_Features_C_Access is
        access Physical_Device_Ray_Tracing_Maintenance_1_Features_C
        with Convention => C;

    type Physical_Device_Shader_Maximal_Reconvergence_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Maximal_Reconvergence_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Maximal_Reconvergence: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Maximal_Reconvergence_Features_C_Access is
        access Physical_Device_Shader_Maximal_Reconvergence_Features_C
        with Convention => C;

    type Physical_Device_Maintenance_5_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_5_Features_Type;
        Next: C.Out_Structure_C_Access;
        Maintenance_5: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_5_Features_C_Access is
        access Physical_Device_Maintenance_5_Features_C
        with Convention => C;

    type Physical_Device_Maintenance_5_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_5_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Early_Fragment_Multisample_Coverage_After_Sample_Counting:
            Interfaces.Unsigned_32;
        Early_Fragment_Sample_Mask_Test_Before_Sample_Counting:
            Interfaces.Unsigned_32;
        Depth_Stencil_Swizzle_One_Support: Interfaces.Unsigned_32;
        Polygon_Mode_Point_Size: Interfaces.Unsigned_32;
        Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram:
            Interfaces.Unsigned_32;
        Non_Strict_Wide_Lines_Use_Parallelogram: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_5_Properties_C_Access is
        access Physical_Device_Maintenance_5_Properties_C
        with Convention => C;

    type Rendering_Area_Info_C is
    record
        Record_Type: In_Structure_Type := Rendering_Area_Info_Type;
        Next: C.In_Structure_C_Access;
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Formats: C_V1_2.Format_Arrays.Pointer;
        Depth_Attachment_Format: Format;
        Stencil_Attachment_Format: Format;
    end record
        with Convention => C;

    type Rendering_Area_Info_C_Access is access Rendering_Area_Info_C
        with Convention => C;

    type Image_Subresource_2_C is
    record
        Record_Type: Out_Structure_Type := Image_Subresource_2_Type;
        Next: C.Out_Structure_C_Access;
        Image_Subresource: Vulkan.Image_Subresource;
    end record
        with Convention => C;

    type Image_Subresource_2_C_Access is access Image_Subresource_2_C
        with Convention => C;

    type Device_Image_Subresource_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Image_Subresource_Info_Type;
        Next: C.In_Structure_C_Access;
        Create_Info: C.Image_Create_Info_C_Access;
        Subresource: Image_Subresource_2_C_Access;
    end record
        with Convention => C;

    type Device_Image_Subresource_Info_C_Access is
        access Device_Image_Subresource_Info_C
        with Convention => C;

    type Subresource_Layout_2_C is
    record
        Record_Type: Out_Structure_Type := Subresource_Layout_2_Type;
        Next: C.Out_Structure_C_Access;
        Subresource_Layout: Vulkan.Subresource_Layout;
    end record
        with Convention => C;

    type Subresource_Layout_2_C_Access is access Subresource_Layout_2_C
        with Convention => C;

    type Pipeline_Create_Flags_2_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Create_Flags_2_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Pipeline_Create_Flags_2;
    end record
        with Convention => C;

    type Pipeline_Create_Flags_2_Create_Info_C_Access is
        access Pipeline_Create_Flags_2_Create_Info_C
        with Convention => C;

    type Buffer_Usage_Flags_2_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Buffer_Usage_Flags_2_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Buffer_Usage_Flags_2;
    end record
        with Convention => C;

    type Buffer_Usage_Flags_2_Create_Info_C_Access is
        access Buffer_Usage_Flags_2_Create_Info_C
        with Convention => C;

    type Physical_Device_Ray_Tracing_Position_Fetch_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Ray_Tracing_Position_Fetch_Features_Type;
        Next: C.Out_Structure_C_Access;
        Ray_Tracing_Position_Fetch: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Ray_Tracing_Position_Fetch_Features_C_Access is
        access Physical_Device_Ray_Tracing_Position_Fetch_Features_C
        with Convention => C;

    type Cooperative_Matrix_Properties_C is
    record
        Record_Type: Out_Structure_Type := Cooperative_Matrix_Properties_Type;
        Next: C.Out_Structure_C_Access;
        M_Size: Interfaces.Unsigned_32;
        N_Size: Interfaces.Unsigned_32;
        K_Size: Interfaces.Unsigned_32;
        A_Type: Extensions.KHR.Component_Type;
        B_Type: Extensions.KHR.Component_Type;
        C_Type: Extensions.KHR.Component_Type;
        Result_Type: Extensions.KHR.Component_Type;
        Saturating_Accumulation: Interfaces.Unsigned_32;
        Scope: Extensions.KHR.Scope;
    end record
        with Convention => C;

    type Cooperative_Matrix_Properties_C_Access is
        access Cooperative_Matrix_Properties_C
        with Convention => C;

    type Physical_Device_Cooperative_Matrix_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Cooperative_Matrix_Features_Type;
        Next: C.Out_Structure_C_Access;
        Cooperative_Matrix: Interfaces.Unsigned_32;
        Cooperative_Matrix_Robust_Buffer_Access: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Cooperative_Matrix_Features_C_Access is
        access Physical_Device_Cooperative_Matrix_Features_C
        with Convention => C;

    type Physical_Device_Cooperative_Matrix_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Cooperative_Matrix_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Cooperative_Matrix_Supported_Stages: Shader_Stage_Flags;
    end record
        with Convention => C;

    type Physical_Device_Cooperative_Matrix_Properties_C_Access is
        access Physical_Device_Cooperative_Matrix_Properties_C
        with Convention => C;

    type Video_Decode_AV1_Profile_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_AV1_Profile_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Profile: Extensions.Std_Video.AV1.Profile;
        Film_Grain_Support: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Decode_AV1_Profile_Info_C_Access is
        access Video_Decode_AV1_Profile_Info_C
        with Convention => C;

    type Video_Decode_AV1_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Decode_AV1_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Max_Level: Extensions.Std_Video.AV1.Level;
    end record
        with Convention => C;

    type Video_Decode_AV1_Capabilities_C_Access is
        access Video_Decode_AV1_Capabilities_C
        with Convention => C;

    type Video_Decode_AV1_Session_Parameters_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Video_Decode_AV1_Session_Parameters_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Sequence_Header: Extensions.Std_Video.AV1.Sequence_Header_Access;
    end record
        with Convention => C;

    type Video_Decode_AV1_Session_Parameters_Create_Info_C_Access is
        access Video_Decode_AV1_Session_Parameters_Create_Info_C
        with Convention => C;

    type Video_Decode_AV1_Picture_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_AV1_Picture_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Picture_Info: Extensions.Std_Video.AV1.Decode.Picture_Info_Access;
        Reference_Name_Slot_Indices: Extensions.KHR.References_Per_Frame_Array;
        Frame_Header_Offset: Interfaces.Unsigned_32;
        Tile_Count: Interfaces.Unsigned_32;
        Tile_Offsets: C.Uint32_t_Arrays.Pointer;
        Tile_Sizes: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Decode_AV1_Picture_Info_C_Access is
        access Video_Decode_AV1_Picture_Info_C
        with Convention => C;

    type Video_Decode_AV1_DPB_Slot_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_AV1_DPB_Slot_Info_Type;
        Next: C.In_Structure_C_Access;
        Std_Reference_Info:
            Extensions.Std_Video.AV1.Decode.Reference_Info_Access;
    end record
        with Convention => C;

    type Video_Decode_AV1_DPB_Slot_Info_C_Access is
        access Video_Decode_AV1_DPB_Slot_Info_C
        with Convention => C;

    type Physical_Device_Video_Maintenance_1_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Video_Maintenance_1_Features_Type;
        Next: C.Out_Structure_C_Access;
        Video_Maintenance_1: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Video_Maintenance_1_Features_C_Access is
        access Physical_Device_Video_Maintenance_1_Features_C
        with Convention => C;

    type Video_Inline_Query_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Inline_Query_Info_Type;
        Next: C.In_Structure_C_Access;
        Query_Pool: Vulkan.Query_Pool;
        First_Query: Interfaces.Unsigned_32;
        Query_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Inline_Query_Info_C_Access is access Video_Inline_Query_Info_C
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vertex_Attribute_Divisor_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Vertex_Attrib_Divisor: Interfaces.Unsigned_32;
        Supports_Non_Zero_First_Instance: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Properties_C_Access is
        access Physical_Device_Vertex_Attribute_Divisor_Properties_C
        with Convention => C;

    package Vertex_Input_Binding_Divisor_Description_Arrays is new C_Arrays
        (Extensions.KHR.Vertex_Input_Binding_Divisor_Description);

    type Pipeline_Vertex_Input_Divisor_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Vertex_Input_Divisor_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Vertex_Binding_Divisor_Count: Interfaces.Unsigned_32;
        Vertex_Binding_Divisors:
            Vertex_Input_Binding_Divisor_Description_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Vertex_Input_Divisor_State_Create_Info_C_Access is
        access Pipeline_Vertex_Input_Divisor_State_Create_Info_C
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vertex_Attribute_Divisor_Features_Type;
        Next: C.Out_Structure_C_Access;
        Vertex_Attribute_Instance_Rate_Divisor: Interfaces.Unsigned_32;
        Vertex_Attribute_Instance_Rate_Zero_Divisor: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Features_C_Access is
        access Physical_Device_Vertex_Attribute_Divisor_Features_C
        with Convention => C;

    type Calibrated_Timestamp_Info_C is
    record
        Record_Type: In_Structure_Type := Calibrated_Timestamp_Info_Type;
        Next: C.In_Structure_C_Access;
        Time_Domain: Extensions.KHR.Time_Domain;
    end record
        with Convention => C;

    type Calibrated_Timestamp_Info_C_Access is
        access Calibrated_Timestamp_Info_C
        with Convention => C;

    type Physical_Device_Maintenance_6_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_6_Features_Type;
        Next: C.Out_Structure_C_Access;
        Maintenance_6: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_6_Features_C_Access is
        access Physical_Device_Maintenance_6_Features_C
        with Convention => C;

    type Physical_Device_Maintenance_6_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_6_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Block_Texel_View_Compatible_Multiple_Layers: Interfaces.Unsigned_32;
        Max_Combined_Image_Sampler_Descriptor_Count: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Clamp_Combiner_Inputs: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_6_Properties_C_Access is
        access Physical_Device_Maintenance_6_Properties_C
        with Convention => C;
    
    type Bind_Memory_Status_C is
    record
        Record_Type: In_Structure_Type := Bind_Memory_Status_Type;
        Next: C.In_Structure_C_Access;
        Result: Extensions.KHR.Result_Access;
    end record
        with Convention => C;

    type Bind_Memory_Status_C_Access is access Bind_Memory_Status_C
        with Convention => C;

    package Descriptor_Set_Arrays is new C_Arrays(Descriptor_Set);

    type Bind_Descriptor_Sets_Info_C is
    record
        Record_Type: In_Structure_Type := Bind_Descriptor_Sets_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        First_Set: Interfaces.Unsigned_32;
        Descriptor_Set_Count: Interfaces.Unsigned_32;
        Descriptor_Sets: Descriptor_Set_Arrays.Pointer;
        Dynamic_Offset_Count: Interfaces.Unsigned_32;
        Dynamic_Offsets: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Bind_Descriptor_Sets_Info_C_Access is
        access Bind_Descriptor_Sets_Info_C
        with Convention => C;

    type Push_Constants_Info_C is
    record
        Record_Type: In_Structure_Type := Push_Constants_Info_Type;
        Next: C.In_Structure_C_Access;
        Layout: Pipeline_Layout;
        Stage_Flags: Shader_Stage_Flags;
        Offset: Interfaces.Unsigned_32;
        Size: Interfaces.Unsigned_32;
        Values: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Push_Constants_Info_C_Access is access Push_Constants_Info_C
        with Convention => C;

    package Write_Descriptor_Set_C_Arrays is
        new C_Arrays(C.Write_Descriptor_Set_C);

    type Push_Descriptor_Set_Info_C is
    record
        Record_Type: In_Structure_Type := Push_Descriptor_Set_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
        Descriptor_Write_Count: Interfaces.Unsigned_32;
        Descriptor_Writes: Write_Descriptor_Set_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Push_Descriptor_Set_Info_C_Access is
        access Push_Descriptor_Set_Info_C
        with Convention => C;

    type Push_Descriptor_Set_With_Template_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Push_Descriptor_Set_With_Template_Info_Type;
        Next: C.In_Structure_C_Access;
        Descriptor_Update_Template: Vulkan.Descriptor_Update_Template;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
        Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Push_Descriptor_Set_With_Template_Info_C_Access is
        access Push_Descriptor_Set_With_Template_Info_C
        with Convention => C;

    package Device_Size_Arrays is new C_Arrays(Device_Size);

    type Set_Descriptor_Buffer_Offsets_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Set_Descriptor_Buffer_Offsets_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        First_Set: Interfaces.Unsigned_32;
        Set_Count: Interfaces.Unsigned_32;
        Buffer_Indices: C.Uint32_t_Arrays.Pointer;
        Offsets: Device_Size_Arrays.Pointer;
    end record
        with Convention => C;

    type Set_Descriptor_Buffer_Offsets_Info_C_Access is
        access Set_Descriptor_Buffer_Offsets_Info_C
        with Convention => C;

    type Bind_Descriptor_Buffer_Embedded_Samplers_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Bind_Descriptor_Buffer_Embedded_Samplers_Info_C_Access is
        access Bind_Descriptor_Buffer_Embedded_Samplers_Info_C
        with Convention => C;

    type Xlib_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Xlib_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Xlib_Surface_Create_Flags;
        Dpy: Xlib.Display;
        Window: Xlib.Window;
    end record
        with Convention => C;

    type Xlib_Surface_Create_Info_C_Access is access Xlib_Surface_Create_Info_C
        with Convention => C;

    type Xcb_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Xcb_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Xcb_Surface_Create_Flags;
        Connection: Xcb.Connection;
        Window: Xcb.Window;
    end record
        with Convention => C;

    type Xcb_Surface_Create_Info_C_Access is access Xcb_Surface_Create_Info_C
        with Convention => C;

    type Wayland_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Wayland_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Wayland_Surface_Create_Flags;
        Display: Wayland.Display;
        Surface: Wayland.Surface;
    end record
        with Convention => C;

    type Wayland_Surface_Create_Info_C_Access is
        access Wayland_Surface_Create_Info_C
        with Convention => C;
    
    type Win32_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Win32_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.KHR.Win32_Surface_Create_Flags;
        Instance: Win32.Instance;
        Window: Win32.Window;
    end record
        with Convention => C;

    type Win32_Surface_Create_Info_C_Access is
        access Win32_Surface_Create_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Extensions.KHR.Swapchain_Create_Info)
        return Swapchain_Create_Info_C;
    procedure Free(Struct: in out Swapchain_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Present_Info) return Present_Info_C;
    procedure Free(Struct: in out Present_Info_C);

    function To_C(Struct: in Extensions.KHR.Image_Swapchain_Create_Info)
        return Image_Swapchain_Create_Info_C;
    procedure Free(Struct: in out Image_Swapchain_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Bind_Image_Memory_Swapchain_Info)
        return Bind_Image_Memory_Swapchain_Info_C;
    procedure Free(Struct: in out Bind_Image_Memory_Swapchain_Info_C);

    function To_C(Struct: in Extensions.KHR.Acquire_Next_Image_Info)
        return Acquire_Next_Image_Info_C;
    procedure Free(Struct: in out Acquire_Next_Image_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Device_Group_Present_Capabilities;
         C_Struct: in Device_Group_Present_Capabilities_C);

    function To_C(Struct: in Extensions.KHR.Device_Group_Present_Info)
        return Device_Group_Present_Info_C;
    procedure Free(Struct: in out Device_Group_Present_Info_C);

    function To_C(Struct: in Extensions.KHR.Device_Group_Swapchain_Create_Info)
        return Device_Group_Swapchain_Create_Info_C;
    procedure Free(Struct: in out Device_Group_Swapchain_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Display_Mode_Create_Info)
        return Display_Mode_Create_Info_C;
    procedure Free(Struct: in out Display_Mode_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Display_Properties)
        return Display_Properties_C;
    function To_Ada(DPC: Display_Properties_C)
        return Extensions.KHR.Display_Properties;
    procedure Free(Struct: in out Display_Properties_C);

    function To_C(Struct: in Extensions.KHR.Display_Surface_Create_Info)
        return Display_Surface_Create_Info_C;
    procedure Free(Struct: in out Display_Surface_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Display_Present_Info)
        return Display_Present_Info_C;
    procedure Free(Struct: in out Display_Present_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Queue_Family_Query_Result_Status_Properties;
         C_Struct: in Queue_Family_Query_Result_Status_Properties_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Queue_Family_Video_Properties;
         C_Struct: in Queue_Family_Video_Properties_C);

    function To_C(Struct: in Extensions.KHR.Video_Profile_Info)
        return Video_Profile_Info_C;
    procedure Free(Struct: in out Video_Profile_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Profile_List_Info)
        return Video_Profile_List_Info_C;
    procedure Free(Struct: in out Video_Profile_List_Info_C);

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Video_Capabilities;
                     C_Struct: in Video_Capabilities_C);

    function To_C(Struct: in Extensions.KHR.Physical_Device_Video_Format_Info)
        return Physical_Device_Video_Format_Info_C;
    procedure Free(Struct: in out Physical_Device_Video_Format_Info_C);

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Video_Format_Properties;
                     C_Struct: in Video_Format_Properties_C);

    function To_C(Struct: in Extensions.KHR.Video_Picture_Resource_Info)
        return Video_Picture_Resource_Info_C;
    procedure Free(Struct: in out Video_Picture_Resource_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Reference_Slot_Info)
        return Video_Reference_Slot_Info_C;
    procedure Free(Struct: in out Video_Reference_Slot_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Session_Memory_Requirements;
         C_Struct: in Video_Session_Memory_Requirements_C);

    function To_C(Struct: in Extensions.KHR.Bind_Video_Session_Memory_Info)
        return Bind_Video_Session_Memory_Info_C;
    procedure Free(Struct: in out Bind_Video_Session_Memory_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Session_Create_Info)
        return Video_Session_Create_Info_C;
    procedure Free(Struct: in out Video_Session_Create_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Session_Parameters_Create_Info)
        return Video_Session_Parameters_Create_Info_C;
    procedure Free(Struct: in out Video_Session_Parameters_Create_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Session_Parameters_Update_Info)
        return Video_Session_Parameters_Update_Info_C;
    procedure Free(Struct: in out Video_Session_Parameters_Update_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Begin_Coding_Info)
        return Video_Begin_Coding_Info_C;
    procedure Free(Struct: in out Video_Begin_Coding_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_End_Coding_Info)
        return Video_End_Coding_Info_C;
    procedure Free(Struct: in out Video_End_Coding_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Coding_Control_Info)
        return Video_Coding_Control_Info_C;
    procedure Free(Struct: in out Video_Coding_Control_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Decode_Capabilities;
         C_Struct: in Video_Decode_Capabilities_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_Usage_Info)
        return Video_Decode_Usage_Info_C;
    procedure Free(Struct: in out Video_Decode_Usage_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_Info)
        return Video_Decode_Info_C;
    procedure Free(Struct: in out Video_Decode_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Encode_H264_Capabilities;
         C_Struct: in Video_Encode_H264_Capabilities_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Video_Encode_H264_Quality_Level_Properties;
         C_Struct: in Video_Encode_H264_Quality_Level_Properties_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H264_Session_Create_Info)
        return Video_Encode_H264_Session_Create_Info_C;
    procedure Free(Struct: in out Video_Encode_H264_Session_Create_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H264_Session_Parameters_Add_Info)
        return Video_Encode_H264_Session_Parameters_Add_Info_C;
    procedure Free
        (Struct: in out Video_Encode_H264_Session_Parameters_Add_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H264_Session_Parameters_Create_Info)
        return Video_Encode_H264_Session_Parameters_Create_Info_C;
    procedure Free
        (Struct: in out Video_Encode_H264_Session_Parameters_Create_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H264_Session_Parameters_Get_Info)
        return Video_Encode_H264_Session_Parameters_Get_Info_C;
    procedure Free
        (Struct: in out Video_Encode_H264_Session_Parameters_Get_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out
              Extensions.KHR.Video_Encode_H264_Session_Parameters_Feedback_Info;
         C_Struct: in Video_Encode_H264_Session_Parameters_Feedback_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_Nalu_Slice_Info)
        return Video_Encode_H264_Nalu_Slice_Info_C;
    procedure Free(Struct: in out Video_Encode_H264_Nalu_Slice_Info_C);
 
    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_Picture_Info)
        return Video_Encode_H264_Picture_Info_C;
    procedure Free(Struct: in out Video_Encode_H264_Picture_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_DPB_Slot_Info)
        return Video_Encode_H264_DPB_Slot_Info_C;
    procedure Free(Struct: in out Video_Encode_H264_DPB_Slot_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_Profile_Info)
        return Video_Encode_H264_Profile_Info_C;
    procedure Free(Struct: in out Video_Encode_H264_Profile_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_Rate_Control_Info)
        return Video_Encode_H264_Rate_Control_Info_C;
    procedure Free(Struct: in out Video_Encode_H264_Rate_Control_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H264_Rate_Control_Layer_Info)
        return Video_Encode_H264_Rate_Control_Layer_Info_C;
    procedure Free(Struct: in out Video_Encode_H264_Rate_Control_Layer_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H264_GOP_Remaining_Frame_Info)
        return Video_Encode_H264_GOP_Remaining_Frame_Info_C;
    procedure Free(Struct: in out Video_Encode_H264_GOP_Remaining_Frame_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Encode_H265_Capabilities;
         C_Struct: in Video_Encode_H265_Capabilities_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H265_Session_Create_Info)
        return Video_Encode_H265_Session_Create_Info_C;
    procedure Free(Struct: in out Video_Encode_H265_Session_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Video_Encode_H265_Quality_Level_Properties;
         C_Struct: in Video_Encode_H265_Quality_Level_Properties_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H265_Session_Parameters_Add_Info)
        return Video_Encode_H265_Session_Parameters_Add_Info_C;
    procedure Free
        (Struct: in out Video_Encode_H265_Session_Parameters_Add_Info_C);

    function To_C
        (Struct: 
            in Extensions.KHR.Video_Encode_H265_Session_Parameters_Create_Info)
        return Video_Encode_H265_Session_Parameters_Create_Info_C;
    procedure Free
        (Struct: in out Video_Encode_H265_Session_Parameters_Create_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H265_Session_Parameters_Get_Info)
        return Video_Encode_H265_Session_Parameters_Get_Info_C;
    procedure Free
        (Struct: in out Video_Encode_H265_Session_Parameters_Get_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out
              Extensions.KHR.Video_Encode_H265_Session_Parameters_Feedback_Info;
         C_Struct: in Video_Encode_H265_Session_Parameters_Feedback_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H265_Nalu_Slice_Segment_Info)
        return Video_Encode_H265_Nalu_Slice_Segment_Info_C;
    procedure Free(Struct: in out Video_Encode_H265_Nalu_Slice_Segment_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_H265_Picture_Info)
        return Video_Encode_H265_Picture_Info_C;
    procedure Free(Struct: in out Video_Encode_H265_Picture_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_H265_DPB_Slot_Info)
        return Video_Encode_H265_DPB_Slot_Info_C;
    procedure Free(Struct: in out Video_Encode_H265_DPB_Slot_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_H265_Profile_Info)
        return Video_Encode_H265_Profile_Info_C;
    procedure Free(Struct: in out Video_Encode_H265_Profile_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_H265_Rate_Control_Info)
        return Video_Encode_H265_Rate_Control_Info_C;
    procedure Free(Struct: in out Video_Encode_H265_Rate_Control_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H265_Rate_Control_Layer_Info)
        return Video_Encode_H265_Rate_Control_Layer_Info_C;
    procedure Free(Struct: in out Video_Encode_H265_Rate_Control_Layer_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H265_GOP_Remaining_Frame_Info)
        return Video_Encode_H265_GOP_Remaining_Frame_Info_C;
    procedure Free(Struct: in out Video_Encode_H265_GOP_Remaining_Frame_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_H264_Profile_Info)
        return Video_Decode_H264_Profile_Info_C;
    procedure Free(Struct: in out Video_Decode_H264_Profile_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Decode_H264_Capabilities;
         C_Struct: in Video_Decode_H264_Capabilities_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_H264_Session_Parameters_Add_Info)
        return Video_Decode_H264_Session_Parameters_Add_Info_C;
    procedure Free
        (Struct: in out Video_Decode_H264_Session_Parameters_Add_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_H264_Session_Parameters_Create_Info)
        return Video_Decode_H264_Session_Parameters_Create_Info_C;
    procedure Free
        (Struct: in out Video_Decode_H264_Session_Parameters_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_H264_Picture_Info)
        return Video_Decode_H264_Picture_Info_C;
    procedure Free(Struct: in out Video_Decode_H264_Picture_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_H264_DPB_Slot_Info)
        return Video_Decode_H264_DPB_Slot_Info_C;
    procedure Free(Struct: in out Video_Decode_H264_DPB_Slot_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Rendering_Fragment_Shading_Rate_Attachment_Info)
        return Rendering_Fragment_Shading_Rate_Attachment_Info_C;
    procedure Free
        (Struct: in out Rendering_Fragment_Shading_Rate_Attachment_Info_C);

    function To_C(Struct: in Extensions.KHR.Import_Memory_FD_Info)
        return Import_Memory_FD_Info_C;
    procedure Free(Struct: in out Import_Memory_FD_Info_C);

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Memory_FD_Properties;
                     C_Struct: in Memory_FD_Properties_C);
    
    function To_C(Struct: in Extensions.KHR.Memory_Get_FD_Info)
        return Memory_Get_FD_Info_C;
    procedure Free(Struct: in out Memory_Get_FD_Info_C);

    function To_C(Struct: in Extensions.KHR.Import_Semaphore_FD_Info)
        return Import_Semaphore_FD_Info_C;
    procedure Free(Struct: in out Import_Semaphore_FD_Info_C);

    function To_C(Struct: in Extensions.KHR.Semaphore_Get_FD_Info)
        return Semaphore_Get_FD_Info_C;
    procedure Free(Struct: in out Semaphore_Get_FD_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Push_Descriptor_Properties;
         C_Struct: in Physical_Device_Push_Descriptor_Properties_C);

    function To_C(Struct: in Extensions.KHR.Present_Region)
        return Present_Region_C;
    procedure Free(Struct: in out Present_Region_C);

    function To_C(Struct: in Extensions.KHR.Present_Regions)
        return Present_Regions_C;
    procedure Free(Struct: in out Present_Regions_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Shared_Present_Surface_Capabilities;
         C_Struct: in Shared_Present_Surface_Capabilities_C);

    function To_C(Struct: in Extensions.KHR.Import_Fence_FD_Info)
        return Import_Fence_FD_Info_C;
    procedure Free(Struct: in out Import_Fence_FD_Info_C);

    function To_C(Struct: in Extensions.KHR.Fence_Get_FD_Info)
        return Fence_Get_FD_Info_C;
    procedure Free(Struct: in out Fence_Get_FD_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Performance_Query_Features;
         C_Struct: in Physical_Device_Performance_Query_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Performance_Query_Properties;
         C_Struct: in Physical_Device_Performance_Query_Properties_C);

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Performance_Counter;
                     C_Struct: in Performance_Counter_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Performance_Counter_Description;
         C_Struct: in Performance_Counter_Description_C);

    function To_C(Struct: in Extensions.KHR.Query_Pool_Performance_Create_Info)
        return Query_Pool_Performance_Create_Info_C;
    procedure Free(Struct: in out Query_Pool_Performance_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Acquire_Profiling_Lock_Info)
        return Acquire_Profiling_Lock_Info_C;
    procedure Free(Struct: in out Acquire_Profiling_Lock_Info_C);

    function To_C(Struct: in Extensions.KHR.Performance_Query_Submit_Info)
        return Performance_Query_Submit_Info_C;
    procedure Free(Struct: in out Performance_Query_Submit_Info_C);

    function To_C(Struct: in Extensions.KHR.Physical_Device_Surface_Info_2)
        return Physical_Device_Surface_Info_2_C;
    procedure Free(Struct: in out Physical_Device_Surface_Info_2_C);

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Surface_Capabilities_2;
                     C_Struct: in Surface_Capabilities_2_C);

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Surface_Format_2;
                     C_Struct: in Surface_Format_2_C);

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Display_Properties_2;
                     C_Struct: in Display_Properties_2_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Display_Plane_Properties_2;
         C_Struct: in Display_Plane_Properties_2_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Display_Mode_Properties_2;
         C_Struct: in Display_Mode_Properties_2_C);

    function To_C(Struct: in Extensions.KHR.Display_Plane_Info_2)
        return Display_Plane_Info_2_C;
    procedure Free(Struct: in out Display_Plane_Info_2_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Display_Plane_Capabilities_2;
         C_Struct: in Display_Plane_Capabilities_2_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Shader_Clock_Features;
         C_Struct: in Physical_Device_Shader_Clock_Features_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_H265_Profile_Info)
        return Video_Decode_H265_Profile_Info_C;
    procedure Free(Struct: in out Video_Decode_H265_Profile_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Decode_H265_Capabilities;
         C_Struct: in Video_Decode_H265_Capabilities_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_H265_Session_Parameters_Add_Info)
        return Video_Decode_H265_Session_Parameters_Add_Info_C;
    procedure Free
        (Struct: in out Video_Decode_H265_Session_Parameters_Add_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_H265_Session_Parameters_Create_Info)
        return Video_Decode_H265_Session_Parameters_Create_Info_C;
    procedure Free
        (Struct: in out Video_Decode_H265_Session_Parameters_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_H265_Picture_Info)
        return Video_Decode_H265_Picture_Info_C;
    procedure Free(Struct: in out Video_Decode_H265_Picture_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_H265_DPB_Slot_Info)
        return Video_Decode_H265_DPB_Slot_Info_C;
    procedure Free(Struct: in out Video_Decode_H265_DPB_Slot_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Device_Queue_Global_Priority_Create_Info)
        return Device_Queue_Global_Priority_Create_Info_C;
    procedure Free(Struct: in out Device_Queue_Global_Priority_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out
                Extensions.KHR.Physical_Device_Global_Priority_Query_Features;
         C_Struct: in Physical_Device_Global_Priority_Query_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Queue_Family_Global_Priority_Properties;
         C_Struct: in Queue_Family_Global_Priority_Properties_C);

    function To_C
        (Struct: in Extensions.KHR.Fragment_Shading_Rate_Attachment_Info)
        return Fragment_Shading_Rate_Attachment_Info_C;
    procedure Free(Struct: in out Fragment_Shading_Rate_Attachment_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Pipeline_Fragment_Shading_Rate_State_Create_Info)
        return Pipeline_Fragment_Shading_Rate_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Fragment_Shading_Rate_State_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out
                Extensions.KHR.Physical_Device_Fragment_Shading_Rate_Features;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_Features_C);

    procedure To_Ada
        (Ada_Struct: in out
            Extensions.KHR.Physical_Device_Fragment_Shading_Rate_Properties;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_Properties_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Fragment_Shading_Rate;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_C);

    procedure To_Ada
        (Ada_Struct:
            in out
           Extensions.KHR.Physical_Device_Dynamic_Rendering_Local_Read_Features;
         C_Struct: in Physical_Device_Dynamic_Rendering_Local_Read_Features_C);

    function To_C(Struct: in Extensions.KHR.Rendering_Attachment_Location_Info)
        return Rendering_Attachment_Location_Info_C;
    procedure Free(Struct: in out Rendering_Attachment_Location_Info_C);
 
    function To_C
        (Struct: in Extensions.KHR.Rendering_Input_Attachment_Index_Info)
        return Rendering_Input_Attachment_Index_Info_C;
    procedure Free(Struct: in out Rendering_Input_Attachment_Index_Info_C);
                 
    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Shader_Quad_Control_Features;
         C_Struct: in Physical_Device_Shader_Quad_Control_Features_C);

    function To_C(Struct: in Extensions.KHR.Surface_Protected_Capabilities)
        return Surface_Protected_Capabilities_C;
    procedure Free(Struct: in out Surface_Protected_Capabilities_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Present_Wait_Features;
         C_Struct: in Physical_Device_Present_Wait_Features_C);

    procedure To_Ada
        (Ada_Struct: in out
         Extensions.KHR.Physical_Device_Pipeline_Executable_Properties_Features;
         C_Struct: in
            Physical_Device_Pipeline_Executable_Properties_Features_C);

    function To_C(Struct: in Extensions.KHR.Pipeline_Info) return Pipeline_Info_C;
    procedure Free(Struct: in out Pipeline_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Pipeline_Executable_Properties;
         C_Struct: in Pipeline_Executable_Properties_C);

    function To_C(Struct: in Extensions.KHR.Pipeline_Executable_Info)
        return Pipeline_Executable_Info_C;
    procedure Free(Struct: in out Pipeline_Executable_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Pipeline_Executable_Statistic;
         C_Struct: in Pipeline_Executable_Statistic_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Pipeline_Executable_Internal_Representation;
         C_Struct: in Pipeline_Executable_Internal_Representation_C);

    function To_C(Struct: in Extensions.KHR.Memory_Map_Info)
        return Memory_Map_Info_C;
    procedure Free(Struct: in out Memory_Map_Info_C);

    function To_C(Struct: in Extensions.KHR.Memory_Unmap_Info)
        return Memory_Unmap_Info_C;
    procedure Free(Struct: in out Memory_Unmap_Info_C);

    function To_C(Struct: in Extensions.KHR.Pipeline_Library_Create_Info)
        return Pipeline_Library_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Library_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Present_ID) return Present_ID_C;
    procedure Free(Struct: in out Present_ID_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Physical_Device_Present_ID_Features;
         C_Struct: in Physical_Device_Present_ID_Features_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_Info)
        return Video_Encode_Info_C;
    procedure Free(Struct: in out Video_Encode_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Encode_Capabilities;
         C_Struct: in Video_Encode_Capabilities_C);

    function To_C
        (Struct: in Extensions.KHR.Query_Pool_Video_Encode_Feedback_Create_Info)
        return Query_Pool_Video_Encode_Feedback_Create_Info_C;
    procedure Free
        (Struct: in out Query_Pool_Video_Encode_Feedback_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_Usage_Info)
        return Video_Encode_Usage_Info_C;
    procedure Free(Struct: in out Video_Encode_Usage_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_Rate_Control_Layer_Info)
        return Video_Encode_Rate_Control_Layer_Info_C;
    procedure Free(Struct: in out Video_Encode_Rate_Control_Layer_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_Rate_Control_Info)
        return Video_Encode_Rate_Control_Info_C;
    procedure Free(Struct: in out Video_Encode_Rate_Control_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Physical_Device_Video_Encode_Quality_Level_Info)
        return Physical_Device_Video_Encode_Quality_Level_Info_C;
    procedure Free
        (Struct: in out Physical_Device_Video_Encode_Quality_Level_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Video_Encode_Quality_Level_Properties;
         C_Struct: in Video_Encode_Quality_Level_Properties_C);

    function To_C(Struct: in Extensions.KHR.Video_Encode_Quality_Level_Info)
        return Video_Encode_Quality_Level_Info_C;
    procedure Free(Struct: in out Video_Encode_Quality_Level_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_Session_Parameters_Get_Info)
        return Video_Encode_Session_Parameters_Get_Info_C;
    procedure Free(Struct: in out Video_Encode_Session_Parameters_Get_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Video_Encode_Session_Parameters_Feedback_Info;
         C_Struct: in Video_Encode_Session_Parameters_Feedback_Info_C);

    procedure To_Ada
        (Ada_Struct: in out
            Extensions.KHR.Physical_Device_Fragment_Shader_Barycentric_Features;
         C_Struct: in Physical_Device_Fragment_Shader_Barycentric_Features_C);

    procedure To_Ada
        (Ada_Struct: in out
          Extensions.KHR.Physical_Device_Fragment_Shader_Barycentric_Properties;
         C_Struct: in Physical_Device_Fragment_Shader_Barycentric_Properties_C);

    procedure To_Ada
        (Ada_Struct: in out
   Extensions.KHR.Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features;
         C_Struct: in
            Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C);

    procedure To_Ada
        (Ada_Struct: in out
       Extensions.KHR.Physical_Device_Workgroup_Memory_Explicit_Layout_Features;
         C_Struct: in
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C);

    procedure To_Ada
        (Ada_Struct: in out
              Extensions.KHR.Physical_Device_Ray_Tracing_Maintenance_1_Features;
         C_Struct: in Physical_Device_Ray_Tracing_Maintenance_1_Features_C);

    procedure To_Ada
        (Ada_Struct: in out
           Extensions.KHR.Physical_Device_Shader_Maximal_Reconvergence_Features;
         C_Struct: in Physical_Device_Shader_Maximal_Reconvergence_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Physical_Device_Maintenance_5_Features;
         C_Struct: in Physical_Device_Maintenance_5_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Maintenance_5_Properties;
         C_Struct: in Physical_Device_Maintenance_5_Properties_C);

    function To_C(Struct: in Extensions.KHR.Rendering_Area_Info)
        return Rendering_Area_Info_C;
    procedure Free(Struct: in out Rendering_Area_Info_C);

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Image_Subresource_2;
                     C_Struct: in Image_Subresource_2_C);

    function To_C(Struct: in Extensions.KHR.Device_Image_Subresource_Info)
        return Device_Image_Subresource_Info_C;
    procedure Free(Struct: in out Device_Image_Subresource_Info_C);

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Subresource_Layout_2;
                     C_Struct: in Subresource_Layout_2_C);

    function To_C(Struct: in Extensions.KHR.Pipeline_Create_Flags_2_Create_Info)
        return Pipeline_Create_Flags_2_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Create_Flags_2_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Buffer_Usage_Flags_2_Create_Info)
        return Buffer_Usage_Flags_2_Create_Info_C;
    procedure Free(Struct: in out Buffer_Usage_Flags_2_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out
             Extensions.KHR.Physical_Device_Ray_Tracing_Position_Fetch_Features;
         C_Struct: in Physical_Device_Ray_Tracing_Position_Fetch_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Cooperative_Matrix_Properties;
         C_Struct: in Cooperative_Matrix_Properties_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Cooperative_Matrix_Features;
         C_Struct: in Physical_Device_Cooperative_Matrix_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Cooperative_Matrix_Properties;
         C_Struct: in Physical_Device_Cooperative_Matrix_Properties_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_AV1_Profile_Info)
        return Video_Decode_AV1_Profile_Info_C;
    procedure Free(Struct: in out Video_Decode_AV1_Profile_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Decode_AV1_Capabilities;
         C_Struct: in Video_Decode_AV1_Capabilities_C);

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_AV1_Session_Parameters_Create_Info)
        return Video_Decode_AV1_Session_Parameters_Create_Info_C;
    procedure Free
        (Struct: in out Video_Decode_AV1_Session_Parameters_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_AV1_Picture_Info)
        return Video_Decode_AV1_Picture_Info_C;
    procedure Free(Struct: in out Video_Decode_AV1_Picture_Info_C);

    function To_C(Struct: in Extensions.KHR.Video_Decode_AV1_DPB_Slot_Info)
        return Video_Decode_AV1_DPB_Slot_Info_C;
    procedure Free(Struct: in out Video_Decode_AV1_DPB_Slot_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Video_Maintenance_1_Features;
         C_Struct: in Physical_Device_Video_Maintenance_1_Features_C);

    function To_C(Struct: in Extensions.KHR.Video_Inline_Query_Info)
        return Video_Inline_Query_Info_C;
    procedure Free(Struct: in out Video_Inline_Query_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out
             Extensions.KHR.Physical_Device_Vertex_Attribute_Divisor_Properties;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Properties_C);

    function To_C
        (Struct:
            in Extensions.KHR.Pipeline_Vertex_Input_Divisor_State_Create_Info)
        return Pipeline_Vertex_Input_Divisor_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Vertex_Input_Divisor_State_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out
               Extensions.KHR.Physical_Device_Vertex_Attribute_Divisor_Features;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Features_C);

    function To_C(Struct: in Extensions.KHR.Calibrated_Timestamp_Info)
        return Calibrated_Timestamp_Info_C;
    procedure Free(Struct: in out Calibrated_Timestamp_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Maintenance_6_Features;
         C_Struct: in Physical_Device_Maintenance_6_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Maintenance_6_Properties;
         C_Struct: in Physical_Device_Maintenance_6_Properties_C);

    function To_C(Struct: in Extensions.KHR.Bind_Memory_Status)
        return Bind_Memory_Status_C;
    procedure Free(Struct: in out Bind_Memory_Status_C);

    function To_C(Struct: in Extensions.KHR.Bind_Descriptor_Sets_Info)
        return Bind_Descriptor_Sets_Info_C;
    procedure Free(Struct: in out Bind_Descriptor_Sets_Info_C);

    function To_C(Struct: in Extensions.KHR.Push_Constants_Info)
        return Push_Constants_Info_C;
    procedure Free(Struct: in out Push_Constants_Info_C);

    function To_C(Struct: in Extensions.KHR.Push_Descriptor_Set_Info)
        return Push_Descriptor_Set_Info_C;
    procedure Free(Struct: in out Push_Descriptor_Set_Info_C);

    function To_C
        (Struct: in Extensions.KHR.Push_Descriptor_Set_With_Template_Info)
        return Push_Descriptor_Set_With_Template_Info_C;
    procedure Free(Struct: in out Push_Descriptor_Set_With_Template_Info_C);

    function To_C(Struct: in Extensions.KHR.Set_Descriptor_Buffer_Offsets_Info)
        return Set_Descriptor_Buffer_Offsets_Info_C;
    procedure Free(Struct: in out Set_Descriptor_Buffer_Offsets_Info_C);

    function To_C
        (Struct:
            in Extensions.KHR.Bind_Descriptor_Buffer_Embedded_Samplers_Info)
        return Bind_Descriptor_Buffer_Embedded_Samplers_Info_C;
    procedure Free
        (Struct: in out Bind_Descriptor_Buffer_Embedded_Samplers_Info_C);

    function To_C(Struct: in Extensions.KHR.Xlib_Surface_Create_Info)
        return Xlib_Surface_Create_Info_C;
    procedure Free(Struct: in out Xlib_Surface_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Xcb_Surface_Create_Info)
        return Xcb_Surface_Create_Info_C;
    procedure Free(Struct: in out Xcb_Surface_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Wayland_Surface_Create_Info)
        return Wayland_Surface_Create_Info_C;
    procedure Free(Struct: in out Wayland_Surface_Create_Info_C);

    function To_C(Struct: in Extensions.KHR.Win32_Surface_Create_Info)
        return Win32_Surface_Create_Info_C;
    procedure Free(Struct: in out Win32_Surface_Create_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_KHR;


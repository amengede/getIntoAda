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

-- Khronos extensions root package

with Vulkan.Extensions.Std_Video.H264.Decode;
with Vulkan.Extensions.Std_Video.H264.Encode;
with Vulkan.Extensions.Std_Video.H265.Decode;
with Vulkan.Extensions.Std_Video.H265.Encode;
with Vulkan.Extensions.Std_Video.AV1.Decode;
with Vulkan.Xlib;
with Vulkan.Xcb;
with Vulkan.Wayland;
with Vulkan.Win32;

package Vulkan.Extensions.KHR is
    -- Handle types.
    type Surface is new Object_Handle;
    type Swapchain is new Object_Handle;
    type Display is new Object_Handle;
    type Display_Mode is new Object_Handle;
    type Video_Session is new Object_Handle;
    type Video_Session_Parameters is new Object_Handle;
    type Deferred_Operation is new Object_Handle;
    
    No_Surface: constant Surface := Surface(System.Null_Address);
    No_Swapchain: constant Swapchain := Swapchain(System.Null_Address);
    No_Display: constant Display := Display(System.Null_Address);
    No_Display_Mode: constant Display_Mode := Display_Mode(System.Null_Address);
    No_Video_Session: constant Video_Session :=
        Video_Session(System.Null_Address);
    No_Video_Session_Parameters: constant Video_Session_Parameters :=
        Video_Session_Parameters(System.Null_Address);
    No_Deferred_Operation: constant Deferred_Operation :=
        Deferred_Operation(System.Null_Address);

    -- Handle vector types.
    package Display_Vectors is new Ada.Containers.Vectors(Positive, Display);
    package Swapchain_Vectors is
        new Ada.Containers.Vectors(Positive, Swapchain);

    -- Constants.
    Max_Global_Priority_Size: constant := 16;
    Max_Video_AV1_References_Per_Frame: constant := 7;

    -- Enumerations.
    type Present_Mode is (Immediate,
                          Mailbox,
                          FIFO,
                          FIFO_Relaxed,
                          Shared_Demand_Refresh,
                          Shared_Continuous_Refresh)
        with Convention => C;

    for Present_Mode'Size use 32;

    for Present_Mode use (Immediate => 0,
                          Mailbox => 1,
                          FIFO => 2,
                          FIFO_Relaxed => 3,
                          Shared_Demand_Refresh => 1_000_111_000,
                          Shared_Continuous_Refresh => 1_000_111_001);

    package Present_Mode_Vectors is
        new Ada.Containers.Vectors(Positive, Present_Mode);

    type Color_Space is (SRGB_Nonlinear,
                         Display_P3_Nonlinear,
                         Extended_SRGB_Linear,
                         Display_P3_Linear,
                         DCI_P3_Nonlinear,
                         BT709_Linear,
                         BT709_Nonlinear,
                         BT2020_Linear,
                         HDR10_ST2084,
                         DolbyVision,
                         HDR10_HLG,
                         AdobeRGB_Linear,
                         AdobeRGB_Nonlinear,
                         Pass_Through,
                         Extended_SRGB_Nonlinear,
                         Display_Native)
        with Convention => C;

    for Color_Space'Size use 32;

    for Color_Space use (SRGB_Nonlinear => 0,
                         Display_P3_Nonlinear => 1_000_104_001,
                         Extended_SRGB_Linear => 1_000_104_002,
                         Display_P3_Linear => 1_000_104_003,
                         DCI_P3_Nonlinear => 1_000_104_004,
                         BT709_Linear => 1_000_104_005,
                         BT709_Nonlinear => 1_000_104_006,
                         BT2020_Linear => 1_000_104_007,
                         HDR10_ST2084 => 1_000_104_008,
                         DolbyVision => 1_000_104_009,
                         HDR10_HLG => 1_000_104_010,
                         AdobeRGB_Linear => 1_000_104_011,
                         AdobeRGB_Nonlinear => 1_000_104_012,
                         Pass_Through => 1_000_104_013,
                         Extended_SRGB_Nonlinear => 1_000_104_014,
                         Display_Native => 1_000_213_000);

    type Query_Results_Status is (Insufficient_Bitstream_Buffer_Range,
                                  Error,
                                  Not_Ready,
                                  Complete)
        with Convention => C;

    for Query_Results_Status'Size use 32;

    for Query_Results_Status use
        (Insufficient_Bitstream_Buffer_Range => -1_000_299_000,
         Error => -1,
         Not_Ready => 0,
         Complete => 1);

    type Performance_Counter_Unit is (Generic_Unit,
                                      Percentage,
                                      Nanoseconds,
                                      Bytes,
                                      Bytes_Per_Second,
                                      Kelvin,
                                      Watts,
                                      Volts,
                                      Amps,
                                      Hertz,
                                      Cycles)
        with Convention => C;

    for Performance_Counter_Unit'Size use 32;

    for Performance_Counter_Unit use (Generic_Unit => 0,
                                      Percentage => 1,
                                      Nanoseconds => 2,
                                      Bytes => 3,
                                      Bytes_Per_Second => 4,
                                      Kelvin => 5,
                                      Watts => 6,
                                      Volts => 7,
                                      Amps => 8,
                                      Hertz => 9,
                                      Cycles => 10);

    type Performance_Counter_Scope is (Command_Buffer_Scope,
                                       Render_Pass_Scope,
                                       Command_Scope)
        with Convention => C;

    for Performance_Counter_Scope'Size use 32;

    for Performance_Counter_Scope use (Command_Buffer_Scope => 0,
                                       Render_Pass_Scope => 1,
                                       Command_Scope => 2);

    type Performance_Counter_Storage is (Int32,
                                         Int64,
                                         Uint32,
                                         Uint64,
                                         Float32,
                                         Float64)
        with Convention => C;

    for Performance_Counter_Storage'Size use 32;

    for Performance_Counter_Storage use (Int32 => 0,
                                         Int64 => 1,
                                         Uint32 => 2,
                                         Uint64 => 3,
                                         Float32 => 4,
                                         Float64 => 5);
 
    type Fragment_Shading_Rate_Combiner_Op is (Keep,
                                               Replace,
                                               Min,
                                               Max,
                                               Mul)
        with Convention => C;

    for Fragment_Shading_Rate_Combiner_Op'Size use 32;

    for Fragment_Shading_Rate_Combiner_Op use (Keep => 0,
                                               Replace => 1,
                                               Min => 2,
                                               Max => 3,
                                               Mul => 4);

    type Pipeline_Executable_Statistic_Format is (Bool32,
                                                  Int64,
                                                  Uint64,
                                                  Float64)
        with Convention => C;

    for Pipeline_Executable_Statistic_Format'Size use 32;

    for Pipeline_Executable_Statistic_Format use (Bool32 => 0,
                                                  Int64 => 1,
                                                  Uint64 => 2,
                                                  Float64 => 3);

    type Video_Encode_Tuning_Mode is (Default,
                                      High_Quality,
                                      Low_Latency,
                                      Ultra_Low_Latency,
                                      Lossless)
        with Convention => C;

    for Video_Encode_Tuning_Mode'Size use 32;

    for Video_Encode_Tuning_Mode use (Default => 0,
                                      High_Quality => 1,
                                      Low_Latency => 2,
                                      Ultra_Low_Latency => 3,
                                      Lossless => 4);

    type Component_Type is (Float_16,
                            Float_32,
                            Float_64,
                            SInt_8,
                            SInt_16,
                            SInt_32,
                            SInt_64,
                            UInt_8,
                            UInt_16,
                            UInt_32,
                            UInt_64)
        with Convention => C;

    for Component_Type'Size use 32;

    for Component_Type use (Float_16 => 0,
                            Float_32 => 1,
                            Float_64 => 2,
                            SInt_8 => 3,
                            SInt_16 => 4,
                            SInt_32 => 5,
                            SInt_64 => 6,
                            UInt_8 => 7,
                            UInt_16 => 8,
                            UInt_32 => 9,
                            UInt_64 => 10);

    type Scope is (Scope_Device,
                   Scope_Workgroup,
                   Scope_Subgroup,
                   Scope_Queue_Family)
        with Convention => C;

    for Scope'Size use 32;

    for Scope use (Scope_Device => 1,
                   Scope_Workgroup => 2,
                   Scope_Subgroup => 3,
                   Scope_Queue_Family => 5);

    type Time_Domain is (Domain_Device,
                         Clock_Monotonic,
                         Clock_Monotonic_Raw,
                         Query_Performance_Counter)
        with Convention => C;

    for Time_Domain'Size use 32;

    for Time_Domain use (Domain_Device => 0,
                         Clock_Monotonic => 1,
                         Clock_Monotonic_Raw => 2,
                         Query_Performance_Counter => 3);

    package Time_Domain_Vectors is new Ada.Containers.Vectors
        (Positive, Time_Domain);

    -- Bitfields.
    type Surface_Transform_Flags is new Flags;

    Surface_Transform_No_Bit: constant Surface_Transform_Flags := 0;
    Surface_Transform_Identity_Bit:
        constant Surface_Transform_Flags := 16#00000001#;
    Surface_Transform_Rotate_90_Bit:
        constant Surface_Transform_Flags := 16#00000002#;
    Surface_Transform_Rotate_180_Bit:
        constant Surface_Transform_Flags := 16#00000004#;
    Surface_Transform_Rotate_270_Bit:
        constant Surface_Transform_Flags := 16#00000008#;
    Surface_Transform_Horizontal_Mirror_Bit:
        constant Surface_Transform_Flags := 16#00000010#;
    Surface_Transform_Horizontal_Mirror_Rotate_90_Bit:
        constant Surface_Transform_Flags := 16#00000020#;
    Surface_Transform_Horizontal_Mirror_Rotate_180_Bit:
        constant Surface_Transform_Flags := 16#00000040#;
    Surface_Transform_Horizontal_Mirror_Rotate_270_Bit:
        constant Surface_Transform_Flags := 16#00000080#;
    Surface_Transform_Inherit_Bit:
        constant Surface_Transform_Flags := 16#00000100#;

    type Composite_Alpha_Flags is new Flags;

    Composite_Alpha_No_Bit: constant Composite_Alpha_Flags := 0;
    Composite_Alpha_Opaque_Bit: constant Composite_Alpha_Flags := 16#00000001#;
    Composite_Alpha_Pre_Multiplied_Bit:
        constant Composite_Alpha_Flags := 16#00000002#;
    Composite_Alpha_Post_Multiplied_Bit:
        constant Composite_Alpha_Flags := 16#00000004#;
    Composite_Alpha_Inherit_Bit: constant Composite_Alpha_Flags := 16#00000008#;

    type Swapchain_Create_Flags is new Flags;

    Swapchain_Create_No_Bit: constant Swapchain_Create_Flags := 0;
    Swapchain_Create_Split_Instance_Bind_Regions_Bit:
        constant Swapchain_Create_Flags := 16#00000001#;
    Swapchain_Create_Protected_Bit:
        constant Swapchain_Create_Flags := 16#00000002#;
    Swapchain_Create_Mutable_Format_Bit:
        constant Swapchain_Create_Flags := 16#00000004#;

    type Device_Group_Present_Mode_Flags is new Flags;

    Device_Group_Present_Mode_No_Bit:
        constant Device_Group_Present_Mode_Flags := 0;
    Device_Group_Present_Mode_Local_Bit:
        constant Device_Group_Present_Mode_Flags := 16#00000001#;
    Device_Group_Present_Mode_Remote_Bit:
        constant Device_Group_Present_Mode_Flags := 16#00000002#;
    Device_Group_Present_Mode_Sum_Bit:
        constant Device_Group_Present_Mode_Flags := 16#00000004#;
    Device_Group_Present_Mode_Local_Multi_Device_Bit:
        constant Device_Group_Present_Mode_Flags := 16#00000008#;

    type Display_Mode_Create_Flags is new Flags;

    Display_Mode_Create_No_Bit: constant Display_Mode_Create_Flags := 0;

    type Display_Plane_Alpha_Flags is new Flags;

    Display_Plane_Alpha_No_Bit: constant Display_Plane_Alpha_Flags := 0;
    Display_Plane_Alpha_Opaque_Bit:
        constant Display_Plane_Alpha_Flags := 16#00000001#;
    Display_Plane_Alpha_Global_Bit:
        constant Display_Plane_Alpha_Flags := 16#00000002#;
    Display_Plane_Alpha_Per_Pixel_Bit:
        constant Display_Plane_Alpha_Flags := 16#00000004#;
    Display_Plane_Alpha_Per_Pixel_Premultiplied_Bit:
        constant Display_Plane_Alpha_Flags := 16#00000008#;

    type Display_Surface_Create_Flags is new Flags;

    Display_Surface_Create_No_Bit: constant Display_Surface_Create_Flags := 0;

    type Video_Codec_Operation_Flags is new Flags;

    Video_Codec_Operation_No_Bit: constant Video_Codec_Operation_Flags := 0;
    Video_Codec_Operation_Decode_H264_Bit:
        constant Video_Codec_Operation_Flags := 16#00000001#;
    Video_Codec_Operation_Decode_H265_Bit:
        constant Video_Codec_Operation_Flags := 16#00000002#;

    type Video_Chroma_Subsampling_Flags is new Flags;

    Video_Chroma_Subsampling_Invalid_Bit:
        constant Video_Chroma_Subsampling_Flags := 0;
    Video_Chroma_Subsampling_Monochrome_Bit:
        constant Video_Chroma_Subsampling_Flags := 16#00000001#;
    Video_Chroma_Subsampling_420_Bit:
        constant Video_Chroma_Subsampling_Flags := 16#00000002#;
    Video_Chroma_Subsampling_422_Bit:
        constant Video_Chroma_Subsampling_Flags := 16#00000004#;
    Video_Chroma_Subsampling_444_Bit:
        constant Video_Chroma_Subsampling_Flags := 16#00000008#;

    type Video_Component_Bit_Depth_Flags is new Flags;

    Video_Component_Bit_Depth_Invalid_Bit:
        constant Video_Component_Bit_Depth_Flags := 0;
    Video_Component_Bit_Depth_8_Bit:
        constant Video_Component_Bit_Depth_Flags := 16#00000001#;
    Video_Component_Bit_Depth_10_Bit:
        constant Video_Component_Bit_Depth_Flags := 16#00000004#;
    Video_Component_Bit_Depth_12_Bit:
        constant Video_Component_Bit_Depth_Flags := 16#00000010#;

    type Video_Capability_Flags is new Flags;

    Video_Capability_No_Bit: constant Video_Capability_Flags := 0;
    Video_Capability_Protected_Content_Bit:
        constant Video_Capability_Flags := 16#00000001#;
    Video_Capability_Separate_Reference_Images_Bit:
        constant Video_Capability_Flags := 16#00000002#;

    type Video_Session_Create_Flags is new Flags;

    Video_Session_Create_No_Bit: constant Video_Session_Create_Flags := 0;
    Video_Session_Create_Protected_Content_Bit:
        constant Video_Session_Create_Flags := 16#00000001#;

    type Video_Session_Parameters_Create_Flags is new Flags;

    Video_Session_Parameters_Create_No_Bit:
        constant Video_Session_Parameters_Create_Flags := 0;

    type Video_Begin_Coding_Flags is new Flags;

    Video_Begin_Coding_No_Bit: constant Video_Begin_Coding_Flags := 0;

    type Video_End_Coding_Flags is new Flags;

    Video_End_Coding_No_Bit: constant Video_End_Coding_Flags := 0;

    type Video_Coding_Control_Flags is new Flags;

    Video_Coding_Control_No_Bit: constant Video_Coding_Control_Flags := 0;
    Video_Coding_Control_Reset_Bit:
        constant Video_Coding_Control_Flags := 16#00000001#;

    type Video_Decode_Capability_Flags is new Flags;

    Video_Decode_Capability_No_Bit: constant Video_Decode_Capability_Flags := 0;
    Video_Decode_Capability_DPB_And_Output_Coincide_Bit:
        constant Video_Decode_Capability_Flags := 16#00000001#;
    Video_Decode_Capability_DPB_And_Output_Distinct_Bit:
        constant Video_Decode_Capability_Flags := 16#00000002#;

    type Video_Decode_Usage_Flags is new Flags;

    Video_Decode_Usage_Default_Bit: constant Video_Decode_Usage_Flags := 0;
    Video_Decode_Usage_Transcoding_Bit:
        constant Video_Decode_Usage_Flags := 16#00000001#;
    Video_Decode_Usage_Offline_Bit:
        constant Video_Decode_Usage_Flags := 16#00000002#;
    Video_Decode_Usage_Streaming_Bit:
        constant Video_Decode_Usage_Flags := 16#00000004#;

    type Video_Decode_Flags is new Flags;

    Video_Decode_No_Bit: constant Video_Decode_Flags := 0;

    type Video_Encode_H264_Capability_Flags is new Flags;

    Video_Encode_H264_Capability_No_Bit:
        constant Video_Encode_H264_Capability_Flags := 0;
    Video_Encode_H264_Capability_HRD_Compliance_Bit:
        constant Video_Encode_H264_Capability_Flags := 16#00000001#;
    Video_Encode_H264_Capability_Prediction_Weight_Table_Generated_Bit:
        constant Video_Encode_H264_Capability_Flags := 16#00000002#;
    Video_Encode_H264_Capability_Row_Unaligned_Slice_Bit:
        constant Video_Encode_H264_Capability_Flags := 16#00000004#;
    Video_Encode_H264_Capability_Difference_Slice_Type_Bit:
        constant Video_Encode_H264_Capability_Flags := 16#00000008#;
    Video_Encode_H264_Capability_B_Frame_In_L0_List_Bit:
        constant Video_Encode_H264_Capability_Flags := 16#00000010#;
    Video_Encode_H264_Capability_B_Frame_In_L1_List_Bit:
        constant Video_Encode_H264_Capability_Flags := 16#00000020#;
    Video_Encode_H264_Capability_Per_Picture_Type_Min_Max_QP_Bit:
        constant Video_Encode_H264_Capability_Flags := 16#00000040#;
    Video_Encode_H264_Capability_Per_Slice_Constant_QP_Bit:
        constant Video_Encode_H264_Capability_Flags := 16#00000080#;
    Video_Encode_H264_Capability_Generate_Prefix_Nalu_Bit:
        constant Video_Encode_H264_Capability_Flags := 16#00000100#;

    type Video_Encode_H264_Std_Flags is new Flags;

    Video_Encode_H264_Std_No_Bit: constant Video_Encode_H264_Std_Flags := 0;
    Video_Encode_H264_Std_Separate_Color_Plane_Flag_Set_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000001#;
    Video_Encode_H264_Std_QPPrime_Y_Zero_Transform_Bypass_Flag_Set_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000002#;
    Video_Encode_H264_Std_Scaling_Matrix_Present_Flag_Set_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000004#;
    Video_Encode_H264_Std_Chroma_QP_Index_Offset_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000008#;
    Video_Encode_H264_Std_Second_Chroma_QP_Index_Offset_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000010#;
    Video_Encode_H264_Std_Pic_Init_QP_Minus_26_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000020#;
    Video_Encode_H264_Std_Weighted_Pred_Flag_Set_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000040#;
    Video_Encode_H264_Std_Weighted_Bipred_IDC_Explicit_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000080#;
    Video_Encode_H264_Std_Weighted_Bipred_IDC_Implicit_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000100#;
    Video_Encode_H264_Std_Transform_8x8_Mode_Flag_Set_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000200#;
    Video_Encode_H264_Std_Direct_Spatial_MV_Pred_Flag_Unset_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000400#;
    Video_Encode_H264_Std_Entropy_Coding_Mode_Flag_Unset_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00000800#;
    Video_Encode_H264_Std_Entropy_Coding_Mode_Flag_Set_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00001000#;
    Video_Encode_H264_Std_Direct_8x8_Inference_Flag_Unset_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00002000#;
    Video_Encode_H264_Std_Constrained_Intra_Pred_Flag_Set_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00004000#;
    Video_Encode_H264_Std_Deblocking_Filter_Disabled_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00008000#;
    Video_Encode_H264_Std_Deblocking_Filter_Enabled_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00010000#;
    Video_Encode_H264_Std_Deblocking_Filter_Partial_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00020000#;
    Video_Encode_H264_Std_Slice_QP_Delta_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00080000#;
    Video_Encode_H264_Std_Different_Slice_QP_Delta_Bit:
        constant Video_Encode_H264_Std_Flags := 16#00100000#;

    type Video_Encode_H264_Rate_Control_Flags is new Flags;

    Video_Encode_H264_Rate_Control_No_Bit:
        constant Video_Encode_H264_Rate_Control_Flags := 0;
    Video_Encode_H264_Rate_Control_Attempt_HRD_Compliance_Bit:
        constant Video_Encode_H264_Rate_Control_Flags := 16#00000001#;
    Video_Encode_H264_Rate_Control_Regular_GOP_Bit:
        constant Video_Encode_H264_Rate_Control_Flags := 16#00000002#;
    Video_Encode_H264_Rate_Control_Reference_Pattern_Flat_Bit:
        constant Video_Encode_H264_Rate_Control_Flags := 16#00000004#;
    Video_Encode_H264_Rate_Control_Reference_Pattern_Dyadic_Bit:
        constant Video_Encode_H264_Rate_Control_Flags := 16#00000008#;
    Video_Encode_H264_Rate_Control_Temporal_Layer_Pattern_Dyadic_Bit:
        constant Video_Encode_H264_Rate_Control_Flags := 16#00000010#;

    type Video_Encode_H265_Capability_Flags is new Flags;

    Video_Encode_H265_Capability_No_Bit:
        constant Video_Encode_H265_Capability_Flags := 0;
    Video_Encode_H265_Capability_Compliance_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000001#;
    Video_Encode_H265_Capability_Predication_Weight_Table_Generated_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000002#;
    Video_Encode_H265_Capability_Row_Unaligned_Slice_Segment_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000004#;
    Video_Encode_H265_Capability_Different_Slice_Segment_Type_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000008#;
    Video_Encode_H265_Capability_B_Frame_In_L0_List_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000010#;
    Video_Encode_H265_Capability_B_Frame_In_L1_List_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000020#;
    Video_Encode_H265_Capability_Per_Picture_Type_Min_Max_QP_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000040#;
    Video_Encode_H265_Capability_Per_Slice_Segment_Constant_QP_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000080#;
    Video_Encode_H265_Capability_Multiple_Tiles_Per_Slice_Segment_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000100#;
    Video_Encode_H265_Capability_Multiple_Slice_Segments_Per_Tile_Bit:
        constant Video_Encode_H265_Capability_Flags := 16#00000200#;

    type Video_Encode_H265_Std_Flags is new Flags;

    Video_Encode_H265_Std_No_Bit: constant Video_Encode_H265_Std_Flags := 0;
    Video_Encode_H265_Std_Separate_Color_Plane_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000001#;
    Video_Encode_H265_Std_Sample_Adaptive_Offset_Enabled_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000002#;
    Video_Encode_H265_Std_Scaling_List_Data_Present_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000004#;
    Video_Encode_H265_Std_PCM_Enabled_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000008#;
    Video_Encode_H265_Std_SPS_Temporal_MVP_Enabled_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000010#;
    Video_Encode_H265_Std_Init_QP_Minus_26_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000020#;
    Video_Encode_H265_Std_Weighted_Pred_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000040#;
    Video_Encode_H265_Std_Weighted_Bipred_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000080#;
    Video_Encode_H265_Std_Log2_Parallel_Merge_Level_Minus_2_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000100#;
    Video_Encode_H265_Std_Sign_Data_Hiding_Enabled_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000200#;
    Video_Encode_H265_Std_Transform_Skip_Enabled_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000400#;
    Video_Encode_H265_Std_Transform_Skip_Enabled_Flag_Unset_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00000800#;
    Video_Encode_H265_Std_PPS_Slice_Chroma_QP_Offsets_Present_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00001000#;
    Video_Encode_H265_Std_Transquant_Bypass_Enabled_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00002000#;
    Video_Encode_H265_Std_Constrained_Intra_Pred_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00004000#;
    Video_Encode_H265_Std_Entropy_Coding_Sync_Enabled_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00008000#;
    Video_Encode_H265_Std_Deblocking_Filter_Override_Enabled_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00010000#;
    Video_Encode_H265_Std_Dependent_Slice_Segments_Enabled_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00020000#;
    Video_Encode_H265_Std_Dependent_Slice_Segment_Flag_Set_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00040000#;
    Video_Encode_H265_Std_Slice_QP_Delta_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00080000#;
    Video_Encode_H265_Std_Different_Slice_QP_Delta_Bit:
        constant Video_Encode_H265_Std_Flags := 16#00100000#;

    type Video_Encode_H265_CTB_Size_Flags is new Flags;

    Video_Encode_H265_CTB_Size_No_Bit:
        constant Video_Encode_H265_CTB_Size_Flags := 0;
    Video_Encode_H265_CTB_Size_16_Bit:
        constant Video_Encode_H265_CTB_Size_Flags := 16#00000001#;
    Video_Encode_H265_CTB_Size_32_Bit:
        constant Video_Encode_H265_CTB_Size_Flags := 16#00000002#;
    Video_Encode_H265_CTB_Size_64_Bit:
        constant Video_Encode_H265_CTB_Size_Flags := 16#00000004#;

    type Video_Encode_H265_Transform_Block_Size_Flags is new Flags;

    Video_Encode_H265_Transform_Block_Size_No_Bit:
        constant Video_Encode_H265_Transform_Block_Size_Flags := 0;
    Video_Encode_H265_Transform_Block_Size_4_Bit:
        constant Video_Encode_H265_Transform_Block_Size_Flags := 16#00000001#;
    Video_Encode_H265_Transform_Block_Size_8_Bit:
        constant Video_Encode_H265_Transform_Block_Size_Flags := 16#00000002#;
    Video_Encode_H265_Transform_Block_Size_16_Bit:
        constant Video_Encode_H265_Transform_Block_Size_Flags := 16#00000004#;
    Video_Encode_H265_Transform_Block_Size_32_Bit:
        constant Video_Encode_H265_Transform_Block_Size_Flags := 16#00000008#;

    type Video_Encode_H265_Rate_Control_Flags is new Flags;

    Video_Encode_H265_Rate_Control_No_Bit:
        constant Video_Encode_H265_Rate_Control_Flags := 0;
    Video_Encode_H265_Rate_Control_Attempt_HRD_Compliance_Bit:
        constant Video_Encode_H265_Rate_Control_Flags := 16#00000001#;
    Video_Encode_H265_Rate_Control_Regular_GOP_Bit:
        constant Video_Encode_H265_Rate_Control_Flags := 16#00000002#;
    Video_Encode_H265_Rate_Control_Reference_Pattern_Flat_Bit:
        constant Video_Encode_H265_Rate_Control_Flags := 16#00000004#;
    Video_Encode_H265_Rate_Control_Reference_Pattern_Dyadic_Bit:
        constant Video_Encode_H265_Rate_Control_Flags := 16#00000008#;
    Video_Encode_H265_Rate_Control_Temporal_Sub_Layer_Pattern_Dyadic_Bit:
        constant Video_Encode_H265_Rate_Control_Flags := 16#00000010#;

    type Video_Decode_H264_Picture_Layout_Flags is new Flags;

    Video_Decode_H264_Picture_Layout_Progressive_Bit:
        constant Video_Decode_H264_Picture_Layout_Flags := 0;
    Video_Decode_H264_Picture_Layout_Interlaced_Interleaved_Lines_Bit:
        constant Video_Decode_H264_Picture_Layout_Flags := 16#00000001#;
    Video_Decode_H264_Picture_Layout_Interlaces_Separate_Planes_Bit:
        constant Video_Decode_H264_Picture_Layout_Flags := 16#00000002#;

    type Performance_Counter_Description_Flags is new Flags;

    Performance_Counter_Description_No_Bit:
        constant Performance_Counter_Description_Flags := 0;
    Performance_Counter_Description_Performance_Impacting_Bit:
        constant Performance_Counter_Description_Flags := 16#00000001#;
    Performance_Counter_Description_Performance_Impacted_Bit:
        constant Performance_Counter_Description_Flags := 16#00000002#;

    type Acquire_Profiling_Lock_Flags is new Flags;

    Acquire_Profiling_Lock_No_Bit: constant Acquire_Profiling_Lock_Flags := 0;

    type Video_Encode_Flags is new Flags;

    Video_Encode_No_Bit: constant Video_Encode_Flags := 0;

    type Video_Encode_Capability_Flags is new Flags;

    Video_Encode_Capability_No_Bit: constant Video_Encode_Capability_Flags := 0;
    Video_Encode_Capability_Preceding_Externally_Encoded_Bytes_Bit:
        constant Video_Encode_Capability_Flags := 16#00000001#;
    Video_Encode_Capability_Insufficient_Bitstream_Buffer_Range_Detection_Bit:
        constant Video_Encode_Capability_Flags := 16#00000002#;

    type Video_Encode_Rate_Control_Mode_Flags is new Flags;

    Video_Encode_Rate_Control_Mode_Default_Bit:
        constant Video_Encode_Rate_Control_Mode_Flags := 0;
    Video_Encode_Rate_Control_Mode_Disabled_Bit:
        constant Video_Encode_Rate_Control_Mode_Flags := 16#00000001#;
    Video_Encode_Rate_Control_Mode_CBR_Bit:
        constant Video_Encode_Rate_Control_Mode_Flags := 16#00000002#;
    Video_Encode_Rate_Control_Mode_VBR_Bit:
        constant Video_Encode_Rate_Control_Mode_Flags := 16#00000004#;

    type Video_Encode_Feedback_Flags is new Flags;

    Video_Encode_Feedback_No_Bit: constant Video_Encode_Feedback_Flags := 0;
    Video_Encode_Feedback_Bitstream_Buffer_Offset_Bit:
        constant Video_Encode_Feedback_Flags := 16#00000001#;
    Video_Encode_Feedback_Bitstream_Bytes_Written_Bit:
        constant Video_Encode_Feedback_Flags := 16#00000002#;
    Video_Encode_Feedback_Bitstream_Has_Overrides_Bit:
        constant Video_Encode_Feedback_Flags := 16#00000004#;

    type Video_Encode_Usage_Flags is new Flags;

    Video_Encode_Usage_Default_Bit: constant Video_Encode_Usage_Flags := 0;
    Video_Encode_Usage_Transcoding_Bit:
        constant Video_Encode_Usage_Flags := 16#00000001#;
    Video_Encode_Usage_Streaming_Bit:
        constant Video_Encode_Usage_Flags := 16#00000002#;
    Video_Encode_Usage_Recording_Bit:
        constant Video_Encode_Usage_Flags := 16#00000004#;
    Video_Encode_Usage_Conferencing_Bit:
        constant Video_Encode_Usage_Flags := 16#00000008#;

    type Video_Encode_Content_Flags is new Flags;

    Video_Encode_Content_Default_Bit: constant Video_Encode_Content_Flags := 0;
    Video_Encode_Content_Camera_Bit:
        constant Video_Encode_Content_Flags := 16#00000001#;
    Video_Encode_Content_Desktop_Bit:
        constant Video_Encode_Content_Flags := 16#00000002#;
    Video_Encode_Content_Rendered_Bit:
        constant Video_Encode_Content_Flags := 16#00000004#;

    type Video_Encode_Rate_Control_Flags is new Flags;

    Video_Encode_Rate_Control_No_Bit:
        constant Video_Encode_Rate_Control_Flags := 0;

    type Xlib_Surface_Create_Flags is new Flags;

    Xlib_Surface_Create_No_Bit: constant Xlib_Surface_Create_Flags := 0;

    type Xcb_Surface_Create_Flags is new Flags;

    Xcb_Surface_Create_No_Bit: constant Xcb_Surface_Create_Flags := 0;

    type Wayland_Surface_Create_Flags is new Flags;

    Wayland_Surface_Create_No_Bit: constant Wayland_Surface_Create_Flags := 0;

    type Win32_Surface_Create_Flags is new Flags;

    Win32_Surface_Create_No_Bit: constant Win32_Surface_Create_Flags := 0;

    -- Records.
    type Surface_Capabilities is
    record
        Min_Image_Count: Interfaces.Unsigned_32;
        Max_Image_Count: Interfaces.Unsigned_32;
        Current_Extent: Extent_2D;
        Min_Image_Extent: Extent_2D;
        Max_Image_Extent: Extent_2D;
        Max_Image_Array_Layers: Interfaces.Unsigned_32;
        Supported_Transforms:
            Surface_Transform_Flags := Surface_Transform_No_Bit;
        Current_Transform:
            Surface_Transform_Flags := Surface_Transform_No_Bit;
        Supported_Composite_Alpha:
            Composite_Alpha_Flags := Composite_Alpha_No_Bit;
        Supported_Usage_Flags: Image_Usage_Flags := Image_Usage_No_Bit;
    end record
        with Convention => C;

    type Surface_Format is
    record
        Format: Vulkan.Format;
        Color_Space: KHR.Color_Space;
    end record
        with Convention => C;

    package Surface_Format_Vectors is
        new Ada.Containers.Vectors(Positive, Surface_Format);

    type Swapchain_Create_Info is
        new In_Structure(Swapchain_Create_Info_Type) with
    record
        Flags: Swapchain_Create_Flags := Swapchain_Create_No_Bit;
        Surface: KHR.Surface;
        Min_Image_Count: Interfaces.Unsigned_32;
        Image_Format: Format;
        Image_Color_Space: Color_Space;
        Image_Extent: Extent_2D;
        Image_Array_Layers: Interfaces.Unsigned_32;
        Image_Usage: Image_Usage_Flags := Image_Usage_No_Bit;
        Image_Sharing_Mode: Sharing_Mode;
        Queue_Family_Indices: Queue_Family_Index_Vectors.Vector;
        Pre_Transform: Surface_Transform_Flags := Surface_Transform_No_Bit;
        Composite_Alpha: Composite_Alpha_Flags := Composite_Alpha_No_Bit;
        Present_Mode: KHR.Present_Mode;
        Clipped: Boolean;
        Old_Swapchain: Swapchain;
    end record;

    package Swapchain_Create_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Swapchain_Create_Info);

    type Present_Info is new In_Structure(Present_Info_Type) with
    record
        Wait_Semaphores: Semaphore_Vectors.Vector;
        Swapchains: Swapchain_Vectors.Vector;
        Image_Indices: Unsigned_32_Vectors.Vector;
        Results: Result_Vectors.Vector;
    end record;

    type Present_Mask_Array is array (1 .. Max_Device_Group_Size)
        of Interfaces.Unsigned_32
        with Convention => C;
    
    type Image_Swapchain_Create_Info is
        new In_Structure(Image_Swapchain_Create_Info_Type) with
    record
        Swapchain: KHR.Swapchain;
    end record;

    type Bind_Image_Memory_Swapchain_Info is
        new In_Structure(Bind_Image_Memory_Swapchain_Info_Type) with
    record
        Swapchain: KHR.Swapchain;
        Image_Index: Interfaces.Unsigned_32;
    end record;

    type Acquire_Next_Image_Info is
        new In_Structure(Acquire_Next_Image_Info_Type) with
    record
        Swapchain: KHR.Swapchain;
        Timeout: Interfaces.Unsigned_64;
        Semaphore: Vulkan.Semaphore;
        Fence: Vulkan.Fence;
        Device_Mask: Interfaces.Unsigned_32;
    end record;

    type Device_Group_Present_Capabilities is
        new Out_Structure(Device_Group_Present_Capabilities_Type) with
    record
        Present_Mask: Present_Mask_Array;
        Modes: Device_Group_Present_Mode_Flags
                := Device_Group_Present_Mode_No_Bit;
    end record;

    type Device_Group_Present_Info is
        new In_Structure(Device_Group_Present_Info_Type) with
    record
        Swapchain_Count: Interfaces.Unsigned_32;
        Device_Masks: Unsigned_32_Vectors.Vector;
        Mode: Device_Group_Present_Mode_Flags
                := Device_Group_Present_Mode_No_Bit;
    end record;

    type Device_Group_Swapchain_Create_Info is
        new In_Structure(Device_Group_Swapchain_Create_Info_Type) with
    record
        Modes: Device_Group_Present_Mode_Flags
            := Device_Group_Present_Mode_No_Bit;
    end record;

    type Display_Mode_Parameters is
    record
        Visible_Region: Extent_2D;
        Refresh_Rate: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Display_Mode_Create_Info is
        new In_Structure(Display_Mode_Create_Info_Type) with
    record
        Flags: Display_Mode_Create_Flags := Display_Mode_Create_No_Bit;
        Parameters: Display_Mode_Parameters;
    end record;

    type Display_Mode_Properties is
    record
        Display_Mode: KHR.Display_Mode;
        Parameters: Display_Mode_Parameters;
    end record
        with Convention => C;

    package Display_Mode_Properties_Vectors is
        new Ada.Containers.Vectors(Positive, Display_Mode_Properties);
    
    type Display_Plane_Capabilities is
    record
        Supported_Alpha: Display_Plane_Alpha_Flags :=
            Display_Plane_Alpha_No_Bit;
        Min_Src_Position: Offset_2D;
        Max_Src_Position: Offset_2D;
        Min_Src_Extent: Extent_2D;
        Max_Src_Extent: Extent_2D;
        Min_Dst_Position: Offset_2D;
        Max_Dst_Position: Offset_2D;
        Min_Dst_Extent: Extent_2D;
        Max_Dst_Extent: Extent_2D;
    end record
        with Convention => C;

    type Display_Plane_Properties is
    record
        Current_Display: Display;
        Current_Stack_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    package Display_Plane_Properties_Vectors is
        new Ada.Containers.Vectors(Positive, Display_Plane_Properties);

    type Display_Properties is
    record
        Display: KHR.Display;
        Display_Name: Ada.Strings.Unbounded.Unbounded_String;
        Physical_Dimensions: Extent_2D;
        Physical_Resolution: Extent_2D;
        Supported_Transforms: Surface_Transform_Flags :=
            Surface_Transform_No_Bit;
        Plane_Reorder_Possible: Boolean;
        Persistent_Content: Boolean;
    end record;

    package Display_Properties_Vectors is
        new Ada.Containers.Vectors(Positive, Display_Properties);

    type Display_Surface_Create_Info is
        new In_Structure(Display_Surface_Create_Info_Type) with
    record
        Flags: Display_Surface_Create_Flags := Display_Surface_Create_No_Bit;
        Display_Mode: KHR.Display_Mode;
        Plane_Index: Interfaces.Unsigned_32;
        Plane_Stack_Index: Interfaces.Unsigned_32;
        Transform: Surface_Transform_Flags := Surface_Transform_No_Bit;
        Global_Alpha: Float;
        Alpha_Mode: Display_Plane_Alpha_Flags := Display_Plane_Alpha_No_Bit;
        Image_Extent: Extent_2D;
    end record;

    type Display_Present_Info is
        new In_Structure(Display_Present_Info_Type) with
    record
        Src_Rect: Rect_2D;
        Dst_Rect: Rect_2D;
        Persistent: Boolean;
    end record;

    type Queue_Family_Query_Result_Status_Properties is new Out_Structure
        (Queue_Family_Query_Result_Status_Properties_Type) with
    record
        Query_Result_Status_Support: Boolean;
    end record;

    type Queue_Family_Video_Properties is new Out_Structure
        (Queue_Family_Video_Properties_Type) with
    record
        Video_Codec_Operations: Video_Codec_Operation_Flags :=
            Video_Codec_Operation_No_Bit;
    end record;

    type Video_Profile_Info is new In_Structure(Video_Profile_Info_Type) with
    record
        Video_Codec_Operation: Video_Codec_Operation_Flags :=
            Video_Codec_Operation_No_Bit;
        Chroma_Subsampling: Video_Chroma_Subsampling_Flags :=
            Video_Chroma_Subsampling_Invalid_Bit;
        Luma_Bit_Depth: Video_Component_Bit_Depth_Flags :=
            Video_Component_Bit_Depth_Invalid_Bit;
        Chroma_Bit_Depth: Video_Component_Bit_Depth_Flags :=
            Video_Component_Bit_Depth_Invalid_Bit;
    end record;

    type Video_Profile_Info_Access is access constant Video_Profile_Info
        with Storage_Size => 0;

    package Video_Profile_Info_Vectors is new Ada.Containers.Vectors
        (Positive, Video_Profile_Info);

    type Video_Profile_List_Info is new In_Structure
        (Video_Profile_List_Info_Type) with
    record
        Profiles: Video_Profile_Info_Vectors.Vector;
    end record;

    type Video_Capabilities is new Out_Structure(Video_Capabilities_Type) with
    record
        Flags: Video_Capability_Flags := Video_Capability_No_Bit;
        Min_Bitstream_Buffer_Offset_Alignment: Device_Size;
        Min_Bitstream_Buffer_Size_Alignment: Device_Size;
        Picture_Access_Granularity: Extent_2D;
        Min_Coded_Extent: Extent_2D;
        Max_Coded_Extent: Extent_2D;
        Max_DPB_Slots: Interfaces.Unsigned_32;
        Max_Active_Reference_Pictures: Interfaces.Unsigned_32;
        Std_Header_Version: Extension_Properties;
    end record;

    type Physical_Device_Video_Format_Info is new In_Structure
        (Physical_Device_Video_Format_Info_Type) with
    record
        Image_Usage: Image_Usage_Flags := Image_Usage_No_Bit;
    end record;

    type Video_Format_Properties is new Out_Structure
        (Video_Format_Properties_Type) with
    record
        Format: Vulkan.Format;
        Component_Mapping: Vulkan.Component_Mapping;
        Image_Create_Flags: Vulkan.Image_Create_Flags := Image_Create_No_Bit;
        Image_Type: Vulkan.Image_Type;
        Image_Tiling: Vulkan.Image_Tiling;
        Image_Usage_Flags: Vulkan.Image_Usage_Flags := Image_Usage_No_Bit;
    end record;

    package Video_Format_Properties_Vectors is new Ada.Containers.Vectors
        (Positive, Video_Format_Properties);

    type Video_Picture_Resource_Info is new In_Structure
        (Video_Picture_Resource_Info_Type) with
    record
        Coded_Offset: Offset_2D;
        Coded_Extent: Extent_2D;
        Base_Array_Layer: Array_Layers;
        Image_View_Binding: Image_View;
    end record;

    type Video_Picture_Resource_Info_Access is
        access constant Video_Picture_Resource_Info
        with Storage_Size => 0;

    type Video_Reference_Slot_Info is new In_Structure
        (Video_Reference_Slot_Info_Type) with
    record
        Slot_Index: Interfaces.Unsigned_32;
        Picture_Resource: Video_Picture_Resource_Info_Access;
    end record;

    type Video_Reference_Slot_Info_Access is
        access constant Video_Reference_Slot_Info
        with Storage_Size => 0;

    package Video_Reference_Slot_Info_Vectors is new Ada.Containers.Vectors
        (Positive, Video_Reference_Slot_Info);

    type Video_Session_Memory_Requirements is new Out_Structure
        (Video_Session_Memory_Requirements_Type) with
    record
        Memory_Bind_Index: Interfaces.Unsigned_32;
        Memory_Requirements: Vulkan.Memory_Requirements;
    end record;

    package Video_Session_Memory_Requirements_Vectors is
        new Ada.Containers.Vectors(Positive, Video_Session_Memory_Requirements);

    type Bind_Video_Session_Memory_Info is new In_Structure
        (Bind_Video_Session_Memory_Info_Type) with
    record
        Memory_Bind_Index: Interfaces.Unsigned_32;
        Memory: Device_Memory;
        Memory_Offset: Device_Size;
        Memory_Size: Device_Size;
    end record;

    package Bind_Video_Session_Memory_Info_Vectors is new Ada.Containers.Vectors
        (Positive, Bind_Video_Session_Memory_Info);

    type Video_Session_Create_Info is new In_Structure
        (Video_Session_Create_Info_Type) with
    record
        Queue_Family_Index: Vulkan.Queue_Family_Index;
        Flags: Video_Session_Create_Flags := Video_Session_Create_No_Bit;
        Video_Profile: Video_Profile_Info_Access;
        Picture_Format: Format;
        Max_Coded_Extent: Extent_2D;
        Reference_Picture_Format: Format;
        Max_DPB_Slots: Interfaces.Unsigned_32;
        Max_Active_Reference_Pictures: Interfaces.Unsigned_32;
        Std_Header_Version: Extension_Properties_Access;
    end record;

    type Video_Session_Parameters_Create_Info is new In_Structure
        (Video_Session_Parameters_Create_Info_Type) with
    record
        Flags: Video_Session_Parameters_Create_Flags :=
            Video_Session_Parameters_Create_No_Bit;
        Video_Session_Parameters_Template: Video_Session_Parameters;
        Video_Session: KHR.Video_Session;
    end record;

    type Video_Session_Parameters_Update_Info is new In_Structure
        (Video_Session_Parameters_Update_Info_Type) with
    record
        Update_Sequence_Count: Interfaces.Unsigned_32;
    end record;

    type Video_Begin_Coding_Info is new In_Structure
        (Video_Begin_Coding_Info_Type) with
    record
        Flags: Video_Begin_Coding_Flags := Video_Begin_Coding_No_Bit;
        Video_Session: KHR.Video_Session;
        Video_Session_Parameters: KHR.Video_Session_Parameters;
        Reference_Slots: Video_Reference_Slot_Info_Vectors.Vector;
    end record;

    type Video_End_Coding_Info is new In_Structure
        (Video_End_Coding_Info_Type) with
    record
        Flags: Video_End_Coding_Flags := Video_End_Coding_No_Bit;
    end record;

    type Video_Coding_Control_Info is new In_Structure
        (Video_Coding_Control_Info_Type) with
    record
        Flags: Video_Coding_Control_Flags := Video_Coding_Control_No_Bit;
    end record;

    type Video_Decode_Capabilities is new Out_Structure
        (Video_Decode_Capabilities_Type) with
    record
        Flags: Video_Decode_Capability_Flags := Video_Decode_Capability_No_Bit;
    end record;

    type Video_Decode_Usage_Info is new In_Structure
        (Video_Decode_Usage_Info_Type) with
    record
        Video_Usage_Hints: Video_Decode_Usage_Flags :=
            Video_Decode_Usage_Default_Bit;
    end record;

    type Video_Decode_Info is new In_Structure(Video_Decode_Info_Type) with
    record
        Flags: Video_Decode_Flags := Video_Decode_No_Bit;
        Src_Buffer: Buffer;
        Src_Buffer_Offset: Device_Size;
        Src_Buffer_Range: Device_Size;
        Dst_Picture_Resource: Video_Picture_Resource_Info;
        Setup_Reference_Slot: Video_Reference_Slot_Info_Access;
        Reference_Slots: Video_Reference_Slot_Info_Vectors.Vector;
    end record;

    type Video_Encode_H264_Capabilities is new Out_Structure
        (Video_Encode_H264_Capabilities_Type) with
    record
        Flags: Video_Encode_H264_Capability_Flags :=
            Video_Encode_H264_Capability_No_Bit;
        Max_Level_IDC: Std_Video.H264.Level_IDC;
        Max_Slice_Count: Interfaces.Unsigned_32;
        Max_PPicture_L0_Reference_Count: Interfaces.Unsigned_32;
        Max_BPicture_L0_Reference_Count: Interfaces.Unsigned_32;
        Max_L1_Reference_Count: Interfaces.Unsigned_32;
        Max_Temporal_Layer_Count: Interfaces.Unsigned_32;
        Expect_Dyadic_Temporal_Layer_Pattern: Boolean;
        Min_QP: Interfaces.Integer_32;
        Max_QP: Interfaces.Integer_32;
        Prefers_GOP_Remaining_Frames: Boolean;
        Requires_GOP_Remaining_Frames: Boolean;
        Std_Syntax_Flags: Video_Encode_H264_Std_Flags :=
            Video_Encode_H264_Std_No_Bit;
    end record;

    type Video_Encode_H264_QP is
    record
        QPI: Interfaces.Integer_32;
        QPP: Interfaces.Integer_32;
        QPB: Interfaces.Integer_32;
    end record
        with Convention => C;

    type Video_Encode_H264_Quality_Level_Properties is new Out_Structure
        (Video_Encode_H264_Quality_Level_Properties_Type) with
    record
        Preferred_Rate_Control_Flags: Video_Encode_H264_Rate_Control_Flags :=
            Video_Encode_H264_Rate_Control_No_Bit;
        Preferred_GOP_Frame_Count: Interfaces.Unsigned_32;
        Preferred_IDR_Period: Interfaces.Unsigned_32;
        Preferred_Consecutive_B_Frame_Count: Interfaces.Unsigned_32;
        Preferred_Temporal_Layer_Count: Interfaces.Unsigned_32;
        Preferred_Constant_QP: Video_Encode_H264_QP;
        Preferred_Max_L0_Reference_Count: Interfaces.Unsigned_32;
        Preferred_Max_L1_Reference_Count: Interfaces.Unsigned_32;
        Preferred_Std_Entropy_Coding_Mode_Flag: Boolean;
    end record;

    type Video_Encode_H264_Session_Create_Info is new In_Structure
        (Video_Encode_H264_Session_Create_Info_Type) with
    record
        Use_Max_Level_IDC: Boolean;
        Max_Level_IDC: Std_Video.H264.Level_IDC;
    end record;

    type Video_Encode_H264_Session_Parameters_Add_Info is new In_Structure
        (Video_Encode_H264_Session_Parameters_Add_Info_Type) with
    record
        Std_SPSs: Std_Video.H264.Sequence_Parameter_Set_Vectors.Vector;
        Std_PPSs: Std_Video.H264.Picture_Parameter_Set_Vectors.Vector;
    end record;

    type Video_Encode_H264_Session_Parameters_Add_Info_Access is
        access constant Video_Encode_H264_Session_Parameters_Add_Info
        with Storage_Size => 0;

    type Video_Encode_H264_Session_Parameters_Create_Info is new In_Structure
        (Video_Encode_H264_Session_Parameters_Create_Info_Type) with
    record
        Max_Std_SPS_Count: Interfaces.Unsigned_32;
        Max_Std_PPS_Count: Interfaces.Unsigned_32;
        Parameters_Add_Info:
            Video_Encode_H264_Session_Parameters_Add_Info_Access;
    end record;

    type Video_Encode_H264_Session_Parameters_Get_Info is new In_Structure
        (Video_Encode_H264_Session_Parameters_Get_Info_Type) with
    record
        Write_Std_SPS: Boolean;
        Write_Std_PPS: Boolean;
        Std_SPS_ID: Interfaces.Unsigned_32;
        Std_PPS_ID: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_H264_Session_Parameters_Feedback_Info is new Out_Structure
        (Video_Encode_H264_Session_Parameters_Feedback_Info_Type) with
    record
        Has_Std_SPS_Overrides: Boolean;
        Has_Std_PPS_Overrides: Boolean;
    end record;

    type Video_Encode_H264_Nalu_Slice_Info is new In_Structure
        (Video_Encode_H264_Nalu_Slice_Info_Type) with
    record
        Constant_QP: Interfaces.Integer_32;
        Std_Slice_Header: Std_Video.H264.Encode.Slice_Header_Access;
    end record;

    package Video_Encode_H264_Nalu_Slice_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Video_Encode_H264_Nalu_Slice_Info);

    type Video_Encode_H264_Picture_Info is new In_Structure
        (Video_Encode_H264_Picture_Info_Type) with
    record
        Nalu_Slice_Entries: Video_Encode_H264_Nalu_Slice_Info_Vectors.Vector;
        Std_Picture_Info: Std_Video.H264.Encode.Picture_Info_Access;
        Generate_Prefix_Nalu: Boolean;
    end record;

    type Video_Encode_H264_DPB_Slot_Info is new In_Structure
        (Video_Encode_H264_DPB_Slot_Info_Type) with
    record
        Std_Reference_Info: Std_Video.H264.Encode.Reference_Info_Access;
    end record;

    type Video_Encode_H264_Profile_Info is new In_Structure
        (Video_Encode_H264_Profile_Info_Type) with
    record
        Std_Profile_IDC: Std_Video.H264.Profile_IDC;
    end record;

    type Video_Encode_H264_Rate_Control_Info is new In_Structure
        (Video_Encode_H264_Rate_Control_Info_Type) with
    record
        Flags: Video_Encode_H264_Rate_Control_Flags :=
            Video_Encode_H264_Rate_Control_No_Bit;
        GOP_Frame_Count: Interfaces.Unsigned_32;
        IDR_Period: Interfaces.Unsigned_32;
        Consecutive_B_Frame_Count: Interfaces.Unsigned_32;
        Temporal_Layer_Count: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_H264_Frame_Size is
    record
        Frame_I_Size: Interfaces.Unsigned_32;
        Frame_P_Size: Interfaces.Unsigned_32;
        Frame_B_Size: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H264_Rate_Control_Layer_Info is new In_Structure
        (Video_Encode_H264_Rate_Control_Layer_Info_Type) with
    record
        Use_Min_QP: Boolean;
        Min_QP: Video_Encode_H264_QP;
        Use_Max_QP: Boolean;
        Max_QP: Video_Encode_H264_QP;
        Use_Max_Frame_Size: Boolean;
        Max_Frame_Size: Video_Encode_H264_Frame_Size;
    end record;

    type Video_Encode_H264_GOP_Remaining_Frame_Info is new In_Structure
        (Video_Encode_H264_GOP_Remaining_Frame_Info_Type) with
    record
        Use_GOP_Remaining_Frames: Boolean;
        GOP_Remaining_I: Interfaces.Unsigned_32;
        GOP_Remaining_P: Interfaces.Unsigned_32;
        GOP_Remaining_B: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_H265_Capabilities is new Out_Structure
        (Video_Encode_H265_Capabilities_Type) with
    record
        Flags: Video_Encode_H265_Capability_Flags :=
            Video_Encode_H265_Capability_No_Bit;
        Max_Level_IDC: Std_Video.H265.Level_IDC;
        Max_Slice_Segment_Count: Interfaces.Unsigned_32;
        Max_Tiles: Extent_2D;
        CTB_Sizes: Video_Encode_H265_CTB_Size_Flags :=
            Video_Encode_H265_CTB_Size_No_Bit;
        Transform_Block_Sizes: Video_Encode_H265_Transform_Block_Size_Flags :=
            Video_Encode_H265_Transform_Block_Size_No_Bit;
        Max_P_Picture_L0_Reference_Count: Interfaces.Unsigned_32;
        Max_B_Picture_L0_Reference_Count: Interfaces.Unsigned_32;
        Max_L1_Reference_Count: Interfaces.Unsigned_32;
        Max_Sub_Layer_Count: Interfaces.Unsigned_32;
        Expect_Dyadic_Temporal_Sub_Layer_Pattern: Boolean;
        Min_QP: Interfaces.Integer_32;
        Max_QP: Interfaces.Integer_32;
        Prefers_GOP_Remaining_Frames: Boolean;
        Requires_GOP_Remaining_Frames: Boolean;
        Std_Syntax_Flags: Video_Encode_H265_Std_Flags :=
            Video_Encode_H265_Std_No_Bit;
    end record;

    type Video_Encode_H265_Session_Create_Info is new In_Structure
        (Video_Encode_H265_Session_Create_Info_Type) with
    record
        Use_Max_Level_IDC: Boolean;
        Max_Level_IDC: Std_Video.H265.Level_IDC;
    end record;

    type Video_Encode_H265_QP is
    record
        QP_I: Interfaces.Integer_32;
        QP_P: Interfaces.Integer_32;
        QP_B: Interfaces.Integer_32;
    end record
        with Convention => C;

    type Video_Encode_H265_Quality_Level_Properties is new Out_Structure
        (Video_Encode_H265_Quality_Level_Properties_Type) with
    record
        Preferred_Rate_Control_Flags: Video_Encode_H265_Rate_Control_Flags :=
            Video_Encode_H265_Rate_Control_No_Bit;
        Preferred_GOP_Frame_Count: Interfaces.Unsigned_32;
        Preferred_IDR_Period: Interfaces.Unsigned_32;
        Preferred_Consecutive_B_Frame_Count: Interfaces.Unsigned_32;
        Preferred_Sub_Layer_Count: Interfaces.Unsigned_32;
        Preferred_Constant_QP: Video_Encode_H265_QP;
        Preferred_Max_L0_Reference_Count: Interfaces.Unsigned_32;
        Preferred_Max_L1_Reference_Count: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_H265_Session_Parameters_Add_Info is new In_Structure
        (Video_Encode_H265_Session_Parameters_Add_Info_Type) with
    record
        Std_VPSs: Std_Video.H265.Video_Parameter_Set_Vectors.Vector;
        Std_SPSs: Std_Video.H265.Sequence_Parameter_Set_Vectors.Vector;
        Std_PPSs: Std_Video.H265.Picture_Parameter_Set_Vectors.Vector;
    end record;

    type Video_Encode_H265_Session_Parameters_Add_Info_Access is
        access constant Video_Encode_H265_Session_Parameters_Add_Info
        with Storage_Size => 0;

    type Video_Encode_H265_Session_Parameters_Create_Info is new In_Structure
        (Video_Encode_H265_Session_Parameters_Create_Info_Type) with
    record
        Max_Std_VPS_Count: Interfaces.Unsigned_32;
        Max_Std_SPS_Count: Interfaces.Unsigned_32;
        Max_Std_PPS_Count: Interfaces.Unsigned_32;
        Parameters_Add_Info:
            Video_Encode_H265_Session_Parameters_Add_Info_Access;
    end record;

    type Video_Encode_H265_Session_Parameters_Get_Info is new In_Structure
        (Video_Encode_H265_Session_Parameters_Get_Info_Type) with
    record
        Write_Std_VPS: Boolean;
        Write_Std_SPS: Boolean;
        Write_Std_PPS: Boolean;
        Std_VPS_ID: Interfaces.Unsigned_32;
        Std_SPS_ID: Interfaces.Unsigned_32;
        Std_PPS_ID: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_H265_Session_Parameters_Feedback_Info is new Out_Structure
        (Video_Encode_H265_Session_Parameters_Feedback_Info_Type) with
    record
        Has_Std_VPS_Overrides: Boolean;
        Has_Std_SPS_Overrides: Boolean;
        Has_Std_PPS_Overrides: Boolean;
    end record;

    type Video_Encode_H265_Nalu_Slice_Segment_Info is new In_Structure
        (Video_Encode_H265_Nalu_Slice_Segment_Info_Type) with
    record
        Constant_QP: Interfaces.Integer_32;
        Std_Slice_Segment_Header:
            Std_Video.H265.Encode.Slice_Segment_Header_Access;
    end record;

    package Video_Encode_H265_Nalu_Slice_Segment_Info_Vectors is
        new Ada.Containers.Vectors
            (Positive, Video_Encode_H265_Nalu_Slice_Segment_Info);

    type Video_Encode_H265_Picture_Info is new In_Structure
        (Video_Encode_H265_Picture_Info_Type) with
    record
        Nalu_Slice_Segment_Entries:
            Video_Encode_H265_Nalu_Slice_Segment_Info_Vectors.Vector;
        Std_Picture_Info: Std_Video.H265.Encode.Picture_Info_Access;
    end record;

    type Video_Encode_H265_DPB_Slot_Info is new In_Structure
        (Video_Encode_H265_DPB_Slot_Info_Type) with
    record
        Std_Reference_Info: Std_Video.H265.Encode.Reference_Info_Access;
    end record;

    type Video_Encode_H265_Profile_Info is new In_Structure
        (Video_Encode_H265_Profile_Info_Type) with
    record
        Std_Profile_IDC: Std_Video.H265.Profile_IDC;
    end record;

    type Video_Encode_H265_Rate_Control_Info is new In_Structure
        (Video_Encode_H265_Rate_Control_Info_Type) with
    record
        Flags: Video_Encode_H265_Rate_Control_Flags :=
            Video_Encode_H265_Rate_Control_No_Bit;
        GOP_Frame_Count: Interfaces.Unsigned_32;
        IDR_Period: Interfaces.Unsigned_32;
        Consecutive_B_Frame_Count: Interfaces.Unsigned_32;
        Sub_Layer_Count: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_H265_Frame_Size is
    record
        Frame_I_Size: Interfaces.Unsigned_32;
        Frame_P_Size: Interfaces.Unsigned_32;
        Frame_B_Size: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Video_Encode_H265_Rate_Control_Layer_Info is new In_Structure
        (Video_Encode_H265_Rate_Control_Layer_Info_Type) with
    record
        Use_Min_QP: Boolean;
        Min_QP: Video_Encode_H265_QP;
        Use_Max_QP: Boolean;
        Max_QP: Video_Encode_H265_QP;
        Use_Max_Frame_Size: Boolean;
        Max_Frame_Size: Video_Encode_H265_Frame_Size;
    end record;

    type Video_Encode_H265_GOP_Remaining_Frame_Info is new In_Structure
        (Video_Encode_H265_GOP_Remaining_Frame_Info_Type) with
    record
        Use_GOP_Remaining_Frames: Boolean;
        GOP_Remaining_I: Interfaces.Unsigned_32;
        GOP_Remaining_P: Interfaces.Unsigned_32;
        GOP_Remaining_B: Interfaces.Unsigned_32;
    end record;

    type Video_Decode_H264_Profile_Info is new In_Structure
        (Video_Decode_H264_Profile_Info_Type) with
    record
        Std_Profile_IDC: Std_Video.H264.Profile_IDC;
        Picture_Layout: Video_Decode_H264_Picture_Layout_Flags := 
            Video_Decode_H264_Picture_Layout_Progressive_Bit;
    end record;

    type Video_Decode_H264_Capabilities is new Out_Structure
        (Video_Decode_H264_Capabilities_Type) with
    record
        Max_Level_IDC: Std_Video.H264.Level_IDC;
        Field_Offset_Granularity: Offset_2D;
    end record;

    type Video_Decode_H264_Session_Parameters_Add_Info is new In_Structure
        (Video_Decode_H264_Session_Parameters_Add_Info_Type) with
    record
        Std_SPSs: Std_Video.H264.Sequence_Parameter_Set_Vectors.Vector;
        Std_PPSs: Std_Video.H264.Picture_Parameter_Set_Vectors.Vector;
    end record;

    type Video_Decode_H264_Session_Parameters_Add_Info_Access is
        access constant Video_Decode_H264_Session_Parameters_Add_Info
        with Convention => C,
              Storage_Size => 0;

    type Video_Decode_H264_Session_Parameters_Create_Info is new In_Structure
        (Video_Decode_H264_Session_Parameters_Create_Info_Type) with
    record
        Max_Std_SPS_Count: Interfaces.Unsigned_32;
        Max_Std_PPS_Count: Interfaces.Unsigned_32;
        Parameters_Add_Info:
            Video_Decode_H264_Session_Parameters_Add_Info_Access;
    end record;

    type Video_Decode_H264_Picture_Info is new In_Structure
        (Video_Decode_H264_Picture_Info_Type) with
    record
        Std_Picture_Info: Std_Video.H264.Decode.Picture_Info_Access;
        Slice_Offsets: Unsigned_32_Vectors.Vector;
    end record;

    type Video_Decode_H264_DPB_Slot_Info is new In_Structure
        (Video_Decode_H264_DPB_Slot_Info_Type) with
    record
        Std_Reference_Info: Std_Video.H264.Decode.Reference_Info_Access;
    end record;

    type Rendering_Fragment_Shading_Rate_Attachment_Info is new In_Structure
        (Rendering_Fragment_Shading_Rate_Attachment_Info_Type) with
    record
        Image_View: Vulkan.Image_View;
        Image_Layout: Vulkan.Image_Layout;
        Shading_Rate_Attachment_Texel_Size: Extent_2D;
    end record;

    type Import_Memory_FD_Info is new In_Structure
        (Import_Memory_FD_Info_Type) with
    record
        Handle_Type: External_Memory_Handle_Type_Flags :=
            External_Memory_Handle_Type_No_Bit;
        FD: File_Descriptor;
    end record;

    type Memory_FD_Properties is new Out_Structure
        (Memory_FD_Properties_Type) with
    record
        Memory_Type_Bits: Interfaces.Unsigned_32;
    end record;

    type Memory_Get_FD_Info is new In_Structure(Memory_Get_FD_Info_Type) with
    record
        Memory: Device_Memory;
        Handle_Type: External_Memory_Handle_Type_Flags :=
            External_Memory_Handle_Type_No_Bit;
    end record;

    type Import_Semaphore_FD_Info is new In_Structure
        (Import_Semaphore_FD_Info_Type) with
    record
        Semaphore: Vulkan.Semaphore;
        Flags: Semaphore_Import_Flags := Semaphore_Import_No_Bit;
        Handle_Type: External_Semaphore_Handle_Type_Flags :=
            External_Semaphore_Handle_Type_No_Bit;
        FD: File_Descriptor;
    end record;

    type Semaphore_Get_FD_Info is new In_Structure
        (Semaphore_Get_FD_Info_Type) with
    record
        Semaphore: Vulkan.Semaphore;
        Handle_Type: External_Semaphore_Handle_Type_Flags :=
            External_Semaphore_Handle_Type_No_Bit;
    end record;

    type Rect_Layer is
    record
        Offset: Offset_2D;
        Extent: Extent_2D;
        Layer: Array_Layers;
    end record
        with Convention => C;

    package Rect_Layer_Vectors is new Ada.Containers.Vectors
        (Positive, Rect_Layer);

    type Present_Region is
    record
        Rectangles: Rect_Layer_Vectors.Vector;
    end record;

    package Present_Region_Vectors is new Ada.Containers.Vectors
        (Positive, Present_Region);

    type Present_Regions is new In_Structure(Present_Regions_Type) with
    record
        Swapchain_Count: Interfaces.Unsigned_32;
        Regions: Present_Region_Vectors.Vector;
    end record;

    type Shared_Present_Surface_Capabilities is new Out_Structure
        (Shared_Present_Surface_Capabilities_Type) with
    record
        Shared_Present_Supported_Usage_Flags: Image_Usage_Flags :=
            Image_Usage_No_Bit;
    end record;

    type Import_Fence_FD_Info is new In_Structure
        (Import_Fence_FD_Info_Type) with
    record
        Fence: Vulkan.Fence;
        Flags: Fence_Import_Flags := Fence_Import_No_Bit;
        Handle_Type: External_Fence_Handle_Type_Flags :=
            External_Fence_Handle_Type_No_Bit;
        FD: File_Descriptor;
    end record;

    type Fence_Get_FD_Info is new In_Structure(Fence_Get_FD_Info_Type) with
    record
        Fence: Vulkan.Fence;
        Handle_Type: External_Fence_Handle_Type_Flags :=
            External_Fence_Handle_Type_No_Bit;
    end record;

    type Physical_Device_Performance_Query_Features is new Out_Structure
        (Physical_Device_Performance_Query_Features_Type) with
    record
        Performance_Counter_Query_Pools: Boolean;
        Performance_Counter_Multiple_Query_Pools: Boolean;
    end record;

    type Physical_Device_Performance_Query_Properties is new Out_Structure
        (Physical_Device_Performance_Query_Properties_Type) with
    record
        Allow_Command_Buffer_Query_Copies: Boolean;
    end record;

    type Performance_Counter is new Out_Structure(Performance_Counter_Type) with
    record
        Unit: Performance_Counter_Unit;
        Scope: Performance_Counter_Scope;
        Storage: Performance_Counter_Storage;
        UUID: Vulkan.UUID;
    end record;

    package Performance_Counter_Vectors is new Ada.Containers.Vectors
        (Positive, Performance_Counter);

    type Performance_Counter_Description is new Out_Structure
        (Performance_Counter_Description_Type) with
    record
        Flags: Performance_Counter_Description_Flags :=
            Performance_Counter_Description_No_Bit;
        Name: Ada.Strings.Unbounded.Unbounded_String;
        Category: Ada.Strings.Unbounded.Unbounded_String;
        Description: Ada.Strings.Unbounded.Unbounded_String;
    end record;

    package Performance_Counter_Description_Vectors is
        new Ada.Containers.Vectors(Positive, Performance_Counter_Description);

    type Query_Pool_Performance_Create_Info is new In_Structure
        (Query_Pool_Performance_Create_Info_Type) with
    record
        Queue_Family_Index: Vulkan.Queue_Family_Index;
        Counter_Indices: Unsigned_32_Vectors.Vector;
    end record;

    -- Since this is passed directly into the query system via
    -- a void* I can't really wrap it in any convenient way.
    -- Use with caution.
    type Peformance_Counter_Result(Storage: Performance_Counter_Storage) is
    record
        case Storage is
            when Int32 =>
                Int32: Interfaces.Integer_32;
            when Int64 =>
                Int64: Interfaces.Integer_64;
            when Uint32 =>
                Uint32: Interfaces.Unsigned_32;
            when Uint64 =>
                Uint64: Interfaces.Unsigned_64;
            when Float32 =>
                Float32: Interfaces.C.C_float;
            when Float64 =>
                Float64: Interfaces.C.double;
        end case;
    end record
        with Convention => C,
             Unchecked_Union;

    type Acquire_Profiling_Lock_Info is new In_Structure
        (Acquire_Profiling_Lock_Info_Type) with
    record
        Flags: Acquire_Profiling_Lock_Flags := Acquire_Profiling_Lock_No_Bit;
        Timeout: Interfaces.Unsigned_64;
    end record;

    type Performance_Query_Submit_Info is new In_Structure
        (Performance_Query_Submit_Info_Type) with
    record
        Counter_Pass_Index: Interfaces.Unsigned_32;
    end record;

    type Physical_Device_Surface_Info_2 is new In_Structure
        (Physical_Device_Surface_Info_2_Type) with
    record
        Surface: KHR.Surface;
    end record;

    type Surface_Capabilities_2 is new Out_Structure
        (Surface_Capabilities_2_Type) with
    record
        Surface_Capabilities: KHR.Surface_Capabilities;
    end record;

    type Surface_Format_2 is new Out_Structure(Surface_Format_2_Type) with
    record
        Surface_Format: KHR.Surface_Format;
    end record;

    package Surface_Format_2_Vectors is new Ada.Containers.Vectors
        (Positive, Surface_Format_2);

    type Display_Properties_2 is new Out_Structure
        (Display_Properties_2_Type) with
    record
        Display_Properties: KHR.Display_Properties;
    end record;

    package Display_Properties_2_Vectors is new Ada.Containers.Vectors
        (Positive, Display_Properties_2);

    type Display_Plane_Properties_2 is new Out_Structure
        (Display_Plane_Properties_2_Type) with
    record
        Display_Plane_Properties: KHR.Display_Plane_Properties;
    end record;

    package Display_Plane_Properties_2_Vectors is new Ada.Containers.Vectors
        (Positive, Display_Plane_Properties_2);

    type Display_Mode_Properties_2 is new Out_Structure
        (Display_Mode_Properties_2_Type) with
    record
        Display_Mode_Properties: KHR.Display_Mode_Properties;
    end record;

    package Display_Mode_Properties_2_Vectors is new Ada.Containers.Vectors
        (Positive, Display_Mode_Properties_2);

    type Display_Plane_Info_2 is new In_Structure
        (Display_Plane_Info_2_Type) with
    record
        Mode: Display_Mode;
        Plane_Index: Interfaces.Unsigned_32;
    end record;

    type Display_Plane_Capabilities_2 is new Out_Structure
        (Display_Plane_Capabilities_2_Type) with
    record
        Capabilities: Display_Plane_Capabilities;
    end record;

    type Physical_Device_Shader_Clock_Features is new Out_Structure
        (Physical_Device_Shader_Clock_Features_Type) with
    record
        Shader_Subgroup_Clock: Boolean;
        Shader_Device_Clock: Boolean;
    end record;

    type Video_Decode_H265_Profile_Info is new In_Structure
        (Video_Decode_H265_Profile_Info_Type) with
    record
        Std_Profile_IDC: Std_Video.H265.Profile_IDC;
    end record;

    type Video_Decode_H265_Capabilities is new Out_Structure
        (Video_Decode_H265_Capabilities_Type) with
    record
        Max_Level_IDC: Std_Video.H265.Level_IDC;
    end record;

    type Video_Decode_H265_Session_Parameters_Add_Info is new In_Structure
        (Video_Decode_H265_Session_Parameters_Add_Info_Type) with
    record
        Std_VPSs: Std_Video.H265.Video_Parameter_Set_Vectors.Vector;
        Std_SPSs: Std_Video.H265.Sequence_Parameter_Set_Vectors.Vector;
        Std_PPSs: Std_Video.H265.Picture_Parameter_Set_Vectors.Vector;
    end record;

    type Video_Decode_H265_Session_Parameters_Add_Info_Access is
        access constant Video_Decode_H265_Session_Parameters_Add_Info
        with Storage_Size => 0;

    type Video_Decode_H265_Session_Parameters_Create_Info is new In_Structure
        (Video_Decode_H265_Session_Parameters_Create_Info_Type) with
    record
        Max_Std_VPS_Count: Interfaces.Unsigned_32;
        Max_Std_SPS_Count: Interfaces.Unsigned_32;
        Max_Std_PPS_Count: Interfaces.Unsigned_32;
        Parameters_Add_Info:
            Video_Decode_H265_Session_Parameters_Add_Info_Access;
    end record;

    type Video_Decode_H265_Picture_Info is new In_Structure
        (Video_Decode_H265_Picture_Info_Type) with
    record
        Std_Picture_Info: Std_Video.H265.Decode.Picture_Info_Access;
        Slice_Segment_Offsets: Unsigned_32_Vectors.Vector;
    end record;

    type Video_Decode_H265_DPB_Slot_Info is new In_Structure
        (Video_Decode_H265_DPB_Slot_Info_Type) with
    record
        Std_Reference_Info: Std_Video.H265.Decode.Reference_Info_Access;
    end record;
 
    type Fragment_Shading_Rate_Attachment_Info is new In_Structure
        (Fragment_Shading_Rate_Attachment_Info_Type) with
    record
        Fragment_Shading_Rate_Attachment: Attachment_Reference_2_Access;
        Shading_Rate_Attachment_Texel_Size: Extent_2D;
    end record;

    type Fragment_Shading_Rate_Combiner_Op_Array is
        array (1 .. 2) of Fragment_Shading_Rate_Combiner_Op
        with Convention => C;

    type Pipeline_Fragment_Shading_Rate_State_Create_Info is new In_Structure
        (Pipeline_Fragment_Shading_Rate_State_Create_Info_Type) with
    record
        Fragment_Size: Extent_2D;
        Combiner_Ops: Fragment_Shading_Rate_Combiner_Op_Array;
    end record;

    type Physical_Device_Fragment_Shading_Rate_Features is new Out_Structure
        (Physical_Device_Fragment_Shading_Rate_Features_Type) with
    record
        Pipeline_Fragment_Shading_Rate: Boolean;
        Primitive_Fragment_Shading_Rate: Boolean;
        Attachment_Fragment_Shading_Rate: Boolean;
    end record;

    type Physical_Device_Fragment_Shading_Rate_Properties is new Out_Structure
        (Physical_Device_Fragment_Shading_Rate_Properties_Type) with
    record
        Min_Fragment_Shading_Rate_Attachment_Texel_Size: Extent_2D;
        Max_Fragment_Shading_Rate_Attachment_Texel_Size: Extent_2D;
        Max_Fragment_Shading_Rate_Attachment_Texel_Size_Aspect_Ratio:
            Interfaces.Unsigned_32;
        Primitive_Fragment_Shading_Rate_With_Multiple_Viewports: Boolean;
        Layered_Shading_Rate_Attachments: Boolean;
        Fragment_Shading_Rate_Non_Trivial_Combiner_Ops: Boolean;
        Max_Fragment_Size: Extent_2D;
        Max_Fragment_Size_Aspect_Ratio: Interfaces.Unsigned_32;
        Max_Fragment_Shading_Rate_Coverage_Samples: Interfaces.Unsigned_32;
        Max_Fragment_Shading_Rate_Rasterization_Samples: Sample_Count_Flags :=
            Sample_Count_No_Bit;
        Fragment_Shading_Rate_With_Shader_Depth_Stencil_Writes: Boolean;
        Fragment_Shading_Rate_With_Sample_Mask: Boolean;
        Fragment_Shading_Rate_With_Shader_Sample_Mask: Boolean;
        Fragment_Shading_Rate_With_Conservative_Rasterization: Boolean;
        Fragment_Shading_Rate_With_Fragment_Shader_Interlock: Boolean;
        Fragment_Shading_Rate_With_Custom_Sample_Locations: Boolean;
        Fragment_Shading_Rate_Strict_Multiply_Combiner: Boolean;
    end record;

    type Physical_Device_Fragment_Shading_Rate is new Out_Structure
        (Physical_Device_Fragment_Shading_Rate_Type) with
    record
        Sample_Counts: Sample_Count_Flags := Sample_Count_No_Bit;
        Fragment_Size: Extent_2D;
    end record;

    package Physical_Device_Fragment_Shading_Rate_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Physical_Device_Fragment_Shading_Rate);

    type Physical_Device_Shader_Quad_Control_Features is new Out_Structure
        (Physical_Device_Shader_Quad_Control_Features_Type) with
    record
        Shader_Quad_Control: Boolean;
    end record;

    type Surface_Protected_Capabilities is new In_Structure
        (Surface_Protected_Capabilities_Type) with
    record
        Supports_Protected: Boolean;
    end record;

    type Physical_Device_Present_Wait_Features is new Out_Structure
        (Physical_Device_Present_Wait_Features_Type) with
    record
        Present_Wait: Boolean;
    end record;

    type Physical_Device_Pipeline_Executable_Properties_Features is
        new Out_Structure
            (Physical_Device_Pipeline_Executable_Properties_Features_Type) with
    record
        Pipeline_Executable_Info: Boolean;
    end record;

    type Pipeline_Info is new In_Structure(Pipeline_Info_Type) with
    record
        Pipeline: Vulkan.Pipeline;
    end record;

    type Pipeline_Executable_Properties is new Out_Structure
        (Pipeline_Executable_Properties_Type) with
    record
        Stages: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Name: Ada.Strings.Unbounded.Unbounded_String;
        Description: Ada.Strings.Unbounded.Unbounded_String;
        Subgroup_Size: Interfaces.Unsigned_32;
    end record;

    package Pipeline_Executable_Properties_Vectors is new Ada.Containers.Vectors
        (Positive, Pipeline_Executable_Properties);

    type Pipeline_Executable_Info is new In_Structure
        (Pipeline_Executable_Info_Type) with
    record
        Pipeline: Vulkan.Pipeline;
        Executable_Index: Interfaces.Unsigned_32;
    end record;

    type Pipeline_Executable_Statistic_Value
        (Format: Pipeline_Executable_Statistic_Format) is
    record
        case Format is
            when Bool32 =>
                B32: Boolean;
            when Int64 =>
                I64: Interfaces.Integer_64;
            when UInt64 =>
                U64: Interfaces.Unsigned_64;
            when Float64 =>
                F64: Interfaces.C.double;
        end case;
    end record;

    type Pipeline_Executable_Statistic
        (Format: Pipeline_Executable_Statistic_Format) is
            new Out_Structure(Pipeline_Executable_Statistic_Type) with
    record
        Name: Ada.Strings.Unbounded.Unbounded_String;
        Description: Ada.Strings.Unbounded.Unbounded_String;
        Value: Pipeline_Executable_Statistic_Value(Format);
    end record;

    package Pipeline_Executable_Statistic_Vectors is
        new Ada.Containers.Indefinite_Vectors
        (Positive, Pipeline_Executable_Statistic);

    type Pipeline_Executable_Internal_Representation is new Out_Structure
        (Pipeline_Executable_Internal_Representation_Type) with
    record
        Name: Ada.Strings.Unbounded.Unbounded_String;
        Description: Ada.Strings.Unbounded.Unbounded_String;
        Is_Text: Boolean;
        Data_Size: Interfaces.C.size_t;
        Data: Interfaces.C.Extensions.void_ptr;
    end record;

    package Pipeline_Executable_Internal_Representation_Vectors is
        new Ada.Containers.Vectors
            (Positive, Pipeline_Executable_Internal_Representation);

    type Pipeline_Library_Create_Info is new In_Structure
        (Pipeline_Library_Create_Info_Type) with
    record
        Libraries: Pipeline_Vectors.Vector;
    end record;

    package Unsigned_64_Vectors is new Ada.Containers.Vectors
        (Positive, Interfaces.Unsigned_64, Interfaces."=");

    type Present_ID is new In_Structure(Present_ID_Type) with
    record
        Present_IDs: Unsigned_64_Vectors.Vector;
    end record;

    type Physical_Device_Present_ID_Features is new Out_Structure
        (Physical_Device_Present_ID_Features_Type) with
    record
        Present_ID: Boolean;
    end record;

    type Video_Encode_Info is new In_Structure(Video_Encode_Info_Type) with
    record
        Flags: Video_Encode_Flags := Video_Encode_No_Bit;
        Dst_Buffer: Buffer;
        Dst_Buffer_Offset: Device_Size;
        Dst_Buffer_Range: Device_Size;
        Src_Picture_Resource: Video_Picture_Resource_Info;
        Setup_Reference_Slot: Video_Reference_Slot_Info_Access;
        Reference_Slots: Video_Reference_Slot_Info_Vectors.Vector;
        Preceding_Externally_Encoded_Bytes: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_Capabilities is new Out_Structure
        (Video_Encode_Capabilities_Type) with
    record
        Flags: Video_Encode_Capability_Flags := Video_Encode_Capability_No_Bit;
        Rate_Control_Modes: Video_Encode_Rate_Control_Mode_Flags :=
            Video_Encode_Rate_Control_Mode_Default_Bit;
        Max_Rate_Control_Layers: Interfaces.Unsigned_32;
        Max_Bitrate: Interfaces.Unsigned_64;
        Max_Quality_Levels: Interfaces.Unsigned_32;
        Encode_Input_Picture_Granularity: Extent_2D;
        Supported_Encode_Feedback_Flags: Video_Encode_Feedback_Flags :=
            Video_Encode_Feedback_No_Bit;
    end record;

    type Query_Pool_Video_Encode_Feedback_Create_Info is new In_Structure
        (Query_Pool_Video_Encode_Feedback_Create_Info_Type) with
    record
        Encode_Feedback_Flags: Video_Encode_Feedback_Flags :=
            Video_Encode_Feedback_No_Bit;
    end record;

    type Video_Encode_Usage_Info is new In_Structure
        (Video_Encode_Usage_Info_Type) with
    record
        Video_Usage_Hints: Video_Encode_Usage_Flags :=
            Video_Encode_Usage_Default_Bit;
        Video_Content_Hints: Video_Encode_Content_Flags :=
            Video_Encode_Content_Default_Bit;
        Tuning_Mode: Video_Encode_Tuning_Mode;
    end record;

    type Video_Encode_Rate_Control_Layer_Info is new In_Structure
        (Video_Encode_Rate_Control_Layer_Info_Type) with
    record
        Average_Bitrate: Interfaces.Unsigned_64;
        Max_Bitrate: Interfaces.Unsigned_64;
        Frame_Rate_Numerator: Interfaces.Unsigned_32;
        Frame_Rate_Denominator: Interfaces.Unsigned_32;
    end record;

    package Video_Encode_Rate_Control_Layer_Info_Vectors is
        new Ada.Containers.Vectors
            (Positive, Video_Encode_Rate_Control_Layer_Info);
                                   
    type Video_Encode_Rate_Control_Info is new In_Structure
        (Video_Encode_Rate_Control_Info_Type) with
    record
        Flags: Video_Encode_Rate_Control_Flags :=
            Video_Encode_Rate_Control_No_Bit;
        Rate_Control_Mode: Video_Encode_Rate_Control_Mode_Flags :=
            Video_Encode_Rate_Control_Mode_Default_Bit;
        Layers: Video_Encode_Rate_Control_Layer_Info_Vectors.Vector;
        Virtual_Buffer_Size_In_Ms: Interfaces.Unsigned_32;
        Initial_Virtual_Buffer_Size_In_Ms: Interfaces.Unsigned_32;
    end record;

    type Physical_Device_Video_Encode_Quality_Level_Info is new In_Structure
        (Physical_Device_Video_Encode_Quality_Level_Info_Type) with
    record
        Video_Profile: Video_Profile_Info_Access;
        Quality_Level: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_Quality_Level_Properties is new Out_Structure
        (Video_Encode_Quality_Level_Properties_Type) with
    record
        Preferred_Rate_Control_Mode: Video_Encode_Rate_Control_Mode_Flags :=
            Video_Encode_Rate_Control_Mode_Default_Bit;
        Preferred_Rate_Control_Layer_Count: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_Quality_Level_Info is new In_Structure
        (Video_Encode_Quality_Level_Info_Type) with
    record
        Quality_Level: Interfaces.Unsigned_32;
    end record;

    type Video_Encode_Session_Parameters_Get_Info is new In_Structure
        (Video_Encode_Session_Parameters_Get_Info_Type) with
    record
        Video_Session_Parameters: KHR.Video_Session_Parameters;
    end record;

    type Video_Encode_Session_Parameters_Feedback_Info is new Out_Structure
        (Video_Encode_Session_Parameters_Feedback_Info_Type) with
    record
        Has_Overrides: Boolean;
    end record;

    type Physical_Device_Fragment_Shader_Barycentric_Features is
        new Out_Structure
            (Physical_Device_Fragment_Shader_Barycentric_Features_Type) with
    record
        Fragment_Shader_Barycentric: Boolean;
    end record;

    type Physical_Device_Fragment_Shader_Barycentric_Properties is
        new Out_Structure
            (Physical_Device_Fragment_Shader_Barycentric_Properties_Type) with
    record
        Tri_Strip_Vertex_Order_Independent_Of_Provoking_Vertex: Boolean;
    end record;

    type Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features is
        new Out_Structure
            (Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_Type)
                with
    record
        Shader_Subgroup_Uniform_Control_Flow: Boolean;
    end record;

    type Physical_Device_Workgroup_Memory_Explicit_Layout_Features is
        new Out_Structure
            (Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type)
                with
    record
        Workgroup_Memory_Explicit_Layout: Boolean;
        Workgroup_Memory_Explicit_Layout_Scalar_Block_Layout: Boolean;
        Workgroup_Memory_Explicit_Layout_8_Bit_Access: Boolean;
        Workgroup_Memory_Explicit_Layout_16_Bit_Access: Boolean;
    end record;

    type Physical_Device_Ray_Tracing_Maintenance_1_Features is
        new Out_Structure
            (Physical_Device_Ray_Tracing_Maintenance_1_Features_Type) with
    record
        Ray_Tracing_Maintenance_1: Boolean;
        Ray_Tracing_Pipeline_Trace_Rays_Indirect_2: Boolean;
    end record;

    type Trace_Rays_Indirect_Command_2 is
    record
        Raygen_Shader_Record_Address: Device_Address;
        Raygen_Shader_Record_Size: Device_Size;
        Miss_Shader_Binding_Table_Address: Device_Address;
        Miss_Shader_Binding_Table_Size: Device_Size;
        Miss_Shader_Binding_Table_Stride: Device_Size;
        Hit_Shader_Binding_Table_Address: Device_Address;
        Hit_Shader_Binding_Table_Size: Device_Size;
        Hit_Shader_Binding_Table_Stride: Device_Size;
        Callable_Shader_Binding_Table_Address: Device_Address;
        Callable_Shader_Binding_Table_Size: Device_Size;
        Callable_Shader_Binding_Table_Stride: Device_Size;
        Width: Vulkan.Width;
        Height: Vulkan.Height;
        Depth: Vulkan.Depth;
    end record
        with Convention => C;

    type Physical_Device_Shader_Maximal_Reconvergence_Features is
        new Out_Structure
            (Physical_Device_Shader_Maximal_Reconvergence_Features_Type) with
    record
        Shader_Maximal_Reconvergence: Boolean;
    end record;
 
    type Physical_Device_Ray_Tracing_Position_Fetch_Features is
        new Out_Structure
            (Physical_Device_Ray_Tracing_Position_Fetch_Features_Type) with
    record
        Ray_Tracing_Position_Fetch: Boolean;
    end record;

    type Cooperative_Matrix_Properties is new Out_Structure
        (Cooperative_Matrix_Properties_Type) with
    record
        M_Size: Interfaces.Unsigned_32;
        N_Size: Interfaces.Unsigned_32;
        K_Size: Interfaces.Unsigned_32;
        A_Type: Component_Type;
        B_Type: Component_Type;
        C_Type: Component_Type;
        Result_Type: Component_Type;
        Saturating_Accumulation: Boolean;
        Scope: KHR.Scope;
    end record;

    package Cooperative_Matrix_Properties_Vectors is new Ada.Containers.Vectors
        (Positive, Cooperative_Matrix_Properties);

    type Physical_Device_Cooperative_Matrix_Features is new Out_Structure
        (Physical_Device_Cooperative_Matrix_Features_Type) with
    record
        Cooperative_Matrix: Boolean;
        Cooperative_Matrix_Robust_Buffer_Access: Boolean;
    end record;

    type Physical_Device_Cooperative_Matrix_Properties is new Out_Structure
        (Physical_Device_Cooperative_Matrix_Properties_Type) with
    record
        Cooperative_Matrix_Supported_Stages: Shader_Stage_Flags :=
            Shader_Stage_No_Bit;
    end record;

    type Video_Decode_AV1_Profile_Info is new In_Structure
        (Video_Decode_AV1_Profile_Info_Type) with
    record
        Std_Profile: Std_Video.AV1.Profile;
        Film_Grain_Support: Boolean;
    end record;

    type Video_Decode_AV1_Capabilities is new Out_Structure
        (Video_Decode_AV1_Capabilities_Type) with
    record
        Max_Level: Std_Video.AV1.Level;
    end record;

    type Video_Decode_AV1_Session_Parameters_Create_Info is new In_Structure
        (Video_Decode_AV1_Session_Parameters_Create_Info_Type) with
    record
        Std_Sequence_Header: Std_Video.AV1.Sequence_Header_Access;
    end record;

    type References_Per_Frame_Array is
        array (1 .. Max_Video_AV1_References_Per_Frame) of Interfaces.Integer_32
        with Convention => C;

    type Video_Decode_AV1_Picture_Info is new In_Structure
        (Video_Decode_AV1_Picture_Info_Type) with
    record
        Std_Picture_Info: Std_Video.AV1.Decode.Picture_Info_Access;
        Reference_Name_Slot_Indices: References_Per_Frame_Array;
        Frame_Header_Offset: Interfaces.Unsigned_32;
        Tile_Offsets: Unsigned_32_Vectors.Vector;
        Tile_Sizes: Unsigned_32_Vectors.Vector;
    end record;

    type Video_Decode_AV1_DPB_Slot_Info is new In_Structure
        (Video_Decode_AV1_DPB_Slot_Info_Type) with
    record
        Std_Reference_Info: Std_Video.AV1.Decode.Reference_Info_Access;
    end record;

    type Physical_Device_Video_Maintenance_1_Features is new Out_Structure
        (Physical_Device_Video_Maintenance_1_Features_Type) with
    record
        Video_Maintenance_1: Boolean;
    end record;

    type Video_Inline_Query_Info is
        new In_Structure(Video_Inline_Query_Info_Type) with
    record
        Query_Pool: Vulkan.Query_Pool;
        First_Query: Interfaces.Unsigned_32;
        Query_Count: Interfaces.Unsigned_32;
    end record;
    
    type Calibrated_Timestamp_Info is
        new In_Structure(Calibrated_Timestamp_Info_Type) with
    record
        Time_Domain: KHR.Time_Domain;
    end record;

    package Calibrated_Timestamp_Info_Vectors is new Ada.Containers.Vectors
        (Positive, Calibrated_Timestamp_Info);

    type Set_Descriptor_Buffer_Offsets_Info is new In_Structure
        (Set_Descriptor_Buffer_Offsets_Info_Type) with
    record
        Stage_Flags: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Layout: Pipeline_Layout;
        First_Set: Interfaces.Unsigned_32;
        Buffer_Indices: Unsigned_32_Vectors.Vector;
        Offsets: Device_Size_Vectors.Vector;
    end record;

    type Bind_Descriptor_Buffer_Embedded_Samplers_Info is new In_Structure
        (Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type) with
    record
        Stage_Flags: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
    end record;

    type Xlib_Surface_Create_Info is
        new In_Structure(Xlib_Surface_Create_Info_Type) with
    record
        Flags: Xlib_Surface_Create_Flags := Xlib_Surface_Create_No_Bit;
        Dpy: Xlib.Display;
        Window: Xlib.Window;
    end record;

    type Xcb_Surface_Create_Info is
        new In_Structure(Xcb_Surface_Create_Info_Type) with
    record
        Flags: Xcb_Surface_Create_Flags := Xcb_Surface_Create_No_Bit;
        Connection: Xcb.Connection;
        Window: Xcb.Window;
    end record;

    type Wayland_Surface_Create_Info is
        new In_Structure(Wayland_Surface_Create_Info_Type) with
    record
        Flags: Wayland_Surface_Create_Flags := Wayland_Surface_Create_No_Bit;
        Display: Wayland.Display;
        Surface: Wayland.Surface;
    end record;

    type Win32_Surface_Create_Info is
        new In_Structure(Win32_Surface_Create_Info_Type) with
    record
        Flags: Win32_Surface_Create_Flags := Win32_Surface_Create_No_Bit;
        Instance: Win32.Instance;
        Window: Win32.Window;
    end record;
end Vulkan.Extensions.KHR;


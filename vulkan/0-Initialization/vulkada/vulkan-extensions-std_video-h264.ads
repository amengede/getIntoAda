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

-- Common constants and types for H264 extensions

package Vulkan.Extensions.Std_Video.H264 is
    -- Constants.
    CPB_Cnt_List_Size: constant := 32;
    Scaling_List_4x4_Num_Lists: constant := 6;
    Scaling_List_4x4_Num_Elements: constant := 16;
    Scaling_List_8x8_Num_Lists: constant := 6;
    Scaling_List_8x8_Num_Elements: constant := 64;
    Max_Num_List_Ref: constant := 32;
    Max_Chroma_Planes: constant := 2;
    No_Reference_Picture: constant := 16#ff#;

    -- Enumerations,
    type Chroma_Format_IDC is (Monochrome,
                               Format_420,
                               Format_422,
                               Format_444,
                               Invalid)
        with Convention => C;

    for Chroma_Format_IDC'Size use 32;

    for Chroma_Format_IDC use (Monochrome => 0,
                               Format_420 => 1,
                               Format_422 => 2,
                               Format_444 => 3,
                               Invalid => 16#7fffffff#);

    type Profile_IDC is (Baseline,
                         Main,
                         High,
                         High_444_Predictive,
                         Invalid)
        with Convention => C;

    for Profile_IDC'Size use 32;

    for Profile_IDC use (Baseline => 66,
                         Main => 77,
                         High => 100,
                         High_444_Predictive => 244,
                         Invalid => 16#7fffffff#);

    type Level_IDC is (Level_1_0,
                       Level_1_1,
                       Level_1_2,
                       Level_1_3,
                       Level_2_0,
                       Level_2_1,
                       Level_2_2,
                       Level_3_0,
                       Level_3_1,
                       Level_3_2,
                       Level_4_0,
                       Level_4_1,
                       Level_4_2,
                       Level_5_0,
                       Level_5_1,
                       Level_5_2,
                       Level_6_0,
                       Level_6_1,
                       Level_6_2,
                       Invalid)
        with Convention => C;

    for Level_IDC'Size use 32;

    for Level_IDC use (Level_1_0 => 0,
                       Level_1_1 => 1,
                       Level_1_2 => 2,
                       Level_1_3 => 3,
                       Level_2_0 => 4,
                       Level_2_1 => 5,
                       Level_2_2 => 6,
                       Level_3_0 => 7,
                       Level_3_1 => 8,
                       Level_3_2 => 9,
                       Level_4_0 => 10,
                       Level_4_1 => 11,
                       Level_4_2 => 12,
                       Level_5_0 => 13,
                       Level_5_1 => 14,
                       Level_5_2 => 15,
                       Level_6_0 => 16,
                       Level_6_1 => 17,
                       Level_6_2 => 18,
                       Invalid => 16#7fffffff#);

    type Poc_Type is (Type_0,
                      Type_1,
                      Type_2,
                      Invalid)
        with Convention => C;

    for Poc_Type'Size use 32;

    for Poc_Type use (Type_0 => 0,
                      Type_1 => 1,
                      Type_2 => 2,
                      Invalid => 16#7fffffff#);

    type Aspect_Ratio_IDC is (Unspecified,
                              Square,
                              Ratio_12_11,
                              Ratio_10_11,
                              Ratio_16_11,
                              Ratio_40_33,
                              Ratio_24_11,
                              Ratio_20_11,
                              Ratio_32_11,
                              Ratio_80_33,
                              Ratio_18_11,
                              Ratio_15_11,
                              Ratio_64_33,
                              Ratio_160_99,
                              Ratio_4_3,
                              Ratio_3_2,
                              Ratio_2_1,
                              Extended_SAR,
                              Invalid)
        with Convention => C;

    for Aspect_Ratio_IDC'Size use 32;

    for Aspect_Ratio_IDC use (Unspecified => 0,
                              Square => 1,
                              Ratio_12_11 => 2,
                              Ratio_10_11 => 3,
                              Ratio_16_11 => 4,
                              Ratio_40_33 => 5,
                              Ratio_24_11 => 6,
                              Ratio_20_11 => 7,
                              Ratio_32_11 => 8,
                              Ratio_80_33 => 9,
                              Ratio_18_11 => 10,
                              Ratio_15_11 => 11,
                              Ratio_64_33 => 12,
                              Ratio_160_99 => 13,
                              Ratio_4_3 => 14,
                              Ratio_3_2 => 15,
                              Ratio_2_1 => 16,
                              Extended_SAR => 255,
                              Invalid => 16#7fffffff#);

    type Weighted_Bipred_IDC is (Default,
                                 Explicit,
                                 Implicit,
                                 Invalid)
        with Convention => C;

    for Weighted_Bipred_IDC'Size use 32;

    for Weighted_Bipred_IDC use (Default => 0,
                                 Explicit => 1,
                                 Implicit => 2,
                                 Invalid => 16#7fffffff#);

    type Modification_Of_Pic_Nums_IDC is (Short_Term_Subtract,
                                          Short_Term_Add,
                                          Long_Term,
                                          IDC_End,
                                          Invalid)
        with Convention => C;

    for Modification_Of_Pic_Nums_IDC'Size use 32;

    for Modification_Of_Pic_Nums_IDC use (Short_Term_Subtract => 0,
                                          Short_Term_Add => 1,
                                          Long_Term => 2,
                                          IDC_End => 3,
                                          Invalid => 16#7fffffff#);

    type Mem_Mgmt_Control_Op is (Op_End,
                                 Unmark_Short_Term,
                                 Unmark_Long_Term,
                                 Mark_Long_Term,
                                 Set_Max_Long_Term_Index,
                                 Unmark_All,
                                 Mark_Current_As_Long_Term,
                                 Invalid)
        with Convention => C;

    for Mem_Mgmt_Control_Op'Size use 32;

    for Mem_Mgmt_Control_Op use (Op_End => 0,
                                 Unmark_Short_Term => 1,
                                 Unmark_Long_Term => 2,
                                 Mark_Long_Term => 3,
                                 Set_Max_Long_Term_Index => 4,
                                 Unmark_All => 5,
                                 Mark_Current_As_Long_Term => 6,
                                 Invalid => 16#7fffffff#);

    type Cabac_Init_IDC is (Init_0,
                            Init_1,
                            Init_2,
                            Invalid)
        with Convention => C;

    for Cabac_Init_IDC'Size use 32;

    for Cabac_Init_IDC use (Init_0 => 0,
                            Init_1 => 1,
                            Init_2 => 2,
                            Invalid => 16#7fffffff#);

    type Disable_Deblocking_Filter_IDC is (Disabled,
                                           Enabled,
                                           Partial,
                                           Invalid)
        with Convention => C;

    for Disable_Deblocking_Filter_IDC'Size use 32;

    for Disable_Deblocking_Filter_IDC use (Disabled => 0,
                                           Enabled => 1,
                                           Partial => 2,
                                           Invalid => 16#7fffffff#);

    type Slice_Type is (P,
                        B,
                        I,
                        Invalid)
        with Convention => C;

    for Slice_Type'Size use 32;

    for Slice_Type use (P => 0,
                        B => 1,
                        I => 2,
                        Invalid => 16#7fffffff#);

    type Picture_Type is (P,
                          B,
                          I,
                          IDR,
                          Invalid)
        with Convention => C;

    for Picture_Type'Size use 32;

    for Picture_Type use (P => 0,
                          B => 1,
                          I => 2,
                          IDR => 5,
                          Invalid => 16#7fffffff#);

    type Non_Vcl_Nalu_Type is (SPS,
                               PPS,
                               AUD,
                               Prefix,
                               End_Of_Sequence,
                               End_Of_Stream,
                               Precoded,
                               Invalid)
        with Convention => C;

    for Non_Vcl_Nalu_Type'Size use 32;

    for Non_Vcl_Nalu_Type use (SPS => 0,
                               PPS => 1,
                               AUD => 2,
                               Prefix => 3,
                               End_Of_Sequence => 4,
                               End_Of_Stream => 5,
                               Precoded => 6,
                               Invalid => 16#7fffffff#);

    -- Records.
    type Sps_Vui_Flags is
    record
        Aspect_Ratio_Info_Present_Flag: Packed_Bit := 0;
        Overscan_Info_Present_Flag: Packed_Bit := 0;
        Overscan_Appropriate_Flag: Packed_Bit := 0;
        Video_Signal_Type_Present_Flag: Packed_Bit := 0;
        Video_Full_Range_Flag: Packed_Bit := 0;
        Color_Description_Present_Flag: Packed_Bit := 0;
        Chroma_LOC_Info_Present_Flag: Packed_Bit := 0;
        Timing_Info_Present_Flag: Packed_Bit := 0;
        Fixed_Frame_Rate_Flag: Packed_Bit := 0;
        Bitstream_Restriction_Flag: Packed_Bit := 0;
        NAL_HRD_Parameters_Present_Flag: Packed_Bit := 0;
        VLC_HRD_Parameters_Present_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for Sps_Vui_Flags use
    record
        Aspect_Ratio_Info_Present_Flag at 0 range 0 .. 0;
        Overscan_Info_Present_Flag at 0 range 1 .. 1;
        Overscan_Appropriate_Flag at 0 range 2 .. 2;
        Video_Signal_Type_Present_Flag at 0 range 3 .. 3;
        Video_Full_Range_Flag at 0 range 4 .. 4;
        Color_Description_Present_Flag at 0 range 5 .. 5;
        Chroma_LOC_Info_Present_Flag at 0 range 6 .. 6;
        Timing_Info_Present_Flag at 0 range 7 .. 7;
        Fixed_Frame_Rate_Flag at 0 range 8 .. 8;
        Bitstream_Restriction_Flag at 0 range 9 .. 9;
        NAL_HRD_Parameters_Present_Flag at 0 range 10 .. 10;
        VLC_HRD_Parameters_Present_Flag at 0 range 11 .. 11;
    end record;

    type CPB_Cnt_List_32 is array 
        (1 .. CPB_Cnt_List_Size) of Interfaces.Unsigned_32
        with Convention => C;

    type CPB_Cnt_List_8 is array 
        (1 .. CPB_Cnt_List_Size) of Interfaces.Unsigned_8
        with Convention => C;

    type Hrd_Parameters is
    record
        CPB_Cnt_Minus_1: Interfaces.Unsigned_8;
        Bit_Rate_Scale: Interfaces.Unsigned_8;
        CPB_Size_Scale: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_8;
        Bit_Rate_Value_Minus_1: CPB_Cnt_List_32;
        CPB_Size_Value_Minus_1: CPB_Cnt_List_32;
        CBR_Flag: CPB_Cnt_List_8;
        Initial_CPB_Removal_Delay_Length_Minus_1: Interfaces.Unsigned_32;
        CPB_Removal_Delay_Length_Minus_1: Interfaces.Unsigned_32;
        DPB_Output_Delay_Length_Minus_1: Interfaces.Unsigned_32;
        Time_Offset_Length: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Hrd_Parameters_Access is access constant Hrd_Parameters
        with Convention => C,
             Storage_Size => 0;

    type Sequence_Parameter_Set_Vui is
    record
        Flags: Sps_Vui_Flags;
        Aspect_Ratio_IDC: H264.Aspect_Ratio_IDC;
        Sar_Width: Interfaces.Unsigned_16;
        Sar_Height: Interfaces.Unsigned_16;
        Video_Format: Interfaces.Unsigned_8;
        Colour_Primaries: Interfaces.Unsigned_8;
        Transfer_Characteristics: Interfaces.Unsigned_8;
        Matrix_Coefficients: Interfaces.Unsigned_8;
        Num_Units_In_Tick: Interfaces.Unsigned_32;
        Time_Scale: Interfaces.Unsigned_32;
        Max_Num_Reorder_Frames: Interfaces.Unsigned_8;
        Max_Dec_Frame_Buffering: Interfaces.Unsigned_8;
        Chroma_Sample_LOC_Type_Top_Field: Interfaces.Unsigned_8;
        Chroma_Sample_LOC_Type_Bottom_Field: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_32;
        Hrd_Parameters: Hrd_Parameters_Access;
    end record
        with Convention => C;

    type Sequence_Parameter_Set_Vui_Access is
        access constant Sequence_Parameter_Set_Vui
        with Convention => C,
             Storage_Size => 0;

    type Sps_Flags is
    record
        Constraint_Set_0_Flag: Packed_Bit := 0;
        Constraint_Set_1_Flag: Packed_Bit := 0;
        Constraint_Set_2_Flag: Packed_Bit := 0;
        Constraint_Set_3_Flag: Packed_Bit := 0;
        Constraint_Set_4_Flag: Packed_Bit := 0;
        Constraint_Set_5_Flag: Packed_Bit := 0;
        Direct_8x8_Inference_Flag: Packed_Bit := 0;
        Mb_Adaptive_Frame_Field_Flag: Packed_Bit := 0;
        Frame_Mbs_Only_Flag: Packed_Bit := 0;
        Delta_Pic_Order_Always_Zero_Flag: Packed_Bit := 0;
        Separate_Colour_Plane_Flag: Packed_Bit := 0;
        Gaps_In_Frame_Num_Value_Allowed_Flag: Packed_Bit := 0;
        QPPrime_Y_Zero_Transform_Bypass_Flag: Packed_Bit := 0;
        Frame_Cropping_Flag: Packed_Bit := 0;
        Seq_Scaling_Matrix_Present_Flag: Packed_Bit := 0;
        Vui_Parameters_Present_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for Sps_Flags use
    record
        Constraint_Set_0_Flag at 0 range 0 .. 0;
        Constraint_Set_1_Flag at 0 range 1 .. 1;
        Constraint_Set_2_Flag at 0 range 2 .. 2;
        Constraint_Set_3_Flag at 0 range 3 .. 3;
        Constraint_Set_4_Flag at 0 range 4 .. 4;
        Constraint_Set_5_Flag at 0 range 5 .. 5;
        Direct_8x8_Inference_Flag at 0 range 6 .. 6;
        Mb_Adaptive_Frame_Field_Flag at 0 range 7 .. 7;
        Frame_Mbs_Only_Flag at 0 range 8 .. 8;
        Delta_Pic_Order_Always_Zero_Flag at 0 range 9 .. 9;
        Separate_Colour_Plane_Flag at 0 range 10 .. 10;
        Gaps_In_Frame_Num_Value_Allowed_Flag at 0 range 11 .. 11;
        QPPrime_Y_Zero_Transform_Bypass_Flag at 0 range 12 .. 12;
        Frame_Cropping_Flag at 0 range 13 .. 13;
        Seq_Scaling_Matrix_Present_Flag at 0 range 14 .. 14;
        Vui_Parameters_Present_Flag at 0 range 15 .. 15;
    end record;

    type Scaling_List_4x4 is
        array (1 .. Scaling_List_4x4_Num_Lists,
               1 .. Scaling_List_4x4_Num_Elements)
               of Interfaces.Unsigned_8
        with Convention => C;

    type Scaling_List_8x8 is
        array (1 .. Scaling_List_8x8_Num_Lists,
               1 .. Scaling_List_8x8_Num_Elements)
               of Interfaces.Unsigned_8
        with Convention => C;

    type Scaling_Lists is
    record
        Scaling_List_Present_Mask: Interfaces.Unsigned_16;
        Use_Default_Scaling_Matrix_Mask: Interfaces.Unsigned_16;
        Scaling_List_4x4: H264.Scaling_List_4x4;
        Scaling_List_8x8: H264.Scaling_List_8x8;
    end record
        with Convention => C;

    type Scaling_Lists_Access is access constant Scaling_Lists
        with Convention => C,
             Storage_Size => 0;

    type Integer_32_Access is access constant Interfaces.Integer_32
        with Convention => C,
             Storage_Size => 0;

    type Sequence_Parameter_Set is
    record
        Flags: Sps_Flags;
        Profile_IDC: H264.Profile_IDC;
        Level_IDC: H264.Level_IDC;
        Chroma_Format_IDC: H264.Chroma_Format_IDC;
        Seq_Parameter_Set_ID: Interfaces.Unsigned_8;
        Bit_Depth_Luma_Minus_8: Interfaces.Unsigned_8;
        Bit_Depth_Chroma_Minus_8: Interfaces.Unsigned_8;
        Log_2_Max_Frame_Num_Minus_4: Interfaces.Unsigned_8;
        Pic_Order_Cnt_Type: Poc_Type;
        Offset_For_Non_Ref_Pic: Interfaces.Integer_32;
        Offset_For_Top_To_Bottom_Field: Interfaces.Integer_32;
        Log_2_Max_Pic_Order_Cnt_LSB_Minus_4: Interfaces.Unsigned_8;
        Num_Ref_Frames_In_Pic_Order_Cnt_Cycle: Interfaces.Unsigned_8;
        Max_Num_Ref_Frames: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_8;
        Pic_Width_In_MBs_Minus_1: Interfaces.Unsigned_32;
        Pic_Height_In_Map_Units_Minus_1: Interfaces.Unsigned_32;
        Frame_Crop_Left_Offset: Interfaces.Unsigned_32;
        Frame_Crop_Right_Offset: Interfaces.Unsigned_32;
        Frame_Crop_Top_Offset: Interfaces.Unsigned_32;
        Frame_Crop_Bottom_Offset: Interfaces.Unsigned_32;
        Reserved_2: Interfaces.Unsigned_32;
        Offset_For_Ref_Frame: Integer_32_Access;
        Scaling_Lists: Scaling_Lists_Access;
        Sequence_Parameter_Set_Vui: Sequence_Parameter_Set_Vui_Access;
    end record
        with Convention => C;

    package Sequence_Parameter_Set_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Sequence_Parameter_Set);

    type PPS_Flags is
    record
        Transform_8x8_Mode_Flag: Packed_Bit := 0;
        Redundant_Pic_Cnt_Present_Flag: Packed_Bit := 0;
        Constrained_Intra_Pred_Flag: Packed_Bit := 0;
        Deblocking_Filter_Control_Present_Flag: Packed_Bit := 0;
        Weighted_Pred_Flag: Packed_Bit := 0;
        Bottom_Field_Pic_Order_In_Frame_Present_Flag: Packed_Bit := 0;
        Entropy_Coding_Mode_Flag: Packed_Bit := 0;
        Pic_Scaling_Matrix_Present_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for PPS_Flags use
    record
        Transform_8x8_Mode_Flag at 0 range 0 .. 0;
        Redundant_Pic_Cnt_Present_Flag at 0 range 1 .. 1;
        Constrained_Intra_Pred_Flag at 0 range 2 .. 2;
        Deblocking_Filter_Control_Present_Flag at 0 range 3 .. 3;
        Weighted_Pred_Flag at 0 range 4 .. 4;
        Bottom_Field_Pic_Order_In_Frame_Present_Flag at 0 range 5 .. 5;
        Entropy_Coding_Mode_Flag at 0 range 6 .. 6;
        Pic_Scaling_Matrix_Present_Flag at 0 range 7 .. 7;
    end record;

    type Picture_Parameter_Set is
    record
        Flags: PPS_Flags;
        Seq_Parameter_Set_Id: Interfaces.Unsigned_8;
        Pic_Parameter_Set_Id: Interfaces.Unsigned_8;
        Num_Ref_Idx_L0_Default_Active_Minus_1: Interfaces.Unsigned_8;
        Num_Ref_Idx_L1_Default_Active_Minus_1: Interfaces.Unsigned_8;
        Weighted_Bipred: Weighted_Bipred_IDC;
        Pic_Init_Qp_Minus_26: Interfaces.Integer_8;
        Pic_Init_Qs_Minus_26: Interfaces.Integer_8;
        Chroma_Qp_Index_Offset: Interfaces.Integer_8;
        Second_Chroma_Qp_Index_Offset: Interfaces.Integer_8;
        Scaling_Lists: Scaling_Lists_Access;
    end record
        with Convention => C;

    package Picture_Parameter_Set_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Picture_Parameter_Set);
end Vulkan.Extensions.Std_Video.H264;


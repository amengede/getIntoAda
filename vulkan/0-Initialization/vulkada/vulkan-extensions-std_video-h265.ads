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

package Vulkan.Extensions.Std_Video.H265 is
    -- Constants.
    CPB_Cnt_List_Size: constant := 32;
    Sub_Layers_List_Size: constant := 7;
    Scaling_List_4x4_Num_Lists: constant := 6;
    Scaling_List_4x4_Num_Elements: constant := 16;
    Scaling_List_8x8_Num_Lists: constant := 6;
    Scaling_List_8x8_Num_Elements: constant := 64;
    Scaling_List_16x16_Num_Lists: constant := 6;
    Scaling_List_16x16_Num_Elements: constant := 64;
    Scaling_List_32x32_Num_Lists: constant := 2;
    Scaling_List_32x32_Num_Elements: constant := 64;
    Chroma_QP_Offset_List_Size: constant := 6;
    Chroma_QP_Offset_Tile_Cols_List_Size: constant := 19;
    Chroma_QP_Offset_Tile_Rows_List_Size: constant := 21;
    Predictor_Palette_Components_List_Size: constant := 3;
    Predictor_Palette_Comp_Entries_List_Size: constant := 128;
    Max_Num_List_Ref: constant := 15;
    Max_Chroma_Planes: constant := 2;
    Max_Short_Term_Ref_Pics_Sets: constant := 64;
    Max_DPB_Size: constant := 16;
    Max_Long_Term_Ref_Pics_SPS: constant := 32;
    Max_Long_Term_Pics_SPS: constant := 16;
    Max_Delta_POC: constant := 48;
    No_Reference_Picture: constant := 16#ff#;

    -- Enumerations.
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

    type Profile_IDC is (Main,
                         Main_10,
                         Main_Still_Picture,
                         Format_Range_Extensions,
                         SCC_Extensions,
                         Invalid)
        with Convention => C;

    for Profile_IDC'Size use 32;

    for Profile_IDC use (Main => 1,
                         Main_10 => 2,
                         Main_Still_Picture => 3,
                         Format_Range_Extensions => 4,
                         SCC_Extensions => 9,
                         Invalid => 16#7fffffff#);
    
    type Level_IDC is (Level_1_0,
                       Level_2_0,
                       Level_2_1,
                       Level_3_0,
                       Level_3_1,
                       Level_4_0,
                       Level_4_1,
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
                       Level_2_0 => 1,
                       Level_2_1 => 2,
                       Level_3_0 => 3,
                       Level_3_1 => 4,
                       Level_4_0 => 5,
                       Level_4_1 => 6,
                       Level_5_0 => 7,
                       Level_5_1 => 8,
                       Level_5_2 => 9,
                       Level_6_0 => 10,
                       Level_6_1 => 11,
                       Level_6_2 => 12,
                       Invalid => 16#7fffffff#);
       
    type Slice_Type is (B,
                        P,
                        I,
                        Invalid)
        with Convention => C;

    for Slice_Type'Size use 32;

    for Slice_Type use (B => 0,
                        P => 1,
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
                          IDR => 3,
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

    -- Records.
    type Sub_Layers_List_8 is array (1 .. Sub_Layers_List_Size)
        of Interfaces.Unsigned_8
        with Convention => C;
    
    type Sub_Layers_List_16 is array (1 .. Sub_Layers_List_Size)
        of Interfaces.Unsigned_16
        with Convention => C;

    type Sub_Layers_List_32 is array (1 .. Sub_Layers_List_Size)
        of Interfaces.Unsigned_32
        with Convention => C;

    type Dec_Pic_Buf_Mgr is
    record
        Max_Latency_Increase_Plus_1: Sub_Layers_List_32;
        Max_Dec_Pic_Buffering_Minus_1: Sub_Layers_List_8;
        Max_Num_Reorder_Pics: Sub_Layers_List_8;
    end record
        with Convention => C;

    type Dec_Pic_Buf_Mgr_Access is access constant Dec_Pic_Buf_Mgr
        with Convention => C,
             Storage_Size => 0;

    type CPB_Cnt_List is array (1 .. CPB_Cnt_List_Size)
        of Interfaces.Unsigned_32
        with Convention => C;

    type Sub_Layer_Hrd_Parameters is
    record
        Bit_Rate_Value_Minus_1: CPB_Cnt_List;
        CPB_Size_Value_Minus_1: CPB_Cnt_List;
        CPB_Size_DU_Value_Minus_1: CPB_Cnt_List;
        Bit_Rate_DU_Value_Minus_1: CPB_Cnt_List;
        CBR_Flag: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Sub_Layer_Hrd_Parameters_Access is
        access constant Sub_Layer_Hrd_Parameters
        with Convention => C,
             Storage_Size => 0;

    type Hrd_Flags is
    record
        NAL_Hrd_Parameters_Present_Flag: Packed_Bit := 0;
        VCL_Hrd_Parameters_Present_Flag: Packed_Bit := 0;
        Sub_Pic_Hrd_Params_Present_Flag: Packed_Bit := 0;
        Sub_Pic_CPB_Params_In_Pic_Timing_SEI_Flag: Packed_Bit := 0;
        Fixed_Pic_Rate_General_Flag: Packed_Bit := 0;
        Fixed_Pic_Rate_Within_CVS_Flag: Packed_Bit := 0;
        Low_Delay_Hrd_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for Hrd_Flags use
    record
        NAL_Hrd_Parameters_Present_Flag at 0 range 0 .. 0;
        VCL_Hrd_Parameters_Present_Flag at 0 range 1 .. 1;
        Sub_Pic_Hrd_Params_Present_Flag at 0 range 2 .. 2;
        Sub_Pic_CPB_Params_In_Pic_Timing_SEI_Flag at 0 range 3 .. 3;
        Fixed_Pic_Rate_General_Flag at 0 range 4 .. 11;
        Fixed_Pic_Rate_Within_CVS_Flag at 0 range 12 .. 19;
        Low_Delay_Hrd_Flag at 0 range 20 .. 27;
    end record;

    type Hrd_Parameters is
    record
        Flags: Hrd_Flags;
        Tick_Divisor_Minus_2: Interfaces.Unsigned_8;
        DU_CPB_Removal_Delay_Increment_Length_Minus_1: Interfaces.Unsigned_8;
        DPB_Output_Delay_DU_Length_Minus_1: Interfaces.Unsigned_8;
        Bit_Rate_Scale: Interfaces.Unsigned_8;
        CPB_Size_Scale: Interfaces.Unsigned_8;
        CPB_Size_DU_Scale: Interfaces.Unsigned_8;
        Initial_CPB_Removal_Delay_Length_Minus_1: Interfaces.Unsigned_8;
        AU_CPB_Removal_Delay_Length_Minus_1: Interfaces.Unsigned_8;
        DPB_Output_Delay_Length_Minus_1: Interfaces.Unsigned_8;
        CPB_Cnt_Minus_1: Sub_Layers_List_8;
        Elemental_Duration_In_TC_Minus_1: Sub_Layers_List_16;
        Reserved: Reserved_16(1 .. 3);
        Sub_Layer_Hrd_Parameters_NAL: Sub_Layer_Hrd_Parameters_Access;
        Sub_Layer_Hrd_Parameters_VCL: Sub_Layer_Hrd_Parameters_Access;
    end record
        with Convention => C;

    type Hrd_Parameters_Access is access constant Hrd_Parameters
        with Convention => C,
             Storage_Size => 0;

    type VPS_Flags is
    record
        VPS_Temporal_ID_Nesting_Flag: Packed_Bit := 0;
        VPS_Sub_Layer_Ordering_Info_Present_Flag: Packed_Bit := 0;
        VPS_Timing_Info_Present_Flag: Packed_Bit := 0;
        VPS_POC_Proportional_To_Timing_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for VPS_Flags use
    record
        VPS_Temporal_ID_Nesting_Flag at 0 range 0 .. 0;
        VPS_Sub_Layer_Ordering_Info_Present_Flag at 0 range 1 .. 1;
        VPS_Timing_Info_Present_Flag at 0 range 2 .. 2;
        VPS_POC_Proportional_To_Timing_Flag at 0 range 3 .. 3;
    end record;

    type Profile_Tier_Level_Flags is
    record
        General_Tier_Flag: Packed_Bit := 0;
        General_Progressive_Source_Flag: Packed_Bit := 0;
        General_Interlaced_Source_Flag: Packed_Bit := 0;
        General_Non_Packed_Constraint_Flag: Packed_Bit := 0;
        General_Frame_Only_Constraint_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for Profile_Tier_Level_Flags use
    record
        General_Tier_Flag at 0 range 0 .. 0;
        General_Progressive_Source_Flag at 0 range 1 .. 1;
        General_Interlaced_Source_Flag at 0 range 2 .. 2;
        General_Non_Packed_Constraint_Flag at 0 range 3 .. 3;
        General_Frame_Only_Constraint_Flag at 0 range 4 .. 4;
    end record;

    type Profile_Tier_Level is
    record
        Flags: Profile_Tier_Level_Flags;
        General_Profile_IDC: Profile_IDC;
        General_Level_IDC: Level_IDC;
    end record
        with Convention => C;

    type Profile_Tier_Level_Access is access constant Profile_Tier_Level
        with Convention => C,
             Storage_Size => 0;

    type Video_Parameter_Set is
    record
        Flags: VPS_Flags;
        VPS_Video_Parameter_Set_ID: Interfaces.Unsigned_8;
        VPS_Max_Sub_Layers_Minus_1: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_8;
        Reserved_2: Interfaces.Unsigned_8;
        VPS_Num_Units_In_Tick: Interfaces.Unsigned_32;
        VPS_Time_Scale: Interfaces.Unsigned_32;
        VPS_Num_Ticks_POC_Diff_One_Minus_1: Interfaces.Unsigned_32;
        Reserved_3: Interfaces.Unsigned_32;
        Dec_Pic_Buf_Mgr: Dec_Pic_Buf_Mgr_Access;
        Hrd_Parameters: Hrd_Parameters_Access;
        Profile_Tier_Level: Profile_Tier_Level_Access;
    end record
        with Convention => C;

    package Video_Parameter_Set_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Video_Parameter_Set);

    type Scaling_List_4x4_List is
        array (1 .. Scaling_List_4x4_Num_Lists,
               1 .. Scaling_List_4x4_Num_Elements) of Interfaces.Unsigned_8
        with Convention => C;

    type Scaling_List_8x8_List is
        array (1 .. Scaling_List_8x8_Num_Lists,
               1 .. Scaling_List_8x8_Num_Elements) of Interfaces.Unsigned_8
        with Convention => C;

    type Scaling_List_16x16_List is
        array (1 .. Scaling_List_16x16_Num_Lists,
               1 .. Scaling_List_16x16_Num_Elements) of Interfaces.Unsigned_8
        with Convention => C;

    type Scaling_List_32x32_List is
        array (1 .. Scaling_List_32x32_Num_Lists,
               1 .. Scaling_List_32x32_Num_Elements) of Interfaces.Unsigned_8
        with Convention => C;

    type Scaling_List_16x16 is
        array (1 .. Scaling_List_16x16_Num_Lists) of Interfaces.Unsigned_8
        with Convention => C;

    type Scaling_List_32x32 is
        array (1 .. Scaling_List_32x32_Num_Lists) of Interfaces.Unsigned_8
        with Convention => C;

    type Scaling_Lists is
    record
        Scaling_List_4x4: Scaling_List_4x4_List;
        Scaling_List_8x8: Scaling_List_8x8_List;
        Scaling_List_16x16: Scaling_List_16x16_List;
        Scaling_List_32x32: Scaling_List_32x32_List;
        Scaling_List_DC_Coef_16x16: H265.Scaling_List_16x16;
        Scaling_List_DC_Coef_32x32: H265.Scaling_List_32x32;
    end record
        with Convention => C;

    type Scaling_Lists_Access is access constant Scaling_Lists
        with Convention => C,
             Storage_Size => 0;

    type SPS_VUI_Flags is
    record
        Aspect_Ratio_Info_Present_Flag: Packed_Bit := 0;
        Overscan_Info_Present_Flag: Packed_Bit := 0;
        Overscan_Appropriate_Flag: Packed_Bit := 0;
        Video_Signal_Type_Present_Flag: Packed_Bit := 0;
        Video_Full_Range_Flag: Packed_Bit := 0;
        Colour_Description_Present_Flag: Packed_Bit := 0;
        Chroma_LOC_Info_Present_Flag: Packed_Bit := 0;
        Neutral_Chroma_Indication_Flag: Packed_Bit := 0;
        Field_Seq_Flag: Packed_Bit := 0;
        Frame_Field_Info_Present_Flag: Packed_Bit := 0;
        Default_Display_Window_Flag: Packed_Bit := 0;
        VUI_Timing_Info_Present_Flag: Packed_Bit := 0;
        VUI_POC_Proportional_To_Timing_Flag: Packed_Bit := 0;
        VUI_Hrd_Parameters_Present_Flag: Packed_Bit := 0;
        Bitstream_Restriction_Flag: Packed_Bit := 0;
        Tiles_Fixed_Structure_Flag: Packed_Bit := 0;
        Motion_Vectors_Over_Pic_Boundaries_Flag: Packed_Bit := 0;
        Restricted_Ref_Pic_Lists_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for SPS_VUI_Flags use
    record
        Aspect_Ratio_Info_Present_Flag at 0 range 0 .. 0;
        Overscan_Info_Present_Flag at 0 range 1 .. 1;
        Overscan_Appropriate_Flag at 0 range 2 .. 2;
        Video_Signal_Type_Present_Flag at 0 range 3 .. 3;
        Video_Full_Range_Flag at 0 range 4 .. 4;
        Colour_Description_Present_Flag at 0 range 5 .. 5;
        Chroma_LOC_Info_Present_Flag at 0 range 6 .. 6;
        Neutral_Chroma_Indication_Flag at 0 range 7 .. 7;
        Field_Seq_Flag at 0 range 8 .. 8;
        Frame_Field_Info_Present_Flag at 0 range 9 .. 9;
        Default_Display_Window_Flag at 0 range 10 .. 10;
        VUI_Timing_Info_Present_Flag at 0 range 11 .. 11;
        VUI_POC_Proportional_To_Timing_Flag at 0 range 12 .. 12;
        VUI_Hrd_Parameters_Present_Flag at 0 range 13 .. 13;
        Bitstream_Restriction_Flag at 0 range 14 .. 14;
        Tiles_Fixed_Structure_Flag at 0 range 15 .. 15;
        Motion_Vectors_Over_Pic_Boundaries_Flag at 0 range 16 .. 16;
        Restricted_Ref_Pic_Lists_Flag at 0 range 17 .. 17;
    end record;

    type Sequence_Parameter_Set_VUI is
    record
        Flags: SPS_VUI_Flags;
        Aspect_Ratio_IDC: H265.Aspect_Ratio_IDC;
        SAR_Width: Interfaces.Unsigned_16;
        SAR_Height: Interfaces.Unsigned_16;
        Video_Format: Interfaces.Unsigned_8;
        Colour_Primaries: Interfaces.Unsigned_8;
        Transfer_Characteristics: Interfaces.Unsigned_8;
        Matrix_Coeffs: Interfaces.Unsigned_8;
        Chroma_Sample_LOC_Type_Top_Field: Interfaces.Unsigned_8;
        Chroma_Sample_LOC_Type_Bottom_Field: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_8;
        Reserved_2: Interfaces.Unsigned_8;
        Def_Disp_Win_Left_Offset: Interfaces.Unsigned_16;
        Def_Disp_Win_Right_Offset: Interfaces.Unsigned_16;
        Def_Disp_Win_Top_Offset: Interfaces.Unsigned_16;
        Def_Disp_Win_Bottom_Offset: Interfaces.Unsigned_16;
        VUI_Num_Units_In_Tick: Interfaces.Unsigned_32;
        VUI_Time_Scale: Interfaces.Unsigned_32;
        VUI_Num_Ticks_POC_Diff_One_Minus_1: Interfaces.Unsigned_32;
        Min_Spation_Segmentation: Interfaces.Unsigned_16;
        Reserved_3: Interfaces.Unsigned_16;
        Max_Bytes_Per_Pic_Denom: Interfaces.Unsigned_8;
        Mex_Bits_Per_Min_CU_Denom: Interfaces.Unsigned_8;
        Log2_Max_MV_Length_Horizontal: Interfaces.Unsigned_8;
        Log2_Max_MV_Length_Vertical: Interfaces.Unsigned_8;
        Hrd_Parameters: Hrd_Parameters_Access;
    end record
        with Convention => C;

    type Sequence_Parameter_Set_VUI_Access is
        access constant Sequence_Parameter_Set_VUI
        with Convention => C,
             Storage_Size => 0;

    type Predictor_Palette_Components_List is array
        (1 .. Predictor_Palette_Components_List_Size,
         1 .. Predictor_Palette_Comp_Entries_List_Size)
         of Interfaces.Unsigned_16
        with Convention => C;

    type Predictor_Palette_Entries is
    record
        Predictor_Palette_Entries: Predictor_Palette_Components_List;
    end record
        with Convention => C;

    type Predictor_Palette_Entries_Access is
        access constant Predictor_Palette_Entries
        with Convention => C,
             Storage_Size => 0;

    type Sps_Flags is
    record
        SPS_Temporal_ID_Nesting_Flag: Packed_Bit := 0;
        Separate_Colour_Plane_Flag: Packed_Bit := 0;
        Conformance_Window_Flag: Packed_Bit := 0;
        SPS_Sub_Layer_Ordering_Info_Present_Flag: Packed_Bit := 0;
        Scaling_List_Enabled_Flag: Packed_Bit := 0;
        SPS_Scaling_List_Data_Present_Flag: Packed_Bit := 0;
        Amp_Enabled_Flag: Packed_Bit := 0;
        Sample_Adaptive_Offset_Enabled_Flag: Packed_Bit := 0;
        PCM_Enabled_Flag: Packed_Bit := 0;
        PCM_Loop_Filter_Disabled_Flag: Packed_Bit := 0;
        Long_Term_Ref_Pics_Present_Flag: Packed_Bit := 0;
        SPS_Temporal_MVP_Enabled_Flag: Packed_Bit := 0;
        Strong_Intra_Smoothing_Enabled_Flag: Packed_Bit := 0;
        VUI_Parameters_Present_Flag: Packed_Bit := 0;
        SPS_Extension_Present_Flag: Packed_Bit := 0;
        SPS_Range_Extension_Flag: Packed_Bit := 0;
        Transform_Skip_Rotation_Enabled_Flag: Packed_Bit := 0;
        Transform_Skip_Context_Enabled_Flag: Packed_Bit := 0;
        Implicit_RDPCM_Enabled_Flag: Packed_Bit := 0;
        Explicit_RDPCM_Enabled_Flag: Packed_Bit := 0;
        Extended_Precision_Processing_Flag: Packed_Bit := 0;
        Intra_Smoothing_Disabled_Flag: Packed_Bit := 0;
        High_Precision_Offsets_Enabled_Flag: Packed_Bit := 0;
        Persistent_Rice_Adaptation_Enabled_Flag: Packed_Bit := 0;
        CABAC_Bypass_Alignment_Enabled_Flag: Packed_Bit := 0;
        SPS_SCC_Extension_Flag: Packed_Bit := 0;
        SPS_Curr_Pic_Ref_Enabled_Flag: Packed_Bit := 0;
        Palette_Mode_Enabled_Flag: Packed_Bit := 0;
        SPS_Palette_Predictor_Initializers_Present_Flag: Packed_Bit := 0;
        Intra_Boundary_Filtering_Disabled_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for Sps_Flags use
    record
        SPS_Temporal_ID_Nesting_Flag at 0 range 0 .. 0;
        Separate_Colour_Plane_Flag at 0 range 1 .. 1;
        Conformance_Window_Flag at 0 range 2 .. 2;
        SPS_Sub_Layer_Ordering_Info_Present_Flag at 0 range 3 .. 3;
        Scaling_List_Enabled_Flag at 0 range 4 .. 4;
        SPS_Scaling_List_Data_Present_Flag at 0 range 5 .. 5;
        Amp_Enabled_Flag at 0 range 6 .. 6;
        Sample_Adaptive_Offset_Enabled_Flag at 0 range 7 .. 7;
        PCM_Enabled_Flag at 0 range 8 .. 8;
        PCM_Loop_Filter_Disabled_Flag at 0 range 9 .. 9;
        Long_Term_Ref_Pics_Present_Flag at 0 range 10 .. 10;
        SPS_Temporal_MVP_Enabled_Flag at 0 range 11 .. 11;
        Strong_Intra_Smoothing_Enabled_Flag at 0 range 12 .. 12;
        VUI_Parameters_Present_Flag at 0 range 13 .. 13;
        SPS_Extension_Present_Flag at 0 range 14 .. 14;
        SPS_Range_Extension_Flag at 0 range 15 .. 15;
        Transform_Skip_Rotation_Enabled_Flag at 0 range 16 .. 16;
        Transform_Skip_Context_Enabled_Flag at 0 range 17 .. 17;
        Implicit_RDPCM_Enabled_Flag at 0 range 18 .. 18;
        Explicit_RDPCM_Enabled_Flag at 0 range 19 .. 19;
        Extended_Precision_Processing_Flag at 0 range 20 .. 20;
        Intra_Smoothing_Disabled_Flag at 0 range 21 .. 21;
        High_Precision_Offsets_Enabled_Flag at 0 range 22 .. 22;
        Persistent_Rice_Adaptation_Enabled_Flag at 0 range 23 .. 23;
        CABAC_Bypass_Alignment_Enabled_Flag at 0 range 24 .. 24;
        SPS_SCC_Extension_Flag at 0 range 25 .. 25;
        SPS_Curr_Pic_Ref_Enabled_Flag at 0 range 26 .. 26;
        Palette_Mode_Enabled_Flag at 0 range 27 .. 27;
        SPS_Palette_Predictor_Initializers_Present_Flag at 0 range 28 .. 28;
        Intra_Boundary_Filtering_Disabled_Flag at 0 range 29 .. 29;
    end record;

    type Short_Term_Ref_Pic_Set_Flags is
    record
        Inter_Ref_Pic_Set_Prediction_Flag: Packed_Bit := 0;
        Delta_RPS_Sign: Packed_Bit := 0;
    end record
        with Convention => C;

    for Short_Term_Ref_Pic_Set_Flags use
    record
        Inter_Ref_Pic_Set_Prediction_Flag at 0 range 0 .. 0;
        Delta_RPS_Sign at 0 range 1 .. 1;
    end record;

    type Max_DPB_List is array (1 .. Max_DPB_Size) of Interfaces.Unsigned_16
        with Convention => C;

    type Short_Term_Ref_Pic_Set is
    record
        Flags: Short_Term_Ref_Pic_Set_Flags;
        Delta_IDX_Minus_1: Interfaces.Unsigned_32;
        Use_Delta_Flag: Interfaces.Unsigned_16;
        Abs_Delta_RPS_Minus_1: Interfaces.Unsigned_16;
        Used_By_Curr_Pic_Flag: Interfaces.Unsigned_16;
        Used_By_Curr_Pic_S0_Flag: Interfaces.Unsigned_16;
        Used_By_Curr_Pic_S1_Flag: Interfaces.Unsigned_16;
        Reserved_1: Interfaces.Unsigned_16;
        Reserved_2: Interfaces.Unsigned_8;
        Reserved_3: Interfaces.Unsigned_8;
        Num_Negative_Pics: Interfaces.Unsigned_8;
        Num_Positive_Pics: Interfaces.Unsigned_8;
        Delta_POC_S0_Minus_1: Max_DPB_List;
        Delta_POC_S1_Minus_1: Max_DPB_List;
    end record
        with Convention => C;

    type Short_Term_Ref_Pic_Set_Access is access constant Short_Term_Ref_Pic_Set
        with Convention => C,
             Storage_Size => 0;

    type Max_Long_Term_Ref_Pics_List is
        array (1 .. Max_Long_Term_Ref_Pics_SPS) of Interfaces.Unsigned_32
        with Convention => C;

    type Long_Term_Ref_Pics_SPS is
    record
        Used_By_Curr_Pic_LT_SPS_Flag: Interfaces.Unsigned_32;
        LT_Ref_Pic_POC_LSB_SPS: Max_Long_Term_Ref_Pics_List;
    end record
        with Convention => C;

    type Long_Term_Ref_Pics_SPS_Access is access constant Long_Term_Ref_Pics_SPS
        with Convention => C,
             Storage_Size => 0;

    type Sequence_Parameter_Set is
    record
        Flags: SPS_Flags;
        Chroma_Format_IDC: H265.Chroma_Format_IDC;
        Pic_Width_In_Luma_Samples: Interfaces.Unsigned_32;
        Pic_Height_in_Luma_Samples: Interfaces.Unsigned_32;
        SPS_Video_Parameter_Set_ID: Interfaces.Unsigned_8;
        SPS_Max_Sub_Layers_Minus_1: Interfaces.Unsigned_8;
        SPS_Seq_Parameter_Set_ID: Interfaces.Unsigned_8;
        Bit_Depth_Luma_Minus_8: Interfaces.Unsigned_8;
        Bit_Depth_Chroma_Minus_8: Interfaces.Unsigned_8;
        Log2_Max_Pic_Order_Cnt_LSB_Minus_4: Interfaces.Unsigned_8;
        Log2_Min_Luma_Coding_Block_Size_Minus_3: Interfaces.Unsigned_8;
        Log2_Diff_Max_Min_Luma_Coding_Block_Size: Interfaces.Unsigned_8;
        Log2_Min_Luma_Transform_Block_Size_Minus_2: Interfaces.Unsigned_8;
        Log2_Diff_Max_Min_Luma_Transform_Block_Size: Interfaces.Unsigned_8;
        Max_Transform_Hierarchy_Depth_Inter: Interfaces.Unsigned_8;
        Max_Transform_Hierarchy_Depth_Intra: Interfaces.Unsigned_8;
        Num_Short_Term_Ref_Pic_Sets: Interfaces.Unsigned_8;
        Num_Long_Term_Ref_Pics_SPS: Interfaces.Unsigned_8;
        PCM_Sample_Bit_Depth_Luma_Minus_1: Interfaces.Unsigned_8;
        PCM_Sample_Bit_Depth_Chroma_Minus_1: Interfaces.Unsigned_8;
        Log2_Min_PCM_Luma_Coding_Block_Size_Minus_3: Interfaces.Unsigned_8;
        Log2_Diff_Max_Min_PCM_Luma_Coding_Block_Size: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_8;
        Reserved_2: Interfaces.Unsigned_8;
        Palette_Max_Size: Interfaces.Unsigned_8;
        Delta_Palette_Max_Predictor_Size: Interfaces.Unsigned_8;
        Motion_Vector_Resolution_Control: Interfaces.Unsigned_8;
        SPS_Num_Palette_Predictor_Initializer_Minus_1: Interfaces.Unsigned_8;
        Conf_Win_Left_Offset: Interfaces.Unsigned_32;
        Conf_Win_Right_Offset: Interfaces.Unsigned_32;
        Conf_Win_Top_Offset: Interfaces.Unsigned_32;
        Conf_Win_Bottom_Offset: Interfaces.Unsigned_32;
        Profile_Tier_Level: Profile_Tier_Level_Access;
        Dec_Pic_Buf_Mgr: Dec_Pic_Buf_Mgr_Access;
        Scaling_Lists: Scaling_Lists_Access;
        Short_Term_Ref_Pic_Set: Short_Term_Ref_Pic_Set_Access;
        Long_Term_Ref_Pics_SPS: Long_Term_Ref_Pics_SPS_Access;
        Sequence_Parameter_Set_VUI: Sequence_Parameter_Set_VUI_Access;
        Predictor_Palette_Entries: Predictor_Palette_Entries_Access;
    end record
        with Convention => C;

    package Sequence_Parameter_Set_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Sequence_Parameter_Set);

    type PPS_Flags is
    record
        Dependent_Slice_Segments_Enabled_Flag: Packed_Bit := 0;
        Output_Flag_Present_Flag: Packed_Bit := 0;
        Sign_Data_Hiding_Enabled_Flag: Packed_Bit := 0;
        Cabac_Init_Present_Flag: Packed_Bit := 0;
        Constrained_Intra_Pred_Flag: Packed_Bit := 0;
        Transform_Skip_Enabled_Flag: Packed_Bit := 0;
        CU_QP_Delta_Enabled_Flag: Packed_Bit := 0;
        PPS_Slice_Chroma_QP_Offsets_Present_Flag: Packed_Bit := 0;
        Weighted_Pred_Flag: Packed_Bit := 0;
        Weighted_Bipred_Flag: Packed_Bit := 0;
        Transquant_Bypass_Enabled_Flag: Packed_Bit := 0;
        Tiles_Enabled_Flag: Packed_Bit := 0;
        Entropy_Coding_Sync_Enabled_Flag: Packed_Bit := 0;
        Uniform_Spacing_Flag: Packed_Bit := 0;
        Loop_Filter_Across_Tiles_Enabled_Flag: Packed_Bit := 0;
        PPS_Loop_Filer_Across_Slices_Enabled_Flag: Packed_Bit := 0;
        Deblocking_Filter_Control_Present_Flag: Packed_Bit := 0;
        Deblocking_Filter_Override_Enabled_Flag: Packed_Bit := 0;
        PPS_Deblocking_Filter_Disabled_Flag: Packed_Bit := 0;
        PPS_Scaling_List_Data_Present_Flag: Packed_Bit := 0;
        Lists_Modification_Present_Flag: Packed_Bit := 0;
        Slice_Segment_Header_Extension_Present_Flag: Packed_Bit := 0;
        PPS_Extension_Present_Flag: Packed_Bit := 0;
        Cross_Component_Prediction_Enabled_Flag: Packed_Bit := 0;
        Chroma_QP_Offset_List_Enabled_Flag: Packed_Bit := 0;
        PPS_Curr_Pic_Ref_Enabled_Flag: Packed_Bit := 0;
        Residual_Adaptive_Colour_Transform_Enabled_Flag: Packed_Bit := 0;
        PPS_Slice_Act_QP_Offsets_Present_Flag: Packed_Bit := 0;
        PPS_Palette_Predictor_Initializers_Present_Flag: Packed_Bit := 0;
        Monochrome_Palette_Flag: Packed_Bit := 0;
        PPS_Range_Extension_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for PPS_Flags use
    record
        Dependent_Slice_Segments_Enabled_Flag at 0 range 0 .. 0;
        Output_Flag_Present_Flag at 0 range 1 .. 1;
        Sign_Data_Hiding_Enabled_Flag at 0 range 2 .. 2;
        Cabac_Init_Present_Flag at 0 range 3 .. 3;
        Constrained_Intra_Pred_Flag at 0 range 4 .. 4;
        Transform_Skip_Enabled_Flag at 0 range 5 .. 5;
        CU_QP_Delta_Enabled_Flag at 0 range 6 .. 6;
        PPS_Slice_Chroma_QP_Offsets_Present_Flag at 0 range 7 .. 7;
        Weighted_Pred_Flag at 0 range 8 .. 8;
        Weighted_Bipred_Flag at 0 range 9 .. 9;
        Transquant_Bypass_Enabled_Flag at 0 range 10 .. 10;
        Tiles_Enabled_Flag at 0 range 11 .. 11;
        Entropy_Coding_Sync_Enabled_Flag at 0 range 12 .. 12;
        Uniform_Spacing_Flag at 0 range 13 .. 13;
        Loop_Filter_Across_Tiles_Enabled_Flag at 0 range 14 .. 14;
        PPS_Loop_Filer_Across_Slices_Enabled_Flag at 0 range 15 .. 15;
        Deblocking_Filter_Control_Present_Flag at 0 range 16 .. 16;
        Deblocking_Filter_Override_Enabled_Flag at 0 range  17 .. 17;
        PPS_Deblocking_Filter_Disabled_Flag at 0 range 18 .. 18;
        PPS_Scaling_List_Data_Present_Flag at 0 range 19 .. 19;
        Lists_Modification_Present_Flag at 0 range 20 .. 20;
        Slice_Segment_Header_Extension_Present_Flag at 0 range 21 .. 21;
        PPS_Extension_Present_Flag at 0 range 22 .. 22;
        Cross_Component_Prediction_Enabled_Flag at 0 range 23 .. 23;
        Chroma_QP_Offset_List_Enabled_Flag at 0 range 24 .. 24;
        PPS_Curr_Pic_Ref_Enabled_Flag at 0 range 25 .. 25;
        Residual_Adaptive_Colour_Transform_Enabled_Flag at 0 range 26 .. 26;
        PPS_Slice_Act_QP_Offsets_Present_Flag at 0 range 27 .. 27;
        PPS_Palette_Predictor_Initializers_Present_Flag at 0 range 28 .. 28;
        Monochrome_Palette_Flag at 0 range 29 .. 29;
        PPS_Range_Extension_Flag at 0 range 30 .. 30;
    end record;

    type Chroma_QP_Offset_List is
        array (1 .. Chroma_QP_Offset_List_Size) of Interfaces.Integer_8
        with Convention => C;

    type Chroma_QP_Offset_Tile_Cols_List is
        array (1 .. Chroma_QP_Offset_Tile_Cols_List_Size)
        of Interfaces.Unsigned_16
        with Convention => C;

    type Chroma_QP_Offset_Tile_Rows_List is
        array (1 .. Chroma_QP_Offset_Tile_Rows_List_Size)
        of Interfaces.Unsigned_16
        with Convention => C;

    type Picture_Parameter_Set is
    record
        Flags: PPS_Flags;
        PPC_Pic_Parameter_Set_ID: Interfaces.Unsigned_8;
        PPS_Seq_Parameter_Set_ID: Interfaces.Unsigned_8;
        SPS_Video_Parameter_Set_ID: Interfaces.Unsigned_8;
        Num_Extra_Slice_Header_Bits: Interfaces.Unsigned_8;
        Num_Ref_Idx_L0_Default_Active_Minus_1: Interfaces.Unsigned_8;
        Num_Ref_Idx_L1_Default_Active_Minus_1: Interfaces.Unsigned_8;
        Init_QP_Minus_26: Interfaces.Integer_8;
        Diff_CU_QP_Delta_Depth: Interfaces.Unsigned_8;
        PPS_CB_QP_Offset: Interfaces.Integer_8;
        PPS_CR_QP_Offset: Interfaces.Integer_8;
        PPS_Beta_Offset_Div_2: Interfaces.Integer_8;
        PPS_TC_Offset_Div_2: Interfaces.Integer_8;
        Log2_Parallel_Merge_Level_Minus_2: Interfaces.Unsigned_8;
        Log2_Max_Transform_Skip_Block_Size_Minus_2: Interfaces.Unsigned_8;
        Diff_CU_Chroma_QP_Offset_Depth: Interfaces.Unsigned_8;
        Chroma_QP_Offset_List_Len_Minus_1: Interfaces.Unsigned_8;
        CB_QP_Offset_List: Chroma_QP_Offset_List;
        CR_QP_Offset_List: Chroma_QP_Offset_List;
        Log2_SAO_Offset_Scale_Luma: Interfaces.Unsigned_8;
        Log2_SAO_Offset_Scale_Chroma: Interfaces.Unsigned_8;
        PPS_Act_Y_QP_Offset_Plus_5: Interfaces.Integer_8;
        PPS_Act_CB_QP_Offset_Plus_5: Interfaces.Integer_8;
        PPS_Act_CR_QP_Offset_Plus_3: Interfaces.Integer_8;
        PPS_Num_Palette_Predictor_Initializers: Interfaces.Unsigned_8;
        Luma_Bit_Depth_Entry_Minus_8: Interfaces.Unsigned_8;
        Chroma_Bit_Depth_Entry_Minus_8: Interfaces.Unsigned_8;
        Num_Tile_Columns_Minus_1: Interfaces.Unsigned_8;
        Num_Tile_Rows_Minus_1: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_8;
        Reserved_2: Interfaces.Unsigned_8;
        Column_Width_Minus_1: Chroma_QP_Offset_Tile_Cols_List;
        Row_Height_Minus_1: Chroma_QP_Offset_Tile_Rows_List;
        Reserved_3: Interfaces.Unsigned_32;
        Scaling_Lists: Scaling_Lists_Access;
        Predictor_Palette_Entries: Predictor_Palette_Entries_Access;
    end record
        with Convention => C;

    package Picture_Parameter_Set_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Picture_Parameter_Set);
end Vulkan.Extensions.Std_Video.H265;


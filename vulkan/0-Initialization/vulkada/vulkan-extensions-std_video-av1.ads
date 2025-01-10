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

-- Common constants and types for AV1 extensions

package Vulkan.Extensions.Std_Video.AV1 is
    -- Constants.
    Num_Ref_Frames: constant := 8;
    Refs_Per_Frame: constant := 7;
    Total_Refs_Per_Frame: constant := 8;
    Max_Tile_Cols: constant := 64;
    Max_Tile_Rows: constant := 64;
    Max_Segments: constant := 8;
    Seg_Lvl_Max: constant := 8;
    Primary_Ref_None: constant := 7;
    Select_Integer_Mv: constant := 2;
    Select_Screen_Content_Tools: constant := 2;
    Skip_Mode_Frames: constant := 2;
    Max_Loop_Filter_Strengths: constant := 4;
    Loop_Filter_Adjustments: constant := 2;
    Max_CDEF_Filter_Strengths: constant := 8;
    Max_Num_Planes: constant := 3;
    Global_Motion_Params: constant := 6;
    Max_Num_Y_Points: constant := 14;
    Max_Num_Cb_Points: constant := 10;
    Max_Num_Cr_Points: constant := 10;
    Max_Num_Pos_Luma: constant := 24;
    Max_Num_Pos_Chroma: constant := 25;

    -- Enumerations.
    type Profile is (Main,
                     High,
                     Professional,
                     Invalid)
        with Convention => C;

    for Profile'Size use 32;

    for Profile use (Main => 0,
                     High => 1,
                     Professional => 2,
                     Invalid => 16#7fffffff#);

    type Level is (Level_2_0,
                   Level_2_1,
                   Level_2_2,
                   Level_2_3,
                   Level_3_0,
                   Level_3_1,
                   Level_3_2,
                   Level_3_3,
                   Level_4_0,
                   Level_4_1,
                   Level_4_2,
                   Level_4_3,
                   Level_5_0,
                   Level_5_1,
                   Level_5_2,
                   Level_5_3,
                   Level_6_0,
                   Level_6_1,
                   Level_6_2,
                   Level_6_3,
                   Level_7_0,
                   Level_7_1,
                   Level_7_2,
                   Level_7_3,
                   Invalid)
        with Convention => C;

    for Level'Size use 32;

    for Level use (Level_2_0 => 0,
                   Level_2_1 => 1,
                   Level_2_2 => 2,
                   Level_2_3 => 3,
                   Level_3_0 => 4,
                   Level_3_1 => 5,
                   Level_3_2 => 6,
                   Level_3_3 => 7,
                   Level_4_0 => 8,
                   Level_4_1 => 9,
                   Level_4_2 => 10,
                   Level_4_3 => 11,
                   Level_5_0 => 12,
                   Level_5_1 => 13,
                   Level_5_2 => 14,
                   Level_5_3 => 15,
                   Level_6_0 => 16,
                   Level_6_1 => 17,
                   Level_6_2 => 18,
                   Level_6_3 => 19,
                   Level_7_0 => 20,
                   Level_7_1 => 21,
                   Level_7_2 => 22,
                   Level_7_3 => 23,
                   Invalid => 16#7fffffff#);

    type Frame_Type is (Key,
                        Inter,
                        Intra_Only,
                        Switch,
                        Invalid)
        with Convention => C;

    for Frame_Type'Size use 32;

    for Frame_Type use (Key => 0,
                        Inter => 1,
                        Intra_Only => 2,
                        Switch => 3,
                        Invalid => 16#7fffffff#);

    type Reference_Name is (Intra_Frame,
                            Last_Frame,
                            Last_2_Frame,
                            Last_3_Frame,
                            Golden_Frame,
                            BWD_Ref_Frame,
                            Alt_Ref_2_Frame,
                            Alt_Ref_Frame,
                            Invalid)
        with Convention => C;

    for Reference_Name'Size use 32;

    for Reference_Name use (Intra_Frame => 0,
                            Last_Frame => 1,
                            Last_2_Frame => 2,
                            Last_3_Frame => 3,
                            Golden_Frame => 4,
                            BWD_Ref_Frame => 5,
                            Alt_Ref_2_Frame => 6,
                            Alt_Ref_Frame => 7,
                            Invalid => 16#7fffffff#);

    type Interpolation_Filter is (Eighttap,
                                  Eighttap_Smooth,
                                  Eighttap_Sharp,
                                  Bilinear,
                                  Switchable,
                                  Invalid)
        with Convention => C;

    for Interpolation_Filter'Size use 32;

    for Interpolation_Filter use (Eighttap => 0,
                                  Eighttap_Smooth => 1,
                                  Eighttap_Sharp => 2,
                                  Bilinear => 3,
                                  Switchable => 4,
                                  Invalid => 16#7fffffff#);

    type Tx_Mode is (Only_4x4,
                     Largest,
                     Mode_Select,
                     Invalid)
        with Convention => C;

    for Tx_Mode'Size use 32;

    for Tx_Mode use (Only_4x4 => 0,
                     Largest => 1,
                     Mode_Select => 2,
                     Invalid => 16#7fffffff#);

    type Frame_Restoration_Type is (None,
                                    Wiener,
                                    SGR_Proj,
                                    Switchable,
                                    Invalid)
        with Convention => C;

    for Frame_Restoration_Type'Size use 32;

    for Frame_Restoration_Type use (None => 0,
                                    Wiener => 1,
                                    SGR_Proj => 2,
                                    Switchable => 3,
                                    Invalid => 16#7fffffff#);

    type Color_Primaries is (BT_709,
                             BT_Unspecified,
                             BT_470_M,
                             BT_470_B_G,
                             BT_601,
                             SMPTE_240,
                             Generic_Film,
                             BT_2020,
                             XYZ,
                             SMPTE_431,
                             SMPTE_432,
                             EBU_3213,
                             Invalid)
        with Convention => C;

    for Color_Primaries'Size use 32;

    for Color_Primaries use (BT_709 => 1,
                             BT_Unspecified => 2,
                             BT_470_M => 4,
                             BT_470_B_G => 5,
                             BT_601 => 6,
                             SMPTE_240 => 7,
                             Generic_Film => 8,
                             BT_2020 => 9,
                             XYZ => 10,
                             SMPTE_431 => 11,
                             SMPTE_432 => 12,
                             EBU_3213 => 22,
                             Invalid => 16#7fffffff#);

    type Transfer_Characteristics is (Reserved_0,
                                      BT_709,
                                      Unspecified,
                                      Reserved_3,
                                      BT_470_M,
                                      BT_470_B_G,
                                      BT_601,
                                      SMPTE_240,
                                      Linear,
                                      Log_100,
                                      Log_100_Sqrt_10,
                                      IEC_61966,
                                      BT_1361,
                                      SRGB,
                                      BT_2020_10_Bit,
                                      BT_2020_12_Bit,
                                      SMPTE_2084,
                                      SMPTE_428,
                                      HLG,
                                      Invalid)
        with Convention => C;

    for Transfer_Characteristics'Size use 32;

    for Transfer_Characteristics use (Reserved_0 => 0,
                                      BT_709 => 1,
                                      Unspecified => 2,
                                      Reserved_3 => 3,
                                      BT_470_M => 4,
                                      BT_470_B_G => 5,
                                      BT_601 => 6,
                                      SMPTE_240 => 7,
                                      Linear => 8,
                                      Log_100 => 9,
                                      Log_100_Sqrt_10 => 10,
                                      IEC_61966 => 11,
                                      BT_1361 => 12,
                                      SRGB => 13,
                                      BT_2020_10_Bit => 14,
                                      BT_2020_12_Bit => 15,
                                      SMPTE_2084 => 16,
                                      SMPTE_428 => 17,
                                      HLG => 18,
                                      Invalid => 16#7fffffff#);

    type Matrix_Coefficients is (Identity,
                                 BT_709,
                                 Unspecified,
                                 Reserved_3,
                                 FCC,
                                 BT_470_B_G,
                                 BT_601,
                                 SMPTE_240,
                                 SMPTE_YCGCO,
                                 BT_2020_NCL,
                                 BT_2020_CL,
                                 SMPTE_2085,
                                 Chromat_NCL,
                                 Chromat_CL,
                                 ICTCP,
                                 Invalid)
        with Convention => C;

    for Matrix_Coefficients'Size use 32;

    for Matrix_Coefficients use (Identity => 0,
                                 BT_709 => 1,
                                 Unspecified => 2,
                                 Reserved_3 => 3,
                                 FCC => 4,
                                 BT_470_B_G => 5,
                                 BT_601 => 6,
                                 SMPTE_240 => 7,
                                 SMPTE_YCGCO => 8,
                                 BT_2020_NCL => 9,
                                 BT_2020_CL => 10,
                                 SMPTE_2085 => 11,
                                 Chromat_NCL => 12,
                                 Chromat_CL => 13,
                                 ICTCP => 14,
                                 Invalid => 16#7fffffff#);

    type Chroma_Sample_Position is (Unknown,
                                    Vertical,
                                    Colocated,
                                    Reserved,
                                    Invalid)
        with Convention => C;

    for Chroma_Sample_Position'Size use 32;

    for Chroma_Sample_Position use (Unknown => 0,
                                    Vertical => 1,
                                    Colocated => 2,
                                    Reserved => 3,
                                    Invalid => 16#7fffffff#);

    -- Records.
    type Color_Config_Flags is
    record
        Mono_Chrome: Packed_Bit := 0;
        Color_Range: Packed_Bit := 0;
        Separate_UV_Delta_Q: Packed_Bit := 0;
        Color_Description_Present_Flag: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Color_Config_Flags use
    record
        Mono_Chrome at 0 range 0 .. 0;
        Color_Range at 0 range 1 .. 1;
        Separate_UV_Delta_Q at 0 range 2 .. 2;
        Color_Description_Present_Flag at 0 range 3 .. 3;
        Reserved at 0 range 4 .. 31;
    end record;

    type Color_Config is
    record
        Flags: Color_Config_Flags;
        Bit_Depth: Interfaces.Unsigned_8;
        Subsampling_X: Interfaces.Unsigned_8;
        Subsampling_Y: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_8;
        Color_Primaries: AV1.Color_Primaries;
        Transfer_Characteristics: AV1.Transfer_Characteristics;
        Matrix_Coefficients: AV1.Matrix_Coefficients;
        Chroma_Sample_Position: AV1.Chroma_Sample_Position;
    end record
        with Convention => C;

    type Color_Config_Access is access constant Color_Config
        with Convention => C,
             Storage_Size => 0;

    type Timing_Info_Flags is
    record
        Equal_Picture_Interval: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Timing_Info_Flags use
    record
        Equal_Picture_Interval at 0 range 0 .. 0;
        Reserved at 0 range 1 .. 31;
    end record;

    type Timing_Info is
    record
        Flags: Timing_Info_Flags;
        Num_Units_In_Display_Tick: Interfaces.Unsigned_32;
        Time_Scale: Interfaces.Unsigned_32;
        Num_Ticks_Per_Picture_Minus_1: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Timing_Info_Access is access constant Timing_Info
        with Convention => C,
             Storage_Size => 0;

    type Loop_Filter_Flags is
    record
        Loop_Filter_Delta_Enabled: Packed_Bit := 0;
        Loop_Filter_Delta_Update: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Loop_Filter_Flags use
    record
        Loop_Filter_Delta_Enabled at 0 range 0 .. 0;
        Loop_Filter_Delta_Update at 0 range 1 .. 1;
        Reserved at 0 range 2 .. 31;
    end record;

    type Loop_Filter_Strengths_Array is
        array (1 .. Max_Loop_Filter_Strengths) of Interfaces.Unsigned_8
        with Convention => C;

    type Total_Refs_Per_Frame_Array is
        array (1 .. Total_Refs_Per_Frame) of Interfaces.Integer_8
        with Convention => C;

    type Loop_Filter_Adjustments_Array is
        array (1 .. Loop_Filter_Adjustments) of Interfaces.Integer_8
        with Convention => C;

    type Loop_Filter is
    record
        Flags: Loop_Filter_Flags;
        Loop_Filter_Level: Loop_Filter_Strengths_Array;
        Loop_Filter_Sharpness: Interfaces.Unsigned_8;
        Update_Ref_Delta: Interfaces.Unsigned_8;
        Loop_Filter_Ref_Deltas: Total_Refs_Per_Frame_Array;
        Update_Mode_Delta: Interfaces.Unsigned_8;
        Loop_Filter_Mode_Deltas: Loop_Filter_Adjustments_Array;
    end record
        with Convention => C;

    type Loop_Filter_Access is access constant Loop_Filter
        with Convention => C,
             Storage_Size => 0;
            
    type Quantization_Flags is
    record
        Using_QMatrix: Packed_Bit := 0;
        Diff_UV_Delta: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Quantization_Flags use
    record
        Using_QMatrix at 0 range 0 .. 0;
        Diff_UV_Delta at 0 range 1 .. 1;
        Reserved at 0 range 2 .. 31;
    end record;

    type Quantization is
    record
        Flags: Quantization_Flags;
        Base_Q_IDX: Interfaces.Unsigned_8;
        Delta_Q_Y_Dc: Interfaces.Integer_8;
        Delta_Q_U_Dc: Interfaces.Integer_8;
        Delta_Q_U_Ac: Interfaces.Integer_8;
        Delta_Q_V_Dc: Interfaces.Integer_8;
        Delta_Q_V_Ac: Interfaces.Integer_8;
        QM_Y: Interfaces.Unsigned_8;
        QM_U: Interfaces.Unsigned_8;
        QM_V: Interfaces.Unsigned_8;
    end record
        with Convention => C;

    type Quantization_Access is access constant Quantization
        with Convention => C,
             Storage_Size => 0;

    type Segments_Array is array (1 .. Max_Segments) of Interfaces.Unsigned_8
        with Convention => C;

    type Feature_Data_Array is array(1 .. Max_Segments, 1 .. Seg_Lvl_Max)
        of Interfaces.Integer_16
        with Convention => C;

    type Segmentation is
    record
        Feature_Enabled: Segments_Array;
        Feature_Data: Feature_Data_Array;
    end record
        with Convention => C;

    type Segmentation_Access is access constant Segmentation
        with Convention => C,
             Storage_Size => 0;

    type Tile_Info_Flags is
    record
        Uniform_Tile_Spacing_Flag: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Tile_Info_Flags use
    record
        Uniform_Tile_Spacing_Flag at 0 range 0 .. 0;
        Reserved at 0 range 1 .. 31;
    end record;

    type Unsigned_16_Access is access constant Interfaces.Unsigned_16
        with Convention => C,
             Storage_Size => 0;

    type Tile_Info is
    record
        Flags: Tile_Info_Flags;
        Tile_Cols: Interfaces.Unsigned_8;
        Tile_Rows: Interfaces.Unsigned_8;
        Context_Update_Tile_ID: Interfaces.Unsigned_16;
        Tile_Size_Bytes_Minus_1: Interfaces.Unsigned_8;
        Reserved_1: Reserved_Array(1 .. 7);
        MI_Col_Starts: Unsigned_16_Access;
        MI_Row_Starts: Unsigned_16_Access;
        Width_In_Sbs_Minus_1: Unsigned_16_Access;
        Height_In_Sbs_Minus_1: Unsigned_16_Access;
    end record
        with Convention => C;

    type Tile_Info_Access is access constant Tile_Info
        with Convention => C,
             Storage_Size => 0;

    type CDEF_Filter_Strengths_Array is array (1 .. Max_CDEF_Filter_Strengths)
        of Interfaces.Unsigned_8
        with Convention => C;

    type CDEF is
    record
        CDEF_Damping_Minus_3: Interfaces.Unsigned_8;
        CDEF_Bits: Interfaces.Unsigned_8;
        CDEF_Y_Pri_Strength: CDEF_Filter_Strengths_Array;
        CDEF_Y_Sec_Strength: CDEF_Filter_Strengths_Array;
        CDEF_UV_Pri_Strength: CDEF_Filter_Strengths_Array;
        CDEF_UV_Sec_Strength: CDEF_Filter_Strengths_Array;
    end record
        with Convention => C;

    type CDEF_Access is access constant CDEF
        with Convention => C,
             Storage_Size => 0;

    type Frame_Restoration_Array is array (1 .. Max_Num_Planes)
        of Frame_Restoration_Type
        with Convention => C;

    type Loop_Restoration_Size_Array is array (1 .. Max_Num_Planes)
        of Interfaces.Unsigned_16
        with Convention => C;

    type Loop_Restoration is
    record
        Frame_Restoration_Type: Frame_Restoration_Array;
        Loop_Restoration_Size: Loop_Restoration_Size_Array;
    end record
        with Convention => C;

    type Loop_Restoration_Access is access constant Loop_Restoration
        with Convention => C,
             Storage_Size => 0;

    type Ref_Frames_Array is array (1 .. Num_Ref_Frames) of
        Interfaces.Unsigned_8
        with Convention => C;

    type Global_Motion_Params_Array is
        array (1 .. Num_Ref_Frames, 1 .. Global_Motion_Params)
            of Interfaces.Integer_32
        with Convention => C;

    type Global_Motion is
    record
        GM_Type: Ref_Frames_Array;
        GM_Params: Global_Motion_Params_Array;
    end record
        with Convention => C;

    type Global_Motion_Access is access constant Global_Motion
        with Convention => C,
             Storage_Size => 0;

    type Film_Grain_Flags is
    record
        Chroma_Scaling_From_Luma: Packed_Bit := 0;
        Overlap_Flag: Packed_Bit := 0;
        Clip_To_Restricted_Range: Packed_Bit := 0;
        Update_Grain: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Film_Grain_Flags use
    record
        Chroma_Scaling_From_Luma at 0 range 0 .. 0;
        Overlap_Flag at 0 range 1 .. 1;
        Clip_To_Restricted_Range at 0 range 2 .. 2;
        Update_Grain at 0 range 3 .. 3;
        Reserved at 0 range 4 .. 31;
    end record;

    type Y_Points_Array is array (1 .. Max_Num_Y_Points)
        of Interfaces.Unsigned_8
        with Convention => C;

    type CB_Points_Array is array (1 .. Max_Num_CB_Points)
        of Interfaces.Unsigned_8
        with Convention => C;

    type CR_Points_Array is array (1 .. Max_Num_CR_Points)
        of Interfaces.Unsigned_8
        with Convention => C;

    type Pos_Luma_Array is array (1 .. Max_Num_Pos_Luma)
        of Interfaces.Integer_8
        with Convention => C;

    type Pos_Chroma_Array is array (1 .. Max_Num_Pos_Chroma)
        of Interfaces.Integer_8
        with Convention => C;

    type Film_Grain is
    record
        Flags: Film_Grain_Flags;
        Grain_Scaling_Minus_8: Interfaces.Unsigned_8;
        AR_Coeff_Lag: Interfaces.Unsigned_8;
        AR_Coeff_Shift_Minus_6: Interfaces.Unsigned_8;
        Grain_Scale_Shift: Interfaces.Unsigned_8;
        Grain_Seed: Interfaces.Unsigned_16;
        Film_Grain_Params_Ref_IDX: Interfaces.Unsigned_8;
        Num_Y_Points: Interfaces.Unsigned_8;
        Point_Y_Value: Y_Points_Array;
        Point_Y_Scaling: Y_Points_Array;
        Num_CB_Points: Interfaces.Unsigned_8;
        Point_CB_Value: CB_Points_Array;
        Point_CB_Scaling: CB_Points_Array;
        Num_CR_Points: Interfaces.Unsigned_8;
        Point_CR_Value: CR_Points_Array;
        Point_CR_Scaling: CR_Points_Array;
        AR_Coeffs_Y_Plus_128: Pos_Luma_Array;
        AR_Coeffs_CB_Plus_128: Pos_Chroma_Array;
        AR_Coeffs_CR_Plus_128: Pos_Chroma_Array;
        CB_Mult: Interfaces.Unsigned_8;
        CB_Luma_Mult: Interfaces.Unsigned_8;
        CB_Offset: Interfaces.Unsigned_16;
        CR_Mult: Interfaces.Unsigned_8;
        CR_Luma_Mult: Interfaces.Unsigned_8;
        CR_Offset: Interfaces.Unsigned_16;
    end record
        with Convention => C;

    type Film_Grain_Access is access constant Film_Grain
        with Convention => C,
             Storage_Size => 0;

    type Sequence_Header_Flags is
    record
        Still_Picture: Packed_Bit := 0;
        Reduced_Still_Picture_Header: Packed_Bit := 0;
        Use_128x128_Superblock: Packed_Bit := 0;
        Enable_Filter_Intra: Packed_Bit := 0;
        Enable_Intra_Edge_Filter: Packed_Bit := 0;
        Enable_Interintra_Compound: Packed_Bit := 0;
        Enable_Masked_Compound: Packed_Bit := 0;
        Enable_Warped_Motion: Packed_Bit := 0;
        Enable_Dual_Filter: Packed_Bit := 0;
        Enable_Order_Hint: Packed_Bit := 0;
        Enable_JNT_Comp: Packed_Bit := 0;
        Enable_Ref_Frame_MVS: Packed_Bit := 0;
        Frame_ID_Numbers_Present_Flag: Packed_Bit := 0;
        Enable_Superres: Packed_Bit := 0;
        Enable_CDEF: Packed_Bit := 0;
        Enable_Restoration: Packed_Bit := 0;
        Film_Grain_Params_Present: Packed_Bit := 0;
        Timing_Info_Present_Flag: Packed_Bit := 0;
        Initial_Display_Delay_Present_Flag: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Sequence_Header_Flags use
    record
        Still_Picture at 0 range 0 .. 0;
        Reduced_Still_Picture_Header at 0 range 1 .. 1;
        Use_128x128_Superblock at 0 range 2 .. 2;
        Enable_Filter_Intra at 0 range 3 .. 3;
        Enable_Intra_Edge_Filter at 0 range 4 .. 4;
        Enable_Interintra_Compound at 0 range 5 .. 5;
        Enable_Masked_Compound at 0 range 6 .. 6;
        Enable_Warped_Motion at 0 range 7 .. 7;
        Enable_Dual_Filter at 0 range 8 .. 8;
        Enable_Order_Hint at 0 range 9 .. 9;
        Enable_JNT_Comp at 0 range 10 .. 10;
        Enable_Ref_Frame_MVS at 0 range 11 .. 11;
        Frame_ID_Numbers_Present_Flag at 0 range 12 .. 12;
        Enable_Superres at 0 range 13 .. 13;
        Enable_CDEF at 0 range 14 .. 14;
        Enable_Restoration at 0 range 15 .. 15;
        Film_Grain_Params_Present at 0 range 16 .. 16;
        Timing_Info_Present_Flag at 0 range 17 .. 17;
        Initial_Display_Delay_Present_Flag at 0 range 18 .. 18;
        Reserved at 0 range 19 .. 31;
    end record;
    
    type Sequence_Header is
    record
        Flags: Sequence_Header_Flags;
        Seq_Profile: Profile;
        Frame_Width_Bits_Minus_1: Interfaces.Unsigned_8;
        Frame_Height_Bits_Minus_1: Interfaces.Unsigned_8;
        Max_Frame_Width_Minus_1: Interfaces.Unsigned_16;
        Max_Frame_Height_Minus_1: Interfaces.Unsigned_16;
        Delta_Frame_ID_Length_Minus_2: Interfaces.Unsigned_8;
        Additional_Frame_ID_Length_Minus_1: Interfaces.Unsigned_8;
        Order_Hint_Bits_Minus_1: Interfaces.Unsigned_8;
        Seq_Force_Integer_MV: Interfaces.Unsigned_8;
        Seq_Force_Screen_Content_Tools: Interfaces.Unsigned_8;
        Reserved_1: Reserved_Array(1 .. 5);
        Color_Config: Color_Config_Access;
        Timing_Info: Timing_Info_Access;
    end record
        with Convention => C;

    type Sequence_Header_Access is access constant Sequence_Header
        with Convention => C,
             Storage_Size => 0;
end Vulkan.Extensions.Std_Video.AV1;


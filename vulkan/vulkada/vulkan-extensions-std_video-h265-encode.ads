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

-- Common constants and types for H265 encode extensions

package Vulkan.Extensions.Std_Video.H265.Encode is
    -- Records.
    type Weight_Table_Flags is
    record
        Luma_Weight_L0_Flag: Interfaces.Unsigned_32;
        Chroma_Weight_L0_Flag: Interfaces.Unsigned_32;
        Luma_Weight_L1_Flag: Interfaces.Unsigned_32;
        Chroma_Weight_L1_Flag: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Delta_Luma_Weight_Array is array (1 .. Max_Num_List_Ref)
        of Interfaces.Integer_8
        with Convention => C;

    type Luma_Offset_Array is array (1 .. Max_Num_List_Ref)
        of Interfaces.Integer_8
        with Convention => C;

    type Delta_Chroma_Weight_Array is array (1 .. Max_Num_List_Ref,
                                             1 .. Max_Chroma_Planes)
        of Interfaces.Integer_8
        with Convention => C;

    type Delta_Chroma_Offset_Array is array (1 .. Max_Num_List_Ref,
                                             1 .. Max_Chroma_Planes)
        of Interfaces.Integer_8
        with Convention => C;

    type Weight_Table is
    record
        Flags: Weight_Table_Flags;
        Luma_Log2_Weight_Denom: Interfaces.Unsigned_8;
        Delta_Chroma_Log2_Weight_Denom: Interfaces.Integer_8;
        Delta_Luma_Weight_L0: Delta_Luma_Weight_Array;
        Luma_Offset_L0: Luma_Offset_Array;
        Delta_Chroma_Weight_L0: Delta_Chroma_Weight_Array;
        Delta_Chroma_Offset_L0: Delta_Chroma_Offset_Array;
        Delta_Luma_Weight_L1: Delta_Luma_Weight_Array;
        Luma_Offset_L1: Luma_Offset_Array;
        Delta_Chroma_Weight_L1: Delta_Chroma_Weight_Array;
        Delta_Chroma_Offset_L1: Delta_Chroma_Offset_Array;
    end record
        with Convention => C;

    type Weight_Table_Access is access constant Weight_Table
        with Convention => C,
             Storage_Size => 0;

    type Slice_Segment_Header_Flags is
    record
        First_Slice_Segment_In_Pic_Flag: Packed_Bit := 0;
        Dependent_Slice_Segment_Flag: Packed_Bit := 0;
        Slice_SAO_Luma_Flag: Packed_Bit := 0;
        Slice_SAO_Chroma_Flag: Packed_Bit := 0;
        Num_Ref_IDX_Active_Override_Flag: Packed_Bit := 0;
        MVD_L1_Zero_Flag: Packed_Bit := 0;
        Cabac_Init_Flag: Packed_Bit := 0;
        CU_Chroma_QP_Offset_Enabled_Flag: Packed_Bit := 0;
        Deblocking_Filter_Override_Flag: Packed_Bit := 0;
        Slice_Deblocking_Filter_Disabled_Flag: Packed_Bit := 0;
        Collocated_From_L0_Flag: Packed_Bit := 0;
        Slice_Loop_Filter_Across_Slices_Enabled_Flag: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Slice_Segment_Header_Flags use
    record
        First_Slice_Segment_In_Pic_Flag at 0 range 0 .. 0;
        Dependent_Slice_Segment_Flag at 0 range 1 .. 1;
        Slice_SAO_Luma_Flag at 0 range 2 .. 2;
        Slice_SAO_Chroma_Flag at 0 range 3 .. 3;
        Num_Ref_IDX_Active_Override_Flag at 0 range 4 .. 4;
        MVD_L1_Zero_Flag at 0 range 5 .. 5;
        Cabac_Init_Flag at 0 range 6 .. 6;
        CU_Chroma_QP_Offset_Enabled_Flag at 0 range 7 .. 7;
        Deblocking_Filter_Override_Flag at 0 range 8 .. 8;
        Slice_Deblocking_Filter_Disabled_Flag at 0 range 9 .. 9;
        Collocated_From_L0_Flag at 0 range 10 .. 10;
        Slice_Loop_Filter_Across_Slices_Enabled_Flag at 0 range 11 .. 11;
        Reserved at 0 range 12 .. 31;
    end record;

    type Slice_Segment_Header is
    record
        Flags: Slice_Segment_Header_Flags;
        Slice_Type: H265.Slice_Type;
        Slice_Segment_Address: Interfaces.Unsigned_32;
        Collocated_Ref_IDX: Interfaces.Unsigned_8;
        Max_Num_Merge_Cand: Interfaces.Unsigned_8;
        Slice_CB_QP_Offset: Interfaces.Integer_8;
        Slice_CR_QP_Offset: Interfaces.Integer_8;
        Slice_Beta_Offset_Div_2: Interfaces.Integer_8;
        Slice_TC_Offset_Div_2: Interfaces.Integer_8;
        Slice_Act_Y_QP_Offset: Interfaces.Integer_8;
        Slice_Act_CB_QP_Offset: Interfaces.Integer_8;
        Slice_Act_CR_QP_Offset: Interfaces.Integer_8;
        Slice_QP_Delta: Interfaces.Integer_8;
        Reserved_1: Interfaces.Unsigned_16;
        Weight_Table: Weight_Table_Access;
    end record
        with Convention => C;

    type Slice_Segment_Header_Access is access constant Slice_Segment_Header
        with Convention => C,
             Storage_Size => 0;

    type Reference_Lists_Info_Flags is
    record
        Ref_Pic_List_Modifications_Flag_L0: Packed_Bit := 0;
        Ref_Pic_List_Modifications_Flag_L1: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Reference_Lists_Info_Flags use
    record
        Ref_Pic_List_Modifications_Flag_L0 at 0 range 0 .. 0;
        Ref_Pic_List_Modifications_Flag_L1 at 0 range 1 .. 1;
        Reserved at 0 range 2 .. 31;
    end record;

    type Ref_Pic_Array is array (1 .. Max_Num_List_Ref) of Interfaces.Unsigned_8
        with Convention => C;

    type List_Entry_Array is array (1 .. Max_Num_List_Ref)
        of Interfaces.Unsigned_8
        with Convention => C;

    type Reference_Lists_Info is
    record
        Flags: Reference_Lists_Info_Flags;
        Num_Ref_IDX_L0_Active_Minus_1: Interfaces.Unsigned_8;
        Num_Ref_IDX_L1_Active_Minus_1: Interfaces.Unsigned_8;
        Ref_Pic_List_0: Ref_Pic_Array;
        Ref_Pic_List_1: Ref_Pic_Array;
        List_Entry_L0: List_Entry_Array;
        List_Entry_L1: List_Entry_Array;
    end record
        with Convention => C;

    type Reference_Lists_Info_Access is access constant Reference_Lists_Info
        with Convention => C;

    type Picture_Info_Flags is
    record
        Is_Reference: Packed_Bit := 0;
        IRAP_Pic_Flag: Packed_Bit := 0;
        Used_For_Long_Term_Reference: Packed_Bit := 0;
        Discardable_Flag: Packed_Bit := 0;
        Cross_Layer_BLA_Flag: Packed_Bit := 0;
        Pic_Output_Flag: Packed_Bit := 0;
        No_Output_Of_Prior_Pics_Flag: Packed_Bit := 0;
        Short_Term_Ref_Pic_Set_SPS_Flag: Packed_Bit := 0;
        Slice_Temporal_MVP_Enabled_Flag: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Picture_Info_Flags use
    record
        Is_Reference at 0 range 0 .. 0;
        IRAP_Pic_Flag at 0 range 1 .. 1;
        Used_For_Long_Term_Reference at 0 range 2 .. 2;
        Discardable_Flag at 0 range 3 .. 3;
        Cross_Layer_BLA_Flag at 0 range 4 .. 4;
        Pic_Output_Flag at 0 range 5 .. 5;
        No_Output_Of_Prior_Pics_Flag at 0 range 6 .. 6;
        Short_Term_Ref_Pic_Set_SPS_Flag at 0 range 7 .. 7;
        Slice_Temporal_MVP_Enabled_Flag at 0 range 8 .. 8;
        Reserved at 0 range 9 .. 31;
    end record;

    type Long_Term_Ref_Pics_Array is
        array (1 .. Max_Long_Term_Ref_Pics_SPS) of Interfaces.Unsigned_8
        with Convention => C;

    type Long_Term_Pics_Array is array (1 .. Max_Long_Term_Pics_SPS)
        of Interfaces.Unsigned_8
        with Convention => C;

    type Delta_POC_Present_Array is array (1 .. Max_Delta_POC)
        of Interfaces.Unsigned_8
        with Convention => C;

    type Delta_POC_Cycle_Array is array (1 .. Max_Delta_POC)
        of Interfaces.Unsigned_8
        with Convention => C;

    type Long_Term_Ref_Pics is
    record
        Num_Long_Term_SPS: Interfaces.Unsigned_8;
        Num_Long_Term_Pics: Interfaces.Unsigned_8;
        LT_IDX_SPS: Long_Term_Ref_Pics_Array;
        POC_LSB_LT: Long_Term_Pics_Array;
        Used_By_Curr_Pic_LT_Flag: Interfaces.Unsigned_16;
        Delta_POC_MSB_Present_Flag: Delta_POC_Present_Array;
        Delta_POC_MSB_Cycle_LT: Delta_POC_Cycle_Array;
    end record
        with Convention => C;

    type Long_Term_Ref_Pics_Access is access constant Long_Term_Ref_Pics
        with Convention => C;

    type Picture_Info is
    record
        Flags: Picture_Info_Flags;
        Pic_Type: Picture_Type;
        SPS_Video_Parameter_Set_ID: Interfaces.Unsigned_8;
        PPS_Seq_Parameter_Set_ID: Interfaces.Unsigned_8;
        PPS_Pic_Parameter_Set_ID: Interfaces.Unsigned_8;
        Short_Term_Ref_Pic_Set_IDX: Interfaces.Unsigned_8;
        Pic_Order_Cnt_Val: Interfaces.Integer_32;
        Temporal_ID: Interfaces.Unsigned_8;
        Reserved_1: Reserved_Array(1 .. 7);
        Ref_Lists: Reference_Lists_Info_Access;
        Short_Term_Ref_Pic_Set: Short_Term_Ref_Pic_Set_Access;
        Long_Term_Ref_Pics: Long_Term_Ref_Pics_Access;
    end record
        with Convention => C;

    type Picture_Info_Access is access constant Picture_Info
        with Convention => C,
             Storage_Size => 0;

    type Reference_Info_Flags is
    record
        Used_For_Long_Term_Reference: Packed_Bit := 0;
        Unused_For_Reference: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Reference_Info_Flags use
    record
        Used_For_Long_Term_Reference at 0 range 0 .. 0;
        Unused_For_Reference at 0 range 1 .. 1;
        Reserved at 0 range 2 .. 31;
    end record;

    type Reference_Info is
    record
        Flags: Reference_Info_Flags;
        Pic_Type: Picture_Type;
        Pic_Order_Cnt_Val: Interfaces.Integer_32;
        Temporal_ID: Interfaces.Unsigned_8;
    end record
        with Convention => C;

    type Reference_Info_Access is access constant Reference_Info
        with Convention => C,
             Storage_Size => 0;
end Vulkan.Extensions.Std_Video.H265.Encode;


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

-- Common constants and types for H264 encode extensions

package Vulkan.Extensions.Std_Video.H264.Encode is
    -- Records.
    type Weight_Table_Flags is
    record
        Luma_Weight_L0_Flag: Interfaces.Unsigned_32;
        Chroma_Weight_L0_Flag: Interfaces.Unsigned_32;
        Luma_Weight_L1_Flag: Interfaces.Unsigned_32;
        Chroma_Weight_L1_Flag: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Luma_Weight_Array is array (1 .. Max_Num_List_Ref)
        of Interfaces.Integer_8
        with Convention => C;

    type Luma_Offset_Array is array (1 .. Max_Num_List_Ref)
        of Interfaces.Integer_8
        with Convention => C;

    type Chroma_Weight_Array is array(1 .. Max_Num_List_Ref,
                                      1 .. Max_Chroma_Planes)
        of Interfaces.Integer_8
        with Convention => C;

    type Chroma_Offset_Array is array(1 .. Max_Num_List_Ref,
                                      1 .. Max_Chroma_Planes)
        of Interfaces.Integer_8
        with Convention => C;

    type Weight_Table is
    record
        Flags: Weight_Table_Flags;
        Luma_Log2_Weight_Denom: Interfaces.Unsigned_8;
        Chroma_Log2_Weight_Denom: Interfaces.Unsigned_8;
        Luma_Weight_L0: Luma_Weight_Array;
        Luma_Offset_L0: Luma_Offset_Array;
        Chroma_Weight_L0: Chroma_Weight_Array;
        Chroma_Offset_L0: Chroma_Offset_Array;
        Luma_Weight_L1: Luma_Weight_Array;
        Luma_Offset_L1: Luma_Offset_Array;
        Chroma_Weight_L1: Chroma_Weight_Array;
        Chroma_Offset_L1: Chroma_Weight_Array;
    end record
        with Convention => C;

    type Weight_Table_Access is access constant Weight_Table
        with Convention => C,
             Storage_Size => 0;

    type Slice_Header_Flags is
    record
        Direct_Spatial_MV_Pred_Flag: Packed_Bit := 0;
        Num_Ref_IDX_Active_Override_Flag: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Slice_Header_Flags use
    record
        Direct_Spatial_MV_Pred_Flag at 0 range 0 .. 0;
        Num_Ref_IDX_Active_Override_Flag at 0 range 1 .. 1;
        Reserved at 0 range 2 .. 31;
    end record;

    type Picture_Info_Flags is
    record
        IDR_Pic_Flag: Packed_Bit := 0;
        Is_Reference: Packed_Bit := 0;
        No_Output_Of_Prior_Pics_Flag: Packed_Bit := 0;
        Long_Term_Reference_Flag: Packed_Bit := 0;
        Adaptive_Ref_Pic_Marking_Mode_Flag: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Picture_Info_Flags use
    record
        IDR_Pic_Flag at 0 range 0 .. 0;
        Is_Reference at 0 range 1 .. 1;
        No_Output_Of_Prior_Pics_Flag at 0 range 2 .. 2;
        Long_Term_Reference_Flag at 0 range 3 .. 3;
        Adaptive_Ref_Pic_Marking_Mode_Flag at 0 range 4 .. 4;
        Reserved at 0 range 5 .. 31;
    end record;

    type Reference_Info_Flags is
    record
        Used_For_Long_Term_Reference: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Reference_Info_Flags use
    record
        Used_For_Long_Term_Reference at 0 range 0 .. 0;
        Reserved at 0 range 1 .. 31;
    end record;

    type Reference_Lists_Info_Flags is
    record
        Ref_Pic_List_Modification_Flag_L0: Packed_Bit := 0;
        Ref_Pic_List_Modification_Flag_L1: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Reference_Lists_Info_Flags use
    record
        Ref_Pic_List_Modification_Flag_L0 at 0 range 0 .. 0;
        Ref_Pic_List_Modification_Flag_L1 at 0 range 1 .. 1;
        Reserved at 0 range 2 .. 31;
    end record;

    type Ref_List_Mod_Entry is
    record
        Modification_Of_Pic_Nums_IDC: H264.Modification_Of_Pic_Nums_IDC;
        Abs_Diff_Pic_Num_Minus_1: Interfaces.Unsigned_16;
        Long_Term_Pic_Num: Interfaces.Unsigned_16;
    end record
        with Convention => C;

    type Ref_List_Mod_Entry_Access is access constant Ref_List_Mod_Entry
        with Convention => C,
             Storage_Size => 0;

    type Ref_Pic_Marking_Entry is
    record
        Memory_Mangement_Control_Operations: Mem_Mgmt_Control_Op;
        Difference_Of_Pic_Nums_Minus_1: Interfaces.Unsigned_16;
        Long_Term_Pic_Num: Interfaces.Unsigned_16;
        Long_Term_Frame_IDX: Interfaces.Unsigned_16;
        Max_Long_Term_Frame_IDX_Plus_1: Interfaces.Unsigned_16;
    end record
        with Convention => C;

    type Ref_Pic_Marking_Entry_Access is access constant Ref_Pic_Marking_Entry
        with Convention => C,
             Storage_Size => 0;

    type Ref_Pic_List is array (1 .. Max_Num_List_Ref) of Interfaces.Unsigned_8
        with Convention => C;

    type Reference_Lists_Info is
    record
        Flags: Reference_Lists_Info_Flags;
        Num_Ref_IDX_L0_Active_Minus_1: Interfaces.Unsigned_8;
        Num_Ref_IDX_L1_Active_Minus_1: Interfaces.Unsigned_8;
        Ref_Pic_List0: Ref_Pic_List;
        Ref_Pic_List1: Ref_Pic_List;
        Ref_List0_Mod_Op_Count: Interfaces.Unsigned_8;
        Ref_List1_Mod_Op_Count: Interfaces.Unsigned_8;
        Ref_Pic_Marking_Op_Count: Interfaces.Unsigned_8;
        Reserved_1: Reserved_Array(1 .. 7);
        Ref_List0_Mod_Operations: Ref_List_Mod_Entry_Access;
        Ref_List1_Mod_Operations: Ref_List_Mod_Entry_Access;
        Ref_Pic_Marking_Operations: Ref_Pic_Marking_Entry_Access;
    end record
        with Convention => C;

    type Reference_Lists_Info_Access is access constant Reference_Lists_Info
        with Convention => C,
             Storage_Size => 0;

    type Picture_Info is
    record
        Flags: Picture_Info_Flags;
        Seq_Parameter_Set_ID: Interfaces.Unsigned_8;
        Pic_Parameter_Set_ID: Interfaces.Unsigned_8;
        IDR_Pic_ID: Interfaces.Unsigned_16;
        Primary_Pic_Type: Picture_Type;
        Frame_Num: Interfaces.Unsigned_32;
        Pic_Order_Cnt: Interfaces.Integer_32;
        Temporal_ID: Interfaces.Unsigned_8;
        Reserved: Reserved_Array(1 .. 3);
        Ref_Lists: Reference_Lists_Info_Access;
    end record
        with Convention => C;
    
    type Picture_Info_Access is access constant Picture_Info
        with Convention => C,
             Storage_Size => 0;

    type Reference_Info is
    record
        Flags: Reference_Info_Flags;
        Primary_Pic_Type: Picture_Type;
        Frame_Num: Interfaces.Unsigned_32;
        Pic_Order_Cnt: Interfaces.Integer_32;
        Long_Term_Pic_Num: Interfaces.Unsigned_16;
        Long_Term_Frame_IDX: Interfaces.Unsigned_16;
        Temporal_ID: Interfaces.Unsigned_8;
    end record
        with Convention => C;

    type Reference_Info_Access is access constant Reference_Info
        with Convention => C,
             Storage_Size => 0;

    type Slice_Header is
    record
        Flags: Slice_Header_Flags;
        First_MB_In_Slice: Interfaces.Unsigned_32;
        Slice_Type: H264.Slice_Type;
        Slice_Alpha_C0_Offset_Div_2: Interfaces.Integer_8;
        Slice_Beta_Offset_Div_2: Interfaces.Integer_8;
        Slice_QP_Delta: Interfaces.Integer_8;
        Reserved_1: Interfaces.Unsigned_8;
        Cabac_Init_IDC: H264.Cabac_Init_IDC;
        Disable_Deblocking_Filter_IDC: H264.Disable_Deblocking_Filter_IDC;
        Weight_Table: Weight_Table_Access;
    end record
        with Convention => C;

    type Slice_Header_Access is access constant Slice_Header
        with Convention => C,
             Storage_Size => 0;
end Vulkan.Extensions.Std_Video.H264.Encode;


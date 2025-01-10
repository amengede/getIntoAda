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

-- Common constants and type for AV1 decode extensions

package Vulkan.Extensions.Std_Video.AV1.Decode is
    -- Records.
    type Picture_Info_Flags is
    record
        Error_Resilient_Mode: Packed_Bit := 0;
        Disable_CDF_Update: Packed_Bit := 0;
        Use_Superres: Packed_Bit := 0;
        Render_And_Frame_Size_Different: Packed_Bit := 0;
        Allow_Screen_Content_Tools: Packed_Bit := 0;
        Is_Filter_Switchable: Packed_Bit := 0;
        Force_Integer_MV: Packed_Bit := 0;
        Frame_Size_Override_Flag: Packed_Bit := 0;
        Buffer_Removal_Time_Present_Flag: Packed_Bit := 0;
        Allow_IntraBC: Packed_Bit := 0;
        Frame_Refs_Short_Signaling: Packed_Bit := 0;
        Allow_High_Precision_MV: Packed_Bit := 0;
        Is_Motion_Mode_Switchable: Packed_Bit := 0;
        Use_Ref_Frame_MVS: Packed_Bit := 0;
        Disable_Frame_End_Update_CDF: Packed_Bit := 0;
        Allow_Warped_Motion: Packed_Bit := 0;
        Reduced_Tx_Set: Packed_Bit := 0;
        Reference_Select: Packed_Bit := 0;
        Skip_Mode_Present: Packed_Bit := 0;
        Delta_Q_Present: Packed_Bit := 0;
        Delta_LF_Present: Packed_Bit := 0;
        Delta_LF_Multi: Packed_Bit := 0;
        Segmentation_Enabled: Packed_Bit := 0;
        Segmentation_Update_Map: Packed_Bit := 0;
        Segmentation_Temporal_Update: Packed_Bit := 0;
        Segmentation_Update_Data: Packed_Bit := 0;
        Uses_LR: Packed_Bit := 0;
        Uses_Chroma_LR: Packed_Bit := 0;
        Apply_Grain: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Picture_Info_Flags use
    record
        Error_Resilient_Mode at 0 range 0 .. 0;
        Disable_CDF_Update at 0 range 1 .. 1;
        Use_Superres at 0 range 2 .. 2;
        Render_And_Frame_Size_Different at 0 range 3 .. 3;
        Allow_Screen_Content_Tools at 0 range 4 .. 4;
        Is_Filter_Switchable at 0 range 5 .. 5;
        Force_Integer_MV at 0 range 6 .. 6;
        Frame_Size_Override_Flag at 0 range 7 .. 7;
        Buffer_Removal_Time_Present_Flag at 0 range 8 .. 8;
        Allow_IntraBC at 0 range 9 .. 9;
        Frame_Refs_Short_Signaling at 0 range 10 .. 10;
        Allow_High_Precision_MV at 0 range 11 .. 11;
        Is_Motion_Mode_Switchable at 0 range 12 .. 12;
        Use_Ref_Frame_MVS at 0 range 13 .. 13;
        Disable_Frame_End_Update_CDF at 0 range 14 .. 14;
        Allow_Warped_Motion at 0 range 15 .. 15;
        Reduced_Tx_Set at 0 range 16 .. 16;
        Reference_Select at 0 range 17 .. 17;
        Skip_Mode_Present at 0 range 18 .. 18;
        Delta_Q_Present at 0 range 19 .. 19;
        Delta_LF_Present at 0 range 20 .. 20;
        Delta_LF_Multi at 0 range 21 .. 21;
        Segmentation_Enabled at 0 range 22 .. 22;
        Segmentation_Update_Map at 0 range 23 .. 23;
        Segmentation_Temporal_Update at 0 range 24 .. 24;
        Segmentation_Update_Data at 0 range 25 .. 25;
        Uses_LR at 0 range 26 .. 26;
        Uses_Chroma_LR at 0 range 27 .. 27;
        Apply_Grain at 0 range 28 .. 28;
        Reserved at 0 range 29 .. 31;
    end record;

    type Skip_Mode_Frames_Array is array (1 .. Skip_Mode_Frames)
        of Interfaces.Unsigned_8
        with Convention => C;

    type Expected_Frames_Array is array (1 .. Num_Ref_Frames)
        of Interfaces.Unsigned_32
        with Convention => C;

    type Picture_Info is
    record
        Flags: Picture_Info_Flags;
        Frame_Type: AV1.Frame_Type;
        Current_Frame_ID: Interfaces.Unsigned_32;
        Order_Hint: Interfaces.Unsigned_8;
        Primary_Ref_Frame: Interfaces.Unsigned_8;
        Refresh_Frame_Flags: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_8;
        Interpolation_Filter: AV1.Interpolation_Filter;
        Tx_Mode: AV1.Tx_Mode;
        Delta_Q_Res: Interfaces.Unsigned_8;
        Delta_LF_Res: Interfaces.Unsigned_8;
        Skip_Mode_Frame: Skip_Mode_Frames_Array;
        Coded_Denom: Interfaces.Unsigned_8;
        Reserved_2: Reserved_Array(1 .. 3);
        Order_Hints: Ref_Frames_Array;
        Expected_Frame_ID: Expected_Frames_Array;
        Tile_Info: Tile_Info_Access;
        Quantization: Quantization_Access;
        Segmentation: Segmentation_Access;
        Loop_Filter: Loop_Filter_Access;
        CDEF: CDEF_Access;
        Loop_Restoration: Loop_Restoration_Access;
        Global_Motion: Global_Motion_Access;
        Film_Grain: Film_Grain_Access;
    end record
        with Convention => C;

    type Picture_Info_Access is access constant Picture_Info
        with Convention => C,
             Storage_Size => 0;

    type Reference_Info_Flags is
    record
        Disable_Frame_End_Update_CDF: Packed_Bit := 0;
        Segmentation_Enabled: Packed_Bit := 0;
        Reserved: Packed_Bit := 0;
    end record
        with Convention => C;

    for Reference_Info_Flags use
    record
        Disable_Frame_End_Update_CDF at 0 range 0 .. 0;
        Segmentation_Enabled at 0 range 1 .. 1;
        Reserved at 0 range 2 .. 31;
    end record;

    type Reference_Info is
    record
        Flags: Reference_Info_Flags;
        Frame_Type: Interfaces.Unsigned_8;
        Ref_Frame_Sign_Bias: Interfaces.Unsigned_8;
        Order_Hint: Interfaces.Unsigned_8;
        Saved_Order_Hints: Ref_Frames_Array;
    end record
        with Convention => C;

    type Reference_Info_Access is access constant Reference_Info
        with Convention => C,
             Storage_Size => 0;
end Vulkan.Extensions.Std_Video.AV1.Decode;


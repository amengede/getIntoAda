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

-- Common constants and types for H265 decode extensions

package Vulkan.Extensions.Std_Video.H265.Decode is
    -- Constants.
    Ref_Pic_Set_List_Size: constant := 8;

    -- Records.
    type Picture_Info_Flags is
    record
        IRAP_Pic_Flag: Packed_Bit := 0;
        IDR_Pic_Flag: Packed_Bit := 0;
        Is_Reference: Packed_Bit := 0;
        Short_Term_Ref_Pic_Set_SPS_Flag: Packed_Bit := 0;
    end record
        with Convention => C;

    for Picture_Info_Flags use
    record
        IRAP_Pic_Flag at 0 range 0 .. 0;
        IDR_Pic_Flag at 0 range 1 .. 1;
        Is_Reference at 0 range 2 .. 2;
        Short_Term_Ref_Pic_Set_SPS_Flag at 0 range 3 .. 3;
    end record;

    type Ref_Pic_Set_List is
        array (1 .. Ref_Pic_Set_List_Size) of Interfaces.Unsigned_8
        with Convention => C;

    type Picture_Info is
    record
        Flags: Picture_Info_Flags;
        SPS_Video_Parameter_Set_ID: Interfaces.Unsigned_8;
        PPS_Seq_Parameter_Set_ID: Interfaces.Unsigned_8;
        PPS_Pic_Parameter_Set_ID: Interfaces.Unsigned_8;
        Num_Delta_POCS_Of_Ref_RPS_Idx: Interfaces.Unsigned_8;
        Pic_Order_Cnt_Val: Interfaces.Integer_32;
        Num_Bits_For_ST_Ref_Pic_Set_In_Slice: Interfaces.Unsigned_16;
        Reserved: Interfaces.Unsigned_16;
        Ref_Pic_Set_Curr_Before: Ref_Pic_Set_List;
        Ref_Pic_Set_Curr_After: Ref_Pic_Set_List;
        Ref_Pic_Set_Lt_Curr: Ref_Pic_Set_List;
    end record
        with Convention => C;

    type Picture_Info_Access is access constant Picture_Info
        with Convention => C,
             Storage_Size => 0;

    type Reference_Info_Flags is
    record
        Used_For_Long_Term_Reference: Packed_Bit := 0;
        Unused_For_Reference: Packed_Bit := 0;
    end record
        with Convention => C;

    for Reference_Info_Flags use
    record
        Used_For_Long_Term_Reference at 0 range 0 .. 0;
        Unused_For_Reference at 0 range 1 .. 1;
    end record;

    type Reference_Info is
    record
        Flags: Reference_Info_Flags;
        Pic_Order_Cnt_Val: Interfaces.Integer_32;
    end record
        with Convention => C;

    type Reference_Info_Access is access constant Reference_Info
        with Convention => C,
             Storage_Size => 0;
end Vulkan.Extensions.Std_Video.H265.Decode;


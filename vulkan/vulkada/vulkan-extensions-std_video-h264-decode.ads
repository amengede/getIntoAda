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

-- Common constants and types for H264 decode extensions

package Vulkan.Extensions.Std_Video.H264.Decode is
    -- Constants.
    Field_Order_Count_List_Size: constant := 2;

    -- Enumerations.
    type Field_Order_Count is (Top,
                               Bottom,
                               Invalid)
        with Convention => C;

    for Field_Order_Count'Size use 32;

    for Field_Order_Count use (Top => 0,
                               Bottom => 1,
                               Invalid => 16#7fffffff#);

    -- Records.
    type Picture_Info_Flags is
    record
        Field_Pic_Flag: Packed_Bit := 0;
        Is_Intra: Packed_Bit := 0;
        Idr_Pic_Flag: Packed_Bit := 0;
        Bottom_Field_Flag: Packed_Bit := 0;
        Is_Reference: Packed_Bit := 0;
        Complementary_Field_Pair: Packed_Bit := 0;
    end record
        with Convention => C;

    for Picture_Info_Flags use
    record
        Field_Pic_Flag at 0 range 0 .. 0;
        Is_Intra at 0 range 1 .. 1;
        Idr_Pic_Flag at 0 range 2 .. 2;
        Bottom_Field_Flag at 0 range 3 .. 3;
        Is_Reference at 0 range 4 .. 4;
        Complementary_Field_Pair at 0 range 5 .. 5;
    end record;

    type Field_Order_Count_List is
        array (1 .. Field_Order_Count_List_Size) of Interfaces.Integer_32
        with Convention => C;

    type Picture_Info is
    record
        Flags: Picture_Info_Flags;
        Seq_Parameter_Set_ID: Interfaces.Unsigned_8;
        Pic_Parameter_Set_ID: Interfaces.Unsigned_8;
        Reserved_1: Interfaces.Unsigned_8;
        Reserved_2: Interfaces.Unsigned_8;
        Frame_Num: Interfaces.Unsigned_16;
        Idr_Pic_ID: Interfaces.Unsigned_16;
        Pic_Order_Count: Field_Order_Count_List;
    end record
        with Convention => C;

    type Picture_Info_Access is access constant Picture_Info
        with Convention => C,
             Storage_Size => 0;

    type Reference_Info_Flags is
    record
        Top_Field_Flag: Packed_Bit := 0;
        Bottom_Field_Flag: Packed_Bit := 0;
        Used_For_Long_Term_Reference: Packed_Bit := 0;
        Is_Non_Existing: Packed_Bit := 0;
    end record
        with Convention => C;

    for Reference_Info_Flags use
    record
        Top_Field_Flag at 0 range 0 .. 0;
        Bottom_Field_Flag at 0 range 1 .. 1;
        Used_For_Long_Term_Reference at 0 range 2 .. 2;
        Is_Non_Existing at 0 range 3 .. 3;
    end record;

    type Reference_Info is
    record
        Flags: Reference_Info_Flags;
        Frame_Num: Interfaces.Unsigned_16;
        Reserved: Interfaces.Unsigned_16;
        Pic_Order_Cnt: Field_Order_Count_List;
    end record
        with Convention => C;

    type Reference_Info_Access is access constant Reference_Info
        with Convention => C,
             Storage_Size => 0;
end Vulkan.Extensions.Std_Video.H264.Decode;


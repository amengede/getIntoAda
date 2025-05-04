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

-- C interface for the video decode queues extension

with Vulkan.C;
with Vulkan.C_Arrays;
with Vulkan.Video_Queues_C;

private package Vulkan.Video_Decode_Queues_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in Video_Decode_Capabilities_Type |
                                              Video_Decode_Usage_Info_Type |
                                              Video_Decode_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Video_Decode_Capabilities_C is
    record
        Record_Type: Out_Structure_Type := Video_Decode_Capabilities_Type;
        Next: C.Out_Structure_C_Access;
        Flags: Video_Decode_Capability_Flags;
    end record
        with Convention => C;

    type Video_Decode_Capabilities_C_Access is
        access Video_Decode_Capabilities_C
        with Convention => C;

    type Video_Decode_Usage_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_Usage_Info_Type;
        Next: C.In_Structure_C_Access;
        Video_Usage_Hints: Video_Decode_Usage_Flags;
    end record
        with Convention => C;

    type Video_Decode_Usage_Info_C_Access is access Video_Decode_Usage_Info_C
        with Convention => C;

    type Video_Decode_Info_C is
    record
        Record_Type: In_Structure_Type := Video_Decode_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Video_Decode_Flags;
        Src_Buffer: Buffer;
        Src_Buffer_Offset: Device_Size;
        Src_Buffer_Range: Device_Size;
        Dst_Picture_Resource: Video_Queues_C.Video_Picture_Resource_Info_C;
        Setup_Reference_Slot: Video_Queues_C.Video_Reference_Slot_Info_C_Access;
        Reference_Slot_Count: Interfaces.Unsigned_32;
        Reference_Slots:
            Video_Queues_C.Video_Reference_Slot_Info_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Video_Decode_Info_C_Access is access Video_Decode_Info_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada(Ada_Struct: in out Video_Decode_Capabilities;
                     C_Struct: in Video_Decode_Capabilities_C);

    function To_C(Struct: in Video_Decode_Usage_Info)
        return Video_Decode_Usage_Info_C;
    procedure Free(Struct: in out Video_Decode_Usage_Info_C);

    function To_C(Struct: in Video_Decode_Info) return Video_Decode_Info_C;
    procedure Free(Struct: in out Video_Decode_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Video_Decode_Queues_C;


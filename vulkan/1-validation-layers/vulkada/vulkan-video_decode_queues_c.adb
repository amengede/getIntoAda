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

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Video_Decode_Queues_C is
    procedure To_Ada(Ada_Struct: in out Video_Decode_Capabilities;
                     C_Struct: in Video_Decode_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Flags := C_Struct.Flags;
    end To_Ada;

    function To_C(Struct: in Video_Decode_Usage_Info)
        return Video_Decode_Usage_Info_C is
        VDUIC: Video_Decode_Usage_Info_C;
    begin
        VDUIC.Next := Extension_Records.To_C(Struct.Next);
        VDUIC.Video_Usage_Hints := Struct.Video_Usage_Hints;

        return VDUIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_Usage_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Video_Decode_Info) return Video_Decode_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Video_Queues_C.Video_Reference_Slot_Info_C_Arrays,
             Video_Reference_Slot_Info_Vectors,
             Video_Queues_C.To_C);

        VDIC: Video_Decode_Info_C;
    begin
        VDIC.Next := Extension_Records.To_C(Struct.Next);
        VDIC.Flags := Struct.Flags;
        VDIC.Src_Buffer := Struct.Src_Buffer;
        VDIC.Src_Buffer_Offset := Struct.Src_Buffer_Offset;
        VDIC.Src_Buffer_Range := Struct.Src_Buffer_Range;
        VDIC.Dst_Picture_Resource :=
            Video_Queues_C.To_C(Struct.Dst_Picture_Resource);
        
        if Struct.Setup_Reference_Slot /= null then
            VDIC.Setup_Reference_Slot :=
                new Video_Queues_C.Video_Reference_Slot_Info_C'
                    (Video_Queues_C.To_C(Struct.Setup_Reference_Slot.all));
        end if;

        To_C_Array(VDIC.Reference_Slot_Count,
                   Struct.Reference_Slots,
                   VDIC.Reference_Slots);

        return VDIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Queues_C.Video_Reference_Slot_Info_C,
             Video_Queues_C.Video_Reference_Slot_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        Free(Struct.Setup_Reference_Slot);
        Video_Queues_C.Video_Reference_Slot_Info_C_Arrays.Free
            (Struct.Reference_Slots, Video_Queues_C.Free'Access);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Video_Decode_Usage_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Decode_Usage_Info,
                         Video_Decode_Usage_Info_C,
                         Video_Decode_Usage_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Video_Decode_Info,
                         Video_Decode_Info_C,
                         Video_Decode_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Video_Decode_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Video_Decode_Capabilities,
                         Video_Decode_Capabilities_C,
                         Video_Decode_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Video_Decode_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Decode_Capabilities_C_Access);
                begin
                    To_Ada(Video_Decode_Capabilities(Ada_Struct),
                           To_Access(Next).all);
                end;
        end case;
    end To_Ada;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case In_Structure(Next.Record_Type) is
            when Video_Decode_Usage_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_Usage_Info_C,
                         Video_Decode_Usage_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_Info_C, Video_Decode_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Video_Decode_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Decode_Capabilities_C,
                         Video_Decode_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Video_Decode_Queues_C;


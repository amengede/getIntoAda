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

-- Operations for the transform feedback extension

with Vulkan.Core;
with Vulkan.Transform_Feedbacks_C;

package body Vulkan.Transform_Feedbacks is
    -- Loaded extension functions.
    type vkCmdBindTransformFeedbackBuffersEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         First_Binding,
                         Binding_Count: in Interfaces.Unsigned_32;
                         Buffers: access constant Buffer;
                         Offsets, Sizes: access constant Device_Size)
        with Convention => C;

    vkCmdBindTransformFeedbackBuffersEXT:
        vkCmdBindTransformFeedbackBuffersEXT_Access;

    type vkCmdBeginTransformFeedbackEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         First_Counter_Buffer,
                         Counter_Buffer_Count: in Interfaces.Unsigned_32;
                         Counter_Buffers: access constant Buffer;
                         Counter_Buffer_Offsets: access constant Device_Size)
        with Convention => C;

    vkCmdBeginTransformFeedbackEXT: vkCmdBeginTransformFeedbackEXT_Access;

    type vkCmdEndTransformFeedbackEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         First_Counter_Buffer,
                         Counter_Buffer_Count: in Interfaces.Unsigned_32;
                         Counter_Buffers: access constant Buffer;
                         Counter_Buffer_Offsets: access constant Device_Size)
        with Convention => C;

    vkCmdEndTransformFeedbackEXT: vkCmdEndTransformFeedbackEXT_Access;

    type vkCmdBeginQueryIndexedEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Query_Pool: in Vulkan.Query_Pool;
                         Query: in Interfaces.Unsigned_32;
                         Flags: in Query_Control_Flags;
                         Index: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdBeginQueryIndexedEXT: vkCmdBeginQueryIndexedEXT_Access;

    type vkCmdEndQueryIndexedEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Query_Pool: in Vulkan.Query_Pool;
                         Query, Index: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdEndQueryIndexedEXT: vkCmdEndQueryIndexedEXT_Access;

    type vkCmdDrawIndirectByteCountEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Instance_Count,
                         First_Instance: in Interfaces.Unsigned_32;
                         Counter_Buffer: in Vulkan.Buffer;
                         Counter_Buffer_Offset: in Device_Size;
                         Counter_Offset,
                         Vertex_Stride: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdDrawIndirectByteCountEXT: vkCmdDrawIndirectByteCountEXT_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is
            new Load_Pointer(vkCmdBindTransformFeedbackBuffersEXT_Access);
        procedure Load is
            new Load_Pointer(vkCmdBeginTransformFeedbackEXT_Access);
        procedure Load is new Load_Pointer(vkCmdEndTransformFeedbackEXT_Access);
        procedure Load is new Load_Pointer(vkCmdBeginQueryIndexedEXT_Access);
        procedure Load is new Load_Pointer(vkCmdEndQueryIndexedEXT_Access);
        procedure Load is
            new Load_Pointer(vkCmdDrawIndirectByteCountEXT_Access);
    begin
        Load(vkCmdBindTransformFeedbackBuffersEXT,
             "vkCmdBindTransformFeedbackBuffersEXT");
        Load(vkCmdBeginTransformFeedbackEXT, "vkCmdBeginTransformFeedbackEXT");
        Load(vkCmdEndTransformFeedbackEXT, "vkCmdEndTransformFeedbackEXT");
        Load(vkCmdBeginQueryIndexedEXT, "vkCmdBeginQueryIndexedEXT");
        Load(vkCmdEndQueryIndexedEXT, "vkCmdEndQueryIndexedEXT");
        Load(vkCmdDrawIndirectByteCountEXT, "vkCmdDrawIndirectByteCountEXT");
    end Load_Extension;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffers: in Buffer_Vectors.Vector;
                   Offsets, Sizes: in Device_Size_Vectors.Vector) is
        Count: Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Buffers.Length);
        Buffers_C: array (1 .. Positive(Count)) of aliased Buffer
            with Convention => C;
        Offsets_C: array (1 .. Positive(Count)) of aliased Device_Size
            with Convention => C;
        Sizes_C: array (1 .. Positive(Count)) of aliased Device_Size
            with Convention => C;
    begin
        for X in Buffers_C'Range loop
            Buffers_C(X) := Buffers(X);
            Offsets_C(X) := Offsets(X);
            Sizes_C(X) := Sizes(X);
        end loop;

        vkCmdBindTransformFeedbackBuffersEXT(Command_Buffer,
                                             First_Binding,
                                             Count,
                                             Buffers_C(1)'Access,
                                             Offsets_C(1)'Access,
                                             Sizes_C(1)'Access);
    end Bind;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffers: in Buffer_Vectors.Vector;
                   Offsets: in Device_Size_Vectors.Vector) is
        Count: Interfaces.Unsigned_32 := Interfaces.Unsigned_32(Buffers.Length);
        Buffers_C: array (1 .. Positive(Count)) of aliased Buffer
            with Convention => C;
        Offsets_C: array (1 .. Positive(Count)) of aliased Device_Size
            with Convention => C;
    begin
        for X in Buffers_C'Range loop
            Buffers_C(X) := Buffers(X);
            Offsets_C(X) := Offsets(X);
        end loop;

        vkCmdBindTransformFeedbackBuffersEXT(Command_Buffer,
                                             First_Binding,
                                             Count,
                                             Buffers_C(1)'Access,
                                             Offsets_C(1)'Access,
                                             null);
    end Bind;

    procedure Begin_Transform_Feedback
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Counter_Buffer: in Interfaces.Unsigned_32;
         Counter_Buffers: in Buffer_Vectors.Vector :=
             Buffer_Vectors.Empty_Vector;
         Counter_Buffer_Offsets: in Device_Size_Vectors.Vector :=
             Device_Size_Vectors.Empty_Vector) is
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Ada.Containers.Count_Type'Max
                (Counter_Buffers.Length, Counter_Buffer_Offsets.Length));
        Buffers_C: array (1 .. Natural(Count)) of aliased Buffer
            with Convention => C;
        Offsets_C: array (1 .. Natural(Count)) of aliased Device_Size
            with Convention => C;
        First_Buffer: access Buffer;
        First_Offset: access Device_Size;
    begin
        if not Counter_Buffers.Is_Empty then
            First_Buffer := Buffers_C(1)'Access;

            for X in Buffers_C'Range loop
                Buffers_C(X) := Counter_Buffers(X);
            end loop;
        end if;

        if not Counter_Buffer_Offsets.Is_Empty then
            First_Offset := Offsets_C(1)'Access;

            for X in Offsets_C'Range loop
                Offsets_C(X) := Counter_Buffer_Offsets(X);
            end loop;
        end if;

        vkCmdBeginTransformFeedbackEXT(Command_Buffer,
                                       First_Counter_Buffer,
                                       Count,
                                       First_Buffer,
                                       First_Offset);
    end Begin_Transform_Feedback;

    procedure End_Transform_Feedback
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Counter_Buffer: in Interfaces.Unsigned_32;
         Counter_Buffers: in Buffer_Vectors.Vector :=
             Buffer_Vectors.Empty_Vector;
         Counter_Buffer_Offsets: in Device_Size_Vectors.Vector :=
             Device_Size_Vectors.Empty_Vector) is
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Ada.Containers.Count_Type'Max
                (Counter_Buffers.Length, Counter_Buffer_Offsets.Length));
        Buffers_C: array (1 .. Natural(Count)) of aliased Buffer
            with Convention => C;
        Offsets_C: array (1 .. Natural(Count)) of aliased Device_Size
            with Convention => C;
        First_Buffer: access Buffer;
        First_Offset: access Device_Size;
    begin
        if not Counter_Buffers.Is_Empty then
            First_Buffer := Buffers_C(1)'Access;

            for X in Buffers_C'Range loop
                Buffers_C(X) := Counter_Buffers(X);
            end loop;
        end if;

        if not Counter_Buffer_Offsets.Is_Empty then
            First_Offset := Offsets_C(1)'Access;

            for X in Offsets_C'Range loop
                Offsets_C(X) := Counter_Buffer_Offsets(X);
            end loop;
        end if;

        vkCmdEndTransformFeedbackEXT(Command_Buffer,
                                     First_Counter_Buffer,
                                     Count,
                                     First_Buffer,
                                     First_Offset);
    end End_Transform_Feedback;

    procedure Begin_Query_Indexed(Command_Buffer: in Vulkan.Command_Buffer;
                                  Query_Pool: in Vulkan.Query_Pool;
                                  Query: in Interfaces.Unsigned_32;
                                  Flags: in Query_Control_Flags;
                                  Index: in Interfaces.Unsigned_32) is
    begin
        vkCmdBeginQueryIndexedEXT(Command_Buffer,
                                  Query_Pool,
                                  Query,
                                  Flags,
                                  Index);
    end Begin_Query_Indexed;

    procedure End_Query_Indexed(Command_Buffer: in Vulkan.Command_Buffer;
                                Query_Pool: in Vulkan.Query_Pool;
                                Query, Index: in Interfaces.Unsigned_32) is
    begin
        vkCmdEndQueryIndexedEXT(Command_Buffer, Query_Pool, Query, Index);
    end End_Query_Indexed;

    procedure Draw_Indirect_Byte_Count
        (Command_Buffer: in Vulkan.Command_Buffer;
         Instance_Count, First_Instance: in Interfaces.Unsigned_32;
         Counter_Buffer: in Vulkan.Buffer;
         Counter_Buffer_Offset: in Device_Size;
         Counter_Offset, Vertex_Stride: in Interfaces.Unsigned_32) is
    begin
        vkCmdDrawIndirectByteCountEXT(Command_Buffer,
                                      Instance_Count,
                                      First_Instance,
                                      Counter_Buffer,
                                      Counter_Buffer_Offset,
                                      Counter_Offset,
                                      Vertex_Stride);
    end Draw_Indirect_Byte_Count;
end Vulkan.Transform_Feedbacks;


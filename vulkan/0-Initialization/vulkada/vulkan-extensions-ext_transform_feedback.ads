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

package Vulkan.Extensions.EXT_Transform_Feedback is
    use type Ada.Containers.Count_Type;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdBindTransformFeedbackBuffersEXT
    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffers: in Buffer_Vectors.Vector;
                   Offsets, Sizes: in Device_Size_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Buffers.Length = Offsets.Length and
                    Buffers.Length = Sizes.Length and
                    not Buffers.Is_Empty and
                    (for all Offset of Offsets => Offset rem 4 = 0);

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffers: in Buffer_Vectors.Vector;
                   Offsets: in Device_Size_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Buffers.Length = Offsets.Length and
                    not Buffers.Is_Empty and
                    (for all Offset of Offsets => Offset rem 4 = 0);

    -- vkCmdBeginTransformFeedbackEXT
    procedure Begin_Transform_Feedback
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Counter_Buffer: in Interfaces.Unsigned_32;
         Counter_Buffers: in Buffer_Vectors.Vector :=
             Buffer_Vectors.Empty_Vector;
         Counter_Buffer_Offsets: in Device_Size_Vectors.Vector :=
             Device_Size_Vectors.Empty_Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
             (if not Counter_Buffers.Is_Empty and
                 not Counter_Buffer_Offsets.Is_Empty then
                Counter_Buffers.Length = Counter_Buffer_Offsets.Length);

    -- vkCmdEndTransformFeedbackEXT
    procedure End_Transform_Feedback
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Counter_Buffer: in Interfaces.Unsigned_32;
         Counter_Buffers: in Buffer_Vectors.Vector :=
             Buffer_Vectors.Empty_Vector;
         Counter_Buffer_Offsets: in Device_Size_Vectors.Vector :=
             Device_Size_Vectors.Empty_Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
             (if not Counter_Buffers.Is_Empty and
                 not Counter_Buffer_Offsets.Is_Empty then
                Counter_Buffers.Length = Counter_Buffer_Offsets.Length);

    -- vkCmdBeginQueryIndexedEXT
    procedure Begin_Query_Indexed(Command_Buffer: in Vulkan.Command_Buffer;
                                  Query_Pool: in Vulkan.Query_Pool;
                                  Query: in Interfaces.Unsigned_32;
                                  Flags: in Query_Control_Flags;
                                  Index: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Query_Pool /= No_Query_Pool;

    -- vkCmdEndQueryIndexedEXT
    procedure End_Query_Indexed(Command_Buffer: in Vulkan.Command_Buffer;
                                Query_Pool: in Vulkan.Query_Pool;
                                Query, Index: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Query_Pool /= No_Query_Pool;

    -- vkCmdDrawIndirectByteCountEXT
    procedure Draw_Indirect_Byte_Count
        (Command_Buffer: in Vulkan.Command_Buffer;
         Instance_Count, First_Instance: in Interfaces.Unsigned_32;
         Counter_Buffer: in Vulkan.Buffer;
         Counter_Buffer_Offset: in Device_Size;
         Counter_Offset, Vertex_Stride: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Counter_BUffer /= No_Buffer;
end Vulkan.Extensions.EXT_Transform_Feedback;


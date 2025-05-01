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

-- Command subprograms

with Interfaces.C.Pointers;

private with Vulkan.C;
private with Vulkan.C_V1_2;
private with Vulkan.C_V1_3;
private with Vulkan.Utilities;

package Vulkan.Commands is
    use type Ada.Containers.Count_Type;
    use type System.Address;
    use type Interfaces.Unsigned_32;

    -- vkCmdBindPipeline
    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
                   Pipeline: in Vulkan.Pipeline)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Pipeline /= No_Pipeline;

    -- vkCmdSetViewport
    procedure Set_Viewport(Command_Buffer: in Vulkan.Command_Buffer;
                           First_Viewport: in Interfaces.Unsigned_32;
                           Viewports: in Viewport_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Viewports.Is_Empty;

    procedure Set_Viewport(Command_Buffer: in Vulkan.Command_Buffer;
                           First_Viewport: in Interfaces.Unsigned_32;
                           Viewport: in Vulkan.Viewport)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetScissor
    procedure Set_Scissor(Command_Buffer: in Vulkan.Command_Buffer;
                          First_Scissor: in Interfaces.Unsigned_32;
                          Scissors: in Rect_2D_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Scissors.Is_Empty;

    procedure Set_Scissor(Command_Buffer: in Vulkan.Command_Buffer;
                          First_Scissor: in Interfaces.Unsigned_32;
                          Scissor: in Rect_2D)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetLineWidth
    procedure Set_Line_Width(Command_Buffer: in Vulkan.Command_Buffer;
                             Line_Width: in Interfaces.C.C_Float)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetDepthBias
    procedure Set_Depth_Bias(Command_Buffer: in Vulkan.Command_Buffer;
                             Depth_Bias_Constant_Factor,
                             Depth_Bias_Clamp,
                             Depth_Bias_Slope_Factor:
                              in Interfaces.C.C_Float)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetBlendConstants
    procedure Set_Blend_Constants(Command_Buffer: in Vulkan.Command_Buffer;
                                  Blend_Constants: in Blend_Constants_Array)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetDepthBounds
    procedure Set_Depth_Bounds(Command_Buffer: in Vulkan.Command_Buffer;
                               Min_Depth_Bounds,
                               Max_Depth_Bounds: in Interfaces.C.C_Float)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetStencilCompareMask
    procedure Set_Stencil_Compare_Mask(Command_Buffer: in Vulkan.Command_Buffer;
                                       Face_Mask: in Stencil_Face_Flags;
                                       Compare_Mask: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Face_Mask /= Stencil_Face_No_Bit;

    -- vkCmdSetStencilWriteMask
    procedure Set_Stencil_Write_Mask(Command_Buffer: in Vulkan.Command_Buffer;
                                     Face_Mask: in Stencil_Face_Flags;
                                     Write_Mask: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Face_Mask /= Stencil_Face_No_Bit;
    
    -- vkCmdSetStencilReference
    procedure Set_Stencil_Reference(Command_Buffer: in Vulkan.Command_Buffer;
                                    Face_Mask: in Stencil_Face_Flags;
                                    Reference: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Face_Mask /= Stencil_Face_No_Bit;

    -- vkCmdBindDescriptorSets
    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   First_Set: in Interfaces.Unsigned_32;
                   Descriptor_Sets: in Descriptor_Set_Vectors.Vector;
                   Dynamic_Offsets: in Unsigned_32_Vectors.Vector
                    := Unsigned_32_Vectors.Empty_Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Layout /= No_Pipeline_Layout and
                    not Descriptor_Sets.Is_Empty and
                    (for all Set of Descriptor_Sets => Set /= No_Descriptor_Set);

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   First_Set: in Interfaces.Unsigned_32;
                   Descriptor_Set: in Vulkan.Descriptor_Set;
                   Dynamic_Offset: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Layout /= No_Pipeline_Layout and
                    Descriptor_Set /= No_Descriptor_Set;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   First_Set: in Interfaces.Unsigned_32;
                   Descriptor_Set: in Vulkan.Descriptor_Set)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Layout /= No_Pipeline_Layout and
                    Descriptor_Set /= No_Descriptor_Set;

    -- vkCmdBindIndexBuffer
    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Buffer: in Vulkan.Buffer;
                   Offset: in Device_Size;
                   Index_Type: in Vulkan.Index_Type)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Buffer /= No_Buffer;

    -- vkCmdBindVertexBuffers
    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffers: in Buffer_Vectors.Vector;
                   Offsets: in Device_Size_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Buffers.Length = Offsets.Length;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffer: in Vulkan.Buffer;
                   Offset: in Device_Size := 0)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Buffer /= No_Buffer;

    -- vkCmdDraw
    procedure Draw(Command_Buffer: in Vulkan.Command_Buffer;
                   Vertex_Count,
                   Instance_Count,
                   First_Vertex,
                   First_Instance: Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdDrawIndexed
    procedure Draw_Indexed(Command_Buffer: in Vulkan.Command_Buffer;
                           Index_Count,
                           Instance_Count,
                           First_Index: in Interfaces.Unsigned_32;
                           Vertex_Offset: in Interfaces.Integer_32;
                           First_Instance: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdDrawIndirect
    procedure Draw_Indirect(Command_Buffer: in Vulkan.Command_Buffer;
                            Buffer: in Vulkan.Buffer;
                            Offset: in Device_Size;
                            Draw_Count, Stride: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Buffer /= No_Buffer;

    -- vkCmdDrawIndexedIndirect
    procedure Draw_Indexed_Indirect(Command_Buffer: in Vulkan.Command_Buffer;
                                    Buffer: in Vulkan.Buffer;
                                    Offset: in Device_Size;
                                    Draw_Count,
                                    Stride: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Buffer /= No_Buffer;

    -- vkCmdDispatch
    procedure Dispatch(Command_Buffer: in Vulkan.Command_Buffer;
                       Group_Count_X,
                       Group_Count_Y,
                       Group_Count_Z: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdDispatchIndirect
    procedure Dispatch_Indirect(Command_Buffer: in Vulkan.Command_Buffer;
                                Buffer: in Vulkan.Buffer;
                                Offset: in Device_Size)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Buffer /= No_Buffer;

    -- vkCmdCopyBuffer
    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Buffer, Dst_Buffer: in Buffer;
                   Regions: in Buffer_Copy_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Buffer /= No_Buffer and
                    Dst_Buffer /= No_Buffer and
                    not Regions.Is_Empty;

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Buffer, Dst_Buffer: in Buffer;
                   Region: in Buffer_Copy)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Buffer /= No_Buffer and
                    Dst_Buffer /= No_Buffer;

    -- vkCmdCopyImage
    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Regions: in Image_Copy_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Image /= No_Image and
                    Dst_Image /= No_Image and
                    not Regions.Is_Empty and
                    (for all Region of Regions =>
                        Region.Src_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Src_Subresource.Layer_Count > 0 and
                        Region.Dst_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Dst_Subresource.Layer_Count > 0 and
                        Region.Extent.Width > 0 and
                        Region.Extent.Height > 0 and
                        Region.Extent.Depth > 0);

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Region: in Image_Copy)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Image /= No_Image and
                    Dst_Image /= No_Image and
                    Region.Src_Subresource.Aspect_Mask /=
                    Image_Aspect_No_Bit and
                    Region.Src_Subresource.Layer_Count > 0 and
                    Region.Dst_Subresource.Aspect_Mask /=
                    Image_Aspect_No_Bit and
                    Region.Dst_Subresource.Layer_Count > 0 and
                    Region.Extent.Width > 0 and
                    Region.Extent.Height > 0 and
                    Region.Extent.Depth > 0;

    -- vkCmdBlitImage
    procedure Blit(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Regions: in Image_Blit_Vectors.Vector;
                   Filter: in Vulkan.Filter)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Image /= No_Image and
                    Dst_Image /= No_Image and
                    not Regions.Is_Empty and
                    (for all Region of Regions =>
                        Region.Src_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Src_Subresource.Aspect_Mask =
                        Region.Dst_Subresource.Aspect_Mask);

    procedure Blit(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Region: in Image_Blit;
                   Filter: in Vulkan.Filter)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Image /= No_Image and
                    Dst_Image /= No_Image and
                    Region.Src_Subresource.Aspect_Mask /=
                    Image_Aspect_No_Bit and
                    Region.Src_Subresource.Aspect_Mask =
                    Region.Dst_Subresource.Aspect_Mask;

    -- vkCmdCopyBufferToImage
    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Buffer: in Buffer;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Regions: in Buffer_Image_Copy_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Buffer /= No_Buffer and
                    Dst_Image /= No_Image and
                    not Regions.Is_Empty and
                    (for all Region of Regions =>
                        Region.Image_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Image_Extent.Width > 0 and
                        Region.Image_Extent.Height > 0 and
                        Region.Image_Extent.Depth > 0);

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Buffer: in Buffer;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Region: in Buffer_Image_Copy)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Buffer /= No_Buffer and
                    Dst_Image /= No_Image and
                    Region.Image_Subresource.Aspect_Mask /=
                    Image_Aspect_No_Bit and
                    Region.Image_Extent.Width > 0 and
                    Region.Image_Extent.Height > 0 and
                    Region.Image_Extent.Depth > 0;

    -- vkCmdCopyImageToBuffer
    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Buffer: in Buffer;
                   Regions: in Buffer_Image_Copy_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Image /= No_Image and
                    Dst_Buffer /= No_Buffer and
                    not Regions.Is_Empty and
                    (for all Region of Regions =>
                        Region.Image_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Image_Extent.Width > 0 and
                        Region.Image_Extent.Height > 0 and
                        Region.Image_Extent.Depth > 0);

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Buffer: in Buffer;
                   Region: in Buffer_Image_Copy)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Image /= No_Image and
                    Dst_Buffer /= No_Buffer and
                    Region.Image_Subresource.Aspect_Mask /=
                    Image_Aspect_No_Bit and
                    Region.Image_Extent.Width > 0 and
                    Region.Image_Extent.Height > 0 and
                    Region.Image_Extent.Depth > 0;


    -- vkCmdUpdateBuffer
    generic
        with package Pointers is new Interfaces.C.Pointers(<>);
    procedure Update_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                            Dst_Buffer: in Buffer;
                            Dst_Offset, Data_Size: in Device_Size;
                            Data: in Pointers.Pointer)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Dst_Buffer /= No_Buffer and
                    Dst_Offset mod 4 = 0 and
                    Data_Size mod 4 = 0 and
                    Data_Size in 1 .. 65536 and
                    Pointers."/="(Data, null);

    -- vkCmdFillBuffer
    procedure Fill_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                          Dst_Buffer: in Buffer;
                          Dst_Offset, Size: in Device_Size;
                          Data: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Dst_Buffer /= No_Buffer and
                    Dst_Offset mod 4 = 0 and
                    (Size mod 4 = 0 or Size = Whole_Size);

    -- vkCmdClearColorImage
    procedure Clear_Color_Image
        (Command_Buffer: in Vulkan.Command_Buffer;
         Image: in Vulkan.Image;
         Image_Layout: in Vulkan.Image_Layout;
         Color: in Clear_Color_Value;
         Ranges: in Image_Subresource_Range_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Image /= No_Image and
                    not Ranges.Is_Empty;

    procedure Clear_Color_Image(Command_Buffer: in Vulkan.Command_Buffer;
                                Image: in Vulkan.Image;
                                Image_Layout: in Vulkan.Image_Layout;
                                Color: in Clear_Color_Value;
                                Clear_Range: in Image_Subresource_Range)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Image /= No_Image;

    -- vkCmdClearDepthStencilImage
    procedure Clear_Depth_Stencil_Image
        (Command_Buffer: in Vulkan.Command_Buffer;
         Image: in Vulkan.Image;
         Image_Layout: in Vulkan.Image_Layout;
         Depth_Stencil: in Clear_Depth_Stencil_Value;
         Ranges: in Image_Subresource_Range_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Image /= No_Image and
                    not Ranges.Is_Empty;

    procedure Clear_Depth_Stencil_Image
        (Command_Buffer: in Vulkan.Command_Buffer;
         Image: in Vulkan.Image;
         Image_Layout: in Vulkan.Image_Layout;
         Depth_Stencil: in Clear_Depth_Stencil_Value;
         Clear_Range: in Image_Subresource_Range)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Image /= No_Image;

    -- vkCmdClearAttachments
    procedure Clear_Attachments(Command_Buffer: in Vulkan.Command_Buffer;
                                Attachments: in Clear_Attachment_Vectors.Vector;
                                Rects: in Clear_Rect_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Attachments.Is_Empty and
                    not Rects.Is_Empty;

    procedure Clear_Attachments(Command_Buffer: in Vulkan.Command_Buffer;
                                Attachments: in Clear_Attachment_Vectors.Vector;
                                Rect: in Clear_Rect)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    not Attachments.Is_Empty;

    procedure Clear_Attachment(Command_Buffer: in Vulkan.Command_Buffer;
                               Attachment: in Vulkan.Clear_Attachment;
                               Rects: in Clear_Rect_Vectors.Vector)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    not Rects.Is_Empty;

    procedure Clear_Attachment(Command_Buffer: in Vulkan.Command_Buffer;
                               Attachment: in Vulkan.Clear_Attachment;
                               Rect: in Clear_Rect)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdResolveImage
    procedure Resolve_Image(Command_Buffer: in Vulkan.Command_Buffer;
                            Src_Image: in Image;
                            Src_Image_Layout: in Image_Layout;
                            Dst_Image: in Image;
                            Dst_Image_Layout: in Image_Layout;
                            Regions: in Image_Resolve_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Image /= No_Image and
                    Dst_Image /= No_Image and
                    not Regions.Is_Empty and
                    (for all Region of Regions =>
                        Region.Src_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Dst_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit);
     
    procedure Resolve_Image(Command_Buffer: in Vulkan.Command_Buffer;
                            Src_Image: in Image;
                            Src_Image_Layout: in Image_Layout;
                            Dst_Image: in Image;
                            Dst_Image_Layout: in Image_Layout;
                            Region: in Image_Resolve)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Image /= No_Image and
                    Dst_Image /= No_Image and
                    Region.Src_Subresource.Aspect_Mask /=
                    Image_Aspect_No_Bit and
                    Region.Dst_Subresource.Aspect_Mask /=
                    Image_Aspect_No_Bit;
   
    -- vkCmdSetEvent
    procedure Set_Event(Command_Buffer: in Vulkan.Command_Buffer;
                        Event: in Vulkan.Event;
                        Stage_Mask: in Pipeline_Stage_Flags)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Event /= No_Event and
                    Stage_Mask /= Pipeline_Stage_No_Bit;

    -- vkCmdResetEvent
    procedure Reset_Event(Command_Buffer: in Vulkan.Command_Buffer;
                          Event: in Vulkan.Event;
                          Stage_Mask: in Pipeline_Stage_Flags)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Event /= No_Event and
                    Stage_Mask /= Pipeline_Stage_No_Bit;

    -- vkCmdWaitEvents
    procedure Wait_Events(Command_Buffer: in Vulkan.Command_Buffer;
                          Events: in Event_Vectors.Vector;
                          Src_Stage_Mask,
                          Dst_Stage_Mask: in Pipeline_Stage_Flags;
                          Memory_Barriers: in Memory_Barrier_Vectors.Vector;
                          Buffer_Memory_Barriers:
                            in Buffer_Memory_Barrier_Vectors.Vector;
                          Image_Memory_Barriers:
                            in Image_Memory_Barrier_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Events.Is_Empty and
                    (for all Event of Events => Event /= No_Event) and
                    Src_Stage_Mask /= Pipeline_Stage_No_Bit and
                    Dst_Stage_Mask /= Pipeline_Stage_No_Bit;

    -- vkCmdPipelineBarrier
    procedure Pipeline_Barrier
        (Command_Buffer: in Vulkan.Command_Buffer;
         Src_Stage_Mask, Dst_Stage_Mask: in Pipeline_Stage_Flags;
         Dependency_Flags: in Vulkan.Dependency_Flags;
         Memory_Barriers: in Memory_Barrier_Vectors.Vector
            := Memory_Barrier_Vectors.Empty_Vector;
         Buffer_Memory_Barriers: in Buffer_Memory_Barrier_Vectors.Vector
            := Buffer_Memory_Barrier_Vectors.Empty_Vector;
         Image_Memory_Barriers: in Image_Memory_Barrier_Vectors.Vector
            := Image_Memory_Barrier_Vectors.Empty_Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Src_Stage_Mask /= Pipeline_Stage_No_Bit and
                    Dst_Stage_Mask /= Pipeline_Stage_No_Bit;

    -- vkCmdBeginQuery
    procedure Begin_Query(Command_Buffer: in Vulkan.Command_Buffer;
                          Query_Pool: in Vulkan.Query_Pool;
                          Query: in Interfaces.Unsigned_32;
                          Flags: in Query_Control_Flags)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Query_Pool /= No_Query_Pool;

    -- vkCmdEndQuery
    procedure End_Query(Command_Buffer: in Vulkan.Command_Buffer;
                        Query_Pool: in Vulkan.Query_Pool;
                        Query: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Query_Pool /= No_Query_Pool;

    -- vkCmdResetQueryPool
    procedure Reset_Query_Pool(Command_Buffer: in Vulkan.Command_Buffer;
                               Query_Pool: in Vulkan.Query_Pool;
                               First_Query,
                               Query_Count: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Query_Pool /= No_Query_Pool;

    -- vkCmdWriteTimestamp
    procedure Write_Timestamp(Command_Buffer: in Vulkan.Command_Buffer;
                              Pipeline_Stage: in Pipeline_Stage_Flags;
                              Query_Pool: in Vulkan.Query_Pool;
                              Query: in Interfaces.Unsigned_32)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Query_Pool /= No_Query_Pool;

    -- vkCmdCopyQueryPoolResults
    procedure Copy_Query_Pool_Results(Command_Buffer: in Vulkan.Command_Buffer;
                                      Query_Pool: in Vulkan.Query_Pool;
                                      First_Query,
                                      Query_Count: in Interfaces.Unsigned_32;
                                      Dst_Buffer: in Buffer;
                                      Dst_Offset,
                                      Stride: in Device_Size;
                                      Flags: in Query_Result_Flags)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Query_Pool /= No_Query_Pool and
                    Dst_Buffer /= No_Buffer;

    -- vkCmdPushConstants
    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Layout: in Pipeline_Layout;
                   Stage_Flags: in Shader_Stage_Flags;
                   Offset, Size: in Interfaces.Unsigned_32;
                   Values: in Interfaces.C.Extensions.void_ptr)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Layout /= No_Pipeline_Layout and
                    Stage_Flags /= Shader_Stage_No_Bit and
                    Offset mod 4 = 0 and
                    Size mod 4 = 0 and
                    Size > 0 and
                    Values /= System.Null_Address;

    -- vkCmdBeginRenderPass
    procedure Begin_Render_Pass(Command_Buffer: in Vulkan.Command_Buffer;
                                Render_Pass_Begin: in Render_Pass_Begin_Info;
                                Contents: in Subpass_Contents)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdNextSubpass
    procedure Next_Subpass(Command_Buffer: in Vulkan.Command_Buffer;
                           Contents: in Subpass_Contents)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdEndRenderPass
    procedure End_Render_Pass(Command_Buffer: in Vulkan.Command_Buffer)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdExecuteCommands
    procedure Execute(Command_Buffer: in Vulkan.Command_Buffer;
                      Command_Buffers: in Command_Buffer_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Command_Buffers.Is_Empty and
                    (for all CB of Command_Buffers => CB /= No_Command_Buffer);

    procedure Execute(Command_Buffer,
                      Secondary_Buffer: in Vulkan.Command_Buffer)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Secondary_Buffer /= No_Command_Buffer;

    -- Vulkan 1.1
    -- vkCmdSetDeviceMask
    procedure Set_Device_Mask(Command_Buffer: in Vulkan.Command_Buffer;
                              Device_Mask: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdDispatchBase
    procedure Dispatch_Base(Command_Buffer: in Vulkan.Command_Buffer;
                            Base_Group_X,
                            Base_Group_Y,
                            Base_Group_Z,
                            Group_Count_X,
                            Group_Count_Y,
                            Group_Count_Z: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- Vulkan 1.2
    -- vkCmdDrawIndirectCount
    procedure Draw_Indirect_Count(Command_Buffer: in Vulkan.Command_Buffer;
                                  Buffer: in Vulkan.Buffer;
                                  Offset: in Device_Size;
                                  Count_Buffer: in Vulkan.Buffer;
                                  Count_Buffer_Offset: in Device_Size;
                                  Max_Draw_Count,
                                  Stride: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Buffer /= No_Buffer and
                    Count_Buffer /= No_Buffer;

    -- vkCmdDrawIndexedIndirectCount
    procedure Draw_Indexed_Indirect_Count
        (Command_Buffer: in Vulkan.Command_Buffer;
         Buffer: in Vulkan.Buffer;
         Offset: in Device_Size;
         Count_Buffer: in Vulkan.Buffer;
         Count_Buffer_Offset: in Device_Size;
         Max_Draw_Count,
         Stride: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Buffer /= No_Buffer and
                    Count_Buffer /= No_Buffer;

    -- vkCmdBeginRenderPass2
    procedure Begin_Render_Pass
        (Command_Buffer: in Vulkan.Command_Buffer;
         Render_Pass_Begin: in Render_Pass_Begin_Info;
         Subpass_Begin_Info: in Vulkan.Subpass_Begin_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdNextSubpass2
    procedure Next_Subpass(Command_Buffer: in Vulkan.Command_Buffer;
                           Subpass_Begin_Info: in Vulkan.Subpass_Begin_Info;
                           Subpass_End_Info: in Vulkan.Subpass_End_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdEndRenderPass2
    procedure End_Render_Pass(Command_Buffer: in Vulkan.Command_Buffer;
                              Subpass_End_Info: in Vulkan.Subpass_End_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- Vulkan 1.3
    -- vkCmdSetEvent2
    procedure Set_Event(Command_Buffer: in Vulkan.Command_Buffer;
                        Event: in Vulkan.Event;
                        Dependency_Info: in Vulkan.Dependency_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Event /= No_Event;

    -- vkCmdResetEvent2
    procedure Reset_Event(Command_Buffer: in Vulkan.Command_Buffer;
                          Event: in Vulkan.Event;
                          Stage_Mask: in Pipeline_Stage_Flags_2)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Event /= No_Event;

    -- vkCmdWaitEvents2
    procedure Wait_Events(Command_Buffer: in Vulkan.Command_Buffer;
                          Events: in Event_Vectors.Vector;
                          Dependency_Infos: in Dependency_Info_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Events.Is_Empty and
                    Events.Length = Dependency_Infos.Length and
                    (for all Event of Events => Event /= No_Event);

    procedure Wait_Event(Command_Buffer: in Vulkan.Command_Buffer;
                         Event: in Vulkan.Event;
                         Dependency_Info: in Vulkan.Dependency_Info)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Event /= No_Event;

    -- vkCmdPipelineBarrier2
    procedure Pipeline_Barrier(Command_Buffer: in Vulkan.Command_Buffer;
                               Dependency_Info: in Vulkan.Dependency_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdWriteTimestamp2
    procedure Write_Timestamp(Command_Buffer: in Vulkan.Command_Buffer;
                              Stage: in Pipeline_Stage_Flags_2;
                              Query_Pool: in Vulkan.Query_Pool;
                              Query: in Interfaces.Unsigned_32)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Query_Pool /= No_Query_Pool;

    -- vkCmdCopyBuffer2
    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Copy_Buffer_Info: in Copy_Buffer_Info_2)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Copy_Buffer_Info.Src_Buffer /= No_Buffer and
                    Copy_Buffer_Info.Dst_Buffer /= No_Buffer and
                    not Copy_Buffer_Info.Regions.Is_Empty and
                    (for all Region of Copy_Buffer_Info.Regions =>
                        Region.Size > 0);

    -- vkCmdCopyImage2
    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Copy_Image_Info: in Copy_Image_Info_2)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Copy_Image_Info.Src_Image /= No_Image and
                    Copy_Image_Info.Dst_Image /= No_Image and
                    not Copy_Image_Info.Regions.Is_Empty and
                    (for all Region of Copy_Image_Info.Regions =>
                        Region.Src_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Src_Subresource.Layer_Count > 0 and
                        Region.Dst_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Dst_Subresource.Layer_Count > 0 and
                        Region.Extent.Width > 0 and
                        Region.Extent.Height > 0 and
                        Region.Extent.Depth > 0);

    -- vkCmdCopyBufferToImage2
    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Copy_Buffer_To_Image_Info: in Copy_Buffer_To_Image_Info_2)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Copy_Buffer_To_Image_Info.Src_Buffer /= No_Buffer and
                    Copy_Buffer_To_Image_Info.Dst_Image /= No_Image and
                    not Copy_Buffer_To_Image_Info.Regions.Is_Empty and
                    (for all Region of Copy_Buffer_To_Image_Info.Regions =>
                        Region.Image_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Image_Extent.Width > 0 and
                        Region.Image_Extent.Height > 0 and
                        Region.Image_Extent.Depth > 0);

    -- vkCmdCopyImageToBuffer2
    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Copy_Image_To_Buffer_Info: in Copy_Image_To_Buffer_Info_2)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Copy_Image_To_Buffer_Info.Src_Image /= No_Image and
                    Copy_Image_To_Buffer_Info.Dst_Buffer /= No_Buffer and
                    not Copy_Image_To_Buffer_Info.Regions.Is_Empty and
                    (for all Region of Copy_Image_To_Buffer_Info.Regions =>
                        Region.Image_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Image_Extent.Width > 0 and
                        Region.Image_Extent.Height > 0 and
                        Region.Image_Extent.Depth > 0);

    -- vkCmdBlitImage2
    procedure Blit(Command_Buffer: in Vulkan.Command_Buffer;
                   Blit_Image_Info: in Blit_Image_Info_2)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Blit_Image_Info.Src_Image /= No_Image and
                    Blit_Image_Info.Dst_Image /= No_Image and
                    not Blit_Image_Info.Regions.Is_Empty and
                    (for all Region of Blit_Image_Info.Regions =>
                        Region.Src_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Src_Subresource.Aspect_Mask =
                        Region.Dst_Subresource.Aspect_Mask);

    -- vkCmdResolveImage2
    procedure Resolve_Image(Command_Buffer: in Vulkan.Command_Buffer;
                            Resolve_Image_Info: in Resolve_Image_Info_2)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Resolve_Image_Info.Src_Image /= No_Image and
                    Resolve_Image_Info.Dst_Image /= No_Image and
                    not Resolve_Image_Info.Regions.Is_Empty and
                    (for all Region of Resolve_Image_Info.Regions =>
                        Region.Src_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit and
                        Region.Dst_Subresource.Aspect_Mask /=
                        Image_Aspect_No_Bit);

    -- vkCmdBeginRendering
    procedure Begin_Rendering(Command_Buffer: in Vulkan.Command_Buffer;
                              Rendering_Info: in Vulkan.Rendering_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;
                    
    -- vkCmdEndRendering
    procedure End_Rendering(Command_Buffer: in Vulkan.Command_Buffer)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetCullMode
    procedure Set_Cull_Mode(Command_Buffer: in Vulkan.Command_Buffer;
                            Cull_Mode: in Cull_Mode_Flags)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetFrontFace
    procedure Set_Front_Face(Command_Buffer: in Vulkan.Command_Buffer;
                             Front_Face: in Vulkan.Front_Face)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetPrimitiveTopology
    procedure Set_Primitive_Topology
        (Command_Buffer: in Vulkan.Command_Buffer;
         Primitive_Topology: in Vulkan.Primitive_Topology)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetViewportWithCount
    procedure Set_Viewport(Command_Buffer: in Vulkan.Command_Buffer;
                           Viewports: in Viewport_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Viewports.Is_Empty;

    procedure Set_Viewport(Command_Buffer: in Vulkan.Command_Buffer;
                           Viewport: in Vulkan.Viewport)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetScissorWithCount
    procedure Set_Scissor(Command_Buffer: in Vulkan.Command_Buffer;
                          Scissors: in Rect_2D_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Scissors.Is_Empty;

    procedure Set_Scissor(Command_Buffer: in Vulkan.Command_Buffer;
                          Scissor: in Rect_2D)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdBindVertexBuffers2
    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffers: in Buffer_Vectors.Vector;
                   Offsets: in Device_Size_Vectors.Vector;
                   Sizes, Strides: in Device_Size_Vectors.Vector :=
                       Device_Size_Vectors.Empty_Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Buffers.Length = Offsets.Length and
                    (Sizes.Is_Empty or Sizes.Length = Buffers.Length) and
                    (Strides.Is_Empty or Strides.Length = Buffers.Length);

    -- vkCmdSetDepthTestEnable
    procedure Set_Depth_Test_Enable(Command_Buffer: in Vulkan.Command_Buffer;
                                    Depth_Test_Enable: in Boolean)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetDepthWriteEnable
    procedure Set_Depth_Write_Enable(Command_Buffer: in Vulkan.Command_Buffer;
                                     Depth_Write_Enable: in Boolean)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetStencilOp
    procedure Set_Stencil_Op(Command_Buffer: in Vulkan.Command_Buffer;
                             Face_Mask: in Stencil_Face_Flags;
                             Fail_Op, Pass_Op, Depth_Fail_Op: in Stencil_Op;
                             Compare_Op: Vulkan.Compare_Op)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Face_Mask /= Stencil_Face_No_Bit;

    -- vkCmdSetRasterizerDiscardEnable
    procedure Set_Rasterizer_Discard_Enable
        (Command_Buffer: in Vulkan.Command_Buffer;
         Rasterizer_Discard_Enable: in Boolean)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetDepthBiasEnable
    procedure Set_Depth_Bias_Enable(Command_Buffer: in Vulkan.Command_Buffer;
                                    Depth_Bias_Enable: in Boolean)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetPrimitiveRestartEnable
    procedure Set_Primitive_Restart_Enable
        (Command_Buffer: in Vulkan.Command_Buffer;
         Primitive_Restart_Enable: in Boolean)
        with Pre => Command_Buffer /= No_Command_Buffer;

private
    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
                   Pipeline: in Vulkan.Pipeline)
        renames C.vkCmdBindPipeline;

    procedure Set_Line_Width(Command_Buffer: in Vulkan.Command_Buffer;
                             Line_Width: in Interfaces.C.C_Float)
        renames C.vkCmdSetLineWidth;

    procedure Set_Depth_Bias(Command_Buffer: in Vulkan.Command_Buffer;
                             Depth_Bias_Constant_Factor,
                             Depth_Bias_Clamp,
                             Depth_Bias_Slope_Factor:
                              in Interfaces.C.C_Float)
        renames C.vkCmdSetDepthBias;
    
    procedure Set_Depth_Bounds(Command_Buffer: in Vulkan.Command_Buffer;
                               Min_Depth_Bounds,
                               Max_Depth_Bounds: in Interfaces.C.C_Float)
        renames C.vkCmdSetDepthBounds;

    procedure Set_Stencil_Compare_Mask(Command_Buffer: in Vulkan.Command_Buffer;
                                       Face_Mask: in Stencil_Face_Flags;
                                       Compare_Mask: in Interfaces.Unsigned_32)
        renames C.vkCmdSetStencilCompareMask;
    
    procedure Set_Stencil_Write_Mask(Command_Buffer: in Vulkan.Command_Buffer;
                                     Face_Mask: in Stencil_Face_Flags;
                                     Write_Mask: in Interfaces.Unsigned_32)
        renames C.vkCmdSetStencilWriteMask;

    procedure Set_Stencil_Reference(Command_Buffer: in Vulkan.Command_Buffer;
                                     Face_Mask: in Stencil_Face_Flags;
                                     Reference: in Interfaces.Unsigned_32)
        renames C.vkCmdSetStencilReference;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Buffer: in Vulkan.Buffer;
                   Offset: in Device_Size;
                   Index_Type: in Vulkan.Index_Type)
        renames C.vkCmdBindIndexBuffer;
    
    procedure Draw(Command_Buffer: in Vulkan.Command_Buffer;
                   Vertex_Count,
                   Instance_Count,
                   First_Vertex,
                   First_Instance: Interfaces.Unsigned_32)
        renames C.vkCmdDraw;
    
    procedure Draw_Indexed(Command_Buffer: in Vulkan.Command_Buffer;
                           Index_Count,
                           Instance_Count,
                           First_Index: in Interfaces.Unsigned_32;
                           Vertex_Offset: in Interfaces.Integer_32;
                           First_Instance: in Interfaces.Unsigned_32)
        renames C.vkCmdDrawIndexed;

    procedure Draw_Indirect(Command_Buffer: in Vulkan.Command_Buffer;
                            Buffer: in Vulkan.Buffer;
                            Offset: in Device_Size;
                            Draw_Count, Stride: in Interfaces.Unsigned_32)
        renames C.vkCmdDrawIndirect;

    procedure Draw_Indexed_Indirect(Command_Buffer: in Vulkan.Command_Buffer;
                                    Buffer: in Vulkan.Buffer;
                                    Offset: in Device_Size;
                                    Draw_Count,
                                    Stride: in Interfaces.Unsigned_32)
        renames C.vkCmdDrawIndexedIndirect;

    procedure Dispatch(Command_Buffer: in Vulkan.Command_Buffer;
                       Group_Count_X,
                       Group_Count_Y,
                       Group_Count_Z: in Interfaces.Unsigned_32)
        renames C.vkCmdDispatch;

    procedure Dispatch_Indirect(Command_Buffer: in Vulkan.Command_Buffer;
                                Buffer: in Vulkan.Buffer;
                                Offset: in Device_Size)
        renames C.vkCmdDispatchIndirect;

    procedure Fill_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                          Dst_Buffer: in Buffer;
                          Dst_Offset, Size: in Device_Size;
                          Data: in Interfaces.Unsigned_32)
        renames C.vkCmdFillBuffer;

    procedure Set_Event(Command_Buffer: in Vulkan.Command_Buffer;
                        Event: in Vulkan.Event;
                        Stage_Mask: in Pipeline_Stage_Flags)
        renames C.vkCmdSetEvent;

    procedure Reset_Event(Command_Buffer: in Vulkan.Command_Buffer;
                          Event: in Vulkan.Event;
                          Stage_Mask: in Pipeline_Stage_Flags)
        renames C.vkCmdResetEvent;
    
    procedure Begin_Query(Command_Buffer: in Vulkan.Command_Buffer;
                          Query_Pool: in Vulkan.Query_Pool;
                          Query: in Interfaces.Unsigned_32;
                          Flags: in Query_Control_Flags)
        renames C.vkCmdBeginQuery;

    procedure End_Query(Command_Buffer: in Vulkan.Command_Buffer;
                        Query_Pool: in Vulkan.Query_Pool;
                        Query: in Interfaces.Unsigned_32)
        renames C.vkCmdEndQuery;

    procedure Reset_Query_Pool(Command_Buffer: in Vulkan.Command_Buffer;
                               Query_Pool: in Vulkan.Query_Pool;
                               First_Query,
                               Query_Count: in Interfaces.Unsigned_32)
        renames C.vkCmdResetQueryPool;

    procedure Write_Timestamp(Command_Buffer: in Vulkan.Command_Buffer;
                              Pipeline_Stage: in Pipeline_Stage_Flags;
                              Query_Pool: in Vulkan.Query_Pool;
                              Query: in Interfaces.Unsigned_32)
        renames C.vkCmdWriteTimestamp;

    procedure Copy_Query_Pool_Results(Command_Buffer: in Vulkan.Command_Buffer;
                                      Query_Pool: in Vulkan.Query_Pool;
                                      First_Query,
                                      Query_Count: in Interfaces.Unsigned_32;
                                      Dst_Buffer: in Buffer;
                                      Dst_Offset,
                                      Stride: in Device_Size;
                                      Flags: in Query_Result_Flags)
        renames C.vkCmdCopyQueryPoolResults;

    procedure Next_Subpass(Command_Buffer: in Vulkan.Command_Buffer;
                           Contents: in Subpass_Contents)
        renames C.vkCmdNextSubpass;

    procedure End_Render_Pass(Command_Buffer: in Vulkan.Command_Buffer)
        renames C.vkCmdEndRenderPass;

    procedure End_Render_Pass_2 is
        new Utilities.Forward_Convert(Subpass_End_Info,
                                      C_V1_2.Subpass_End_Info_C,
                                      C_V1_2.To_C,
                                      C_V1_2.Free,
                                      C_V1_2.vkCmdEndRenderPass2);

    procedure End_Render_Pass(Command_Buffer: in Vulkan.Command_Buffer;
                              Subpass_End_Info: in Vulkan.Subpass_End_Info)
        renames End_Render_Pass_2;

    procedure Pipeline_Barrier_2 is
        new Utilities.Forward_Convert(Dependency_Info,
                                      C_V1_3.Dependency_Info_C,
                                      C_V1_3.To_C,
                                      C_V1_3.Free,
                                      C_V1_3.vkCmdPipelineBarrier2);

    procedure Pipeline_Barrier(Command_Buffer: in Vulkan.Command_Buffer;
                               Dependency_Info: in Vulkan.Dependency_Info)
        renames Pipeline_Barrier_2;

    procedure Copy_Buffer_2 is
        new Utilities.Forward_Convert(Copy_Buffer_Info_2,
                                      C_V1_3.Copy_Buffer_Info_2_C,
                                      C_V1_3.To_C,
                                      C_V1_3.Free,
                                      C_V1_3.vkCmdCopyBuffer2);

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Copy_Buffer_Info: in Copy_Buffer_Info_2)
        renames Copy_Buffer_2;

    procedure Copy_Image_2 is
        new Utilities.Forward_Convert(Copy_Image_Info_2,
                                      C_V1_3.Copy_Image_Info_2_C,
                                      C_V1_3.To_C,
                                      C_V1_3.Free,
                                      C_V1_3.vkCmdCopyImage2);

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Copy_Image_Info: in Copy_Image_Info_2)
        renames Copy_Image_2;

    procedure Copy_Buffer_To_Image_2 is
        new Utilities.Forward_Convert(Copy_Buffer_To_Image_Info_2,
                                      C_V1_3.Copy_Buffer_To_Image_Info_2_C,
                                      C_V1_3.To_C,
                                      C_V1_3.Free,
                                      C_V1_3.vkCmdCopyBufferToImage2);

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Copy_Buffer_To_Image_Info: in Copy_Buffer_To_Image_Info_2)
        renames Copy_Buffer_To_Image_2;

    procedure Copy_Image_To_Buffer_2 is
        new Utilities.Forward_Convert(Copy_Image_To_Buffer_Info_2,
                                      C_V1_3.Copy_Image_To_Buffer_Info_2_C,
                                      C_V1_3.To_C,
                                      C_V1_3.Free,
                                      C_V1_3.vkCmdCopyImageToBuffer2);

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Copy_Image_To_Buffer_Info: in Copy_Image_To_Buffer_Info_2)
        renames Copy_Image_To_Buffer_2;

    procedure Blit_Image_2 is
        new Utilities.Forward_Convert(Blit_Image_Info_2,
                                      C_V1_3.Blit_Image_Info_2_C,
                                      C_V1_3.To_C,
                                      C_V1_3.Free,
                                      C_V1_3.vkCmdBlitImage2);

    procedure Blit(Command_Buffer: in Vulkan.Command_Buffer;
                   Blit_Image_Info: in Blit_Image_Info_2)
        renames Blit_Image_2;

    procedure Resolve_Image_2 is
        new Utilities.Forward_Convert(Resolve_Image_Info_2,
                                      C_V1_3.Resolve_Image_Info_2_C,
                                      C_V1_3.To_C,
                                      C_V1_3.Free,
                                      C_V1_3.vkCmdResolveImage2);

    procedure Resolve_Image(Command_Buffer: in Vulkan.Command_Buffer;
                            Resolve_Image_Info: in Resolve_Image_Info_2)
        renames Resolve_Image_2;

    procedure Cmd_Begin_Rendering is
        new Utilities.Forward_Convert(Rendering_Info,
                                      C_V1_3.Rendering_Info_C,
                                      C_V1_3.To_C,
                                      C_V1_3.Free,
                                      C_V1_3.vkCmdBeginRendering);

    procedure Begin_Rendering(Command_Buffer: in Vulkan.Command_Buffer;
                              Rendering_Info: in Vulkan.Rendering_Info)
        renames Cmd_Begin_Rendering;

    procedure Cmd_Set_Cull_Mode is
        new Utilities.Forward(Cull_Mode_Flags, C_V1_3.vkCmdSetCullMode);

    procedure Set_Cull_Mode(Command_Buffer: in Vulkan.Command_Buffer;
                            Cull_Mode: in Cull_Mode_Flags)
        renames Cmd_Set_Cull_Mode;

    procedure Cmd_Set_Front_Face is
        new Utilities.Forward(Front_Face, C_V1_3.vkCmdSetFrontFace);

    procedure Set_Front_Face(Command_Buffer: in Vulkan.Command_Buffer;
                             Front_Face: in Vulkan.Front_Face)
        renames Cmd_Set_Front_Face;

    procedure Cmd_Set_Primitive_Topology is
        new Utilities.Forward(Primitive_Topology,
                              C_V1_3.vkCmdSetPrimitiveTopology);

    procedure Set_Primitive_Topology
        (Command_Buffer: in Vulkan.Command_Buffer;
         Primitive_Topology: in Vulkan.Primitive_Topology)
        renames Cmd_Set_Primitive_Topology;

    procedure Cmd_Set_Depth_Test_Enable is
        new Utilities.Forward_Boolean(C_V1_3.vkCmdSetDepthTestEnable);

    procedure Set_Depth_Test_Enable(Command_Buffer: in Vulkan.Command_Buffer;
                                    Depth_Test_Enable: in Boolean)
        renames Cmd_Set_Depth_Test_Enable;

    procedure Cmd_Set_Depth_Write_Enable is
        new Utilities.Forward_Boolean(C_V1_3.vkCmdSetDepthWriteEnable);

    procedure Set_Depth_Write_Enable(Command_Buffer: in Vulkan.Command_Buffer;
                                     Depth_Write_Enable: in Boolean)
        renames Cmd_Set_Depth_Write_Enable;

    procedure Cmd_Set_Rasterizer_Discard_Enable
        is new Utilities.Forward_Boolean
            (C_V1_3.vkCmdSetRasterizerDiscardEnable);

    procedure Set_Rasterizer_Discard_Enable
        (Command_Buffer: in Vulkan.Command_Buffer;
         Rasterizer_Discard_Enable: in Boolean)
        renames Cmd_Set_Rasterizer_Discard_Enable;

    procedure Cmd_Set_Depth_Bias_Enable is
        new Utilities.Forward_Boolean(C_V1_3.vkCmdSetDepthBiasEnable);

    procedure Set_Depth_Bias_Enable(Command_Buffer: in Vulkan.Command_Buffer;
                                    Depth_Bias_Enable: in Boolean)
        renames Cmd_Set_Depth_Bias_Enable;

    procedure Cmd_Set_Primitive_Restart_Enable is
        new Utilities.Forward_Boolean(C_V1_3.vkCmdSetPrimitiveRestartEnable);

    procedure Set_Primitive_Restart_Enable
        (Command_Buffer: in Vulkan.Command_Buffer;
         Primitive_Restart_Enable: in Boolean)
        renames Cmd_Set_Primitive_Restart_Enable;
end Vulkan.Commands;


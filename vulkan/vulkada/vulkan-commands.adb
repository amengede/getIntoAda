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

-- Command subprograms

with Ada.Unchecked_Conversion;
with Vulkan.C_V1_1;
with Vulkan.C_V1_4;

package body Vulkan.Commands is
    procedure Set_Viewport(Command_Buffer: in Vulkan.Command_Buffer;
                           First_Viewport: in Interfaces.Unsigned_32;
                           Viewports: in Viewport_Vectors.Vector) is
        C_Viewports: array (1 .. Positive(Viewports.Length)) of aliased Viewport
            with Convention => C;
    begin
        for X in C_Viewports'Range loop
            C_Viewports(X) := Viewports(X);
        end loop;

        C.vkCmdSetViewport(Command_Buffer,
                           First_Viewport,
                           Interfaces.Unsigned_32(C_Viewports'Length),
                           C_Viewports(1)'Access);
    end Set_Viewport;

    procedure Set_Viewport(Command_Buffer: in Vulkan.Command_Buffer;
                           First_Viewport: in Interfaces.Unsigned_32;
                           Viewport: in Vulkan.Viewport) is
    begin
        Set_Viewport(Command_Buffer,
                     First_Viewport,
                     Viewport_Vectors.To_Vector(Viewport, 1));
    end Set_Viewport;

    procedure Set_Scissor(Command_Buffer: in Vulkan.Command_Buffer;
                          First_Scissor: in Interfaces.Unsigned_32;
                          Scissors: in Rect_2D_Vectors.Vector) is
        C_Scissors: array (1 .. Positive(Scissors.Length)) of aliased Rect_2D
            with Convention => C;
    begin
        for X in C_Scissors'Range loop
            C_Scissors(X) := Scissors(X);
        end loop;

        C.vkCmdSetScissor(Command_Buffer,
                          First_Scissor,
                          Interfaces.Unsigned_32(C_Scissors'Length),
                          C_Scissors(1)'Access);
    end Set_Scissor;

    procedure Set_Scissor(Command_Buffer: in Vulkan.Command_Buffer;
                          First_Scissor: in Interfaces.Unsigned_32;
                          Scissor: in Rect_2D) is
    begin
        Set_Scissor(Command_Buffer,
                    First_Scissor,
                    Rect_2D_Vectors.To_Vector(Scissor, 1));
    end Set_Scissor;

    procedure Set_Blend_Constants(Command_Buffer: in Vulkan.Command_Buffer;
                                  Blend_Constants: in Blend_Constants_Array) is
    begin
        C.vkCmdSetBlendConstants(Command_Buffer, Blend_Constants);
    end Set_Blend_Constants;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   First_Set: in Interfaces.Unsigned_32;
                   Descriptor_Sets: in Descriptor_Set_Vectors.Vector;
                   Dynamic_Offsets: in Unsigned_32_Vectors.Vector
                    := Unsigned_32_Vectors.Empty_Vector) is
        C_Descriptor_Sets: array (1 .. Natural(Descriptor_Sets.Length))
                            of aliased Descriptor_Set
            with Convention => C;
        C_Dynamic_Offsets: array (1 .. Natural(Dynamic_Offsets.Length))
                            of aliased Interfaces.Unsigned_32
            with Convention => C;
    begin
        for X in C_Descriptor_Sets'Range loop
            C_Descriptor_Sets(X) := Descriptor_Sets(X);
        end loop;

        for X in C_Dynamic_Offsets'Range loop
            C_Dynamic_Offsets(X) := Dynamic_Offsets(X);
        end loop;

        C.vkCmdBindDescriptorSets
            (Command_Buffer,
             Pipeline_Bind_Point,
             Layout,
             First_Set,
             Interfaces.Unsigned_32(Descriptor_Sets.Length),
             (if C_Descriptor_Sets'Length /= 0 then
                C_Descriptor_Sets(1)'Access else null),
             Interfaces.Unsigned_32(Dynamic_Offsets.Length),
             (if C_Dynamic_Offsets'Length /= 0 then
                C_Dynamic_Offsets(1)'Access else null));
    end Bind;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   First_Set: in Interfaces.Unsigned_32;
                   Descriptor_Set: in Vulkan.Descriptor_Set;
                   Dynamic_Offset: in Interfaces.Unsigned_32) is
    begin
        Bind(Command_Buffer,
             Pipeline_Bind_Point,
             Layout,
             First_Set,
             Descriptor_Set_Vectors.To_Vector(Descriptor_Set, 1),
             Unsigned_32_Vectors.To_Vector(Dynamic_Offset, 1));
    end Bind;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   First_Set: in Interfaces.Unsigned_32;
                   Descriptor_Set: in Vulkan.Descriptor_Set) is
    begin
        Bind(Command_Buffer,
             Pipeline_Bind_Point,
             Layout,
             First_Set,
             Descriptor_Set_Vectors.To_Vector(Descriptor_Set, 1));
    end Bind;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffers: in Buffer_Vectors.Vector;
                   Offsets: in Device_Size_Vectors.Vector) is
        C_Buffers: array (1 .. Positive(Buffers.Length)) of aliased Buffer
            with Convention => C;
        C_Offsets: array (1 .. Positive(Offsets.Length)) of aliased Device_Size
            with Convention => C;
    begin
        for X in C_Buffers'Range loop
            C_Buffers(X) := Buffers(X);
            C_Offsets(X) := Offsets(X);
        end loop;

        C.vkCmdBindVertexBuffers(Command_Buffer,
                                 First_Binding,
                                 Interfaces.Unsigned_32(Buffers.Length),
                                 C_Buffers(1)'Access,
                                 C_Offsets(1)'Access);
    end Bind;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffer: in Vulkan.Buffer;
                   Offset: in Device_Size := 0) is
        C_Buffer: aliased Vulkan.Buffer := Buffer;
        C_Offset: aliased Device_Size := Offset;
    begin
        C.vkCmdBindVertexBuffers(Command_buffer,
                                 First_Binding,
                                 1,
                                 C_Buffer'Access,
                                 C_Offset'Access);
    end Bind;

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Buffer, Dst_Buffer: in Buffer;
                   Regions: in Buffer_Copy_Vectors.Vector) is
        C_Regions: array (1 .. Positive(Regions.Length))
                    of aliased Buffer_Copy
            with Convention => C;
    begin
        for X in C_Regions'Range loop
            C_Regions(X) := Regions(X);
        end loop;

        C.vkCmdCopyBuffer(Command_Buffer,
                          Src_Buffer,
                          Dst_Buffer,
                          Interfaces.Unsigned_32(Regions.Length),
                          C_Regions(1)'Access);
    end Copy;

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Buffer, Dst_Buffer: in Buffer;
                   Region: in Buffer_Copy) is
        Local_Region: aliased Buffer_Copy := Region;
    begin
        C.vkCmdCopyBuffer(Command_Buffer,
                          Src_Buffer,
                          Dst_Buffer,
                          1,
                          Local_Region'Access);
    end Copy;

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Regions: in Image_Copy_Vectors.Vector) is
        C_Regions: array (1 .. Positive(Regions.Length))
                    of aliased Image_Copy
            with Convention => C;
    begin
        for X in C_Regions'Range loop
            C_Regions(X) := Regions(X);
        end loop;

        C.vkCmdCopyImage(Command_Buffer,
                         Src_Image,
                         Src_Image_Layout,
                         Dst_Image,
                         Dst_Image_Layout,
                         Interfaces.Unsigned_32(Regions.Length),
                         C_Regions(1)'Access);
    end Copy;

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Region: in Image_Copy) is
    begin
        Copy(Command_Buffer,
             Src_Image,
             Src_Image_Layout,
             Dst_Image,
             Dst_Image_Layout,
             Image_Copy_Vectors.To_Vector(Region, 1));
    end Copy;

    procedure Blit(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Regions: in Image_Blit_Vectors.Vector;
                   Filter: in Vulkan.Filter) is
        C_Regions: array (1 .. Positive(Regions.Length))
                    of aliased Image_Blit
            with Convention => C;
    begin
        for X in C_Regions'Range loop
            C_Regions(X) := Regions(X);
        end loop;

        C.vkCmdBlitImage(Command_Buffer,
                         Src_Image,
                         Src_Image_Layout,
                         Dst_Image,
                         Dst_Image_Layout,
                         Interfaces.Unsigned_32(Regions.Length),
                         C_Regions(1)'Access,
                         Filter);
    end Blit;
    
    procedure Blit(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Region: in Image_Blit;
                   Filter: in Vulkan.Filter) is
    begin
        Blit(Command_Buffer,
             Src_Image,
             Src_Image_Layout,
             Dst_Image,
             Dst_Image_Layout,
             Image_Blit_Vectors.To_Vector(Region, 1),
             Filter);
    end Blit;

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Buffer: in Buffer;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Regions: in Buffer_Image_Copy_Vectors.Vector) is
        C_Regions: array (1 .. Positive(Regions.Length))
                    of aliased Buffer_Image_Copy
            with Convention => C;
    begin
        for X in C_Regions'Range loop
            C_Regions(X) := Regions(X);
        end loop;

        C.vkCmdCopyBufferToImage(Command_Buffer,
                                 Src_Buffer,
                                 Dst_Image,
                                 Dst_Image_Layout,
                                 Interfaces.Unsigned_32(Regions.Length),
                                 C_Regions(1)'Access);
    end Copy;

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Buffer: in Buffer;
                   Dst_Image: in Image;
                   Dst_Image_Layout: in Image_Layout;
                   Region: in Buffer_Image_Copy) is
        Local_Region: aliased Buffer_Image_Copy := Region;
    begin
        C.vkCmdCopyBufferToImage(Command_Buffer,
                                 Src_Buffer,
                                 Dst_Image,
                                 Dst_Image_Layout,
                                 1,
                                 Local_Region'Access);
    end Copy;

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Buffer: in Buffer;
                   Regions: in Buffer_Image_Copy_Vectors.Vector) is
        C_Regions: array (1 .. Positive(Regions.Length))
                    of aliased Buffer_Image_Copy
            with Convention => C;
    begin
        for X in C_Regions'Range loop
            C_Regions(X) := Regions(X);
        end loop;

        C.vkCmdCopyImageToBuffer(Command_Buffer,
                                 Src_Image,
                                 Src_Image_Layout,
                                 Dst_Buffer,
                                 Interfaces.Unsigned_32(Regions.Length),
                                 C_Regions(1)'Access);
    end Copy;

    procedure Copy(Command_Buffer: in Vulkan.Command_Buffer;
                   Src_Image: in Image;
                   Src_Image_Layout: in Image_Layout;
                   Dst_Buffer: in Buffer;
                   Region: in Buffer_Image_Copy) is
    begin
        Copy(Command_Buffer,
             Src_Image,
             Src_Image_Layout,
             Dst_Buffer,
             Buffer_Image_Copy_Vectors.To_Vector(Region, 1));
    end Copy;

    procedure Update_Buffer(Command_Buffer: in Vulkan.Command_Buffer;
                            Dst_Buffer: in Buffer;
                            Dst_Offset, Data_Size: in Device_Size;
                            Data: in Pointers.Pointer) is
        function To_Pointer is
            new Ada.Unchecked_Conversion(Pointers.Pointer, System.Address);
    begin
        C.vkCmdUpdateBuffer(Command_Buffer,
                            Dst_Buffer,
                            Dst_Offset,
                            Data_Size,
                            To_Pointer(Data));
    end Update_Buffer;

    procedure Clear_Color_Image(Command_Buffer: in Vulkan.Command_Buffer;
                                Image: in Vulkan.Image;
                                Image_Layout: in Vulkan.Image_Layout;
                                Color: in Clear_Color_Value;
                                Ranges: in Image_Subresource_Range_Vectors.Vector) is
        C_Ranges: array (1 .. Positive(Ranges.Length))
                    of aliased Image_Subresource_Range
            with Convention => C;
        Color_Address: System.Address;
    begin
        for X in C_Ranges'Range loop
            C_Ranges(X) := Ranges(X);
        end loop;

        case Color.Color_Type is
            when Clear_Color_Float =>
                Color_Address := Color.Float_Color'Address;
            when Clear_Color_Integer =>
                Color_Address := Color.Integer_Color'Address;
            when Clear_Color_Unsigned =>
                Color_Address := Color.Unsigned_Color'Address;
        end case;

        C.vkCmdClearColorImage(Command_Buffer,
                               Image,
                               Image_Layout,
                               Color_Address,
                               Interfaces.Unsigned_32(Ranges.Length),
                               C_Ranges(1)'Access);
    end Clear_Color_Image;

    procedure Clear_Color_Image(Command_Buffer: in Vulkan.Command_Buffer;
                                Image: in Vulkan.Image;
                                Image_Layout: in Vulkan.Image_Layout;
                                Color: in Clear_Color_Value;
                                Clear_Range: in Image_Subresource_Range) is
    begin
        Clear_Color_Image(Command_Buffer,
                          Image,
                          Image_Layout,
                          Color,
                          Image_Subresource_Range_Vectors.To_Vector(Clear_Range,
                                                                    1));
    end Clear_Color_Image;

    procedure Clear_Depth_Stencil_Image
        (Command_Buffer: in Vulkan.Command_Buffer;
         Image: in Vulkan.Image;
         Image_Layout: in Vulkan.Image_Layout;
         Depth_Stencil: in Clear_Depth_Stencil_Value;
         Ranges: in Image_Subresource_Range_Vectors.Vector) is
        C_Ranges: array (1 .. Positive(Ranges.Length))
                    of aliased Image_Subresource_Range
            with Convention => C;
    begin
        for X in C_Ranges'Range loop
            C_Ranges(X) := Ranges(X);
        end loop;

        C.vkCmdClearDepthStencilImage(Command_Buffer,
                                      Image,
                                      Image_Layout,
                                      Depth_Stencil,
                                      Interfaces.Unsigned_32(Ranges.Length),
                                      C_Ranges(1)'Access);
    end Clear_Depth_Stencil_Image;
    
    procedure Clear_Depth_Stencil_Image
        (Command_Buffer: in Vulkan.Command_Buffer;
         Image: in Vulkan.Image;
         Image_Layout: in Vulkan.Image_Layout;
         Depth_Stencil: in Clear_Depth_Stencil_Value;
         Clear_Range: in Image_Subresource_Range) is
    begin
        Clear_Depth_Stencil_Image
            (Command_Buffer,
             Image,
             Image_Layout,
             Depth_Stencil,
             Image_Subresource_Range_Vectors.To_Vector(Clear_Range, 1));
    end Clear_Depth_Stencil_Image;

    procedure Clear_Attachments(Command_Buffer: in Vulkan.Command_Buffer;
                                Attachments: in Clear_Attachment_Vectors.Vector;
                                Rects: in Clear_Rect_Vectors.Vector) is
        C_Attachments: array (1 .. Positive(Attachments.Length))
                        of aliased C.Clear_Attachment_C(Clear_Color, Clear_Color_Float)
            with Convention => C;
        C_Rects: array (1 .. Positive(Rects.Length))
                    of aliased Clear_Rect
            with Convention => C;
    begin
        for X in C_Attachments'Range loop
            C_Attachments(X) := C.To_C(Attachments(X));
        end loop;

        for X in C_Rects'Range loop
            C_Rects(X) := Rects(X);
        end loop;

        C.vkCmdClearAttachments(Command_Buffer,
                                Interfaces.Unsigned_32(Attachments.Length),
                                C_Attachments(1)'Access,
                                Interfaces.Unsigned_32(Rects.Length),
                                C_Rects(1)'Access);
    end Clear_Attachments;

    procedure Clear_Attachments(Command_Buffer: in Vulkan.Command_Buffer;
                                Attachments: in Clear_Attachment_Vectors.Vector;
                                Rect: in Clear_Rect) is
    begin
        Clear_Attachments(Command_Buffer,
                          Attachments,
                          Clear_Rect_Vectors.To_Vector(Rect, 1));
    end Clear_Attachments;

    procedure Clear_Attachment(Command_Buffer: in Vulkan.Command_Buffer;
                               Attachment: in Vulkan.Clear_Attachment;
                               Rects: in Clear_Rect_Vectors.Vector) is
    begin
        Clear_Attachments(Command_Buffer,
                          Clear_Attachment_Vectors.To_Vector(Attachment, 1),
                          Rects);
    end Clear_Attachment;

    procedure Clear_Attachment(Command_Buffer: in Vulkan.Command_Buffer;
                               Attachment: in Vulkan.Clear_Attachment;
                               Rect: in Clear_Rect) is
    begin
        Clear_Attachments(Command_Buffer,
                          Clear_Attachment_Vectors.To_Vector(Attachment, 1),
                          Clear_Rect_Vectors.To_Vector(Rect, 1));
    end Clear_Attachment;

    procedure Resolve_Image(Command_Buffer: in Vulkan.Command_Buffer;
                            Src_Image: in Image;
                            Src_Image_Layout: in Image_Layout;
                            Dst_Image: in Image;
                            Dst_Image_Layout: in Image_Layout;
                            Regions: in Image_Resolve_Vectors.Vector) is
        C_Regions: array (1 .. Positive(Regions.Length))
                    of aliased Image_Resolve
            with Convention => C;
    begin
        for X in C_Regions'Range loop
            C_Regions(X) := Regions(X);
        end loop;

        C.vkCmdResolveImage(Command_Buffer,
                            Src_Image,
                            Src_Image_Layout,
                            Dst_Image,
                            Dst_Image_Layout,
                            Interfaces.Unsigned_32(Regions.Length),
                            C_Regions(1)'Access);
    end Resolve_Image;

    procedure Resolve_Image(Command_Buffer: in Vulkan.Command_Buffer;
                            Src_Image: in Image;
                            Src_Image_Layout: in Image_Layout;
                            Dst_Image: in Image;
                            Dst_Image_Layout: in Image_Layout;
                            Region: in Image_Resolve) is
    begin
        Resolve_Image(Command_Buffer,
                      Src_Image,
                      Src_Image_Layout,
                      Dst_Image,
                      Dst_Image_Layout,
                      Image_Resolve_Vectors.To_Vector(Region, 1));
    end Resolve_Image;

    procedure Wait_Events(Command_Buffer: in Vulkan.Command_Buffer;
                          Events: in Event_Vectors.Vector;
                          Src_Stage_Mask,
                          Dst_Stage_Mask: in Pipeline_Stage_Flags;
                          Memory_Barriers: in Memory_Barrier_Vectors.Vector;
                          Buffer_Memory_Barriers:
                            in Buffer_Memory_Barrier_Vectors.Vector;
                          Image_Memory_Barriers:
                            in Image_Memory_Barrier_Vectors.Vector) is
        C_Events: array (1 .. Positive(Events.Length)) of aliased Event
            with Convention => C;
        C_Memory_Barriers: array (1 .. Natural(Memory_Barriers.Length))
                            of aliased C.Memory_Barrier_C
            with Convention => C;
        C_Buffer_Memory_Barriers:
            array (1 .. Natural(Buffer_Memory_Barriers.Length))
                of aliased C.Buffer_Memory_Barrier_C
            with Convention => C;
        C_Image_Memory_Barriers:
            array (1 .. Natural(Image_Memory_Barriers.Length))
                of aliased C.Image_Memory_Barrier_C
            with Convention => C;
    begin
        for X in C_Events'Range loop
            C_Events(X) := Events(X);
        end loop;

        for X in C_Memory_Barriers'Range loop
            C_Memory_Barriers(X) := C.To_C(Memory_Barriers(X));
        end loop;

        for X in C_Buffer_Memory_Barriers'Range loop
            C_Buffer_Memory_Barriers(X) := C.To_C(Buffer_Memory_Barriers(X));
        end loop;

        for X in C_Image_Memory_Barriers'Range loop
            C_Image_Memory_Barriers(X) := C.To_C(Image_Memory_Barriers(X));
        end loop;

        C.vkCmdWaitEvents(Command_Buffer,
                          Interfaces.Unsigned_32(Events.Length),
                          C_Events(1)'Access,
                          Src_Stage_Mask,
                          Dst_Stage_Mask,
                          Interfaces.Unsigned_32(Memory_Barriers.Length),
                          C_Memory_Barriers(1)'Access,
                          Interfaces.Unsigned_32(Buffer_Memory_Barriers.Length),
                          C_Buffer_Memory_Barriers(1)'Access,
                          Interfaces.Unsigned_32(Image_Memory_Barriers.Length),
                          C_Image_Memory_Barriers(1)'Access);

        for CMB of C_Memory_Barriers loop
            C.Free(CMB);
        end loop;

        for CBMB of C_Buffer_Memory_Barriers loop
            C.Free(CBMB);
        end loop;

        for CIMB of C_Image_Memory_Barriers loop
            C.Free(CIMB);
        end loop;
    end Wait_Events;

    procedure Pipeline_Barrier
        (Command_Buffer: in Vulkan.Command_Buffer;
         Src_Stage_Mask, Dst_Stage_Mask: in Pipeline_Stage_Flags;
         Dependency_Flags: in Vulkan.Dependency_Flags;
         Memory_Barriers: in Memory_Barrier_Vectors.Vector
            := Memory_Barrier_Vectors.Empty_Vector;
         Buffer_Memory_Barriers: in Buffer_Memory_Barrier_Vectors.Vector
            := Buffer_Memory_Barrier_Vectors.Empty_Vector;
         Image_Memory_Barriers: in Image_Memory_Barrier_Vectors.Vector
            := Image_Memory_Barrier_Vectors.Empty_Vector) is
        C_Memory_Barriers: array (1 .. Natural(Memory_Barriers.Length))
                            of aliased C.Memory_Barrier_C
            with Convention => C;
        C_Buffer_Memory_Barriers:
            array (1 .. Natural(Buffer_Memory_Barriers.Length))
                of aliased C.Buffer_Memory_Barrier_C
            with Convention => C;
        C_Image_Memory_Barriers:
            array (1 .. Natural(Image_Memory_Barriers.Length))
                of aliased C.Image_Memory_Barrier_C
            with Convention => C;
    begin
        for X in C_Memory_Barriers'Range loop
            C_Memory_Barriers(X) := C.To_C(Memory_Barriers(X));
        end loop;

        for X in C_Buffer_Memory_Barriers'Range loop
            C_Buffer_Memory_Barriers(X) := C.To_C(Buffer_Memory_Barriers(X));
        end loop;

        for X in C_Image_Memory_Barriers'Range loop
            C_Image_Memory_Barriers(X) := C.To_C(Image_Memory_Barriers(X));
        end loop;

        C.vkCmdPipelineBarrier
            (Command_Buffer,
             Src_Stage_Mask,
             Dst_Stage_Mask,
             Dependency_Flags,
             Interfaces.Unsigned_32(Memory_Barriers.Length),
             (if C_Memory_Barriers'Length /= 0 then
                C_Memory_Barriers(1)'Access else null),
             Interfaces.Unsigned_32(Buffer_Memory_Barriers.Length),
             (if C_Buffer_Memory_Barriers'Length /= 0 then
                C_Buffer_Memory_Barriers(1)'Access else null),
             Interfaces.Unsigned_32(Image_Memory_Barriers.Length),
             (if C_Image_Memory_Barriers'Length /= 0 then
                C_Image_Memory_Barriers(1)'Access else null));

        for CMB of C_Memory_Barriers loop
            C.Free(CMB);
        end loop;

        for CBMB of C_Buffer_Memory_Barriers loop
            C.Free(CBMB);
        end loop;

        for CIMB of C_Image_Memory_Barriers loop
            C.Free(CIMB);
        end loop;
    end Pipeline_Barrier;

    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Layout: in Pipeline_Layout;
                   Stage_Flags: in Shader_Stage_Flags;
                   Offset, Size: in Interfaces.Unsigned_32;
                   Values: in Interfaces.C.Extensions.void_ptr) is
    begin
        C.vkCmdPushConstants(Command_Buffer,
                             Layout,
                             Stage_Flags,
                             Offset,
                             Size,
                             Values);
    end Push;

    procedure Begin_Render_Pass(Command_Buffer: in Vulkan.Command_Buffer;
                                Render_Pass_Begin: in Render_Pass_Begin_Info;
                                Contents: in Subpass_Contents) is
        RPBIC: C.Render_Pass_Begin_Info_C := C.To_C(Render_Pass_Begin);
    begin
        C.vkCmdBeginRenderPass(Command_Buffer,
                               RPBIC,
                               Contents);
        C.Free(RPBIC);
    end Begin_Render_Pass;

    procedure Execute(Command_Buffer: in Vulkan.Command_Buffer;
                      Command_Buffers: in Command_Buffer_Vectors.Vector) is
        C_Command_Buffers: array (1 .. Positive(Command_Buffers.Length))
                            of aliased Vulkan.Command_Buffer
            with Convention => C;
    begin
        for X in C_Command_Buffers'Range loop
            C_Command_Buffers(X) := Command_Buffers(X);
        end loop;

        C.vkCmdExecuteCommands(Command_Buffer,
                               Interfaces.Unsigned_32(Command_Buffers.Length),
                               C_Command_Buffers(1)'Access);
    end Execute;
    
    procedure Execute(Command_Buffer,
                      Secondary_Buffer: in Vulkan.Command_Buffer) is
    begin
        Execute(Command_Buffer,
                Command_Buffer_Vectors.To_Vector(Secondary_Buffer, 1));
    end Execute;

    procedure Set_Device_Mask(Command_Buffer: in Vulkan.Command_Buffer;
                              Device_Mask: in Interfaces.Unsigned_32) is
    begin
        C_V1_1.vkCmdSetDeviceMask(Command_Buffer, Device_Mask);
    end Set_Device_Mask;

    procedure Dispatch_Base(Command_Buffer: in Vulkan.Command_Buffer;
                            Base_Group_X,
                            Base_Group_Y,
                            Base_Group_Z,
                            Group_Count_X,
                            Group_Count_Y,
                            Group_Count_Z: in Interfaces.Unsigned_32) is
    begin
        C_V1_1.vkCmdDispatchBase(Command_Buffer,
                                 Base_Group_X,
                                 Base_Group_Y,
                                 Base_Group_Z,
                                 Group_Count_X,
                                 Group_COunt_Y,
                                 Group_Count_Z);
    end Dispatch_Base;

    procedure Draw_Indirect_Count(Command_Buffer: in Vulkan.Command_Buffer;
                                  Buffer: in Vulkan.Buffer;
                                  Offset: in Device_Size;
                                  Count_Buffer: in Vulkan.Buffer;
                                  Count_Buffer_Offset: in Device_Size;
                                  Max_Draw_Count,
                                  Stride: in Interfaces.Unsigned_32) is
    begin
        C_V1_2.vkCmdDrawIndirectCount(Command_Buffer,
                                      Buffer,
                                      Offset,
                                      Count_Buffer,
                                      Count_Buffer_Offset,
                                      Max_Draw_Count,
                                      Stride);
    end Draw_Indirect_Count;

    procedure Draw_Indexed_Indirect_Count
        (Command_Buffer: in Vulkan.Command_Buffer;
         Buffer: in Vulkan.Buffer;
         Offset: in Device_Size;
         Count_Buffer: in Vulkan.Buffer;
         Count_Buffer_Offset: in Device_Size;
         Max_Draw_Count,
         Stride: in Interfaces.Unsigned_32) is
    begin
        C_V1_2.vkCmdDrawIndexedIndirectCount(Command_Buffer,
                                             Buffer,
                                             Offset,
                                             Count_Buffer,
                                             Count_Buffer_Offset,
                                             Max_Draw_Count,
                                             Stride);
    end Draw_Indexed_Indirect_Count;
    
    procedure Begin_Render_Pass
        (Command_Buffer: in Vulkan.Command_Buffer;
         Render_Pass_Begin: in Render_Pass_Begin_Info;
         Subpass_Begin_Info: in Vulkan.Subpass_Begin_Info) is
        Render_Pass_Begin_C: C.Render_Pass_Begin_Info_C :=
            C.To_C(Render_Pass_Begin);
        Subpass_Begin_Info_C: C_V1_2.Subpass_Begin_Info_C :=
            C_V1_2.To_C(Subpass_Begin_Info);
    begin
        C_V1_2.vkCmdBeginRenderPass2(Command_Buffer,
                                     Render_Pass_Begin_C,
                                     Subpass_Begin_Info_C);
        C.Free(Render_Pass_Begin_C);
        C_V1_2.Free(Subpass_Begin_Info_C);
    end Begin_Render_Pass;
    
    procedure Next_Subpass(Command_Buffer: in Vulkan.Command_Buffer;
                           Subpass_Begin_Info: in Vulkan.Subpass_Begin_Info;
                           Subpass_End_Info: in Vulkan.Subpass_End_Info) is
        Subpass_Begin_Info_C: C_V1_2.Subpass_Begin_Info_C :=
            C_V1_2.To_C(Subpass_Begin_Info);
        Subpass_End_Info_C: C_V1_2.Subpass_End_Info_C :=
            C_V1_2.To_C(Subpass_End_Info);
    begin
        C_V1_2.vkCmdNextSubpass2(Command_buffer,
                                 Subpass_Begin_Info_C,
                                 Subpass_End_Info_C);
        C_V1_2.Free(Subpass_Begin_Info_C);
        C_V1_2.Free(Subpass_End_Info_C);
    end Next_Subpass;
    
    procedure Set_Event(Command_Buffer: in Vulkan.Command_Buffer;
                        Event: in Vulkan.Event;
                        Dependency_Info: in Vulkan.Dependency_Info) is
        Dependency_Info_C: C_V1_3.Dependency_Info_C :=
            C_V1_3.To_C(Dependency_Info);
    begin
        C_V1_3.vkCmdSetEvent2(Command_Buffer, Event, Dependency_Info_C);
        C_V1_3.Free(Dependency_Info_C);
    end Set_Event;

    procedure Reset_Event(Command_Buffer: in Vulkan.Command_Buffer;
                          Event: in Vulkan.Event;
                          Stage_Mask: in Pipeline_Stage_Flags_2) is
    begin
        C_V1_3.vkCmdResetEvent2(Command_Buffer, Event, Stage_Mask);
    end Reset_Event;
    
    procedure Wait_Events
        (Command_Buffer: in Vulkan.Command_Buffer;
         Events: in Event_Vectors.Vector;
         Dependency_Infos: in Dependency_Info_Vectors.Vector) is
        Events_C: array (1 .. Positive(Events.Length)) of aliased Event;
        Dependency_Infos_C: array (Events_C'Range) of
            aliased C_V1_3.Dependency_Info_C;
    begin
        for X in Events_C'Range loop
            Events_C(X) := Events(X);
            Dependency_Infos_C(X) := C_V1_3.To_C(Dependency_Infos(X));
        end loop;

        C_V1_3.vkCmdWaitEvents2(Command_Buffer,
                                Interfaces.Unsigned_32(Events.Length),
                                Events_C(1)'Access,
                                Dependency_Infos_C(1)'Access);

        for DIC of Dependency_Infos_C loop
            C_V1_3.Free(DIC);
        end loop;
    end Wait_Events;
    
    procedure Wait_Event(Command_Buffer: in Vulkan.Command_Buffer;
                         Event: in Vulkan.Event;
                         Dependency_Info: in Vulkan.Dependency_Info) is
    begin
        Wait_Events(Command_Buffer,
                    Event_Vectors.To_Vector(Event, 1),
                    Dependency_Info_Vectors.To_Vector(Dependency_Info, 1));
    end Wait_Event;
    
    procedure Write_Timestamp(Command_Buffer: in Vulkan.Command_Buffer;
                              Stage: in Pipeline_Stage_Flags_2;
                              Query_Pool: in Vulkan.Query_Pool;
                              Query: in Interfaces.Unsigned_32) is
    begin
        C_V1_3.vkCmdWriteTimestamp2(Command_Buffer, Stage, Query_Pool, Query);
    end Write_Timestamp;
    
    procedure End_Rendering(Command_Buffer: in Vulkan.Command_Buffer) is
    begin
        C_V1_3.vkCmdEndRendering(Command_Buffer);
    end End_Rendering;

    procedure Set_Viewport(Command_Buffer: in Vulkan.Command_Buffer;
                           Viewports: in Viewport_Vectors.Vector) is
        C_Viewports: array (1 .. Positive(Viewports.Length))
            of aliased Viewport;
    begin
        for X in C_Viewports'Range loop
            C_Viewports(X) := Viewports(X);
        end loop;

        C_V1_3.vkCmdSetViewportWithCount(Command_Buffer,
                                         C_Viewports'Length,
                                         C_Viewports(1)'Access);
    end Set_Viewport;

    procedure Set_Viewport(Command_Buffer: in Vulkan.Command_Buffer;
                           Viewport: in Vulkan.Viewport) is
    begin
        Set_Viewport(Command_Buffer, Viewport_Vectors.To_Vector(Viewport, 1));
    end Set_Viewport;

    procedure Set_Scissor(Command_Buffer: in Vulkan.Command_Buffer;
                          Scissors: in Rect_2D_Vectors.Vector) is
        C_Scissors: array (1 .. Positive(Scissors.Length)) of aliased Rect_2D;
    begin
        for X in C_Scissors'Range loop
            C_Scissors(X) := Scissors(X);
        end loop;

        C_V1_3.vkCmdSetScissorWithCount(Command_Buffer,
                                        C_Scissors'Length,
                                        C_Scissors(1)'Access);
    end Set_Scissor;

    procedure Set_Scissor(Command_Buffer: in Vulkan.Command_Buffer;
                          Scissor: in Rect_2D) is
    begin
        Set_Scissor(Command_Buffer, Rect_2D_Vectors.To_Vector(Scissor, 1));
    end Set_Scissor;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   First_Binding: in Interfaces.Unsigned_32;
                   Buffers: in Buffer_Vectors.Vector;
                   Offsets: in Device_Size_Vectors.Vector;
                   Sizes, Strides: in Device_Size_Vectors.Vector :=
                       Device_Size_Vectors.Empty_Vector) is
        C_Buffers: array (1 .. Positive(Buffers.Length)) of aliased Buffer;
        C_Offsets: array (1 .. Positive(Offsets.Length)) of aliased Device_Size;
        C_Sizes: array (1 .. Positive(Sizes.Length)) of aliased Device_Size;
        C_Strides: array (1 .. Positive(Strides.Length)) of aliased Device_Size;
    begin
        for X in C_Buffers'Range loop
            C_Buffers(X) := Buffers(X);
            C_Offsets(X) := Offsets(X);
        end loop;

        for X in C_Sizes'Range loop
            C_Sizes(X) := Sizes(X);
        end loop;

        for X in C_Strides'Range loop
            C_Strides(X) := Strides(X);
        end loop;

        C_V1_3.vkCmdBindVertexBuffers2(Command_Buffer,
                                       First_Binding,
                                       C_Buffers'Length,
                                       C_Buffers(1)'Access,
                                       C_Offsets(1)'Access,
                                       (if Sizes.Is_Empty then null
                                            else C_Sizes(1)'Access),
                                       (if Strides.Is_Empty then null
                                            else C_Strides(1)'Access));
    end Bind;
    
    procedure Set_Stencil_Op(Command_Buffer: in Vulkan.Command_Buffer;
                             Face_Mask: in Stencil_Face_Flags;
                             Fail_Op, Pass_Op, Depth_Fail_Op: in Stencil_Op;
                             Compare_Op: Vulkan.Compare_Op) is
    begin
        C_V1_3.vkCmdSetStencilOp(Command_Buffer,
                                 Face_Mask,
                                 Fail_Op,
                                 Pass_Op,
                                 Depth_Fail_Op,
                                 Compare_Op);
    end Set_Stencil_Op;

    procedure Set_Line_Stipple
        (Command_Buffer: in Vulkan.Command_Buffer;
         Line_Stipple_Factor: in Interfaces.Unsigned_32;
         Line_Stipple_Pattern: in Interfaces.Unsigned_16) is
    begin
        C_V1_4.vkCmdSetLineStipple(Command_Buffer,
                                   Line_Stipple_Factor,
                                   Line_Stipple_Pattern);
    end Set_Line_Stipple;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Buffer: in Vulkan.Buffer;
                   Offset, Size: in Device_Size;
                   Index_Type: in Vulkan.Index_Type) is
    begin
        C_V1_4.vkCmdBindIndexBuffer2(Command_Buffer,
                                     Buffer,
                                     Offset,
                                     Size,
                                     Index_Type);
    end Bind;

    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Bind_Point: in Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   Set: in Interfaces.Unsigned_32;
                   Descriptor_Writes: in Write_Descriptor_Set_Vectors.Vector) is
        C_Descriptor_Writes: array (1 .. Positive(Descriptor_Writes.Length))
            of aliased C.Write_Descriptor_Set_C;
    begin
        for X in C_Descriptor_Writes'Range loop
            C_Descriptor_Writes(X) := C.To_C(Descriptor_Writes(X));
        end loop;

        C_V1_4.vkCmdPushDescriptorSet(Command_Buffer,
                                      Bind_Point,
                                      Layout,
                                      Set,
                                      C_Descriptor_Writes'Length,
                                      C_Descriptor_Writes(1)'Access);

        for Write of C_Descriptor_Writes loop
            C.Free(Write);
        end loop;
    end Push;

    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Bind_Point: in Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   Set: in Interfaces.Unsigned_32;
                   Descriptor_Write: in Write_Descriptor_Set) is
    begin
        Push(Command_Buffer,
             Bind_Point,
             Layout,
             Set,
             Write_Descriptor_Set_Vectors.To_Vector(Descriptor_Write, 1));
    end Push;
    
    procedure Push
        (Command_Buffer: in Vulkan.Command_Buffer;
         Descriptor_Update_Template: in Vulkan.Descriptor_Update_Template;
         Layout: in Pipeline_Layout;
         Set: in Interfaces.Unsigned_32;
         Data: in Interfaces.C.Extensions.void_ptr) is
    begin
        C_V1_4.vkCmdPushDescriptorSetWithTemplate(Command_Buffer,
                                                  Descriptor_Update_Template,
                                                  Layout,
                                                  Set,
                                                  Data);
    end Push;

    procedure Set_Rendering_Attachment_Locations
        (Command_Buffer: in Vulkan.Command_Buffer;
         Location_Info: in Rendering_Attachment_Location_Info) is
        C_Info: C_V1_4.Rendering_Attachment_Location_Info_C :=
            C_V1_4.To_C(Location_Info);
    begin
        C_V1_4.vkCmdSetRenderingAttachmentLocations(Command_Buffer, C_Info);
        C_V1_4.Free(C_Info);
    end Set_Rendering_Attachment_Locations;

    procedure Set_Rendering_Input_Attachment_Indices
        (Command_Buffer: in Vulkan.Command_Buffer;
         Input_Attachment_Index_Info:
            in Rendering_Input_Attachment_Index_Info) is
        C_Info: C_V1_4.Rendering_Input_Attachment_Index_Info_C :=
            C_V1_4.To_C(Input_Attachment_Index_Info);
    begin
        C_V1_4.vkCmdSetRenderingInputAttachmentIndices(Command_Buffer, C_Info);
        C_V1_4.Free(C_Info);
    end Set_Rendering_Input_Attachment_Indices;

    procedure Bind
        (Command_Buffer: in Vulkan.Command_Buffer;
         Bind_Descriptor_Sets_Info: in Vulkan.Bind_Descriptor_Sets_Info) is
        Info_C: C_V1_4.Bind_Descriptor_Sets_Info_C :=
            C_V1_4.To_C(Bind_Descriptor_Sets_Info);
    begin
        c_V1_4.vkCmdBindDescriptorSets2(Command_Buffer, Info_C);
        C_V1_4.Free(Info_C);
    end Bind;

    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Push_Constants_Info: in Vulkan.Push_Constants_Info) is
        Info_C: C_V1_4.Push_Constants_Info_C :=
            C_V1_4.To_C(Push_Constants_Info);
    begin
        C_V1_4.vkCmdPushConstants2(Command_Buffer, Info_C);
        C_V1_4.Free(Info_C);
    end Push;

    procedure Push
        (Command_Buffer: in Vulkan.Command_Buffer;
         Push_Descriptor_Set_Info: in Vulkan.Push_Descriptor_Set_Info) is
        Info_C: C_V1_4.Push_Descriptor_Set_Info_C :=
            C_V1_4.To_C(Push_Descriptor_Set_Info);
    begin
        C_V1_4.vkCmdPushDescriptorSet2(Command_Buffer, Info_C);
        C_V1_4.Free(Info_C);
    end Push;

    procedure Push
        (Command_Buffer: in Vulkan.Command_Buffer;
         Push_Descriptor_Set_With_Template_Info:
            in Vulkan.Push_Descriptor_Set_With_Template_Info) is
        Info_C: C_V1_4.Push_Descriptor_Set_With_Template_Info_C :=
            C_V1_4.To_C(Push_Descriptor_Set_With_Template_Info);
    begin
        C_V1_4.vkCmdPushDescriptorSetWithTemplate2(Command_Buffer, Info_C);
        C_V1_4.Free(Info_C);
    end Push;
end Vulkan.Commands;


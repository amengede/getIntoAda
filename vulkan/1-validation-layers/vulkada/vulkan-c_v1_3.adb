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

-- Subprogram access for Vulkan 1.3

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Vulkan.Utilities;
with Vulkan.Extension_Records;
with Vulkan.Core;

package body Vulkan.C_V1_3 is
    -- Wrapper for deallocation a union.
    procedure Release(E: in out Arrayed_Rendering_Attachment_Info_C);

    procedure Load(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceToolProperties_Access);
        procedure Load is new Load_Pointer(vkCreatePrivateDataSlot_Access);
        procedure Load is new Load_Pointer(vkDestroyPrivateDataSlot_Access);
        procedure Load is new Load_Pointer(vkSetPrivateData_Access);
        procedure Load is new Load_Pointer(vkGetPrivateData_Access);
        procedure Load is new Load_Pointer(vkCmdSetEvent2_Access);
        procedure Load is new Load_Pointer(vkCmdResetEvent2_Access);
        procedure Load is new Load_Pointer(vkCmdWaitEvents2_Access);
        procedure Load is new Load_Pointer(vkCmdPipelineBarrier2_Access);
        procedure Load is new Load_Pointer(vkCmdWriteTimestamp2_Access);
        procedure Load is new Load_Pointer(vkQueueSubmit2_Access);
        procedure Load is new Load_Pointer(vkCmdCopyBuffer2_Access);
        procedure Load is new Load_Pointer(vkCmdCopyImage2_Access);
        procedure Load is new Load_Pointer(vkCmdCopyBufferToImage2_Access);
        procedure Load is new Load_Pointer(vkCmdCopyImageToBuffer2_Access);
        procedure Load is new Load_Pointer(vkCmdBlitImage2_Access);
        procedure Load is new Load_Pointer(vkCmdResolveImage2_Access);
        procedure Load is new Load_Pointer(vkCmdBeginRendering_Access);
        procedure Load is new Load_Pointer(vkCmdEndRendering_Access);
        procedure Load is new Load_Pointer(vkCmdSetCullMode_Access);
        procedure Load is new Load_Pointer(vkCmdSetFrontFace_Access);
        procedure Load is new Load_Pointer(vkCmdSetPrimitiveTopology_Access);
        procedure Load is new Load_Pointer(vkCmdSetViewportWithCount_Access);
        procedure Load is new Load_Pointer(vkCmdSetScissorWithCount_Access);
        procedure Load is new Load_Pointer(vkCmdBindVertexBuffers2_Access);
        procedure Load is new Load_Pointer(vkCmdSetDepthTestEnable_Access);
        procedure Load is new Load_Pointer(vkCmdSetDepthWriteEnable_Access);
        procedure Load is new Load_Pointer(vkCmdSetStencilOp_Access);
        procedure Load is new Load_Pointer
            (vkCmdSetRasterizerDiscardEnable_Access);
        procedure Load is new Load_Pointer(vkCmdSetDepthBiasEnable_Access);
        procedure Load is new Load_Pointer
            (vkCmdSetPrimitiveRestartEnable_Access);
        procedure Load is new Load_Pointer
            (vkGetDeviceBufferMemoryRequirements_Access);
        procedure Load is new Load_Pointer
            (vkGetDeviceImageMemoryRequirements_Access);
        procedure Load is new Load_Pointer
            (vkGetDeviceImageSparseMemoryRequirements_Access);
    begin
        Load(vkGetPhysicalDeviceToolProperties,
             "vkGetPhysicalDeviceToolProperties");
        Load(vkCreatePrivateDataSlot, "vkCreatePrivateDataSlot");
        Load(vkDestroyPrivateDataSlot, "vkDestroyPrivateDataSlot");
        Load(vkSetPrivateData, "vkSetPrivateData");
        Load(vkGetPrivateData, "vkGetPrivateData");
        Load(vkCmdSetEvent2, "vkCmdSetEvent2");
        Load(vkCmdResetEvent2, "vkCmdResetEvent2");
        Load(vkCmdWaitEvents2, "vkCmdWaitEvents2");
        Load(vkCmdPipelineBarrier2, "vkCmdPipelineBarrier2");
        Load(vkCmdWriteTimestamp2, "vkCmdWriteTimestamp2");
        Load(vkQueueSubmit2, "vkQueueSubmit2");
        Load(vkCmdCopyBuffer2, "vkCmdCopyBuffer2");
        Load(vkCmdCopyImage2, "vkCmdCopyImage2");
        Load(vkCmdCopyBufferToImage2, "vkCmdCopyBufferToImage2");
        Load(vkCmdCopyImageToBuffer2, "vkCmdCopyImageToBuffer2");
        Load(vkCmdBlitImage2, "vkCmdBlitImage2");
        Load(vkCmdResolveImage2, "vkCmdResolveImage2");
        Load(vkCmdBeginRendering, "vkCmdBeginRendering");
        Load(vkCmdEndRendering, "vkCmdEndRendering");
        Load(vkCmdSetCullMode, "vkCmdSetCullMode");
        Load(vkCmdSetFrontFace, "vkCmdSetFrontFace");
        Load(vkCmdSetPrimitiveTopology, "vkCmdSetPrimitiveTopology");
        Load(vkCmdSetViewportWithCount, "vkCmdSetViewportWithCount");
        Load(vkCmdSetScissorWithCount, "vkCmdSetScissorWithCount");
        Load(vkCmdBindVertexBuffers2, "vkCmdBindVertexBuffers2");
        Load(vkCmdSetDepthTestEnable, "vkCmdSetDepthTestEnable");
        Load(vkCmdSetDepthWriteEnable, "vkCmdSetDepthWriteEnable");
        Load(vkCmdSetStencilOp, "vkCmdSetStencilOp");
        Load(vkCmdSetRasterizerDiscardEnable,
             "vkCmdSetRasterizerDiscardEnable");
        Load(vkCmdSetDepthBiasEnable, "vkCmdSetDepthBiasEnable");
        Load(vkCmdSetPrimitiveRestartEnable, "vkCmdSetPrimitiveRestartEnable");
        Load(vkGetDeviceBufferMemoryRequirements,
             "vkGetDeviceBufferMemoryRequirements");
        Load(vkGetDeviceImageMemoryRequirements,
             "vkGetDeviceImageMemoryRequirements");
        Load(vkGetDeviceImageSparseMemoryRequirements,
             "vkGetDeviceImageSparseMemoryRequirements");
    end Load;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_3_Features;
                     C_Struct: in Physical_Device_Vulkan_1_3_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Robust_Image_Access :=
            Utilities.To_Ada(C_Struct.Robust_Image_Access); 
        Ada_Struct.Inline_Uniform_Block :=
            Utilities.To_Ada(C_Struct.Inline_Uniform_Block);
        Ada_Struct.Descriptor_Binding_Inline_Uniform_Block_Update_After_Bind :=
            Utilities.To_Ada
           (C_Struct.Descriptor_Binding_Inline_Uniform_Block_Update_After_Bind);
        Ada_Struct.Pipeline_Creation_Cache_Control :=
            Utilities.To_Ada(C_Struct.Pipeline_Creation_Cache_Control);
        Ada_Struct.Private_Data := Utilities.To_Ada(C_Struct.Private_Data);
        Ada_Struct.Shader_Demote_To_Helper_Invocation :=
            Utilities.To_Ada(C_Struct.Shader_Demote_To_Helper_Invocation);
        Ada_Struct.Shader_Terminate_Invocation :=
            Utilities.To_Ada(C_Struct.Shader_Terminate_Invocation);
        Ada_Struct.Subgroup_Size_Control :=
            Utilities.To_Ada(C_Struct.Subgroup_Size_Control);
        Ada_Struct.Compute_Full_Subgroups :=
            Utilities.To_Ada(C_Struct.Compute_Full_Subgroups);
        Ada_Struct.Synchronization_2 :=
            Utilities.To_Ada(C_Struct. Synchronization_2);
        Ada_Struct.Texture_Compression_ASTC_HDR :=
            Utilities.To_Ada(C_Struct.Texture_Compression_ASTC_HDR);
        Ada_Struct.Shader_Zero_Initialize_Workgroup_Memory :=
            Utilities.To_Ada(C_Struct.Shader_Zero_Initialize_Workgroup_Memory);
        Ada_Struct.Dynamic_Rendering :=
            Utilities.To_Ada(C_Struct.Dynamic_Rendering);
        Ada_Struct.Shader_Integer_Dot_Product :=
            Utilities.To_Ada(C_Struct.Shader_Integer_Dot_Product);
        Ada_Struct.Maintenance_4 := Utilities.To_Ada(C_Struct.Maintenance_4);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_3_Properties;
                     C_Struct: in Physical_Device_Vulkan_1_3_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Min_Subgroup_Size := C_Struct.Min_Subgroup_Size;
        Ada_Struct.Max_Subgroup_Size := C_Struct.Max_Subgroup_Size;
        Ada_Struct.Max_Compute_Workgroup_Subgroups :=
            C_Struct.Max_Compute_Workgroup_Subgroups;
        Ada_Struct.Required_Subgroup_Size_Stages :=
            C_Struct.Required_Subgroup_Size_Stages;
        Ada_Struct.Max_Inline_Uniform_Block_Size :=
            C_Struct.Max_Inline_Uniform_Block_Size;
        Ada_Struct.Max_Per_Stage_Descriptor_Inline_Uniform_Blocks := 
            C_Struct.Max_Per_Stage_Descriptor_Inline_Uniform_Blocks;
        Ada_Struct.
            Max_Per_Stage_Descriptor_Update_After_Bind_Inline_Uniform_Blocks := 
                C_Struct.
               Max_Per_Stage_Descriptor_Update_After_Bind_Inline_Uniform_Blocks;
        Ada_Struct.Max_Descriptor_Set_Inline_Uniform_Blocks :=
            C_Struct.Max_Descriptor_Set_Inline_Uniform_Blocks;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Inline_Uniform_Blocks :=
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Inline_Uniform_Blocks;
        Ada_Struct.Max_Inline_Uniform_Total_Size :=
            C_Struct.Max_Inline_Uniform_Total_Size;
        Ada_Struct.Integer_Dot_Product_8Bit_Unsigned_Accelerated :=
           Utilities.To_Ada
               (C_Struct.Integer_Dot_Product_8Bit_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_8Bit_Signed_Accelerated :=
           Utilities.To_Ada
               (C_Struct.Integer_Dot_Product_8Bit_Signed_Accelerated);
        Ada_Struct.Integer_Dot_Product_8Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
               (C_Struct.Integer_Dot_Product_8Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.Integer_Dot_Product_4x8Bit_Packed_Unsigned_Accelerated :=
            Utilities.To_Ada
              (C_Struct.Integer_Dot_Product_4x8Bit_Packed_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_4x8Bit_Packed_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_4x8Bit_Packed_Signed_Accelerated);
        Ada_Struct.
            Integer_Dot_Product_4x8Bit_Packed_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
                Integer_Dot_Product_4x8Bit_Packed_Mixed_Signedness_Accelerated);
        Ada_Struct.Integer_Dot_Product_16Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_16Bit_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_16Bit_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_16Bit_Signed_Accelerated);
        Ada_Struct.Integer_Dot_Product_16Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
                    Integer_Dot_Product_16Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.Integer_Dot_Product_32Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_32Bit_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_32Bit_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_32Bit_Signed_Accelerated);
        Ada_Struct.Integer_Dot_Product_32Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
                    Integer_Dot_Product_32Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.Integer_Dot_Product_64Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_64Bit_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_64Bit_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_64Bit_Signed_Accelerated);
        Ada_Struct.Integer_Dot_Product_64Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
                    Integer_Dot_Product_64Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
         Integer_Dot_Product_Accumulating_Saturating_8Bit_Unsigned_Accelerated);
        Ada_Struct.
          Integer_Dot_Product_Accumulating_Saturating_8Bit_Signed_Accelerated :=
              Utilities.To_Ada
                  (C_Struct.
           Integer_Dot_Product_Accumulating_Saturating_8Bit_Signed_Accelerated);
        Ada_Struct.
Integer_Dot_Product_Accumulating_Saturating_8Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
 Integer_Dot_Product_Accumulating_Saturating_8Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.
  Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Unsigned_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Unsigned_Accelerated);
        Ada_Struct.
 Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
  Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Signed_Accelerated);
        Ada_Struct.
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Mixed_Signedness_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Mixed_Signedness_Accelerated);
        Ada_Struct.
       Integer_Dot_Product_Accumulating_Saturating_16Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Unsigned_Accelerated);
        Ada_Struct.
         Integer_Dot_Product_Accumulating_Saturating_16Bit_Signed_Accelerated :=
             Utilities.To_Ada
                 (C_Struct.
          Integer_Dot_Product_Accumulating_Saturating_16Bit_Signed_Accelerated);
        Ada_Struct.
  Integer_Dot_Product_Accumulating_Saturating_16Bit_Mixed_Signedness_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_16Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.
       Integer_Dot_Product_Accumulating_Saturating_32Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Unsigned_Accelerated);
        Ada_Struct.
         Integer_Dot_Product_Accumulating_Saturating_32Bit_Signed_Accelerated :=
             Utilities.To_Ada
                 (C_Struct.
          Integer_Dot_Product_Accumulating_Saturating_32Bit_Signed_Accelerated);
        Ada_Struct.
  Integer_Dot_Product_Accumulating_Saturating_32Bit_Mixed_Signedness_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_32Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.
       Integer_Dot_Product_Accumulating_Saturating_64Bit_Unsigned_Accelerated :=
           Utilities.To_Ada
               (C_Struct.
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Unsigned_Accelerated);
        Ada_Struct.
         Integer_Dot_Product_Accumulating_Saturating_64Bit_Signed_Accelerated :=
             Utilities.To_Ada
                 (C_Struct.
          Integer_Dot_Product_Accumulating_Saturating_64Bit_Signed_Accelerated);
        Ada_Struct.
  Integer_Dot_Product_Accumulating_Saturating_64Bit_Mixed_Signedness_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_64Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.Storage_Texel_Buffer_Offset_Alignment_Bytes :=
            C_Struct.Storage_Texel_Buffer_Offset_Alignment_Bytes;
        Ada_Struct.Storage_Texel_Buffer_Offset_Single_Texel_Alignment :=
            Utilities.To_Ada
                (C_Struct.Storage_Texel_Buffer_Offset_Single_Texel_Alignment);
        Ada_Struct.Uniform_Texel_Buffer_Offset_Alignment_Bytes :=
            C_Struct.Uniform_Texel_Buffer_Offset_Alignment_Bytes;
        Ada_Struct.Uniform_Texel_Buffer_Offset_Single_Texel_Alignment :=
            Utilities.To_Ada
                (C_Struct.Uniform_Texel_Buffer_Offset_Single_Texel_Alignment);
        Ada_Struct.Max_Buffer_Size := C_Struct.Max_Buffer_Size;
    end To_Ada;

    function To_C(Struct: in Pipeline_Creation_Feedback_Create_Info)
        return Pipeline_Creation_Feedback_Create_Info_C is
        PCFCIC: Pipeline_Creation_Feedback_Create_Info_C;
    begin
        PCFCIC.Next := Extension_Records.To_C(Struct.Next);
        PCFCIC.Pipeline_Creation_Feedback := Struct.Pipeline_Creation_Feedback;
        PCFCIC.Pipeline_Stage_Creation_Feedback_Count :=
            Struct.Pipeline_Stage_Creation_Feedback_Count;
        PCFCIC.Pipeline_Stage_Creation_Feedbacks :=
            Struct.Pipeline_Stage_Creation_Feedbacks;

        return PCFCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Creation_Feedback_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Shader_Terminate_Invocation_Features;
         C_Struct: in Physical_Device_Shader_Terminate_Invocation_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Terminate_Invocation :=
            Utilities.To_Ada(C_Struct.Shader_Terminate_Invocation);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Tool_Properties;
                     C_Struct: in Physical_Device_Tool_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Name, Interfaces.C.To_Ada(C_Struct.Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Version, Interfaces.C.To_Ada(C_Struct.Version));
        Ada_Struct.Purposes := C_Struct.Purposes;
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Description, Interfaces.C.To_Ada(C_Struct.Description));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Layer, Interfaces.C.To_Ada(C_Struct.Layer));
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Shader_Demote_To_Helper_Invocation_Features;
         C_Struct:
            in Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Demote_To_Helper_Invocation :=
            Utilities.To_Ada(C_Struct.Shader_Demote_To_Helper_Invocation);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Private_Data_Features;
                     C_Struct: in Physical_Device_Private_Data_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Private_Data := Utilities.To_Ada(C_Struct.Private_Data);
    end To_Ada;

    function To_C(Struct: in Device_Private_Data_Create_Info)
        return Device_Private_Data_Create_Info_C is
        DPDCIC: Device_Private_Data_Create_Info_C;
    begin
        DPDCIC.Next := Extension_Records.To_C(Struct.Next);
        DPDCIC.Private_Data_Slot_Request_Count :=
            Struct.Private_Data_Slot_Request_Count;

        return DPDCIC;
    end To_C;

    procedure Free(Struct: in out Device_Private_Data_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Private_Data_Slot_Create_Info)
        return Private_Data_Slot_Create_Info_C is
        PDSCIC: Private_Data_Slot_Create_Info_C;
    begin
        PDSCIC.Next := Extension_Records.To_C(Struct.Next);
        PDSCIC.Flags := Struct.Flags;

        return PDSCIC;
    end To_C;

    procedure Free(Struct: in out Private_Data_Slot_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Pipeline_Creation_Cache_Control_Features;
         C_Struct:
            in Physical_Device_Pipeline_Creation_Cache_Control_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Pipeline_Creation_Cache_Control :=
            Utilities.To_Ada(C_Struct.Pipeline_Creation_Cache_Control);
    end To_Ada;

    function To_C(Struct: in Memory_Barrier_2) return Memory_Barrier_2_C is
        MB2C: Memory_Barrier_2_C;
    begin
        MB2C.Next := Extension_Records.To_C(Struct.Next);
        MB2C.Src_Stage_Mask := Struct.Src_Stage_Mask;
        MB2C.Src_Access_Mask := Struct.Src_Access_Mask;
        MB2C.Dst_Stage_Mask := Struct.Src_Stage_Mask;
        MB2C.Dst_Access_Mask := Struct.Src_Access_Mask;

        return MB2C;
    end To_C;

    procedure Free(Struct: in out Memory_Barrier_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Buffer_Memory_Barrier_2)
        return Buffer_Memory_Barrier_2_C is
        BMB2C: Buffer_Memory_Barrier_2_C;
    begin
        BMB2C.Next := Extension_Records.To_C(Struct.Next);
        BMB2C.Src_Stage_Mask := Struct.Src_Stage_Mask;
        BMB2C.Src_Access_Mask := Struct.Src_Access_Mask;
        BMB2C.Dst_Stage_Mask := Struct.Src_Stage_Mask;
        BMB2C.Dst_Access_Mask := Struct.Src_Access_Mask;
        BMB2C.Src_Queue_Family_Index := Struct.Src_Queue_Family_Index;
        BMB2C.Dst_Queue_Family_Index := Struct.Dst_Queue_Family_Index;
        BMB2C.Buffer := Struct.Buffer;
        BMB2C.Offset := Struct.Offset;
        BMB2C.Size := Struct.Size;

        return BMB2C;
    end To_C;

    procedure Free(Struct: in out Buffer_Memory_Barrier_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Image_Memory_Barrier_2)
        return Image_Memory_Barrier_2_C is
        IMB2C: Image_Memory_Barrier_2_C;
    begin
        IMB2C.Next := Extension_Records.To_C(Struct.Next);
        IMB2C.Src_Stage_Mask := Struct.Src_Stage_Mask;
        IMB2C.Src_Access_Mask := Struct.Src_Access_Mask;
        IMB2C.Dst_Stage_Mask := Struct.Src_Stage_Mask;
        IMB2C.Dst_Access_Mask := Struct.Src_Access_Mask;
        IMB2C.Old_Layout := Struct.Old_Layout;
        IMB2C.New_Layout := Struct.New_Layout;
        IMB2C.Src_Queue_Family_Index := Struct.Src_Queue_Family_Index;
        IMB2C.Dst_Queue_Family_Index := Struct.Dst_Queue_Family_Index;
        IMB2C.Image := Struct.Image;
        IMB2C.Subresource_Range := Struct.Subresource_Range;

        return IMB2C;
    end To_C;

    procedure Free(Struct: in out Image_Memory_Barrier_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Dependency_Info) return Dependency_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Memory_Barrier_2_C_Arrays, Memory_Barrier_2_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Buffer_Memory_Barrier_2_C_Arrays, Buffer_Memory_Barrier_2_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Image_Memory_Barrier_2_C_Arrays, Image_Memory_Barrier_2_Vectors);

        DIC: Dependency_Info_C;
    begin
        DIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(DIC.Memory_Barrier_Count,
                   Struct.Memory_Barriers,
                   DIC.Memory_Barriers);
        To_C_Array(DIC.Buffer_Memory_Barrier_Count,
                   Struct.Buffer_Memory_Barriers,
                   DIC.Buffer_Memory_Barriers);
        To_C_Array(DIC.Image_Memory_Barrier_Count,
                   Struct.Image_Memory_Barriers,
                   DIC.Image_Memory_Barriers);

        return DIC;
    end To_C;

    procedure Free(Struct: in out Dependency_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Memory_Barrier_2_C_Arrays.Free(Struct.Memory_Barriers, Free'Access);
        Buffer_Memory_Barrier_2_C_Arrays.Free(Struct.Buffer_Memory_Barriers,
                                              Free'Access);
        Image_Memory_Barrier_2_C_Arrays.Free(Struct.Image_Memory_Barriers,
                                             Free'Access);
    end Free;

    function To_C(Struct: in Semaphore_Submit_Info)
        return Semaphore_Submit_Info_C is
        SSIC: Semaphore_Submit_Info_C;
    begin
        SSIC.Next := Extension_Records.To_C(Struct.Next);
        SSIC.Semaphore := Struct.Semaphore;
        SSIC.Value := Struct.Value;
        SSIC.Stage_Mask := Struct.Stage_Mask;
        SSIC.Device_Index := Struct.Device_Index;

        return SSIC;
    end To_C;

    procedure Free(Struct: in out Semaphore_Submit_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Command_Buffer_Submit_Info)
        return Command_Buffer_Submit_Info_C is
        CBSIC: Command_Buffer_Submit_Info_C;
    begin
        CBSIC.Next := Extension_Records.To_C(Struct.Next);
        CBSIC.Command_Buffer := Struct.Command_Buffer;
        CBSIC.Device_Mask := Struct.Device_Mask;

        return CBSIC;
    end To_C;

    procedure Free(Struct: in out Command_Buffer_Submit_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Submit_Info_2) return Submit_Info_2_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Semaphore_Submit_Info_C_Arrays, Semaphore_Submit_Info_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Command_Buffer_Submit_Info_C_Arrays,
             Command_Buffer_Submit_Info_Vectors);

        SI2C: Submit_Info_2_C;
    begin
        SI2C.Next := Extension_Records.To_C(Struct.Next);
        SI2C.Flags := Struct.Flags;
        To_C_Array(SI2C.Wait_Semaphore_Info_Count,
                   Struct.Wait_Semaphore_Infos,
                   SI2C.Wait_Semaphore_Infos);
        To_C_Array(SI2C.Command_Buffer_Info_Count,
                   Struct.Command_Buffer_Infos,
                   SI2C.Command_Buffer_Infos);
        To_C_Array(SI2C.Signal_Semaphore_Info_Count,
                   Struct.Signal_Semaphore_Infos,
                   SI2C.Signal_Semaphore_Infos);

        return SI2C;
    end To_C;

    procedure Free(Struct: in out Submit_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Semaphore_Submit_Info_C_Arrays.Free(Struct.Wait_Semaphore_Infos,
                                            Free'Access);
        Command_Buffer_Submit_Info_C_Arrays.Free(Struct.Command_Buffer_Infos,
                                                 Free'Access);
        Semaphore_Submit_Info_C_Arrays.Free(Struct.Signal_Semaphore_Infos,
                                            Free'Access);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Synchronization_2_Features;
         C_Struct: in Physical_Device_Synchronization_2_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Synchronization_2 :=
            Utilities.To_Ada(C_Struct.Synchronization_2);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Zero_Initialize_Workgroup_Memory_Features;
         C_Struct:
            in Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Zero_Initialize_Workgroup_Memory :=
            Utilities.To_Ada(C_Struct.Shader_Zero_Initialize_Workgroup_Memory);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Image_Robustness_Features;
         C_Struct: in Physical_Device_Image_Robustness_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Robust_Image_Access :=
            Utilities.To_Ada(C_Struct.Robust_Image_Access);
    end To_Ada;

    function To_C(Struct: in Buffer_Copy_2) return Buffer_Copy_2_C is
        BC2C: Buffer_Copy_2_C;
    begin
        BC2C.Next := Extension_Records.To_C(Struct.Next);
        BC2C.Src_Offset := Struct.Src_Offset;
        BC2C.Dst_Offset := Struct.Dst_Offset;
        BC2C.Size := Struct.Size;

        return BC2C;
    end To_C;

    procedure Free(Struct: in out Buffer_Copy_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Copy_Buffer_Info_2) return Copy_Buffer_Info_2_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Buffer_Copy_2_C_Arrays, Buffer_Copy_2_Vectors);

        CBI2C: Copy_Buffer_Info_2_C;
    begin
        CBI2C.Next := Extension_Records.To_C(Struct.Next);
        CBI2C.Src_Buffer := Struct.Src_Buffer;
        CBI2C.Dst_Buffer := Struct.Dst_Buffer;
        To_C_Array(CBI2C.Region_Count, Struct.Regions, CBI2C.Regions);

        return CBI2C;
    end To_C;

    procedure Free(Struct: in out Copy_Buffer_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Buffer_Copy_2_C_Arrays.Free(Struct.Regions, Free'Access);
    end Free;

    function To_C(Struct: in Image_Copy_2) return Image_Copy_2_C is
        IC2C: Image_Copy_2_C;
    begin
        IC2C.Next := Extension_Records.To_C(Struct.Next);
        IC2C.Src_Subresource := Struct.Src_Subresource;
        IC2C.Src_Offset := Struct.Src_Offset;
        IC2C.Dst_Subresource := Struct.Dst_Subresource;
        IC2C.Dst_Offset := Struct.Dst_Offset;
        IC2C.Extent := Struct.Extent;

        return IC2C;
    end To_C;

    procedure Free(Struct: in out Image_Copy_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Copy_Image_Info_2) return Copy_Image_Info_2_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Image_Copy_2_C_Arrays, Image_Copy_2_Vectors);

        CII2C: Copy_Image_Info_2_C;
    begin
        CII2C.Next := Extension_Records.To_C(Struct.Next);
        CII2C.Src_Image := Struct.Src_Image;
        CII2C.Src_Image_Layout := Struct.Src_Image_Layout;
        CII2C.Dst_Image := Struct.Dst_Image;
        CII2C.Dst_Image_Layout := Struct.Dst_Image_Layout;
        To_C_Array(CII2C.Region_Count, Struct.Regions, CII2C.Regions);

        return CII2C;
    end To_C;
        
    procedure Free(Struct: in out Copy_Image_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Image_Copy_2_C_Arrays.Free(Struct.Regions);
    end Free;

    function To_C(Struct: in Buffer_Image_Copy_2)
        return Buffer_Image_Copy_2_C is
        BIC2C: Buffer_Image_Copy_2_C;
    begin
        BIC2C.Next := Extension_Records.To_C(Struct.Next);
        BIC2C.Buffer_Offset := Struct.Buffer_Offset;
        BIC2C.Buffer_Row_Length := Struct.Buffer_Row_Length;
        BIC2C.Buffer_Image_Height := Struct.Buffer_Image_Height;
        BIC2C.Image_Subresource := Struct.Image_Subresource;
        BIC2C.Image_Offset := Struct.Image_Offset;
        BIC2C.Image_Extent := Struct.Image_Extent;
        
        return BIC2C;
    end To_C;

    procedure Free(Struct: in out Buffer_Image_Copy_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Copy_Buffer_To_Image_Info_2)
        return Copy_Buffer_To_Image_Info_2_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Buffer_Image_Copy_2_C_Arrays, Buffer_Image_Copy_2_Vectors);

        CBTII2C: Copy_Buffer_To_Image_Info_2_C;
    begin
        CBTII2C.Next := Extension_Records.To_C(Struct.Next);
        CBTII2C.Src_Buffer := Struct.Src_Buffer;
        CBTII2C.Dst_Image := Struct.Dst_Image;
        CBTII2C.Dst_Image_Layout := Struct.Dst_Image_Layout;
        To_C_Array(CBTII2C.Region_Count, Struct.Regions, CBTII2C.Regions);

        return CBTII2C;
    end To_C;

    procedure Free(Struct: in out Copy_Buffer_To_Image_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Buffer_Image_Copy_2_C_Arrays.Free(Struct.Regions);
    end Free;

    function To_C(Struct: in Copy_Image_To_Buffer_Info_2)
        return Copy_Image_To_Buffer_Info_2_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Buffer_Image_Copy_2_C_Arrays, Buffer_Image_Copy_2_Vectors);

        CITBI2C: Copy_Image_To_Buffer_Info_2_C;
    begin
        CITBI2C.Next := Extension_Records.To_C(Struct.Next);
        CITBI2C.Src_Image := Struct.Src_Image;
        CITBI2C.Src_Image_Layout := Struct.Src_Image_Layout;
        CITBI2C.Dst_Buffer := Struct.Dst_Buffer;
        To_C_Array(CITBI2C.Region_Count, Struct.Regions, CITBI2C.Regions);

        return CITBI2C;
    end To_C;

    procedure Free(Struct: in out Copy_Image_To_Buffer_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Buffer_Image_Copy_2_C_Arrays.Free(Struct.Regions);
    end Free;

    function To_C(Struct: in Blit_Image_Info_2) return Blit_Image_Info_2_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Image_Blit_2_C_Arrays, Image_Blit_2_Vectors);

        BII2C: Blit_Image_Info_2_C;
    begin
        BII2C.Next := Extension_Records.To_C(Struct.Next);
        BII2C.Src_Image := Struct.Src_Image;
        BII2C.Src_Image_Layout := Struct.Src_Image_Layout;
        BII2C.Dst_Image := Struct.Dst_Image;
        BII2C.Dst_Image_Layout := Struct.Dst_Image_Layout;
        To_C_Array(BII2C.Region_Count, Struct.Regions, BII2C.Regions);
        BII2C.Filter := Struct.Filter;

        return BII2C;
    end To_C;

    procedure Free(Struct: in out Blit_Image_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Image_Blit_2_C_Arrays.Free(Struct.Regions, Free'Access);
    end Free;

    function To_C(Struct: in Image_Blit_2) return Image_Blit_2_C is
        IB2C: Image_Blit_2_C;
    begin
        IB2C.Next := Extension_Records.To_C(Struct.Next);
        IB2C.Src_Subresource := Struct.Src_Subresource;
        IB2C.Src_Offsets := Struct.Src_Offsets;
        IB2C.Dst_Subresource := Struct.Dst_Subresource;
        IB2C.Dst_Offsets := Struct.Dst_Offsets;

        return IB2C;
    end To_C;
    
    procedure Free(Struct: in out Image_Blit_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Image_Resolve_2) return Image_Resolve_2_C is
        IR2C: Image_Resolve_2_C;
    begin
        IR2C.Next := Extension_Records.To_C(Struct.Next);
        IR2C.Src_Subresource := Struct.Src_Subresource;
        IR2C.Src_Offset := Struct.Src_Offset;
        IR2C.Dst_Subresource := Struct.Dst_Subresource;
        IR2C.Dst_Offset := Struct.Dst_Offset;
        IR2C.Extent := Struct.Extent;

        return IR2C;
    end To_C;

    procedure Free(Struct: in out Image_Resolve_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Resolve_Image_Info_2)
        return Resolve_Image_Info_2_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Image_Resolve_2_C_Arrays, Image_Resolve_2_Vectors);

        RII2C: Resolve_Image_Info_2_C;
    begin
        RII2C.Next := Extension_Records.To_C(Struct.Next);
        RII2C.Src_Image := Struct.Src_Image;
        RII2C.Src_Image_Layout := Struct.Src_Image_Layout;
        RII2C.Dst_Image := Struct.Dst_Image;
        RII2C.Dst_Image_Layout := Struct.Dst_Image_Layout;
        To_C_Array(RII2C.Region_Count, Struct.Regions, RII2C.Regions);

        return RII2C;
    end To_C;

    procedure Free(Struct: in out Resolve_Image_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Image_Resolve_2_C_Arrays.Free(Struct.Regions, Free'Access);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Subgroup_Size_Control_Features;
         C_Struct: in Physical_Device_Subgroup_Size_Control_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Subgroup_Size_Control :=
            Utilities.To_Ada(C_Struct.Subgroup_Size_Control);
        Ada_Struct.Compute_Full_Subgroups :=
            Utilities.To_Ada(C_Struct.Compute_Full_Subgroups);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Subgroup_Size_Control_Properties;
         C_Struct: in Physical_Device_Subgroup_Size_Control_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Min_Subgroup_Size := C_Struct.Min_Subgroup_Size;
        Ada_Struct.Max_Subgroup_Size := C_Struct.Max_Subgroup_Size;
        Ada_Struct.Max_Compute_Workgroup_Subgroups :=
            C_Struct.Max_Compute_Workgroup_Subgroups;
        Ada_Struct.Required_Subgroup_Size_Stages :=
            C_Struct.Required_Subgroup_Size_Stages;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info;
         C_Struct:
            in Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Required_Subgroup_Size := C_Struct.Required_Subgroup_Size;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Inline_Uniform_Block_Features;
         C_Struct: in Physical_Device_Inline_Uniform_Block_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Inline_Uniform_Block :=
            Utilities.To_Ada(C_Struct.Inline_Uniform_Block);
        Ada_Struct.Descriptor_Binding_Inline_Uniform_Block_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.
                    Descriptor_Binding_Inline_Uniform_Block_Update_After_Bind);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Inline_Uniform_Block_Properties;
         C_Struct: in Physical_Device_Inline_Uniform_Block_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Inline_Uniform_Block_Size :=
            C_Struct.Max_Inline_Uniform_Block_Size;
        Ada_Struct.Max_Per_Stage_Descriptor_Inline_Uniform_Blocks :=
            C_Struct.Max_Per_Stage_Descriptor_Inline_Uniform_Blocks;
        Ada_Struct.
            Max_Per_Stage_Descriptor_Update_After_Bind_Inline_Uniform_Blocks :=
            C_Struct.
               Max_Per_Stage_Descriptor_Update_After_Bind_Inline_Uniform_Blocks;
        Ada_Struct.Max_Descriptor_Set_Inline_Uniform_Blocks :=
            C_Struct.Max_Descriptor_Set_Inline_Uniform_Blocks;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Inline_Uniform_Blocks :=
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Inline_Uniform_Blocks;
    end To_Ada;

    function To_C(Struct: in Write_Descriptor_Set_Inline_Uniform_Block)
        return Write_Descriptor_Set_Inline_Uniform_Block_C is
        WDSIUBC: Write_Descriptor_Set_Inline_Uniform_Block_C;
    begin
        WDSIUBC.Next := Extension_Records.To_C(Struct.Next);
        WDSIUBC.Data_Size := Struct.Data_Size;
        WDSIUBC.Data := Struct.Data;

        return WDSIUBC;
    end To_C;

    procedure Free
        (Struct: in out Write_Descriptor_Set_Inline_Uniform_Block_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Descriptor_Pool_Inline_Uniform_Block_Create_Info)
        return Descriptor_Pool_Inline_Uniform_Block_Create_Info_C is
        DPIUBCIC: Descriptor_Pool_Inline_Uniform_Block_Create_Info_C;
    begin
        DPIUBCIC.Next := Extension_Records.To_C(Struct.Next);
        DPIUBCIC.Max_Inline_Uniform_Block_Bindings :=
            Struct.Max_Inline_Uniform_Block_Bindings;

        return DPIUBCIC;
    end To_C;

    procedure Free
        (Struct: in out Descriptor_Pool_Inline_Uniform_Block_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Texture_Compression_ASTC_HDR_Features;
         C_Struct:
            in Physical_Device_Texture_Compression_ASTC_HDR_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Texture_Compression_ASTC_HDR :=
            Utilities.To_Ada(C_Struct.Texture_Compression_ASTC_HDR);
    end To_Ada;

    function To_C(Struct: in Rendering_Attachment_Info)
        return Rendering_Attachment_Info_C is
        RAIC: Rendering_Attachment_Info_C(Struct.Clear_Type, Struct.Color_Type);
    begin
        RAIC.Next := Extension_Records.To_C(Struct.Next);
        RAIC.Image_View := Struct.Image_View;
        RAIC.Image_Layout := Struct.Image_Layout;
        RAIC.Resolve_Mode := Struct.Resolve_Mode;
        RAIC.Resolve_Image_View := Struct.Resolve_Image_View;
        RAIC.Resolve_Image_Layout := Struct.Resolve_Image_Layout;
        RAIC.Load_Op := Struct.Load_Op;
        RAIC.Store_Op := Struct.Store_Op;
        
        case Struct.Clear_Type is
            when Clear_Color =>
                RAIC.Color_Clear_Value := C.To_C(Struct.Clear_Value);
            when Clear_Depth_Stencil =>
                RAIC.Depth_Stencil_Clear_Value := C.To_C(Struct.Clear_Value);
        end case;

        return RAIC;
    end To_C;

    procedure Free(Struct: in out Rendering_Attachment_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Rendering_Info) return Rendering_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert_Indefinite
            (Rendering_Attachment_Info_C_Arrays,
             Rendering_Attachment_Info_Vectors);

        RIC: Rendering_Info_C;
    begin
        RIC.Next := Extension_Records.To_C(Struct.Next);
        RIC.Flags := Struct.Flags;
        RIC.Render_Area := Struct.Render_Area;
        RIC.Layer_Count := Struct.Layer_Count;
        RIC.View_Mask := Struct.View_Mask;
        To_C_Array(RIC.Color_Attachment_Count,
                   Struct.Color_Attachments,
                   RIC.Color_Attachments);

        if Struct.Depth_Attachment /= null then
            RIC.Depth_Attachment := new Rendering_Attachment_Info_C'
                (To_C(Struct.Depth_Attachment.all));
        end if;

        if Struct.Stencil_Attachment /= null then
            RIC.Stencil_Attachment := new Rendering_Attachment_Info_C'
                (To_C(Struct.Stencil_Attachment.all));
        end if;

        return RIC;
    end To_C;
        
    procedure Free(Struct: in out Rendering_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Rendering_Attachment_Info_C,
             Rendering_Attachment_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        Rendering_Attachment_Info_C_Arrays.Free(Struct.Color_Attachments,
                                                Release'Access);
        
        if Struct.Depth_Attachment /= null then
            Free(Struct.Depth_Attachment.all);
            Free(Struct.Depth_Attachment);
        end if;

        if Struct.Stencil_Attachment /= null then
            Free(Struct.Stencil_Attachment.all);
            Free(Struct.Stencil_Attachment);
        end if;
    end Free;

    function To_C(Struct: in Pipeline_Rendering_Create_Info)
        return Pipeline_Rendering_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C_V1_2.Format_Arrays,
                                                         Format_Vectors);

        PRCIC: Pipeline_Rendering_Create_Info_C;
    begin
        PRCIC.Next := Extension_Records.To_C(Struct.Next);
        PRCIC.View_Mask := Struct.View_Mask;
        To_C_Array(PRCIC.Color_Attachment_Count,
                   Struct.Color_Attachment_Formats,
                   PRCIC.Color_Attachment_Formats);
        PRCIC.Depth_Attachment_Format := Struct.Depth_Attachment_Format;
        PRCIC.Stencil_Attachment_Format := Struct.Stencil_Attachment_Format;

        return PRCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Rendering_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C_V1_2.Format_Arrays.Free(Struct.Color_Attachment_Formats);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Dynamic_Rendering_Features;
         C_Struct: in Physical_Device_Dynamic_Rendering_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Dynamic_Rendering :=
            Utilities.To_Ada(C_Struct.Dynamic_Rendering);
    end To_Ada;

    function To_C(Struct: in Command_Buffer_Inheritance_Rendering_Info)
        return Command_Buffer_Inheritance_Rendering_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C_V1_2.Format_Arrays,
                                                         Format_Vectors);

        CBIRIC: Command_Buffer_Inheritance_Rendering_Info_C;
    begin
        CBIRIC.Next := Extension_Records.To_C(Struct.Next);
        CBIRIC.Flags := Struct.Flags;
        CBIRIC.View_Mask := Struct.View_Mask;
        To_C_Array(CBIRIC.Color_Attachment_Count,
                   Struct.Color_Attachment_Formats,
                   CBIRIC.Color_Attachment_Formats);
        CBIRIC.Depth_Attachment_Format := Struct.Depth_Attachment_Format;
        CBIRIC.Stencil_Attachment_Format := Struct.Stencil_Attachment_Format;
        CBIRIC.Rasterization_Samples := Struct.Rasterization_Samples;

        return CBIRIC;
    end To_C;

    procedure Free
        (Struct: in out Command_Buffer_Inheritance_Rendering_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C_V1_2.Format_Arrays.Free(Struct.Color_Attachment_Formats);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Integer_Dot_Product_Features;
         C_Struct: in Physical_Device_Shader_Integer_Dot_Product_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Integer_Dot_Product :=
            Utilities.To_Ada(C_Struct.Shader_Integer_Dot_Product);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Shader_Integer_Dot_Product_Properties;
         C_Struct:
            in Physical_Device_Shader_Integer_Dot_Product_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Integer_Dot_Product_8Bit_Unsigned_Accelerated :=
           Utilities.To_Ada
               (C_Struct.Integer_Dot_Product_8Bit_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_8Bit_Signed_Accelerated :=
           Utilities.To_Ada
               (C_Struct.Integer_Dot_Product_8Bit_Signed_Accelerated);
        Ada_Struct.Integer_Dot_Product_8Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
               (C_Struct.Integer_Dot_Product_8Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.Integer_Dot_Product_4x8Bit_Packed_Unsigned_Accelerated :=
            Utilities.To_Ada
              (C_Struct.Integer_Dot_Product_4x8Bit_Packed_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_4x8Bit_Packed_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_4x8Bit_Packed_Signed_Accelerated);
        Ada_Struct.
            Integer_Dot_Product_4x8Bit_Packed_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
                Integer_Dot_Product_4x8Bit_Packed_Mixed_Signedness_Accelerated);
        Ada_Struct.Integer_Dot_Product_16Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_16Bit_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_16Bit_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_16Bit_Signed_Accelerated);
        Ada_Struct.Integer_Dot_Product_16Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
                    Integer_Dot_Product_16Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.Integer_Dot_Product_32Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_32Bit_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_32Bit_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_32Bit_Signed_Accelerated);
        Ada_Struct.Integer_Dot_Product_32Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
                    Integer_Dot_Product_32Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.Integer_Dot_Product_64Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_64Bit_Unsigned_Accelerated);
        Ada_Struct.Integer_Dot_Product_64Bit_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.Integer_Dot_Product_64Bit_Signed_Accelerated);
        Ada_Struct.Integer_Dot_Product_64Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
                    Integer_Dot_Product_64Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
         Integer_Dot_Product_Accumulating_Saturating_8Bit_Unsigned_Accelerated);
        Ada_Struct.
          Integer_Dot_Product_Accumulating_Saturating_8Bit_Signed_Accelerated :=
              Utilities.To_Ada
                  (C_Struct.
           Integer_Dot_Product_Accumulating_Saturating_8Bit_Signed_Accelerated);
        Ada_Struct.
Integer_Dot_Product_Accumulating_Saturating_8Bit_Mixed_Signedness_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
 Integer_Dot_Product_Accumulating_Saturating_8Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.
  Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Unsigned_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Unsigned_Accelerated);
        Ada_Struct.
 Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Signed_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
  Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Signed_Accelerated);
        Ada_Struct.
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Mixed_Signedness_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Mixed_Signedness_Accelerated);
        Ada_Struct.
       Integer_Dot_Product_Accumulating_Saturating_16Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Unsigned_Accelerated);
        Ada_Struct.
         Integer_Dot_Product_Accumulating_Saturating_16Bit_Signed_Accelerated :=
             Utilities.To_Ada
                 (C_Struct.
          Integer_Dot_Product_Accumulating_Saturating_16Bit_Signed_Accelerated);
        Ada_Struct.
  Integer_Dot_Product_Accumulating_Saturating_16Bit_Mixed_Signedness_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_16Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.
       Integer_Dot_Product_Accumulating_Saturating_32Bit_Unsigned_Accelerated :=
            Utilities.To_Ada
                (C_Struct.
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Unsigned_Accelerated);
        Ada_Struct.
         Integer_Dot_Product_Accumulating_Saturating_32Bit_Signed_Accelerated :=
             Utilities.To_Ada
                 (C_Struct.
          Integer_Dot_Product_Accumulating_Saturating_32Bit_Signed_Accelerated);
        Ada_Struct.
  Integer_Dot_Product_Accumulating_Saturating_32Bit_Mixed_Signedness_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_32Bit_Mixed_Signedness_Accelerated);
        Ada_Struct.
       Integer_Dot_Product_Accumulating_Saturating_64Bit_Unsigned_Accelerated :=
           Utilities.To_Ada
               (C_Struct.
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Unsigned_Accelerated);
        Ada_Struct.
         Integer_Dot_Product_Accumulating_Saturating_64Bit_Signed_Accelerated :=
             Utilities.To_Ada
                 (C_Struct.
          Integer_Dot_Product_Accumulating_Saturating_64Bit_Signed_Accelerated);
        Ada_Struct.
  Integer_Dot_Product_Accumulating_Saturating_64Bit_Mixed_Signedness_Accelerated
        :=
            Utilities.To_Ada
                (C_Struct.
Integer_Dot_Product_Accumulating_Saturating_64Bit_Mixed_Signedness_Accelerated);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Texel_Buffer_Alignment_Properties;
         C_Struct: in Physical_Device_Texel_Buffer_Alignment_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Storage_Texel_Buffer_Offset_Alignment_Bytes :=
            C_Struct.Storage_Texel_Buffer_Offset_Alignment_Bytes;
        Ada_Struct.Storage_Texel_Buffer_Offset_Single_Texel_Alignment :=
            Utilities.To_Ada
                (C_Struct.Storage_Texel_Buffer_Offset_Single_Texel_Alignment);
        Ada_Struct.Uniform_Texel_Buffer_Offset_Alignment_Bytes :=
            C_Struct.Uniform_Texel_Buffer_Offset_Alignment_Bytes;
        Ada_Struct.Uniform_Texel_Buffer_Offset_Single_Texel_Alignment :=
            Utilities.To_Ada
                (C_Struct.Uniform_Texel_Buffer_Offset_Single_Texel_Alignment);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Format_Properties_3;
                     C_Struct: in Format_Properties_3_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Linear_Tiling_Features := C_Struct.Linear_Tiling_Features;
        Ada_Struct.Optimal_Tiling_Features := C_Struct.Optimal_Tiling_Features;
        Ada_Struct.Buffer_Features := C_Struct.Buffer_Features;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Maintenance_4_Features;
                     C_Struct: in Physical_Device_Maintenance_4_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Maintenance_4 := Utilities.To_Ada(C_Struct.Maintenance_4);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_4_Properties;
         C_Struct: in Physical_Device_Maintenance_4_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Buffer_Size := C_Struct.Max_Buffer_Size;
    end To_Ada;

    function To_C(Struct: in Device_Buffer_Memory_Requirements)
        return Device_Buffer_Memory_Requirements_C is
        DBMRC: Device_Buffer_Memory_Requirements_C;
    begin
        DBMRC.Next := Extension_Records.To_C(Struct.Next);
        
        if Struct.Create_Info /= null then
            DBMRC.Create_Info := new C.Buffer_Create_Info_C'
                (C.To_C(Struct.Create_Info.all));
        end if;

        return DBMRC;
    end To_C;

    procedure Free(Struct: in out Device_Buffer_Memory_Requirements_C) is
        use type C.Buffer_Create_Info_C_Access;

        procedure Free is new Ada.Unchecked_Deallocation
            (C.Buffer_Create_Info_C, C.Buffer_Create_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Create_Info /= null then
            C.Free(Struct.Create_Info.all);
            Free(Struct.Create_Info);
        end if;
    end Free;

    function To_C(Struct: in Device_Image_Memory_Requirements)
        return Device_Image_Memory_Requirements_C is
        DIMRC: Device_Image_Memory_Requirements_C;
    begin
        DIMRC.Next := Extension_Records.To_C(Struct.Next);
        
        if Struct.Create_Info /= null then
            DIMRC.Create_Info := new C.Image_Create_Info_C'
                (C.To_C(Struct.Create_Info.all));
        end if;

        DIMRC.Plane_Aspect := Struct.Plane_Aspect;

        return DIMRC;
    end To_C;

    procedure Free(Struct: in out Device_Image_Memory_Requirements_C) is
        use type C.Image_Create_Info_C_Access;

        procedure Free is new Ada.Unchecked_Deallocation
            (C.Image_Create_Info_C, C.Image_Create_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Create_Info /= null then
            C.Free(Struct.Create_Info.all);
            Free(Struct.Create_Info);
        end if;
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Pipeline_Creation_Feedback_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Creation_Feedback_Create_Info,
                         Pipeline_Creation_Feedback_Create_Info_C,
                         Pipeline_Creation_Feedback_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Private_Data_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Private_Data_Create_Info,
                         Device_Private_Data_Create_Info_C,
                         Device_Private_Data_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Private_Data_Slot_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Private_Data_Slot_Create_Info,
                         Private_Data_Slot_Create_Info_C,
                         Private_Data_Slot_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Barrier_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_Barrier_2,
                         Memory_Barrier_2_C,
                         Memory_Barrier_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Memory_Barrier_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Memory_Barrier_2,
                         Buffer_Memory_Barrier_2_C,
                         Buffer_Memory_Barrier_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Memory_Barrier_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Memory_Barrier_2,
                         Image_Memory_Barrier_2_C,
                         Image_Memory_Barrier_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Dependency_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Dependency_Info,
                         Dependency_Info_C,
                         Dependency_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Semaphore_Submit_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Semaphore_Submit_Info,
                         Semaphore_Submit_Info_C,
                         Semaphore_Submit_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Command_Buffer_Submit_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Command_Buffer_Submit_Info,
                         Command_Buffer_Submit_Info_C,
                         Command_Buffer_Submit_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Submit_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Submit_Info_2,
                         Submit_Info_2_C,
                         Submit_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Copy_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Copy_2,
                         Buffer_Copy_2_C,
                         Buffer_Copy_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Copy_Buffer_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Copy_Buffer_Info_2,
                         Copy_Buffer_Info_2_C,
                         Copy_Buffer_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Copy_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Copy_2,
                         Image_Copy_2_C,
                         Image_Copy_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Copy_Image_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Copy_Image_Info_2,
                         Copy_Image_Info_2_C,
                         Copy_Image_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Image_Copy_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Image_Copy_2,
                         Buffer_Image_Copy_2_C,
                         Buffer_Image_Copy_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Copy_Buffer_To_Image_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Copy_Buffer_To_Image_Info_2,
                         Copy_Buffer_To_Image_Info_2_C,
                         Copy_Buffer_To_Image_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Copy_Image_To_Buffer_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Copy_Image_To_Buffer_Info_2,
                         Copy_Image_To_Buffer_Info_2_C,
                         Copy_Image_To_Buffer_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Blit_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Blit_2,
                         Image_Blit_2_C,
                         Image_Blit_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Blit_Image_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Blit_Image_Info_2,
                         Blit_Image_Info_2_C,
                         Blit_Image_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Resolve_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Resolve_2,
                         Image_Resolve_2_C,
                         Image_Resolve_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Resolve_Image_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Resolve_Image_Info_2,
                         Resolve_Image_Info_2_C,
                         Resolve_Image_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Write_Descriptor_Set_Inline_Uniform_Block_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Write_Descriptor_Set_Inline_Uniform_Block,
                         Write_Descriptor_Set_Inline_Uniform_Block_C,
                         Write_Descriptor_Set_Inline_Uniform_Block_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Descriptor_Pool_Inline_Uniform_Block_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                    (Descriptor_Pool_Inline_Uniform_Block_Create_Info,
                     Descriptor_Pool_Inline_Uniform_Block_Create_Info_C,
                     Descriptor_Pool_Inline_Uniform_Block_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Rendering_Attachment_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Rendering_Attachment_Info,
                         Rendering_Attachment_Info_C,
                         Rendering_Attachment_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Rendering_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Rendering_Info,
                         Rendering_Info_C,
                         Rendering_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Rendering_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Rendering_Create_Info,
                         Pipeline_Rendering_Create_Info_C,
                         Pipeline_Rendering_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Command_Buffer_Inheritance_Rendering_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Command_Buffer_Inheritance_Rendering_Info,
                         Command_Buffer_Inheritance_Rendering_Info_C,
                         Command_Buffer_Inheritance_Rendering_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Buffer_Memory_Requirements_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Buffer_Memory_Requirements,
                         Device_Buffer_Memory_Requirements_C,
                         Device_Buffer_Memory_Requirements_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Image_Memory_Requirements_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Image_Memory_Requirements,
                         Device_Image_Memory_Requirements_C,
                         Device_Image_Memory_Requirements_C_Access);
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
            when Physical_Device_Vulkan_1_3_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Vulkan_1_3_Features,
                         Physical_Device_Vulkan_1_3_Features_C,
                         Physical_Device_Vulkan_1_3_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_3_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Vulkan_1_3_Properties,
                         Physical_Device_Vulkan_1_3_Properties_C,
                         Physical_Device_Vulkan_1_3_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Terminate_Invocation_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                (Physical_Device_Shader_Terminate_Invocation_Features,
                 Physical_Device_Shader_Terminate_Invocation_Features_C,
                 Physical_Device_Shader_Terminate_Invocation_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Tool_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Tool_Properties,
                         Physical_Device_Tool_Properties_C,
                         Physical_Device_Tool_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when
             Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
         (Physical_Device_Shader_Demote_To_Helper_Invocation_Features,
          Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C,
          Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Private_Data_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Private_Data_Features,
                         Physical_Device_Private_Data_Features_C,
                         Physical_Device_Private_Data_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when
                Physical_Device_Pipeline_Creation_Cache_Control_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
            (Physical_Device_Pipeline_Creation_Cache_Control_Features,
             Physical_Device_Pipeline_Creation_Cache_Control_Features_C,
             Physical_Device_Pipeline_Creation_Cache_Control_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Synchronization_2_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Synchronization_2_Features,
                         Physical_Device_Synchronization_2_Features_C,
                         Physical_Device_Synchronization_2_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when
               Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
           (Physical_Device_Zero_Initialize_Workgroup_Memory_Features,
            Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C,
            Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Image_Robustness_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Image_Robustness_Features,
                         Physical_Device_Image_Robustness_Features_C,
                         Physical_Device_Image_Robustness_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Subgroup_Size_Control_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                      (Physical_Device_Subgroup_Size_Control_Features,
                       Physical_Device_Subgroup_Size_Control_Features_C,
                       Physical_Device_Subgroup_Size_Control_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Subgroup_Size_Control_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Physical_Device_Subgroup_Size_Control_Properties,
                     Physical_Device_Subgroup_Size_Control_Properties_C,
                     Physical_Device_Subgroup_Size_Control_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when
                Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
            (Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info,
             Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C,
             Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Inline_Uniform_Block_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Physical_Device_Inline_Uniform_Block_Features,
                        Physical_Device_Inline_Uniform_Block_Features_C,
                        Physical_Device_Inline_Uniform_Block_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Inline_Uniform_Block_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                     (Physical_Device_Inline_Uniform_Block_Properties,
                      Physical_Device_Inline_Uniform_Block_Properties_C,
                      Physical_Device_Inline_Uniform_Block_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Texture_Compression_ASTC_HDR_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
               (Physical_Device_Texture_Compression_ASTC_HDR_Features,
                Physical_Device_Texture_Compression_ASTC_HDR_Features_C,
                Physical_Device_Texture_Compression_ASTC_HDR_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Dynamic_Rendering_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Dynamic_Rendering_Features,
                         Physical_Device_Dynamic_Rendering_Features_C,
                         Physical_Device_Dynamic_Rendering_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Integer_Dot_Product_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                 (Physical_Device_Shader_Integer_Dot_Product_Features,
                  Physical_Device_Shader_Integer_Dot_Product_Features_C,
                  Physical_Device_Shader_Integer_Dot_Product_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Integer_Dot_Product_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
               (Physical_Device_Shader_Integer_Dot_Product_Properties,
                Physical_Device_Shader_Integer_Dot_Product_Properties_C,
                Physical_Device_Shader_Integer_Dot_Product_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Texel_Buffer_Alignment_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Physical_Device_Texel_Buffer_Alignment_Properties,
                    Physical_Device_Texel_Buffer_Alignment_Properties_C,
                    Physical_Device_Texel_Buffer_Alignment_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Format_Properties_3_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Format_Properties_3,
                         Format_Properties_3_C,
                         Format_Properties_3_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Maintenance_4_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_4_Features,
                         Physical_Device_Maintenance_4_Features_C,
                         Physical_Device_Maintenance_4_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Maintenance_4_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_4_Properties,
                         Physical_Device_Maintenance_4_Properties_C,
                         Physical_Device_Maintenance_4_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Vulkan_1_3_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Vulkan_1_3_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Vulkan_1_3_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Vulkan_1_3_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Vulkan_1_3_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Vulkan_1_3_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Shader_Terminate_Invocation_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                (C.Out_Structure_C_Access,
                 Physical_Device_Shader_Terminate_Invocation_Features_C_Access);
                begin
                    To_Ada
              (Physical_Device_Shader_Terminate_Invocation_Features(Ada_Struct),
               To_Access(Next).all);
                end;
            when Physical_Device_Tool_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Tool_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Tool_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when
             Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
         (C.Out_Structure_C_Access,
          Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C_Access);
                begin
                    To_Ada
       (Physical_Device_Shader_Demote_To_Helper_Invocation_Features(Ada_Struct),
        To_Access(Next).all);
                end;
            when Physical_Device_Private_Data_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Private_Data_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Private_Data_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when
                Physical_Device_Pipeline_Creation_Cache_Control_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
            (C.Out_Structure_C_Access,
             Physical_Device_Pipeline_Creation_Cache_Control_Features_C_Access);
                begin
                    To_Ada
          (Physical_Device_Pipeline_Creation_Cache_Control_Features(Ada_Struct),
           To_Access(Next).all);
                end;
            when Physical_Device_Synchronization_2_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Synchronization_2_Features_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Synchronization_2_Features(Ada_Struct),
                         To_Access(Next).all);
                end;
            when
               Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
           (C.Out_Structure_C_Access,
            Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C_Access);
                begin
                    To_Ada
         (Physical_Device_Zero_Initialize_Workgroup_Memory_Features(Ada_Struct),
          To_Access(Next).all);
                end;
            when Physical_Device_Image_Robustness_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Image_Robustness_Features_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Image_Robustness_Features(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Physical_Device_Subgroup_Size_Control_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Subgroup_Size_Control_Features_C_Access);
                begin
                    To_Ada
                    (Physical_Device_Subgroup_Size_Control_Features(Ada_Struct),
                     To_Access(Next).all);
                end;
            when Physical_Device_Subgroup_Size_Control_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                    (C.Out_Structure_C_Access,
                     Physical_Device_Subgroup_Size_Control_Properties_C_Access);
                begin
                    To_Ada
                  (Physical_Device_Subgroup_Size_Control_Properties(Ada_Struct),
                   To_Access(Next).all);
                end;
            when
                Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
            (C.Out_Structure_C_Access,
             Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C_Access);
                begin
                    To_Ada
          (Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info(Ada_Struct),
           To_Access(Next).all);
                end;
            when Physical_Device_Inline_Uniform_Block_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Inline_Uniform_Block_Features_C_Access);
                begin
                    To_Ada
                     (Physical_Device_Inline_Uniform_Block_Features(Ada_Struct),
                      To_Access(Next).all);
                end;
            when Physical_Device_Inline_Uniform_Block_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                     (C.Out_Structure_C_Access,
                      Physical_Device_Inline_Uniform_Block_Properties_C_Access);
                begin
                    To_Ada
                   (Physical_Device_Inline_Uniform_Block_Properties(Ada_Struct),
                    To_Access(Next).all);
                end;
            when Physical_Device_Texture_Compression_ASTC_HDR_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
               (C.Out_Structure_C_Access,
                Physical_Device_Texture_Compression_ASTC_HDR_Features_C_Access);
                begin
                    To_Ada
             (Physical_Device_Texture_Compression_ASTC_HDR_Features(Ada_Struct),
              To_Access(Next).all);
                end;
            when Physical_Device_Dynamic_Rendering_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Dynamic_Rendering_Features_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Dynamic_Rendering_Features(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Physical_Device_Shader_Integer_Dot_Product_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                 (C.Out_Structure_C_Access,
                  Physical_Device_Shader_Integer_Dot_Product_Features_C_Access);
                begin
                    To_Ada
               (Physical_Device_Shader_Integer_Dot_Product_Features(Ada_Struct),
                 To_Access(Next).all);
                end;
            when Physical_Device_Shader_Integer_Dot_Product_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
               (C.Out_Structure_C_Access,
                Physical_Device_Shader_Integer_Dot_Product_Properties_C_Access);
                begin
                    To_Ada
             (Physical_Device_Shader_Integer_Dot_Product_Properties(Ada_Struct),
               To_Access(Next).all);
                end;
            when Physical_Device_Texel_Buffer_Alignment_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                   (C.Out_Structure_C_Access,
                    Physical_Device_Texel_Buffer_Alignment_Properties_C_Access);
                begin
                    To_Ada
                 (Physical_Device_Texel_Buffer_Alignment_Properties(Ada_Struct),
                   To_Access(Next).all);
                end;
            when Format_Properties_3_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Format_Properties_3_C_Access);
                begin
                    To_Ada(Format_Properties_3(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Maintenance_4_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_4_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_4_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Maintenance_4_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_4_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_4_Properties(Ada_Struct),
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
            when Pipeline_Creation_Feedback_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Creation_Feedback_Create_Info_C,
                         Pipeline_Creation_Feedback_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Private_Data_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Private_Data_Create_Info_C,
                         Device_Private_Data_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Private_Data_Slot_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Private_Data_Slot_Create_Info_C,
                         Private_Data_Slot_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Barrier_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Memory_Barrier_2_C, Memory_Barrier_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Memory_Barrier_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Buffer_Memory_Barrier_2_C,
                         Buffer_Memory_Barrier_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Memory_Barrier_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Memory_Barrier_2_C,
                         Image_Memory_Barrier_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Dependency_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Dependency_Info_C, Dependency_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Semaphore_Submit_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Semaphore_Submit_Info_C,
                         Semaphore_Submit_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Command_Buffer_Submit_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Command_Buffer_Submit_Info_C,
                         Command_Buffer_Submit_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Submit_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Submit_Info_2_C, Submit_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Copy_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Buffer_Copy_2_C, Buffer_Copy_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Copy_Buffer_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Copy_Buffer_Info_2_C, Copy_Buffer_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Copy_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Copy_2_C, Image_Copy_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Copy_Image_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Copy_Image_Info_2_C, Copy_Image_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Image_Copy_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Buffer_Image_Copy_2_C, Buffer_Image_Copy_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Copy_Buffer_To_Image_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Copy_Buffer_To_Image_Info_2_C,
                         Copy_Buffer_To_Image_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Copy_Image_To_Buffer_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Copy_Image_To_Buffer_Info_2_C,
                         Copy_Image_To_Buffer_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Blit_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Blit_2_C, Image_Blit_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Blit_Image_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Blit_Image_Info_2_C, Blit_Image_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Resolve_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Resolve_2_C, Image_Resolve_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Resolve_Image_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Resolve_Image_Info_2_C, Resolve_Image_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Write_Descriptor_Set_Inline_Uniform_Block_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Write_Descriptor_Set_Inline_Uniform_Block_C,
                         Write_Descriptor_Set_Inline_Uniform_Block_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Descriptor_Pool_Inline_Uniform_Block_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Descriptor_Pool_Inline_Uniform_Block_Create_Info_C,
                     Descriptor_Pool_Inline_Uniform_Block_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Rendering_Attachment_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Rendering_Attachment_Info_C,
                         Rendering_Attachment_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Rendering_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Rendering_Info_C, Rendering_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Rendering_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Rendering_Create_Info_C,
                         Pipeline_Rendering_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Command_Buffer_Inheritance_Rendering_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Command_Buffer_Inheritance_Rendering_Info_C,
                         Command_Buffer_Inheritance_Rendering_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Buffer_Memory_Requirements_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Buffer_Memory_Requirements_C,
                         Device_Buffer_Memory_Requirements_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Image_Memory_Requirements_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Image_Memory_Requirements_C,
                         Device_Image_Memory_Requirements_C_Access);
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
            when Physical_Device_Vulkan_1_3_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Vulkan_1_3_Features_C,
                         Physical_Device_Vulkan_1_3_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_3_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Vulkan_1_3_Properties_C,
                         Physical_Device_Vulkan_1_3_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Terminate_Invocation_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                (Physical_Device_Shader_Terminate_Invocation_Features_C,
                 Physical_Device_Shader_Terminate_Invocation_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Tool_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Tool_Properties_C,
                         Physical_Device_Tool_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when
             Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
         (Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C,
          Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Private_Data_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Private_Data_Features_C,
                         Physical_Device_Private_Data_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when
                Physical_Device_Pipeline_Creation_Cache_Control_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
            (Physical_Device_Pipeline_Creation_Cache_Control_Features_C,
             Physical_Device_Pipeline_Creation_Cache_Control_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Synchronization_2_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Synchronization_2_Features_C,
                         Physical_Device_Synchronization_2_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when
               Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
           (Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C,
            Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Image_Robustness_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Image_Robustness_Features_C,
                         Physical_Device_Image_Robustness_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Subgroup_Size_Control_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Subgroup_Size_Control_Features_C,
                       Physical_Device_Subgroup_Size_Control_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Subgroup_Size_Control_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                    (Physical_Device_Subgroup_Size_Control_Properties_C,
                     Physical_Device_Subgroup_Size_Control_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when
                Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
            (Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C,
             Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Inline_Uniform_Block_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Physical_Device_Inline_Uniform_Block_Features_C,
                        Physical_Device_Inline_Uniform_Block_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Inline_Uniform_Block_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                     (Physical_Device_Inline_Uniform_Block_Properties_C,
                      Physical_Device_Inline_Uniform_Block_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Texture_Compression_ASTC_HDR_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
               (Physical_Device_Texture_Compression_ASTC_HDR_Features_C,
                Physical_Device_Texture_Compression_ASTC_HDR_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Dynamic_Rendering_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Dynamic_Rendering_Features_C,
                         Physical_Device_Dynamic_Rendering_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Integer_Dot_Product_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                 (Physical_Device_Shader_Integer_Dot_Product_Features_C,
                  Physical_Device_Shader_Integer_Dot_Product_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Integer_Dot_Product_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
               (Physical_Device_Shader_Integer_Dot_Product_Properties_C,
                Physical_Device_Shader_Integer_Dot_Product_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Texel_Buffer_Alignment_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                   (Physical_Device_Texel_Buffer_Alignment_Properties_C,
                    Physical_Device_Texel_Buffer_Alignment_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Format_Properties_3_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Format_Properties_3_C, Format_Properties_3_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Maintenance_4_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_4_Features_C,
                         Physical_Device_Maintenance_4_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Maintenance_4_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_4_Properties_C,
                         Physical_Device_Maintenance_4_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
        
    procedure Release(E: in out Arrayed_Rendering_Attachment_Info_C) is
    begin
        Free(Rendering_Attachment_Info_C(E));
    end Release;
end Vulkan.C_V1_3;


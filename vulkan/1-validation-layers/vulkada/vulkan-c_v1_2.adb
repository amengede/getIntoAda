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

-- Subprogram access for Vulkan 1.2

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Vulkan.Core;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.C_V1_2 is
    procedure Load(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdDrawIndirectCount_Access);
        procedure Load is
            new Load_Pointer(vkCmdDrawIndexedIndirectCount_Access);
        procedure Load is new Load_Pointer(vkCreateRenderPass2_Access);
        procedure Load is new Load_Pointer(vkCmdBeginRenderPass2_Access);
        procedure Load is new Load_Pointer(vkCmdNextSubpass2_Access);
        procedure Load is new Load_Pointer(vkCmdEndRenderPass2_Access);
        procedure Load is new Load_Pointer(vkResetQueryPool_Access);
        procedure Load is new Load_Pointer(vkGetSemaphoreCounterValue_Access);
        procedure Load is new Load_Pointer(vkWaitSemaphores_Access);
        procedure Load is new Load_Pointer(vkSignalSemaphore_Access);
        procedure Load is new Load_Pointer(vkGetBufferDeviceAddress_Access);
        procedure Load is new Load_Pointer
            (vkGetBufferOpaqueCaptureAddress_Access);
        procedure Load is new Load_Pointer
            (vkGetDeviceMemoryOpaqueCaptureAddress_Access);
    begin
        Load(vkCmdDrawIndirectCount, "vkCmdDrawIndirectCount");
        Load(vkCmdDrawIndexedIndirectCount, "vkCmdDrawIndexedIndirectCount");
        Load(vkCreateRenderPass2, "vkCreateRenderPass2");
        Load(vkCmdBeginRenderPass2, "vkCmdBeginRenderPass2");
        Load(vkCmdNextSubpass2, "vkCmdNextSubpass2");
        Load(vkCmdEndRenderPass2, "vkCmdEndRenderPass2");
        Load(vkResetQueryPool, "vkResetQueryPool");
        Load(vkGetSemaphoreCounterValue, "vkGetSemaphoreCounterValue");
        Load(vkWaitSemaphores, "vkWaitSemaphores");
        Load(vkSignalSemaphore, "vkSignalSemaphore");
        Load(vkGetBufferDeviceAddress, "vkGetBufferDeviceAddress");
        Load(vkGetBufferOpaqueCaptureAddress,
             "vkGetBufferOpaqueCaptureAddress");
        Load(vkGetDeviceMemoryOpaqueCaptureAddress,
             "vkGetDeviceMemoryOpaqueCaptureAddress");
    end Load;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_1_Features;
                     C_Struct: in Physical_Device_Vulkan_1_1_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Storage_Buffer_16Bit_Access :=
            Utilities.To_Ada(C_Struct.Storage_Buffer_16Bit_Access);
        Ada_Struct.Uniform_And_Storage_Buffer_16Bit_Access :=
            Utilities.To_Ada(C_Struct.Uniform_And_Storage_Buffer_16Bit_Access);
        Ada_Struct.Storage_Push_Constant_16 :=
            Utilities.To_Ada(C_Struct.Storage_Push_Constant_16);
        Ada_Struct.Storage_Input_Output_16 :=
            Utilities.To_Ada(C_Struct.Storage_Input_Output_16);
        Ada_Struct.Multiview := Utilities.To_Ada(C_Struct.Multiview);
        Ada_Struct.Multiview_Geometry_Shader :=
            Utilities.To_Ada(C_Struct.Multiview_Geometry_Shader);
        Ada_Struct.Multiview_Tessellation_Shader :=
            Utilities.To_Ada(C_Struct.Multiview_Tessellation_Shader);
        Ada_Struct.Variable_Pointers_Storage_Buffer :=
            Utilities.To_Ada(C_Struct.Variable_Pointers_Storage_Buffer);
        Ada_Struct.Variable_Pointers :=
            Utilities.To_Ada(C_Struct.Variable_Pointers);
        Ada_Struct.Protected_Memory :=
            Utilities.To_Ada(C_Struct.Protected_Memory);
        Ada_Struct.Sampler_YCbCr_Conversion :=
            Utilities.To_Ada(C_Struct.Sampler_YCbCr_Conversion);
        Ada_Struct.Shader_Draw_Parameters :=
            Utilities.To_Ada(C_Struct.Shader_Draw_Parameters);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_1_Properties;
                     C_Struct: in Physical_Device_Vulkan_1_1_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Device_UUID := C_Struct.Device_UUID;
        Ada_Struct.Driver_UUID := C_Struct.Driver_UUID;
        Ada_Struct.Device_LUID := C_Struct.Device_LUID;
        Ada_Struct.Device_Node_Mask := C_Struct.Device_Node_Mask;
        Ada_Struct.Device_LUID_Valid :=
            Utilities.To_Ada(C_Struct.Device_LUID_Valid);
        Ada_Struct.Subgroup_Size := C_Struct.Subgroup_Size;
        Ada_Struct.Subgroup_Supported_Stages :=
            C_Struct.Subgroup_Supported_Stages;
        Ada_Struct.Subgroup_Supported_Operations :=
            C_Struct.Subgroup_Supported_Operations;
        Ada_Struct.Subgroup_Quad_Operations_In_All_Stages :=
            Utilities.To_Ada(C_Struct.Subgroup_Quad_Operations_In_All_Stages);
        Ada_Struct.Point_Clipping_Behavior := C_Struct.Point_Clipping_Behavior;
        Ada_Struct.Max_Multiview_View_Count :=
            C_Struct.Max_Multiview_View_Count;
        Ada_Struct.Max_Multiview_Instance_Index :=
            C_Struct.Max_Multiview_Instance_Index;
        Ada_Struct.Protected_No_Fault :=
            Utilities.To_Ada(C_Struct.Protected_No_Fault);
        Ada_Struct.Max_Per_Set_Descriptors := C_Struct.Max_Per_Set_Descriptors;
        Ada_Struct.Max_Memory_Allocation_Size :=
            C_Struct.Max_Memory_Allocation_Size;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_2_Features;
                     C_Struct: in Physical_Device_Vulkan_1_2_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Sampler_Mirror_Clamp_To_Edge :=
            Utilities.To_Ada(C_Struct.Sampler_Mirror_Clamp_To_Edge);
        Ada_Struct.Draw_Indirect_Count :=
            Utilities.To_Ada(C_Struct.Draw_Indirect_Count);
        Ada_Struct.Storage_Buffer_8Bit_Access :=
            Utilities.To_Ada(C_Struct.Storage_Buffer_8Bit_Access);
        Ada_Struct.Uniform_And_Storage_Buffer_8Bit_Access :=
            Utilities.To_Ada(C_Struct.Uniform_And_Storage_Buffer_8Bit_Access);
        Ada_Struct.Storage_Push_Constant_8 :=
            Utilities.To_Ada(C_Struct.Storage_Push_Constant_8);
        Ada_Struct.Shader_Buffer_Int64_Atomics :=
            Utilities.To_Ada(C_Struct.Shader_Buffer_Int64_Atomics);
        Ada_Struct.Shader_Shared_Int64_Atomics :=
            Utilities.To_Ada(C_Struct.Shader_Shared_Int64_Atomics);
        Ada_Struct.Shader_Float16 := Utilities.To_Ada(C_Struct.Shader_Float16);
        Ada_Struct.Shader_Int8 := Utilities.To_Ada(C_Struct.Shader_Int8);
        Ada_Struct.Descriptor_Indexing :=
            Utilities.To_Ada(C_Struct.Descriptor_Indexing);
        Ada_Struct.Shader_Input_Attachment_Array_Dynamic_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Input_Attachment_Array_Dynamic_Indexing);
        Ada_Struct.Shader_Uniform_Texel_Buffer_Array_Dynamic_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Uniform_Texel_Buffer_Array_Dynamic_Indexing);
        Ada_Struct.Shader_Storage_Texel_Buffer_Array_Dynamic_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Storage_Texel_Buffer_Array_Dynamic_Indexing);
        Ada_Struct.Shader_Uniform_Buffer_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Uniform_Buffer_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Sampled_Image_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Sampled_Image_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Storage_Buffer_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Storage_Buffer_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Storage_Image_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Storage_Image_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Input_Attachment_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Input_Attachment_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Uniform_Texel_Buffer_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.
                    Shader_Uniform_Texel_Buffer_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Storage_Texel_Buffer_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.
                    Shader_Storage_Texel_Buffer_Array_Non_Uniform_Indexing);
        Ada_Struct.Descriptor_Binding_Uniform_Buffer_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Uniform_Buffer_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Sampled_Image_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Sampled_Image_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Storage_Image_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Storage_Image_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Storage_Buffer_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Storage_Buffer_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Uniform_Texel_Buffer_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.
                    Descriptor_Binding_Uniform_Texel_Buffer_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Storage_Texel_Buffer_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.
                    Descriptor_Binding_Storage_Texel_Buffer_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Update_Unused_While_Pending :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Update_Unused_While_Pending);
        Ada_Struct.Descriptor_Binding_Partially_Bound :=
            Utilities.To_Ada(C_Struct.Descriptor_Binding_Partially_Bound);
        Ada_Struct.Descriptor_Binding_Variable_Descriptor_Count :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Variable_Descriptor_Count);
        Ada_Struct.Runtime_Descriptor_Array :=
            Utilities.To_Ada(C_Struct.Runtime_Descriptor_Array);
        Ada_Struct.Sampler_Filter_Minmax :=
            Utilities.To_Ada(C_Struct.Sampler_Filter_Minmax);
        Ada_Struct.Scalar_Block_Layout :=
            Utilities.To_Ada(C_Struct.Scalar_Block_Layout);
        Ada_Struct.Imageless_Framebuffer :=
            Utilities.To_Ada(C_Struct.Imageless_Framebuffer);
        Ada_Struct.Uniform_Buffer_Standard_Layout :=
            Utilities.To_Ada(C_Struct.Uniform_Buffer_Standard_Layout);
        Ada_Struct.Shader_Subgroup_Extended_Types :=
            Utilities.To_Ada(C_Struct.Shader_Subgroup_Extended_Types);
        Ada_Struct.Separate_Depth_Stencil_Layouts :=
            Utilities.To_Ada(C_Struct.Separate_Depth_Stencil_Layouts);
        Ada_Struct.Host_Query_Reset :=
            Utilities.To_Ada(C_Struct.Host_Query_Reset);
        Ada_Struct.Timeline_Semaphore :=
            Utilities.To_Ada(C_Struct.Timeline_Semaphore);
        Ada_Struct.Buffer_Device_Address :=
            Utilities.To_Ada(C_Struct.Buffer_Device_Address);
        Ada_Struct.Buffer_Device_Address_Capture_Replay :=
            Utilities.To_Ada(C_Struct.Buffer_Device_Address_Capture_Replay);
        Ada_Struct.Buffer_Device_Address_Multi_Device :=
            Utilities.To_Ada(C_Struct.Buffer_Device_Address_Multi_Device);
        Ada_Struct.Vulkan_Memory_Model :=
            Utilities.To_Ada(C_Struct.Vulkan_Memory_Model);
        Ada_Struct.Vulkan_Memory_Model_Device_Scope :=
            Utilities.To_Ada(C_Struct.Vulkan_Memory_Model_Device_Scope);
        Ada_Struct.Vulkan_Memory_Model_Availability_Visibility_Chains :=
            Utilities.To_Ada
                (C_Struct.Vulkan_Memory_Model_Availability_Visibility_Chains);
        Ada_Struct.Shader_Output_Viewport_Index :=
            Utilities.To_Ada(C_Struct.Shader_Output_Viewport_Index);
        Ada_Struct.Shader_Output_Layer :=
            Utilities.To_Ada(C_Struct.Shader_Output_Layer);
        Ada_Struct.Subgroup_Broadcast_Dynamic_ID :=
            Utilities.To_Ada(C_Struct.Subgroup_Broadcast_Dynamic_ID);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_2_Properties;
                     C_Struct: in Physical_Device_Vulkan_1_2_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Driver_ID := C_Struct.Driver_ID;
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Driver_Name, Interfaces.C.To_Ada(C_Struct.Driver_Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Driver_Info, Interfaces.C.To_Ada(C_Struct.Driver_Info));
        Ada_Struct.Conformance_Version := C_Struct.Conformance_Version;
        Ada_Struct.Denorm_Behavior_Independence :=
            C_Struct.Denorm_Behavior_Independence;
        Ada_Struct.Rounding_Mode_Independence :=
            C_Struct.Rounding_Mode_Independence;
        Ada_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float16 :=
            Utilities.To_Ada
                (C_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float16);
        Ada_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float32 :=
            Utilities.To_Ada
                (C_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float32);
        Ada_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float64 :=
            Utilities.To_Ada
                (C_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float64);
        Ada_Struct.Shader_Denorm_Preserve_Float16 := 
            Utilities.To_Ada(C_Struct.Shader_Denorm_Preserve_Float16);
        Ada_Struct.Shader_Denorm_Preserve_Float32 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Preserve_Float32);
        Ada_Struct.Shader_Denorm_Preserve_Float64 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Preserve_Float64);
        Ada_Struct.Shader_Denorm_Flush_To_Zero_Float16 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Flush_To_Zero_Float16);
        Ada_Struct.Shader_Denorm_Flush_To_Zero_Float32 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Flush_To_Zero_Float32);
        Ada_Struct.Shader_Denorm_Flush_To_Zero_Float64 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Flush_To_Zero_Float64);
        Ada_Struct.Shader_Rounding_Mode_RTE_Float16 := 
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTE_Float16);
        Ada_Struct.Shader_Rounding_Mode_RTE_Float32 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTE_Float32);
        Ada_Struct.Shader_Rounding_Mode_RTE_Float64 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTE_Float64);
        Ada_Struct.Shader_Rounding_Mode_RTZ_Float16 := 
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTZ_Float16);
        Ada_Struct.Shader_Rounding_Mode_RTZ_Float32 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTZ_Float32);
        Ada_Struct.Shader_Rounding_Mode_RTZ_Float64 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTZ_Float64);
        Ada_Struct.Max_Update_After_Bind_Descriptors_In_All_Pools :=
            C_Struct.Max_Update_After_Bind_Descriptors_In_All_Pools;
        Ada_Struct.Shader_Uniform_Buffer_Array_Non_Uniform_Indexing_Native := 
            Utilities.To_Ada
                (C_Struct.
                    Shader_Uniform_Buffer_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Shader_Sampled_Image_Array_Non_Uniform_Indexing_Native :=
            Utilities.To_Ada
                (C_Struct.
                    Shader_Sampled_Image_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Shader_Storage_Buffer_Array_Non_Uniform_Indexing_Native :=
            Utilities.To_Ada
                (C_Struct.
                    Shader_Storage_Buffer_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Shader_Storage_Image_Array_Non_Uniform_Indexing_Native :=
            Utilities.To_Ada
                (C_Struct.
                    Shader_Storage_Image_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Shader_Input_Attachment_Array_Non_Uniform_Indexing_Native :=
            Utilities.To_Ada
                (C_Struct.
                    Shader_Input_Attachment_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Robust_Buffer_Access_Update_After_Bind :=
            Utilities.To_Ada(C_Struct.Robust_Buffer_Access_Update_After_Bind);
        Ada_Struct.Quad_Divergent_Implicit_LOD :=
            Utilities.To_Ada(C_Struct.Quad_Divergent_Implicit_LOD);
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Samplers :=
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Samplers;
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Uniform_Buffers := 
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Uniform_Buffers;
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Buffers := 
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Buffers;
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Sampled_Images := 
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Sampled_Images;
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Images := 
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Images;
        Ada_Struct.
            Max_Per_Stage_Descriptor_Update_After_Bind_Input_Attachments := 
            C_Struct.
                Max_Per_Stage_Descriptor_Update_After_Bind_Input_Attachments;
        Ada_Struct.Max_Per_Stage_Update_After_Bind_Resources :=
            C_Struct.Max_Per_Stage_Update_After_Bind_Resources;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Samplers :=
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Samplers;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers := 
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers;
        Ada_Struct.
            Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers_Dynamic := 
            C_Struct.
                Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers_Dynamic;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Buffers := 
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Buffers;
        Ada_Struct.
            Max_Descriptor_Set_Update_After_Bind_Storage_Buffers_Dynamic := 
            C_Struct.
                Max_Descriptor_Set_Update_After_Bind_Storage_Buffers_Dynamic;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Sampled_Images := 
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Sampled_Images;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Images := 
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Images;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Input_Attachments := 
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Input_Attachments;
        Ada_Struct.Supported_Depth_Resolve_Modes :=
            C_Struct.Supported_Depth_Resolve_Modes;
        Ada_Struct.Supported_Stencil_Resolve_Modes :=
            C_Struct.Supported_Stencil_Resolve_Modes;
        Ada_Struct.Independent_Resolve_None :=
            Utilities.To_Ada(C_Struct.Independent_Resolve_None);
        Ada_Struct.Independent_Resolve := 
            Utilities.To_Ada(C_Struct.Independent_Resolve);
        Ada_Struct.Filter_Minmax_Single_Component_Formats := 
            Utilities.To_Ada(C_Struct.Filter_Minmax_Single_Component_Formats);
        Ada_Struct.Filter_Minmax_Image_Component_Mapping := 
            Utilities.To_Ada(C_Struct.Filter_Minmax_Image_Component_Mapping);
        Ada_Struct.Max_Timeline_Semaphore_Value_Difference :=
            C_Struct.Max_Timeline_Semaphore_Value_Difference;
        Ada_Struct.Framebuffer_Integer_Color_Sample_Counts :=
            C_Struct.Framebuffer_Integer_Color_Sample_Counts;

    end To_Ada;

    function To_C(Struct: in Image_Format_List_Create_Info)
        return Image_Format_List_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(Format_Arrays,
                                                         Format_Vectors);
        
        IFLCIC: Image_Format_List_Create_Info_C;
    begin
        IFLCIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(IFLCIC.View_Format_Count,
                   Struct.View_Formats,
                   IFLCIC.View_Formats);

        return IFLCIC;
    end To_C;

    procedure Free(Struct: in out Image_Format_List_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Format_Arrays.Free(Struct.View_Formats);
    end Free;

    function To_C(Struct: in Attachment_Description_2)
        return Attachment_Description_2_C is
        AD2C: Attachment_Description_2_C;
    begin
        AD2C.Next := Extension_Records.To_C(Struct.Next);
        AD2C.Flags := Struct.Flags;
        AD2C.Format := Struct.Format;
        AD2C.Samples := Struct.Samples;
        AD2C.Load_Op := Struct.Load_Op;
        AD2C.Store_Op := Struct.Store_Op;
        AD2C.Stencil_Load_Op := Struct.Stencil_Load_Op;
        AD2C.Stencil_Store_Op := Struct.Stencil_Store_Op;
        AD2C.Initial_Layout := Struct.Initial_Layout;
        AD2C.Final_Layout := Struct.Final_Layout;

        return AD2C;
    end To_C;

    procedure Free(Struct: in out Attachment_Description_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Attachment_Reference_2)
        return Attachment_Reference_2_C is
        AR2C: Attachment_Reference_2_C;
    begin
        AR2C.Next := Extension_Records.To_C(Struct.Next);
        AR2C.Attachment := Struct.Attachment;
        AR2C.Layout := Struct.Layout;
        AR2C.Aspect_Mask := Struct.Aspect_Mask;

        return AR2C;
    end To_C;

    procedure Free(Struct: in out Attachment_Reference_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Subpass_Description_2)
        return Subpass_Description_2_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Attachment_Reference_2_C_Arrays, Attachment_Reference_2_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        SD2C: Subpass_Description_2_C;
        Dummy: Interfaces.Unsigned_32;
    begin
        SD2C.Next := Extension_Records.To_C(Struct.Next);
        SD2C.Flags := Struct.Flags;
        SD2C.Pipeline_Bind_Point := Struct.Pipeline_Bind_Point;
        SD2C.View_Mask := Struct.View_Mask;
        To_C_Array(SD2C.Input_Attachment_Count,
                   Struct.Input_Attachments,
                   SD2C.Input_Attachments);
        To_C_Array(SD2C.Color_Attachment_Count,
                   Struct.Color_Attachments,
                   SD2C.Color_Attachments);
        To_C_Array(Dummy,
                   Struct.Resolve_Attachments,
                   SD2C.Resolve_Attachments);

        if Struct.Depth_Stencil_Attachment /= null then
            SD2C.Depth_Stencil_Attachment := new Attachment_Reference_2_C'
                (To_C(Struct.Depth_Stencil_Attachment.all));
        end if;

        To_C_Array(SD2C.Preserve_Attachment_Count,
                   Struct.Preserve_Attachments,
                   SD2C.Preserve_Attachments);

        return SD2C;
    end To_C;

    procedure Free(Struct: in out Subpass_Description_2_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Attachment_Reference_2_C, Attachment_Reference_2_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        Attachment_Reference_2_C_Arrays.Free(Struct.Input_Attachments,
                                             Free'Access);
        Attachment_Reference_2_C_Arrays.Free(Struct.Color_Attachments,
                                             Free'Access);
        Attachment_Reference_2_C_Arrays.Free(Struct.Resolve_Attachments,
                                             Free'Access);
        
        if Struct.Depth_Stencil_Attachment /= null then
            Free(Struct.Depth_Stencil_Attachment.all);
            Free(Struct.Depth_Stencil_Attachment);
        end if;

        C.Uint32_t_Arrays.Free(Struct.Preserve_Attachments);
    end Free;

    function To_C(Struct: in Subpass_Dependency_2)
        return Subpass_Dependency_2_C is
        SD2C: Subpass_Dependency_2_C;
    begin
        SD2C.Next := Extension_Records.To_C(Struct.Next);
        SD2C.Src_Subpass := Struct.Src_Subpass;
        SD2C.Dst_Subpass := Struct.Dst_Subpass;
        SD2C.Src_Stage_Mask := Struct.Src_Stage_Mask;
        SD2C.Dst_Stage_Mask := Struct.Dst_Stage_Mask;
        SD2C.Src_Access_Mask := Struct.Src_Access_Mask;
        SD2C.Dst_Access_Mask := Struct.Dst_Access_Mask;
        SD2C.Dependency_Flags := Struct.Dependency_Flags;
        SD2C.View_Offset := Struct.View_Offset;

        return SD2C;
    end To_C;

    procedure Free(Struct: in out Subpass_Dependency_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Render_Pass_Create_Info_2)
        return Render_Pass_Create_Info_2_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Attachment_Description_2_C_Arrays,
             Attachment_Description_2_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Subpass_Description_2_C_Arrays, Subpass_Description_2_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Subpass_Dependency_2_C_Arrays, Subpass_Dependency_2_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        RPCI2C: Render_Pass_Create_Info_2_C;
    begin
        RPCI2C.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(RPCI2C.Attachment_Count,
                   Struct.Attachments,
                   RPCI2C.Attachments);
        To_C_Array(RPCI2C.Subpass_Count, Struct.Subpasses, RPCI2C.Subpasses);
        To_C_Array(RPCI2C.Dependency_Count,
                   Struct.Dependencies,
                   RPCI2C.Dependencies);
        To_C_Array(RPCI2C.Correlated_View_Mask_Count,
                   Struct.Correlated_View_Masks,
                   RPCI2C.Correlated_View_Masks);

        return RPCI2C;
    end To_C;

    procedure Free(Struct: in out Render_Pass_Create_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Attachment_Description_2_C_Arrays.Free(Struct.Attachments, Free'Access);
        Subpass_Description_2_C_Arrays.Free(Struct.Subpasses, Free'Access);
        Subpass_Dependency_2_C_Arrays.Free(Struct.Dependencies, Free'Access);
        C.Uint32_t_Arrays.Free(Struct.Correlated_View_Masks);
    end Free;

    function To_C(Struct: in Subpass_Begin_Info) return Subpass_Begin_Info_C is
        SBIC: Subpass_Begin_Info_C;
    begin
        SBIC.Next := Extension_Records.To_C(Struct.Next);
        SBIC.Contents := Struct.Contents;

        return SBIC;
    end To_C;

    procedure Free(Struct: in out Subpass_Begin_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Subpass_End_Info) return Subpass_End_Info_C is
        SEIC: Subpass_End_Info_C;
    begin
        SEIC.Next := Extension_Records.To_C(Struct.Next);

        return SEIC;
    end To_C;

    procedure Free(Struct: in out Subpass_End_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Physical_Device_8Bit_Storage_Features;
                     C_Struct: in Physical_Device_8Bit_Storage_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Storage_Buffer_8Bit_Access :=
            Utilities.To_Ada(C_Struct.Storage_Buffer_8Bit_Access);
        Ada_Struct.Uniform_And_Storage_Buffer_8Bit_Access :=
            Utilities.To_Ada(C_Struct.Uniform_And_Storage_Buffer_8Bit_Access);
        Ada_Struct.Storage_Push_Constant_8 :=
            Utilities.To_Ada(C_Struct.Storage_Push_Constant_8);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Driver_Properties;
                     C_Struct: in Physical_Device_Driver_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Driver_ID := C_Struct.Driver_ID;
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Driver_Name, Interfaces.C.To_Ada(C_Struct.Driver_Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Driver_Info, Interfaces.C.To_Ada(C_Struct.Driver_Info));
        Ada_Struct.Conformance_Version := C_Struct.Conformance_Version;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Atomic_Int64_Features;
         C_Struct: in Physical_Device_Shader_Atomic_Int64_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Buffer_Int64_Atomics :=
            Utilities.To_Ada(C_Struct.Shader_Buffer_Int64_Atomics);
        Ada_Struct.Shader_Shared_Int64_Atomics :=
            Utilities.To_Ada(C_Struct.Shader_Shared_Int64_Atomics);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Float16_Int8_Features;
         C_Struct: in Physical_Device_Shader_Float16_Int8_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Float16 := Utilities.To_Ada(C_Struct.Shader_Float16);
        Ada_Struct.Shader_Int8 := Utilities.To_Ada(C_Struct.Shader_Int8);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Float_Controls_Properties;
         C_Struct: in Physical_Device_Float_Controls_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Denorm_Behavior_Independence :=
            C_Struct.Denorm_Behavior_Independence;
        Ada_Struct.Rounding_Mode_Independence :=
            C_Struct.Rounding_Mode_Independence;
        Ada_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float16 :=
            Utilities.To_Ada
                (C_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float16);
        Ada_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float32 :=
            Utilities.To_Ada
                (C_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float32);
        Ada_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float64 :=
            Utilities.To_Ada
                (C_Struct.Shader_Signed_Zero_Inf_Nan_Preserve_Float64);
        Ada_Struct.Shader_Denorm_Preserve_Float16:=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Preserve_Float16);
        Ada_Struct.Shader_Denorm_Preserve_Float32 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Preserve_Float32);
        Ada_Struct.Shader_Denorm_Preserve_Float64 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Preserve_Float64);
        Ada_Struct.Shader_Denorm_Flush_To_Zero_Float16 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Flush_to_Zero_Float16);
        Ada_Struct.Shader_Denorm_Flush_To_Zero_Float32 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Flush_to_Zero_Float32);
        Ada_Struct.Shader_Denorm_Flush_To_Zero_Float64 :=
            Utilities.To_Ada(C_Struct.Shader_Denorm_Flush_to_Zero_Float64);
        Ada_Struct.Shader_Rounding_Mode_RTE_Float16 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTE_Float16);
        Ada_Struct.Shader_Rounding_Mode_RTE_Float32 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTE_Float32);
        Ada_Struct.Shader_Rounding_Mode_RTE_Float64 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTE_Float64);
        Ada_Struct.Shader_Rounding_Mode_RTZ_Float16 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTE_Float16);
        Ada_Struct.Shader_Rounding_Mode_RTZ_Float32 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTE_Float32);
        Ada_Struct.Shader_Rounding_Mode_RTZ_Float64 :=
            Utilities.To_Ada(C_Struct.Shader_Rounding_Mode_RTE_Float64);
    end To_Ada;

    function To_C(Struct: in Descriptor_Set_Layout_Binding_Flags_Create_Info)
        return Descriptor_Set_Layout_Binding_Flags_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Descriptor_Binding_Flags_Arrays,
             Descriptor_Binding_Flags_Vectors);

        DSLBFCIC: Descriptor_Set_Layout_Binding_Flags_Create_Info_C;
    begin
        DSLBFCIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(DSLBFCIC.Binding_Count,
                   Struct.Binding_Flags,
                   DSLBFCIC.Binding_Flags);

        return DSLBFCIC;
    end To_C;

    procedure Free
        (Struct: in out Descriptor_Set_Layout_Binding_Flags_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Descriptor_Binding_Flags_Arrays.Free(Struct.Binding_Flags);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Descriptor_Indexing_Features;
         C_Struct: in Physical_Device_Descriptor_Indexing_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);

        Ada_Struct.Shader_Input_Attachment_Array_Dynamic_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Input_Attachment_Array_Dynamic_Indexing);
        Ada_Struct.Shader_Uniform_Texel_Buffer_Array_Dynamic_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Uniform_Texel_Buffer_Array_Dynamic_Indexing);
        Ada_Struct.Shader_Storage_Texel_Buffer_Array_Dynamic_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Storage_Texel_Buffer_Array_Dynamic_Indexing);
        Ada_Struct.Shader_Uniform_Buffer_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Uniform_Buffer_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Sampled_Image_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Sampled_Image_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Storage_Buffer_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Storage_Buffer_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Storage_Image_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Storage_Image_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Input_Attachment_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
                (C_Struct.Shader_Input_Attachment_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Uniform_Texel_Buffer_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
              (C_Struct.Shader_Uniform_Texel_Buffer_Array_Non_Uniform_Indexing);
        Ada_Struct.Shader_Storage_Texel_Buffer_Array_Non_Uniform_Indexing :=
            Utilities.To_Ada
              (C_Struct.Shader_Storage_Texel_Buffer_Array_Non_Uniform_Indexing);
        Ada_Struct.Descriptor_Binding_Uniform_Buffer_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Uniform_Buffer_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Sampled_Image_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Sampled_Image_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Storage_Image_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Storage_Image_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Storage_Buffer_Update_After_Bind :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Storage_Buffer_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Uniform_Texel_Buffer_Update_After_Bind :=
            Utilities.To_Ada
           (C_Struct.Descriptor_Binding_Uniform_Texel_Buffer_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Storage_Texel_Buffer_Update_After_Bind :=
            Utilities.To_Ada
           (C_Struct.Descriptor_Binding_Storage_Texel_Buffer_Update_After_Bind);
        Ada_Struct.Descriptor_Binding_Update_Unused_While_Pending :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Update_Unused_While_Pending);
        Ada_Struct.Descriptor_Binding_Partially_Bound :=
            Utilities.To_Ada(C_Struct.Descriptor_Binding_Partially_Bound);
        Ada_Struct.Descriptor_Binding_Variable_Descriptor_Count :=
            Utilities.To_Ada
                (C_Struct.Descriptor_Binding_Variable_Descriptor_Count);
        Ada_Struct.Runtime_Descriptor_Array :=
            Utilities.To_Ada(C_Struct.Runtime_Descriptor_Array);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Descriptor_Indexing_Properties;
         C_Struct: in Physical_Device_Descriptor_Indexing_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Update_After_Bind_Descriptors_In_All_Pools :=
            C_Struct.Max_Update_After_Bind_Descriptors_In_All_Pools;
        Ada_Struct.Shader_Uniform_Buffer_Array_Non_Uniform_Indexing_Native :=
            Utilities.To_Ada
             (C_Struct.Shader_Uniform_Buffer_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Shader_Sampled_Image_Array_Non_Uniform_Indexing_Native :=
            Utilities.To_Ada
              (C_Struct.Shader_Sampled_Image_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Shader_Storage_Buffer_Array_Non_Uniform_Indexing_Native :=
            Utilities.To_Ada
             (C_Struct.Shader_Storage_Buffer_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Shader_Storage_Image_Array_Non_Uniform_Indexing_Native :=
            Utilities.To_Ada
              (C_Struct.Shader_Storage_Image_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Shader_Input_Attachment_Array_Non_Uniform_Indexing_Native :=
            Utilities.To_Ada
           (C_Struct.Shader_Input_Attachment_Array_Non_Uniform_Indexing_Native);
        Ada_Struct.Robust_Buffer_Access_Update_After_Bind :=
            Utilities.To_Ada(C_Struct.Robust_Buffer_Access_Update_After_Bind);
        Ada_Struct.Quad_Divergent_Implicit_LOD :=
            Utilities.To_Ada(C_Struct.Quad_Divergent_Implicit_LOD);
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Samplers :=
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Samplers;
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Uniform_Buffers :=
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Uniform_Buffers;
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Buffers :=
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Buffers;
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Sampled_Images :=
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Sampled_Images;
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Images :=
            C_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Images;
        Ada_Struct.Max_Per_Stage_Descriptor_Update_After_Bind_Input_Attachments
            :=
          C_Struct.Max_per_Stage_Descriptor_Update_After_Bind_Input_Attachments;
        Ada_Struct.Max_Per_Stage_Update_After_Bind_Resources :=
            C_Struct.Max_Per_Stage_Update_After_Bind_Resources;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Samplers :=
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Samplers;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers :=
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers_Dynamic
            :=
          C_Struct.Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers_Dynamic;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Buffers :=
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Buffers;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Buffers_Dynamic
            :=
          C_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Buffers_Dynamic;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Sampled_Images :=
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Sampled_Images;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Images :=
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Storage_Images;
        Ada_Struct.Max_Descriptor_Set_Update_After_Bind_Input_Attachments :=
            C_Struct.Max_Descriptor_Set_Update_After_Bind_Input_Attachments;
    end To_Ada;

    function To_C
        (Struct: in Descriptor_Set_Variable_Descriptor_Count_Allocate_Info)
        return Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        DSVDCAIC: Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C;
    begin
        DSVDCAIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(DSVDCAIC.Descriptor_Set_Count,
                   Struct.Descriptor_Counts,
                   DSVDCAIC.Descriptor_Counts);

        return DSVDCAIC;
    end To_C;

    procedure Free
        (Struct:
            in out Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Descriptor_Counts);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Descriptor_Set_Variable_Descriptor_Count_Layout_Support;
         C_Struct:
            in Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Variable_Descriptor_Count :=
            C_Struct.Max_Variable_Descriptor_Count;
    end To_Ada;
    
    function To_C(Struct: in Subpass_Description_Depth_Stencil_Resolve)
        return Subpass_Description_Depth_Stencil_Resolve_C is
        SDDSRC: Subpass_Description_Depth_Stencil_Resolve_C;
    begin
        SDDSRC.Next := Extension_Records.To_C(Struct.Next);
        SDDSRC.Depth_Resolve_Mode := Struct.Depth_Resolve_Mode;
        SDDSRC.Stencil_Resolve_Mode := Struct.Stencil_Resolve_Mode;

        if Struct.Depth_Stencil_Resolve_Attachment /= null then
            SDDSRC.Depth_Stencil_Resolve_Attachment := 
                new Attachment_Reference_2_C'
                    (To_C(Struct.Depth_Stencil_Resolve_Attachment.all));
        end if;

        return SDDSRC;
    end To_C;

    procedure Free
        (Struct: in out Subpass_Description_Depth_Stencil_Resolve_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Attachment_Reference_2_C, Attachment_Reference_2_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Depth_Stencil_Resolve_Attachment /= null then
            Free(Struct.Depth_Stencil_Resolve_Attachment.all);
            Free(Struct.Depth_Stencil_Resolve_Attachment);
        end if;
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Depth_Stencil_Resolve_Properties;
         C_Struct: in Physical_Device_Depth_Stencil_Resolve_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Supported_Depth_Resolve_Modes :=
            C_Struct.Supported_Depth_Resolve_Modes;
        Ada_Struct.Supported_Stencil_Resolve_Modes :=
            C_Struct.Supported_Stencil_Resolve_Modes;
        Ada_Struct.Independent_Resolve_None :=
            Utilities.To_Ada(C_Struct.Independent_Resolve_None);
        Ada_Struct.Independent_Resolve :=
            Utilities.To_Ada(C_Struct.Independent_Resolve_None);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Scalar_Block_Layout_Features;
         C_Struct: in Physical_Device_Scalar_Block_Layout_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Scalar_Block_Layout :=
            Utilities.To_Ada(C_Struct.Scalar_Block_Layout);
    end To_Ada;

    function To_C(Struct: in Image_Stencil_Usage_Create_Info)
        return Image_Stencil_Usage_Create_Info_C is
        ISUCIC: Image_Stencil_Usage_Create_Info_C;
    begin
        ISUCIC.Next := Extension_Records.To_C(Struct.Next);
        ISUCIC.Stencil_Usage := Struct.Stencil_Usage;

        return ISUCIC;
    end To_C;

    procedure Free(Struct: in out Image_Stencil_Usage_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Sampler_Reduction_Mode_Create_Info)
        return Sampler_Reduction_Mode_Create_Info_C is
        SRMCIC: Sampler_Reduction_Mode_Create_Info_C;
    begin
        SRMCIC.Next := Extension_Records.To_C(Struct.Next);
        SRMCIC.Reduction_Mode := Struct.Reduction_Mode;

        return SRMCIC;
    end To_C;

    procedure Free(Struct: in out Sampler_Reduction_Mode_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Sampler_Filter_Minmax_Properties;
         C_Struct: in Physical_Device_Sampler_Filter_Minmax_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Filter_Minmax_Single_Component_Formats :=
            Utilities.To_Ada(C_Struct.Filter_Minmax_Single_Component_Formats);
        Ada_Struct.Filter_Minmax_Image_Component_Mapping :=
            Utilities.To_Ada(C_Struct.Filter_Minmax_Image_Component_Mapping);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vulkan_Memory_Model_Features;
         C_Struct: in Physical_Device_Vulkan_Memory_Model_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Vulkan_Memory_Model :=
            Utilities.To_Ada(C_Struct.Vulkan_Memory_Model);
        Ada_Struct.Vulkan_Memory_Model_Device_Scope :=
            Utilities.To_Ada(C_Struct.Vulkan_Memory_Model_Device_Scope);
        Ada_Struct.Vulkan_Memory_Model_Availability_Visibility_Chains :=
            Utilities.To_Ada
                (C_Struct.Vulkan_Memory_Model_Availability_Visibility_Chains);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Imageless_Framebuffer_Features;
         C_Struct: in Physical_Device_Imageless_Framebuffer_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Imageless_Framebuffer :=
            Utilities.To_Ada(C_Struct.Imageless_Framebuffer);
    end To_Ada;

    function To_C(Struct: in Framebuffer_Attachment_Image_Info)
        return Framebuffer_Attachment_Image_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(Format_Arrays,
                                                         Format_Vectors);

        FAIIC: Framebuffer_Attachment_Image_Info_C;
    begin
        FAIIC.Next := Extension_Records.To_C(Struct.Next);
        FAIIC.Flags := Struct.Flags;
        FAIIC.Usage := Struct.Usage;
        FAIIC.Width := Struct.Width;
        FAIIC.Height := Struct.Height;
        FAIIC.Layer_Count := Struct.Layer_Count;
        To_C_Array(FAIIC.View_Format_Count,
                   Struct.View_Formats,
                   FAIIC.View_Formats);

        return FAIIC;
    end To_C;

    procedure Free(Struct: in out Framebuffer_Attachment_Image_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Format_Arrays.Free(Struct.View_Formats);
    end Free;

    function To_C(Struct: in Framebuffer_Attachments_Create_Info)
        return Framebuffer_Attachments_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Framebuffer_Attachment_Image_Info_C_Arrays,
             Framebuffer_Attachment_Image_Info_Vectors);

        FACIC: Framebuffer_Attachments_Create_Info_C;
    begin
        FACIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(FACIC.Attachment_Image_Info_Count,
                   Struct.Attachment_Image_Infos,
                   FACIC.Attachment_Image_Infos);

        return FACIC;
    end To_C;

    procedure Free(Struct: in out Framebuffer_Attachments_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Framebuffer_Attachment_Image_Info_C_Arrays.Free
            (Struct.Attachment_Image_Infos, Free'Access);
    end Free;

    function To_C(Struct: in Render_Pass_Attachment_Begin_Info)
        return Render_Pass_Attachment_Begin_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Image_View_Arrays,
                                                         Image_View_Vectors);

        RPABIC: Render_Pass_Attachment_Begin_Info_C;
    begin
        RPABIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(RPABIC.Attachment_Count,
                   Struct.Attachments,
                   RPABIC.Attachments);

        return RPABIC;
    end To_C;

    procedure Free(Struct: in out Render_Pass_Attachment_Begin_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Image_View_Arrays.Free(Struct.Attachments);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Uniform_Buffer_Standard_Layout_Features;
         C_Struct:
            in Physical_Device_Uniform_Buffer_Standard_Layout_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Uniform_Buffer_Standard_Layout :=
            Utilities.To_Ada(C_Struct.Uniform_Buffer_Standard_Layout);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Shader_Subgroup_Extended_Types_Features;
         C_Struct:
            in Physical_Device_Shader_Subgroup_Extended_Types_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Subgroup_Extended_Types :=
            Utilities.To_Ada(C_Struct.Shader_Subgroup_Extended_Types);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Separate_Depth_Stencil_Layouts_Features;
         C_Struct:
            in Physical_Device_Separate_Depth_Stencil_Layouts_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Separate_Depth_Stencil_Layouts :=
            Utilities.To_Ada(C_Struct.Separate_Depth_Stencil_Layouts);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Attachment_Reference_Stencil_Layout;
                     C_Struct: in Attachment_Reference_Stencil_Layout_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Stencil_Layout := C_Struct.Stencil_Layout;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Attachment_Description_Stencil_Layout;
                     C_Struct: in Attachment_Description_Stencil_Layout_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Stencil_Initial_Layout := C_Struct.Stencil_Initial_Layout;
        Ada_Struct.Stencil_Final_Layout := C_Struct.Stencil_Final_Layout;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Host_Query_Reset_Features;
         C_Struct: in Physical_Device_Host_Query_Reset_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Host_Query_Reset :=
            Utilities.To_Ada(C_Struct.Host_Query_Reset);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Timeline_Semaphore_Features;
         C_Struct: in Physical_Device_Timeline_Semaphore_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Timeline_Semaphore :=
            Utilities.To_Ada(C_Struct.Timeline_Semaphore);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Timeline_Semaphore_Properties;
         C_Struct: in Physical_Device_Timeline_Semaphore_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Timeline_Semaphore_Value_Difference :=
            C_Struct.Max_Timeline_Semaphore_Value_Difference;
    end To_Ada;

    function To_C(Struct: in Semaphore_Type_Create_Info)
        return Semaphore_Type_Create_Info_C is
        STCIC: Semaphore_Type_Create_Info_C;
    begin
        STCIC.Next := Extension_Records.To_C(Struct.Next);
        STCIC.Semaphore_Type := Struct.Semaphore_Type;
        STCIC.Initial_Value := Struct.Initial_Value;

        return STCIC;
    end To_C;

    procedure Free(Struct: in out Semaphore_Type_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;


    function To_C(Struct: in Timeline_Semaphore_Submit_Info)
        return Timeline_Semaphore_Submit_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Semaphore_Value_Arrays, Semaphore_Value_Vectors);

        TSSIC: Timeline_Semaphore_Submit_Info_C;
    begin
        TSSIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(TSSIC.Wait_Semaphore_Value_Count,
                   Struct.Wait_Semaphore_Values,
                   TSSIC.Wait_Semaphore_Values);
        To_C_Array(TSSIC.Signal_Semaphore_Value_Count,
                   Struct.Signal_Semaphore_Values,
                   TSSIC.Signal_Semaphore_Values);

        return TSSIC;
    end To_C;

    procedure Free(Struct: in out Timeline_Semaphore_Submit_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Semaphore_Value_Arrays.Free(Struct.Wait_Semaphore_Values);
        Semaphore_Value_Arrays.Free(Struct.Signal_Semaphore_Values);
    end Free;

    function To_C(Struct: in Semaphore_Wait_Info)
        return Semaphore_Wait_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Semaphore_Arrays,
                                                         Semaphore_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (Semaphore_Value_Arrays, Semaphore_Value_Vectors);

        TSSIC: Semaphore_Wait_Info_C;
    begin
        TSSIC.Next := Extension_Records.To_C(Struct.Next);
        TSSIC.Flags := Struct.Flags;
        To_C_Array(TSSIC.Semaphore_Count, Struct.Semaphores, TSSIC.Semaphores);
        To_C_Array(TSSIC.Semaphore_Count, Struct.Values, TSSIC.Values);

        return TSSIC;
    end To_C;

    procedure Free(Struct: in out Semaphore_Wait_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Semaphore_Arrays.Free(Struct.Semaphores);
        Semaphore_Value_Arrays.Free(Struct.Values);
    end Free;

    function To_C(Struct: in Semaphore_Signal_Info)
        return Semaphore_Signal_Info_C is
        SSIC: Semaphore_Signal_Info_C;
    begin
        SSIC.Next := Extension_Records.To_C(Struct.Next);
        SSIC.Semaphore := Struct.Semaphore;
        SSIC.Value := Struct.Value;

        return SSIC;
    end To_C;

    procedure Free(Struct: in out Semaphore_Signal_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Buffer_Device_Address_Features;
         C_Struct: in Physical_Device_Buffer_Device_Address_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Buffer_Device_Address := 
            Utilities.To_Ada(C_Struct.Buffer_Device_Address);
        Ada_Struct.Buffer_Device_Address_Capture_Replay :=
            Utilities.To_Ada(C_Struct.Buffer_Device_Address_Capture_Replay);
        Ada_Struct.Buffer_Device_Address_Multi_Device := 
            Utilities.To_Ada(C_Struct.Buffer_Device_Address_Multi_Device);
    end To_Ada;

    function To_C(Struct: in Buffer_Device_Address_Info)
        return Buffer_Device_Address_Info_C is
        BDAIC: Buffer_Device_Address_Info_C;
    begin
        BDAIC.Next := Extension_Records.To_C(Struct.Next);
        BDAIC.Buffer := Struct.Buffer;

        return BDAIC;
    end To_C;

    procedure Free(Struct: in out Buffer_Device_Address_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Buffer_Opaque_Capture_Address_Create_Info)
        return Buffer_Opaque_Capture_Address_Create_Info_C is
        BOCACIC: Buffer_Opaque_Capture_Address_Create_Info_C;
    begin
        BOCACIC.Next := Extension_Records.To_C(Struct.Next);
        BOCACIC.Opaque_Capture_Address := Struct.Opaque_Capture_Address;

        return BOCACIC;
    end To_C;

    procedure Free
        (Struct: in out Buffer_Opaque_Capture_Address_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Memory_Opaque_Capture_Address_Allocate_Info)
        return Memory_Opaque_Capture_Address_Allocate_Info_C is
        MOCAAIC: Memory_Opaque_Capture_Address_Allocate_Info_C;
    begin
        MOCAAIC.Next := Extension_Records.To_C(Struct.Next);
        MOCAAIC.Opaque_Capture_Address := Struct.Opaque_Capture_Address;

        return MOCAAIC;
    end To_C;

    procedure Free
        (Struct: in out Memory_Opaque_Capture_Address_Allocate_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Device_Memory_Opaque_Capture_Address_Info)
        return Device_Memory_Opaque_Capture_Address_Info_C is
        DMOCAIC: Device_Memory_Opaque_Capture_Address_Info_C;
    begin
        DMOCAIC.Next := Extension_Records.To_C(Struct.Next);
        DMOCAIC.Memory := Struct.Memory;

        return DMOCAIC;
    end To_C;

    procedure Free
        (Struct: in out Device_Memory_Opaque_Capture_Address_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Image_Format_List_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Format_List_Create_Info,
                         Image_Format_List_Create_Info_C,
                         Image_Format_List_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Attachment_Description_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Attachment_Description_2,
                         Attachment_Description_2_C,
                         Attachment_Description_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Attachment_Reference_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Attachment_Reference_2,
                         Attachment_Reference_2_C,
                         Attachment_Reference_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Subpass_Description_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Subpass_Description_2,
                         Subpass_Description_2_C,
                         Subpass_Description_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Subpass_Dependency_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Subpass_Dependency_2,
                         Subpass_Dependency_2_C,
                         Subpass_Dependency_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Render_Pass_Create_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Render_Pass_Create_Info_2,
                         Render_Pass_Create_Info_2_C,
                         Render_Pass_Create_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Subpass_Begin_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Subpass_Begin_Info,
                         Subpass_Begin_Info_C,
                         Subpass_Begin_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Subpass_End_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Subpass_End_Info,
                         Subpass_End_Info_C,
                         Subpass_End_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Descriptor_Set_Layout_Binding_Flags_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                     (Descriptor_Set_Layout_Binding_Flags_Create_Info,
                      Descriptor_Set_Layout_Binding_Flags_Create_Info_C,
                      Descriptor_Set_Layout_Binding_Flags_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
              (Descriptor_Set_Variable_Descriptor_Count_Allocate_Info,
               Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C,
               Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Subpass_Description_Depth_Stencil_Resolve_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Subpass_Description_Depth_Stencil_Resolve,
                         Subpass_Description_Depth_Stencil_Resolve_C,
                         Subpass_Description_Depth_Stencil_Resolve_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Stencil_Usage_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Stencil_Usage_Create_Info,
                         Image_Stencil_Usage_Create_Info_C,
                         Image_Stencil_Usage_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Sampler_Reduction_Mode_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Sampler_Reduction_Mode_Create_Info,
                         Sampler_Reduction_Mode_Create_Info_C,
                         Sampler_Reduction_Mode_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Framebuffer_Attachment_Image_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Framebuffer_Attachment_Image_Info,
                         Framebuffer_Attachment_Image_Info_C,
                         Framebuffer_Attachment_Image_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Framebuffer_Attachments_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Framebuffer_Attachments_Create_Info,
                         Framebuffer_Attachments_Create_Info_C,
                         Framebuffer_Attachments_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Render_Pass_Attachment_Begin_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Render_Pass_Attachment_Begin_Info,
                         Render_Pass_Attachment_Begin_Info_C,
                         Render_Pass_Attachment_Begin_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Semaphore_Type_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Semaphore_Type_Create_Info,
                         Semaphore_Type_Create_Info_C,
                         Semaphore_Type_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Timeline_Semaphore_Submit_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Timeline_Semaphore_Submit_Info,
                         Timeline_Semaphore_Submit_Info_C,
                         Timeline_Semaphore_Submit_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Semaphore_Wait_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Semaphore_Wait_Info,
                         Semaphore_Wait_Info_C,
                         Semaphore_Wait_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Semaphore_Signal_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Semaphore_Signal_Info,
                         Semaphore_Signal_Info_C,
                         Semaphore_Signal_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Device_Address_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Device_Address_Info,
                         Buffer_Device_Address_Info_C,
                         Buffer_Device_Address_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Opaque_Capture_Address_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Opaque_Capture_Address_Create_Info,
                         Buffer_Opaque_Capture_Address_Create_Info_C,
                         Buffer_Opaque_Capture_Address_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Opaque_Capture_Address_Allocate_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_Opaque_Capture_Address_Allocate_Info,
                         Memory_Opaque_Capture_Address_Allocate_Info_C,
                         Memory_Opaque_Capture_Address_Allocate_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Memory_Opaque_Capture_Address_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Memory_Opaque_Capture_Address_Info,
                         Device_Memory_Opaque_Capture_Address_Info_C,
                         Device_Memory_Opaque_Capture_Address_Info_C_Access);
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
            when Physical_Device_Vulkan_1_1_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Vulkan_1_1_Features,
                         Physical_Device_Vulkan_1_1_Features_C,
                         Physical_Device_Vulkan_1_1_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_1_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Vulkan_1_1_Properties,
                         Physical_Device_Vulkan_1_1_Properties_C,
                         Physical_Device_Vulkan_1_1_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_2_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Vulkan_1_2_Features,
                         Physical_Device_Vulkan_1_2_Features_C,
                         Physical_Device_Vulkan_1_2_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_2_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Vulkan_1_2_Properties,
                         Physical_Device_Vulkan_1_2_Properties_C,
                         Physical_Device_Vulkan_1_2_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_8Bit_Storage_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_8Bit_Storage_Features,
                         Physical_Device_8Bit_Storage_Features_C,
                         Physical_Device_8Bit_Storage_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Driver_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Driver_Properties,
                         Physical_Device_Driver_Properties_C,
                         Physical_Device_Driver_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Atomic_Int64_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Shader_Atomic_Int64_Features,
                         Physical_Device_Shader_Atomic_Int64_Features_C,
                         Physical_Device_Shader_Atomic_Int64_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Float16_Int8_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Shader_Float16_Int8_Features,
                         Physical_Device_Shader_Float16_Int8_Features_C,
                         Physical_Device_Shader_Float16_Int8_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Float_Controls_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Float_Controls_Properties,
                         Physical_Device_Float_Controls_Properties_C,
                         Physical_Device_Float_Controls_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Descriptor_Indexing_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Descriptor_Indexing_Features,
                         Physical_Device_Descriptor_Indexing_Features_C,
                         Physical_Device_Descriptor_Indexing_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Descriptor_Indexing_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                      (Physical_Device_Descriptor_Indexing_Properties,
                       Physical_Device_Descriptor_Indexing_Properties_C,
                       Physical_Device_Descriptor_Indexing_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
             (Descriptor_Set_Variable_Descriptor_Count_Layout_Support,
              Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C,
              Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Depth_Stencil_Resolve_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Physical_Device_Depth_Stencil_Resolve_Properties,
                     Physical_Device_Depth_Stencil_Resolve_Properties_C,
                     Physical_Device_Depth_Stencil_Resolve_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Scalar_Block_Layout_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Scalar_Block_Layout_Features,
                         Physical_Device_Scalar_Block_Layout_Features_C,
                         Physical_Device_Scalar_Block_Layout_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Sampler_Filter_Minmax_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Physical_Device_Sampler_Filter_Minmax_Properties,
                     Physical_Device_Sampler_Filter_Minmax_Properties_C,
                     Physical_Device_Sampler_Filter_Minmax_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Vulkan_Memory_Model_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Vulkan_Memory_Model_Features,
                         Physical_Device_Vulkan_Memory_Model_Features_C,
                         Physical_Device_Vulkan_Memory_Model_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Imageless_Framebuffer_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                      (Physical_Device_Imageless_Framebuffer_Features,
                       Physical_Device_Imageless_Framebuffer_Features_C,
                       Physical_Device_Imageless_Framebuffer_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
             (Physical_Device_Uniform_Buffer_Standard_Layout_Features,
              Physical_Device_Uniform_Buffer_Standard_Layout_Features_C,
              Physical_Device_Uniform_Buffer_Standard_Layout_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Subgroup_Extended_Types_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
             (Physical_Device_Shader_Subgroup_Extended_Types_Features,
              Physical_Device_Shader_Subgroup_Extended_Types_Features_C,
              Physical_Device_Shader_Subgroup_Extended_Types_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
             (Physical_Device_Separate_Depth_Stencil_Layouts_Features,
              Physical_Device_Separate_Depth_Stencil_Layouts_Features_C,
              Physical_Device_Separate_Depth_Stencil_Layouts_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Attachment_Reference_Stencil_Layout_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Attachment_Reference_Stencil_Layout,
                         Attachment_Reference_Stencil_Layout_C,
                         Attachment_Reference_Stencil_Layout_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Attachment_Description_Stencil_Layout_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Attachment_Description_Stencil_Layout,
                         Attachment_Description_Stencil_Layout_C,
                         Attachment_Description_Stencil_Layout_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Host_Query_Reset_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Host_Query_Reset_Features,
                         Physical_Device_Host_Query_Reset_Features_C,
                         Physical_Device_Host_Query_Reset_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Timeline_Semaphore_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Timeline_Semaphore_Features,
                         Physical_Device_Timeline_Semaphore_Features_C,
                         Physical_Device_Timeline_Semaphore_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Timeline_Semaphore_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Physical_Device_Timeline_Semaphore_Properties,
                        Physical_Device_Timeline_Semaphore_Properties_C,
                        Physical_Device_Timeline_Semaphore_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Buffer_Device_Address_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                      (Physical_Device_Buffer_Device_Address_Features,
                       Physical_Device_Buffer_Device_Address_Features_C,
                       Physical_Device_Buffer_Device_Address_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Vulkan_1_1_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Vulkan_1_1_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Vulkan_1_1_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Vulkan_1_1_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Vulkan_1_1_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Vulkan_1_1_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Vulkan_1_2_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Vulkan_1_2_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Vulkan_1_2_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Vulkan_1_2_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Vulkan_1_2_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Vulkan_1_2_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_8Bit_Storage_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_8Bit_Storage_Features_C_Access);
                begin
                    To_Ada(Physical_Device_8Bit_Storage_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Driver_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Driver_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Driver_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Shader_Atomic_Int64_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Shader_Atomic_Int64_Features_C_Access);
                begin
                    To_Ada
                      (Physical_Device_Shader_Atomic_Int64_Features(Ada_Struct),
                       To_Access(Next).all);
                end;
            when Physical_Device_Shader_Float16_Int8_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Shader_Float16_Int8_Features_C_Access);
                begin
                    To_Ada
                      (Physical_Device_Shader_Float16_Int8_Features(Ada_Struct),
                       To_Access(Next).all);
                end;
            when Physical_Device_Float_Controls_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Float_Controls_Properties_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Float_Controls_Properties(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Physical_Device_Descriptor_Indexing_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Descriptor_Indexing_Features_C_Access);
                begin
                    To_Ada
                      (Physical_Device_Descriptor_Indexing_Features(Ada_Struct),
                       To_Access(Next).all);
                end;
            when Physical_Device_Descriptor_Indexing_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Descriptor_Indexing_Properties_C_Access);
                begin
                    To_Ada
                    (Physical_Device_Descriptor_Indexing_Properties(Ada_Struct),
                     To_Access(Next).all);
                end;
            when Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
             (C.Out_Structure_C_Access,
              Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C_Access);
                begin
                    To_Ada
           (Descriptor_Set_Variable_Descriptor_Count_Layout_Support(Ada_Struct),
            To_Access(Next).all);
                end;
            when Physical_Device_Depth_Stencil_Resolve_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                    (C.Out_Structure_C_Access,
                     Physical_Device_Depth_Stencil_Resolve_Properties_C_Access);
                begin
                    To_Ada
                  (Physical_Device_Depth_Stencil_Resolve_Properties(Ada_Struct),
                   To_Access(Next).all);
                end;
            when Physical_Device_Scalar_Block_Layout_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Scalar_Block_Layout_Features_C_Access);
                begin
                    To_Ada
                      (Physical_Device_Scalar_Block_Layout_Features(Ada_Struct),
                       To_Access(Next).all);
                end;
            when Physical_Device_Sampler_Filter_Minmax_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                    (C.Out_Structure_C_Access,
                     Physical_Device_Sampler_Filter_Minmax_Properties_C_Access);
                begin
                    To_Ada
                  (Physical_Device_Sampler_Filter_Minmax_Properties(Ada_Struct),
                   To_Access(Next).all);
                end;
            when Physical_Device_Vulkan_Memory_Model_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                    (C.Out_Structure_C_Access,
                     Physical_Device_Vulkan_Memory_Model_Features_C_Access);
                begin
                    To_Ada(
                       Physical_Device_Vulkan_Memory_Model_Features(Ada_Struct),
                       To_Access(Next).all);
                end;
            when Physical_Device_Imageless_Framebuffer_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Imageless_Framebuffer_Features_C_Access);
                begin
                    To_Ada
                    (Physical_Device_Imageless_Framebuffer_Features(Ada_Struct),
                     To_Access(Next).all);
                end;
            when Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
             (C.Out_Structure_C_Access,
              Physical_Device_Uniform_Buffer_Standard_Layout_Features_C_Access);
                begin
                    To_Ada
           (Physical_Device_Uniform_Buffer_Standard_Layout_Features(Ada_Struct),
            To_Access(Next).all);
                end;
            when Physical_Device_Shader_Subgroup_Extended_Types_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
             (C.Out_Structure_C_Access,
              Physical_Device_Shader_Subgroup_Extended_Types_Features_C_Access);
                begin
                    To_Ada
           (Physical_Device_Shader_Subgroup_Extended_Types_Features(Ada_Struct),
            To_Access(Next).all);
                end;
            when Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
             (C.Out_Structure_C_Access,
              Physical_Device_Separate_Depth_Stencil_Layouts_Features_C_Access);
                begin
                    To_Ada
           (Physical_Device_Separate_Depth_Stencil_Layouts_Features(Ada_Struct),
            To_Access(Next).all);
                end;
            when Attachment_Reference_Stencil_Layout_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Attachment_Reference_Stencil_Layout_C_Access);
                begin
                    To_Ada(Attachment_Reference_Stencil_Layout(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Attachment_Description_Stencil_Layout_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Attachment_Description_Stencil_Layout_C_Access);
                begin
                    To_Ada(Attachment_Description_Stencil_Layout(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Host_Query_Reset_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Host_Query_Reset_Features_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Host_Query_Reset_Features(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Physical_Device_Timeline_Semaphore_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Timeline_Semaphore_Features_C_Access);
                begin
                    To_Ada
                       (Physical_Device_Timeline_Semaphore_Features(Ada_Struct),
                        To_Access(Next).all);
                end;
            when Physical_Device_Timeline_Semaphore_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Timeline_Semaphore_Properties_C_Access);
                begin
                    To_Ada
                     (Physical_Device_Timeline_Semaphore_Properties(Ada_Struct),
                       To_Access(Next).all);
                end;
            when Physical_Device_Buffer_Device_Address_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Buffer_Device_Address_Features_C_Access);
                begin
                    To_Ada
                    (Physical_Device_Buffer_Device_Address_Features(Ada_Struct),
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
            when Image_Format_List_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Format_List_Create_Info_C,
                         Image_Format_List_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Attachment_Description_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Attachment_Description_2_C,
                         Attachment_Description_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Attachment_Reference_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Attachment_Reference_2_C,
                         Attachment_Reference_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Subpass_Description_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Subpass_Description_2_C,
                         Subpass_Description_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Subpass_Dependency_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Subpass_Dependency_2_C, Subpass_Dependency_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Render_Pass_Create_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Render_Pass_Create_Info_2_C,
                         Render_Pass_Create_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Subpass_Begin_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Subpass_Begin_Info_C, Subpass_Begin_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Subpass_End_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Subpass_End_Info_C, Subpass_End_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Descriptor_Set_Layout_Binding_Flags_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Descriptor_Set_Layout_Binding_Flags_Create_Info_C,
                      Descriptor_Set_Layout_Binding_Flags_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
              (Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C,
               Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Subpass_Description_Depth_Stencil_Resolve_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Subpass_Description_Depth_Stencil_Resolve_C,
                         Subpass_Description_Depth_Stencil_Resolve_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Stencil_Usage_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Stencil_Usage_Create_Info_C,
                         Image_Stencil_Usage_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Sampler_Reduction_Mode_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Sampler_Reduction_Mode_Create_Info_C,
                         Sampler_Reduction_Mode_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Framebuffer_Attachment_Image_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Framebuffer_Attachment_Image_Info_C,
                         Framebuffer_Attachment_Image_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Framebuffer_Attachments_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Framebuffer_Attachments_Create_Info_C,
                         Framebuffer_Attachments_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Render_Pass_Attachment_Begin_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Render_Pass_Attachment_Begin_Info_C,
                         Render_Pass_Attachment_Begin_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Semaphore_Type_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Semaphore_Type_Create_Info_C,
                         Semaphore_Type_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Timeline_Semaphore_Submit_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Timeline_Semaphore_Submit_Info_C,
                         Timeline_Semaphore_Submit_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Semaphore_Wait_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Semaphore_Wait_Info_C, Semaphore_Wait_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Semaphore_Signal_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Semaphore_Signal_Info_C,
                         Semaphore_Signal_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Device_Address_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Buffer_Device_Address_Info_C,
                         Buffer_Device_Address_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Opaque_Capture_Address_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Buffer_Opaque_Capture_Address_Create_Info_C,
                         Buffer_Opaque_Capture_Address_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Opaque_Capture_Address_Allocate_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Memory_Opaque_Capture_Address_Allocate_Info_C,
                         Memory_Opaque_Capture_Address_Allocate_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Memory_Opaque_Capture_Address_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Memory_Opaque_Capture_Address_Info_C,
                         Device_Memory_Opaque_Capture_Address_Info_C_Access);
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
            when Physical_Device_Vulkan_1_1_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Vulkan_1_1_Features_C,
                         Physical_Device_Vulkan_1_1_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_1_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Vulkan_1_1_Properties_C,
                         Physical_Device_Vulkan_1_1_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_2_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Vulkan_1_2_Features_C,
                         Physical_Device_Vulkan_1_2_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_2_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Vulkan_1_2_Properties_C,
                         Physical_Device_Vulkan_1_2_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_8Bit_Storage_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_8Bit_Storage_Features_C,
                         Physical_Device_8Bit_Storage_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Driver_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Driver_Properties_C,
                         Physical_Device_Driver_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Atomic_Int64_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Shader_Atomic_Int64_Features_C,
                         Physical_Device_Shader_Atomic_Int64_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Float16_Int8_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Shader_Float16_Int8_Features_C,
                         Physical_Device_Shader_Float16_Int8_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Float_Controls_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Float_Controls_Properties_C,
                         Physical_Device_Float_Controls_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Descriptor_Indexing_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Descriptor_Indexing_Features_C,
                       Physical_Device_Descriptor_Indexing_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Descriptor_Indexing_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Descriptor_Indexing_Properties_C,
                       Physical_Device_Descriptor_Indexing_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
             (Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C,
              Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Depth_Stencil_Resolve_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                    (Physical_Device_Depth_Stencil_Resolve_Properties_C,
                     Physical_Device_Depth_Stencil_Resolve_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Scalar_Block_Layout_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Scalar_Block_Layout_Features_C,
                         Physical_Device_Scalar_Block_Layout_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Sampler_Filter_Minmax_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                    (Physical_Device_Sampler_Filter_Minmax_Properties_C,
                     Physical_Device_Sampler_Filter_Minmax_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Vulkan_Memory_Model_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Vulkan_Memory_Model_Features_C,
                         Physical_Device_Vulkan_Memory_Model_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Imageless_Framebuffer_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Imageless_Framebuffer_Features_C,
                       Physical_Device_Imageless_Framebuffer_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
             (Physical_Device_Uniform_Buffer_Standard_Layout_Features_C,
              Physical_Device_Uniform_Buffer_Standard_Layout_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Subgroup_Extended_Types_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
             (Physical_Device_Shader_Subgroup_Extended_Types_Features_C,
              Physical_Device_Shader_Subgroup_Extended_Types_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
             (Physical_Device_Separate_Depth_Stencil_Layouts_Features_C,
              Physical_Device_Separate_Depth_Stencil_Layouts_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Attachment_Reference_Stencil_Layout_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Attachment_Reference_Stencil_Layout_C,
                         Attachment_Reference_Stencil_Layout_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Attachment_Description_Stencil_Layout_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Attachment_Description_Stencil_Layout_C,
                         Attachment_Description_Stencil_Layout_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Host_Query_Reset_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Host_Query_Reset_Features_C,
                         Physical_Device_Host_Query_Reset_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Timeline_Semaphore_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Timeline_Semaphore_Features_C,
                         Physical_Device_Timeline_Semaphore_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Timeline_Semaphore_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Physical_Device_Timeline_Semaphore_Properties_C,
                        Physical_Device_Timeline_Semaphore_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Buffer_Device_Address_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Buffer_Device_Address_Features_C,
                       Physical_Device_Buffer_Device_Address_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.C_V1_2;


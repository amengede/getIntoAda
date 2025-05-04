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

with Vulkan.C_Arrays;
with Vulkan.C;

private package Vulkan.C_V1_2 is
    -- Structure classifications.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in 
            Physical_Device_Vulkan_1_1_Features_Type |
            Physical_Device_Vulkan_1_1_Properties_Type |
            Physical_Device_Vulkan_1_2_Features_Type |
            Physical_Device_Vulkan_1_2_Properties_Type |
            Image_Format_List_Create_Info_Type |
            Attachment_Description_2_Type |
            Attachment_Reference_2_Type |
            Subpass_Description_2_Type |
            Subpass_Dependency_2_Type |
            Render_Pass_Create_Info_2_Type |
            Subpass_Begin_Info_Type |
            Subpass_End_Info_Type |
            Physical_Device_8Bit_Storage_Features_Type |
            Physical_Device_Driver_Properties_Type |
            Physical_Device_Shader_Atomic_Int64_Features_Type |
            Physical_Device_Shader_Float16_Int8_Features_Type |
            Physical_Device_Float_Controls_Properties_Type |
            Descriptor_Set_Layout_Binding_Flags_Create_Info_Type |
            Physical_Device_Descriptor_Indexing_Features_Type |
            Physical_Device_Descriptor_Indexing_Properties_Type |
            Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_Type |
            Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type |
            Subpass_Description_Depth_Stencil_Resolve_Type |
            Physical_Device_Depth_Stencil_Resolve_Properties_Type |
            Physical_Device_Scalar_Block_Layout_Features_Type |
            Image_Stencil_Usage_Create_Info_Type |
            Sampler_Reduction_Mode_Create_Info_Type |
            Physical_Device_Sampler_Filter_Minmax_Properties_Type |
            Physical_Device_Vulkan_Memory_Model_Features_Type |
            Physical_Device_Imageless_Framebuffer_Features_Type |
            Framebuffer_Attachment_Image_Info_Type |
            Framebuffer_Attachments_Create_Info_Type |
            Render_Pass_Attachment_Begin_Info_Type |
            Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type |
            Physical_Device_Shader_Subgroup_Extended_Types_Features_Type |
            Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type |
            Attachment_Reference_Stencil_Layout_Type |
            Attachment_Description_Stencil_Layout_Type |
            Physical_Device_Host_Query_Reset_Features_Type |
            Physical_Device_Timeline_Semaphore_Features_Type |
            Physical_Device_Timeline_Semaphore_Properties_Type |
            Semaphore_Type_Create_Info_Type |
            Timeline_Semaphore_Submit_Info_Type |
            Semaphore_Wait_Info_Type |
            Semaphore_Signal_Info_Type |
            Physical_Device_Buffer_Device_Address_Features_Type |
            Buffer_Device_Address_Info_Type |
            Buffer_Opaque_Capture_Address_Create_Info_Type |
            Memory_Opaque_Capture_Address_Allocate_Info_Type |
            Device_Memory_Opaque_Capture_Address_Info_Type;
    
    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in
            Physical_Device_Vulkan_1_1_Features_Type |
            Physical_Device_Vulkan_1_1_Properties_Type |
            Physical_Device_Vulkan_1_2_Features_Type |
            Physical_Device_Vulkan_1_2_Properties_Type |
            Physical_Device_8Bit_Storage_Features_Type |
            Physical_Device_Driver_Properties_Type |
            Physical_Device_Shader_Atomic_Int64_Features_Type |
            Physical_Device_Shader_Float16_Int8_Features_Type |
            Physical_Device_Float_Controls_Properties_Type |
            Physical_Device_Descriptor_Indexing_Features_Type |
            Physical_Device_Descriptor_Indexing_Properties_Type |
            Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type |
            Physical_Device_Depth_Stencil_Resolve_Properties_Type |
            Physical_Device_Scalar_Block_Layout_Features_Type |
            Physical_Device_Sampler_Filter_Minmax_Properties_Type |
            Physical_Device_Vulkan_Memory_Model_Features_Type |
            Physical_Device_Imageless_Framebuffer_Features_Type |
            Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type |
            Physical_Device_Shader_Subgroup_Extended_Types_Features_Type |
            Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type |
            Attachment_Reference_Stencil_Layout_Type |
            Attachment_Description_Stencil_Layout_Type |
            Physical_Device_Host_Query_Reset_Features_Type |
            Physical_Device_Timeline_Semaphore_Features_Type |
            Physical_Device_Timeline_Semaphore_Properties_Type |
            Physical_Device_Buffer_Device_Address_Features_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C compatible records.
    type Physical_Device_Vulkan_1_1_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vulkan_1_1_Features_Type;
        Next: C.Out_Structure_C_Access;
        Storage_Buffer_16Bit_Access: Interfaces.Unsigned_32;
        Uniform_And_Storage_Buffer_16Bit_Access: Interfaces.Unsigned_32;
        Storage_Push_Constant_16: Interfaces.Unsigned_32;
        Storage_Input_Output_16: Interfaces.Unsigned_32;
        Multiview: Interfaces.Unsigned_32;
        Multiview_Geometry_Shader: Interfaces.Unsigned_32;
        Multiview_Tessellation_Shader: Interfaces.Unsigned_32;
        Variable_Pointers_Storage_Buffer: Interfaces.Unsigned_32;
        Variable_Pointers: Interfaces.Unsigned_32;
        Protected_Memory: Interfaces.Unsigned_32;
        Sampler_YCbCr_Conversion: Interfaces.Unsigned_32;
        Shader_Draw_Parameters: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_1_1_Features_C_Access is
        access Physical_Device_Vulkan_1_1_Features_C
        with Convention => C;
    
    type Physical_Device_Vulkan_1_1_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vulkan_1_1_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Device_UUID: UUID;
        Driver_UUID: UUID;
        Device_LUID: LUID;
        Device_Node_Mask: Interfaces.Unsigned_32;
        Device_LUID_Valid: Interfaces.Unsigned_32;
        Subgroup_Size: Interfaces.Unsigned_32;
        Subgroup_Supported_Stages: Shader_Stage_Flags;
        Subgroup_Supported_Operations: Subgroup_Feature_Flags;
        Subgroup_Quad_Operations_In_All_Stages: Interfaces.Unsigned_32;
        Point_Clipping_Behavior: Vulkan.Point_Clipping_Behavior;
        Max_Multiview_View_Count: Interfaces.Unsigned_32;
        Max_Multiview_Instance_Index: Interfaces.Unsigned_32;
        Protected_No_Fault: Interfaces.Unsigned_32;
        Max_Per_Set_Descriptors: Interfaces.Unsigned_32;
        Max_Memory_Allocation_Size: Device_Size;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_1_1_Properties_C_Access is
        access Physical_Device_Vulkan_1_1_Properties_C
        with Convention => C;

    type Physical_Device_Vulkan_1_2_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vulkan_1_2_Features_Type;
        Next: C.Out_Structure_C_Access;
        Sampler_Mirror_Clamp_To_Edge: Interfaces.Unsigned_32;
        Draw_Indirect_Count: Interfaces.Unsigned_32;
        Storage_Buffer_8Bit_Access: Interfaces.Unsigned_32;
        Uniform_And_Storage_Buffer_8Bit_Access: Interfaces.Unsigned_32;
        Storage_Push_Constant_8: Interfaces.Unsigned_32;
        Shader_Buffer_Int64_Atomics: Interfaces.Unsigned_32;
        Shader_Shared_Int64_Atomics: Interfaces.Unsigned_32;
        Shader_Float16: Interfaces.Unsigned_32;
        Shader_Int8: Interfaces.Unsigned_32;
        Descriptor_Indexing: Interfaces.Unsigned_32;
        Shader_Input_Attachment_Array_Dynamic_Indexing: Interfaces.Unsigned_32;
        Shader_Uniform_Texel_Buffer_Array_Dynamic_Indexing:
            Interfaces.Unsigned_32;
        Shader_Storage_Texel_Buffer_Array_Dynamic_Indexing:
            Interfaces.Unsigned_32;
        Shader_Uniform_Buffer_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Shader_Sampled_Image_Array_Non_Uniform_Indexing: Interfaces.Unsigned_32;
        Shader_Storage_Buffer_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Shader_Storage_Image_Array_Non_Uniform_Indexing: Interfaces.Unsigned_32;
        Shader_Input_Attachment_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Shader_Uniform_Texel_Buffer_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Shader_Storage_Texel_Buffer_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Uniform_Buffer_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Sampled_Image_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Storage_Image_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Storage_Buffer_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Uniform_Texel_Buffer_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Storage_Texel_Buffer_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Update_Unused_While_Pending: Interfaces.Unsigned_32;
        Descriptor_Binding_Partially_Bound: Interfaces.Unsigned_32;
        Descriptor_Binding_Variable_Descriptor_Count: Interfaces.Unsigned_32;
        Runtime_Descriptor_Array: Interfaces.Unsigned_32;
        Sampler_Filter_Minmax: Interfaces.Unsigned_32;
        Scalar_Block_Layout: Interfaces.Unsigned_32;
        Imageless_Framebuffer: Interfaces.Unsigned_32;
        Uniform_Buffer_Standard_Layout: Interfaces.Unsigned_32;
        Shader_Subgroup_Extended_Types: Interfaces.Unsigned_32;
        Separate_Depth_Stencil_Layouts: Interfaces.Unsigned_32;
        Host_Query_Reset: Interfaces.Unsigned_32;
        Timeline_Semaphore: Interfaces.Unsigned_32;
        Buffer_Device_Address: Interfaces.Unsigned_32;
        Buffer_Device_Address_Capture_Replay: Interfaces.Unsigned_32;
        Buffer_Device_Address_Multi_Device: Interfaces.Unsigned_32;
        Vulkan_Memory_Model: Interfaces.Unsigned_32;
        Vulkan_Memory_Model_Device_Scope: Interfaces.Unsigned_32;
        Vulkan_Memory_Model_Availability_Visibility_Chains:
            Interfaces.Unsigned_32;
        Shader_Output_Viewport_Index: Interfaces.Unsigned_32;
        Shader_Output_Layer: Interfaces.Unsigned_32;
        Subgroup_Broadcast_Dynamic_ID: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_1_2_Features_C_Access is
        access Physical_Device_Vulkan_1_2_Features_C
        with Convention => C;

    type Physical_Device_Vulkan_1_2_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vulkan_1_2_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Driver_ID: Vulkan.Driver_ID;
        Driver_Name: Interfaces.C.char_array(1 .. Max_Driver_Name_Size);
        Driver_Info: Interfaces.C.char_array(1 .. Max_Driver_Info_Size);
        Conformance_Version: Vulkan.Conformance_Version;
        Denorm_Behavior_Independence: Shader_Float_Controls_Independence;
        Rounding_Mode_Independence: Shader_Float_Controls_Independence;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float16: Interfaces.Unsigned_32;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float32: Interfaces.Unsigned_32;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float64: Interfaces.Unsigned_32;
        Shader_Denorm_Preserve_Float16: Interfaces.Unsigned_32;
        Shader_Denorm_Preserve_Float32: Interfaces.Unsigned_32;
        Shader_Denorm_Preserve_Float64: Interfaces.Unsigned_32;
        Shader_Denorm_Flush_To_Zero_Float16: Interfaces.Unsigned_32;
        Shader_Denorm_Flush_To_Zero_Float32: Interfaces.Unsigned_32;
        Shader_Denorm_Flush_To_Zero_Float64: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTE_Float16: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTE_Float32: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTE_Float64: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTZ_Float16: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTZ_Float32: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTZ_Float64: Interfaces.Unsigned_32;
        Max_Update_After_Bind_Descriptors_In_All_Pools: Interfaces.Unsigned_32;
        Shader_Uniform_Buffer_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Shader_Sampled_Image_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Shader_Storage_Buffer_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Shader_Storage_Image_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Shader_Input_Attachment_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Robust_Buffer_Access_Update_After_Bind: Interfaces.Unsigned_32;
        Quad_Divergent_Implicit_LOD: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Samplers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Uniform_Buffers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Buffers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Sampled_Images:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Images:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Input_Attachments:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Update_After_Bind_Resources: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Samplers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers_Dynamic:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Buffers:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Buffers_Dynamic:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Sampled_Images:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Images:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Input_Attachments:
            Interfaces.Unsigned_32;
        Supported_Depth_Resolve_Modes: Resolve_Mode_Flags;
        Supported_Stencil_Resolve_Modes: Resolve_Mode_Flags;
        Independent_Resolve_None: Interfaces.Unsigned_32;
        Independent_Resolve: Interfaces.Unsigned_32;
        Filter_Minmax_Single_Component_Formats: Interfaces.Unsigned_32;
        Filter_Minmax_Image_Component_Mapping: Interfaces.Unsigned_32;
        Max_Timeline_Semaphore_Value_Difference: Semaphore_Value;
        Framebuffer_Integer_Color_Sample_Counts: Sample_Count_Flags;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_1_2_Properties_C_Access is
        access Physical_Device_Vulkan_1_2_Properties_C
        with Convention => C;

    package Format_Arrays is new C_Arrays(Format);

    type Image_Format_List_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Image_Format_List_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        View_Format_Count: Interfaces.Unsigned_32;
        View_Formats: Format_Arrays.Pointer;
    end record
        with Convention => C;

    type Image_Format_List_Create_Info_C_Access is
        access Image_Format_List_Create_Info_C
        with Convention => C;

    type Attachment_Description_2_C is
    record
        Record_Type: In_Structure_Type := Attachment_Description_2_Type;
        Next: C.In_Structure_C_Access;
        Flags: Attachment_Description_Flags;
        Format: Vulkan.Format;
        Samples: Sample_Count_Flags;
        Load_Op: Attachment_Load_Op;
        Store_Op: Attachment_Store_Op;
        Stencil_Load_Op: Attachment_Load_Op;
        Stencil_Store_Op: Attachment_Store_Op;
        Initial_Layout: Image_Layout;
        Final_Layout: Image_Layout;
    end record
        with Convention => C;

    type Attachment_Description_2_C_Access is access Attachment_Description_2_C
        with Convention => C;

    type Attachment_Reference_2_C is
    record
        Record_Type: In_Structure_Type := Attachment_Reference_2_Type;
        Next: C.In_Structure_C_Access;
        Attachment: Interfaces.Unsigned_32;
        Layout: Image_Layout;
        Aspect_Mask: Image_Aspect_Flags;
    end record
        with Convention => C;

    type Attachment_Reference_2_C_Access is access Attachment_Reference_2_C
        with Convention => C;

    package Attachment_Reference_2_C_Arrays is
        new C_Arrays(Attachment_Reference_2_C);

    type Subpass_Description_2_C is
    record
        Record_Type: In_Structure_Type := Subpass_Description_2_Type;
        Next: C.In_Structure_C_Access;
        Flags: Subpass_Description_Flags;
        Pipeline_Bind_Point: Vulkan.Pipeline_Bind_Point;
        View_Mask: Interfaces.Unsigned_32;
        Input_Attachment_Count: Interfaces.Unsigned_32;
        Input_Attachments: Attachment_Reference_2_C_Arrays.Pointer;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachments: Attachment_Reference_2_C_Arrays.Pointer;
        Resolve_Attachments: Attachment_Reference_2_C_Arrays.Pointer;
        Depth_Stencil_Attachment: Attachment_Reference_2_C_Access;
        Preserve_Attachment_Count: Interfaces.Unsigned_32;
        Preserve_Attachments: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Subpass_Description_2_C_Access is access Subpass_Description_2_C
        with Convention => C;

    type Subpass_Dependency_2_C is
    record
        Record_Type: In_Structure_Type := Subpass_Dependency_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Subpass: Interfaces.Unsigned_32;
        Dst_Subpass: Interfaces.Unsigned_32;
        Src_Stage_Mask: Pipeline_Stage_Flags;
        Dst_Stage_Mask: Pipeline_Stage_Flags;
        Src_Access_Mask: Access_Flags;
        Dst_Access_Mask: Access_Flags;
        Dependency_Flags: Vulkan.Dependency_Flags;
        View_Offset: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Subpass_Dependency_2_C_Access is access Subpass_Dependency_2_C
        with Convention => C;

    package Attachment_Description_2_C_Arrays is
        new C_Arrays(Attachment_Description_2_C);
    package Subpass_Description_2_C_Arrays is
        new C_Arrays(Subpass_Description_2_C);
    package Subpass_Dependency_2_C_Arrays is
        new C_Arrays(Subpass_Dependency_2_C);

    type Render_Pass_Create_Info_2_C is
    record
        Record_Type: In_Structure_Type := Render_Pass_Create_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Attachment_Count: Interfaces.Unsigned_32;
        Attachments: Attachment_Description_2_C_Arrays.Pointer;
        Subpass_Count: Interfaces.Unsigned_32;
        Subpasses: Subpass_Description_2_C_Arrays.Pointer;
        Dependency_Count: Interfaces.Unsigned_32;
        Dependencies: Subpass_Dependency_2_C_Arrays.Pointer;
        Correlated_View_Mask_Count: Interfaces.Unsigned_32;
        Correlated_View_Masks: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Render_Pass_Create_Info_2_C_Access is
        access Render_Pass_Create_Info_2_C
        with Convention => C;

    type Subpass_Begin_Info_C is
    record
        Record_Type: In_Structure_Type := Subpass_Begin_Info_Type;
        Next: C.In_Structure_C_Access;
        Contents: Subpass_Contents;
    end record
        with Convention => C;

    type Subpass_Begin_Info_C_Access is access Subpass_Begin_Info_C
        with Convention => C;

    type Subpass_End_Info_C is
    record
        Record_Type: In_Structure_Type := Subpass_End_Info_Type;
        Next: C.In_Structure_C_Access;
    end record
        with Convention => C;

    type Subpass_End_Info_C_Access is access Subpass_End_Info_C
        with Convention => C;

    type Physical_Device_8Bit_Storage_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_8Bit_Storage_Features_Type;
        Next: C.Out_Structure_C_Access;
        Storage_Buffer_8Bit_Access: Interfaces.Unsigned_32;
        Uniform_And_Storage_Buffer_8Bit_Access: Interfaces.Unsigned_32;
        Storage_Push_Constant_8: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_8Bit_Storage_Features_C_Access is
        access Physical_Device_8Bit_Storage_Features_C
        with Convention => C;

    type Physical_Device_Driver_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Driver_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Driver_ID: Vulkan.Driver_ID;
        Driver_Name: Interfaces.C.char_array(1 .. Max_Driver_Name_Size);
        Driver_Info: Interfaces.C.char_array(1 .. Max_Driver_Info_Size);
        Conformance_Version: Vulkan.Conformance_Version;
    end record
        with Convention => C;

    type Physical_Device_Driver_Properties_C_Access is
        access Physical_Device_Driver_Properties_C
        with Convention => C;

    type Physical_Device_Shader_Atomic_Int64_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Atomic_Int64_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Buffer_Int64_Atomics: Interfaces.Unsigned_32;
        Shader_Shared_Int64_Atomics: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Atomic_Int64_Features_C_Access is
        access Physical_Device_Shader_Atomic_Int64_Features_C
        with Convention => C;

    type Physical_Device_Shader_Float16_Int8_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Float16_Int8_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Float16: Interfaces.Unsigned_32;
        Shader_Int8: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Float16_Int8_Features_C_Access is
        access Physical_Device_Shader_Float16_Int8_Features_C
        with Convention => C;

    type Physical_Device_Float_Controls_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Float_Controls_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Denorm_Behavior_Independence: Shader_Float_Controls_Independence;
        Rounding_Mode_Independence: Shader_Float_Controls_Independence;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float16: Interfaces.Unsigned_32;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float32: Interfaces.Unsigned_32;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float64: Interfaces.Unsigned_32;
        Shader_Denorm_Preserve_Float16: Interfaces.Unsigned_32;
        Shader_Denorm_Preserve_Float32: Interfaces.Unsigned_32;
        Shader_Denorm_Preserve_Float64: Interfaces.Unsigned_32;
        Shader_Denorm_Flush_To_Zero_Float16: Interfaces.Unsigned_32;
        Shader_Denorm_Flush_To_Zero_Float32: Interfaces.Unsigned_32;
        Shader_Denorm_Flush_To_Zero_Float64: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTE_Float16: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTE_Float32: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTE_Float64: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTZ_Float16: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTZ_Float32: Interfaces.Unsigned_32;
        Shader_Rounding_Mode_RTZ_Float64: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Float_Controls_Properties_C_Access is
        access Physical_Device_Float_Controls_Properties_C
        with Convention => C;

    package Descriptor_Binding_Flags_Arrays is
        new C_Arrays(Descriptor_Binding_Flags);

    type Descriptor_Set_Layout_Binding_Flags_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Descriptor_Set_Layout_Binding_Flags_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Binding_Count: Interfaces.Unsigned_32;
        Binding_Flags: Descriptor_Binding_Flags_Arrays.Pointer;
    end record
        with Convention => C;

    type Descriptor_Set_Layout_Binding_Flags_Create_Info_C_Access is
        access Descriptor_Set_Layout_Binding_Flags_Create_Info_C
        with Convention => C;

    type Physical_Device_Descriptor_Indexing_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Descriptor_Indexing_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Input_Attachment_Array_Dynamic_Indexing: Interfaces.Unsigned_32;
        Shader_Uniform_Texel_Buffer_Array_Dynamic_Indexing:
            Interfaces.Unsigned_32;
        Shader_Storage_Texel_Buffer_Array_Dynamic_Indexing:
            Interfaces.Unsigned_32;
        Shader_Uniform_Buffer_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Shader_Sampled_Image_Array_Non_Uniform_Indexing: Interfaces.Unsigned_32;
        Shader_Storage_Buffer_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Shader_Storage_Image_Array_Non_Uniform_Indexing: Interfaces.Unsigned_32;
        Shader_Input_Attachment_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Shader_Uniform_Texel_Buffer_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Shader_Storage_Texel_Buffer_Array_Non_Uniform_Indexing:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Uniform_Buffer_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Sampled_Image_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Storage_Image_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Storage_Buffer_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Uniform_Texel_Buffer_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Storage_Texel_Buffer_Update_After_Bind:
            Interfaces.Unsigned_32;
        Descriptor_Binding_Update_Unused_While_Pending: Interfaces.Unsigned_32;
        Descriptor_Binding_Partially_Bound: Interfaces.Unsigned_32;
        Descriptor_Binding_Variable_Descriptor_Count: Interfaces.Unsigned_32;
        Runtime_Descriptor_Array: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Descriptor_Indexing_Features_C_Access is
        access Physical_Device_Descriptor_Indexing_Features_C
        with Convention => C;

    type Physical_Device_Descriptor_Indexing_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Descriptor_Indexing_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Update_After_Bind_Descriptors_In_All_Pools: Interfaces.Unsigned_32;
        Shader_Uniform_Buffer_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Shader_Sampled_Image_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Shader_Storage_Buffer_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Shader_Storage_Image_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Shader_Input_Attachment_Array_Non_Uniform_Indexing_Native:
            Interfaces.Unsigned_32;
        Robust_Buffer_Access_Update_After_Bind: Interfaces.Unsigned_32;
        Quad_Divergent_Implicit_LOD: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Samplers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Uniform_Buffers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Buffers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Sampled_Images:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Images:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Input_Attachments:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Update_After_Bind_Resources: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Samplers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers_Dynamic:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Buffers:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Buffers_Dynamic:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Sampled_Images:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Images:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Input_Attachments:
            Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Descriptor_Indexing_Properties_C_Access is
        access Physical_Device_Descriptor_Indexing_Properties_C
        with Convention => C;

    type Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_Type;
        Next: C.In_Structure_C_Access;
        Descriptor_Set_Count: Interfaces.Unsigned_32;
        Descriptor_Counts: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C_Access is
        access Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C
        with Convention => C;

    type Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C is
    record
        Record_Type: Out_Structure_Type :=
            Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type;
        Next: C.Out_Structure_C_Access;
        Max_Variable_Descriptor_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C_Access is
        access Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C
        with Convention => C;

    type Subpass_Description_Depth_Stencil_Resolve_C is
    record
        Record_Type: In_Structure_Type :=
            Subpass_Description_Depth_Stencil_Resolve_Type;
        Next: C.In_Structure_C_Access;
        Depth_Resolve_Mode: Resolve_Mode_Flags;
        Stencil_Resolve_Mode: Resolve_Mode_Flags;
        Depth_Stencil_Resolve_Attachment: Attachment_Reference_2_C_Access;
    end record
        with Convention => C;

    type Subpass_Description_Depth_Stencil_Resolve_C_Access is
        access Subpass_Description_Depth_Stencil_Resolve_C
        with Convention => C;

    type Physical_Device_Depth_Stencil_Resolve_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Depth_Stencil_Resolve_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Supported_Depth_Resolve_Modes: Resolve_Mode_Flags;
        Supported_Stencil_Resolve_Modes: Resolve_Mode_Flags;
        Independent_Resolve_None: Interfaces.Unsigned_32;
        Independent_Resolve: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Depth_Stencil_Resolve_Properties_C_Access is
        access Physical_Device_Depth_Stencil_Resolve_Properties_C
        with Convention => C;

    type Physical_Device_Scalar_Block_Layout_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Scalar_Block_Layout_Features_Type;
        Next: C.Out_Structure_C_Access;
        Scalar_Block_Layout: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Scalar_Block_Layout_Features_C_Access is
        access Physical_Device_Scalar_Block_Layout_Features_C
        with Convention => C;

    type Image_Stencil_Usage_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Image_Stencil_Usage_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Stencil_Usage: Image_Usage_Flags;
    end record
        with Convention => C;

    type Image_Stencil_Usage_Create_Info_C_Access is
        access Image_Stencil_Usage_Create_Info_C
        with Convention => C;

    type Sampler_Reduction_Mode_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Sampler_Reduction_Mode_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Reduction_Mode: Sampler_Reduction_Mode;
    end record
        with Convention => C;

    type Sampler_Reduction_Mode_Create_Info_C_Access is
        access Sampler_Reduction_Mode_Create_Info_C
        with Convention => C;

    type Physical_Device_Sampler_Filter_Minmax_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Sampler_Filter_Minmax_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Filter_Minmax_Single_Component_Formats: Interfaces.Unsigned_32;
        Filter_Minmax_Image_Component_Mapping: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Sampler_Filter_Minmax_Properties_C_Access is
        access Physical_Device_Sampler_Filter_Minmax_Properties_C
        with Convention => C;

    type Physical_Device_Vulkan_Memory_Model_Features_C is
    record
        Record_Type: Out_Structure_Type := 
            Physical_Device_Vulkan_Memory_Model_Features_Type;
        Next: C.Out_Structure_C_Access;
        Vulkan_Memory_Model: Interfaces.Unsigned_32;
        Vulkan_Memory_Model_Device_Scope: Interfaces.Unsigned_32;
        Vulkan_Memory_Model_Availability_Visibility_Chains:
            Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_Memory_Model_Features_C_Access is
        access Physical_Device_Vulkan_Memory_Model_Features_C
        with Convention => C;

    type Physical_Device_Imageless_Framebuffer_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Imageless_Framebuffer_Features_Type;
        Next: C.Out_Structure_C_Access;
        Imageless_Framebuffer: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Imageless_Framebuffer_Features_C_Access is
        access Physical_Device_Imageless_Framebuffer_Features_C
        with Convention => C;

    type Framebuffer_Attachment_Image_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Framebuffer_Attachment_Image_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Image_Create_Flags;
        Usage: Image_Usage_Flags;
        Width: Vulkan.Width;
        Height: Vulkan.Height;
        Layer_Count: Vulkan.Array_Layers;
        View_Format_Count: Interfaces.Unsigned_32;
        View_Formats: Format_Arrays.Pointer;
    end record
        with Convention => C;

    type Framebuffer_Attachment_Image_Info_C_Access is
        access Framebuffer_Attachment_Image_Info_C
        with Convention => C;

    package Framebuffer_Attachment_Image_Info_C_Arrays is
        new C_Arrays(Framebuffer_Attachment_Image_Info_C);

    type Framebuffer_Attachments_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Framebuffer_Attachments_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Attachment_Image_Info_Count: Interfaces.Unsigned_32;
        Attachment_Image_Infos:
            Framebuffer_Attachment_Image_Info_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Framebuffer_Attachments_Create_Info_C_Access is
        access Framebuffer_Attachments_Create_Info_C
        with Convention => C;

    type Render_Pass_Attachment_Begin_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Render_Pass_Attachment_Begin_Info_Type;
        Next: C.In_Structure_C_Access;
        Attachment_Count: Interfaces.Unsigned_32;
        Attachments: C.Image_View_Arrays.Pointer;
    end record
        with Convention => C;

    type Render_Pass_Attachment_Begin_Info_C_Access is
        access Render_Pass_Attachment_Begin_Info_C
        with Convention => C;

    type Physical_Device_Uniform_Buffer_Standard_Layout_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type;
        Next: C.Out_Structure_C_Access;
        Uniform_Buffer_Standard_Layout: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Uniform_Buffer_Standard_Layout_Features_C_Access is
        access Physical_Device_Uniform_Buffer_Standard_Layout_Features_C
        with Convention => C;

    type Physical_Device_Shader_Subgroup_Extended_Types_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Subgroup_Extended_Types_Features_Type;
        Next: C.Out_Structure_C_Access;
    Shader_Subgroup_Extended_Types: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Subgroup_Extended_Types_Features_C_Access is
        access Physical_Device_Shader_Subgroup_Extended_Types_Features_C
        with Convention => C;

    type Physical_Device_Separate_Depth_Stencil_Layouts_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type;
        Next: C.Out_Structure_C_Access;
        Separate_Depth_Stencil_Layouts: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Separate_Depth_Stencil_Layouts_Features_C_Access is
        access Physical_Device_Separate_Depth_Stencil_Layouts_Features_C
        with Convention => C;

    type Attachment_Reference_Stencil_Layout_C is
    record
        Record_Type: Out_Structure_Type :=
            Attachment_Reference_Stencil_Layout_Type;
        Next: C.Out_Structure_C_Access;
        Stencil_Layout: Image_Layout;
    end record
        with Convention => C;

    type Attachment_Reference_Stencil_Layout_C_Access is
        access Attachment_Reference_Stencil_Layout_C
        with Convention => C;

    type Attachment_Description_Stencil_Layout_C is
    record
        Record_Type: Out_Structure_Type :=
            Attachment_Description_Stencil_Layout_Type;
        Next: C.Out_Structure_C_Access;
        Stencil_Initial_Layout: Image_Layout;
        Stencil_Final_Layout: Image_Layout;
    end record
        with Convention => C;

    type Attachment_Description_Stencil_Layout_C_Access is
        access Attachment_Description_Stencil_Layout_C
        with Convention => C;

    type Physical_Device_Host_Query_Reset_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Host_Query_Reset_Features_Type;
        Next: C.Out_Structure_C_Access;
        Host_Query_Reset: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Host_Query_Reset_Features_C_Access is
        access Physical_Device_Host_Query_Reset_Features_C
        with Convention => C;

    type Physical_Device_Timeline_Semaphore_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Timeline_Semaphore_Features_Type;
        Next: C.Out_Structure_C_Access;
        Timeline_Semaphore: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Timeline_Semaphore_Features_C_Access is
        access Physical_Device_Timeline_Semaphore_Features_C
        with Convention => C;

    type Physical_Device_Timeline_Semaphore_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Timeline_Semaphore_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Timeline_Semaphore_Value_Difference: Semaphore_Value;
    end record
        with Convention => C;

    type Physical_Device_Timeline_Semaphore_Properties_C_Access is
        access Physical_Device_Timeline_Semaphore_Properties_C
        with Convention => C;

    type Semaphore_Type_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Semaphore_Type_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Semaphore_Type: Vulkan.Semaphore_Type;
        Initial_Value: Semaphore_Value;
    end record
        with Convention => C;

    type Semaphore_Type_Create_Info_C_Access is
        access Semaphore_Type_Create_Info_C
        with Convention => C;

    package Semaphore_Value_Arrays is new C_Arrays(Semaphore_Value);

    type Timeline_Semaphore_Submit_Info_C is
    record
        Record_Type: In_Structure_Type := Timeline_Semaphore_Submit_Info_Type;
        Next: C.In_Structure_C_Access;
        Wait_Semaphore_Value_Count: Interfaces.Unsigned_32;
        Wait_Semaphore_Values: Semaphore_Value_Arrays.Pointer;
        Signal_Semaphore_Value_Count: Interfaces.Unsigned_32;
        Signal_Semaphore_Values: Semaphore_Value_Arrays.Pointer;
    end record
        with Convention => C;

    type Timeline_Semaphore_Submit_Info_C_Access is
        access Timeline_Semaphore_Submit_Info_C
        with Convention => C;

    type Semaphore_Wait_Info_C is
    record
        Record_Type: In_Structure_Type := Semaphore_Wait_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Semaphore_Wait_Flags;
        Semaphore_Count: Interfaces.Unsigned_32;
        Semaphores: C.Semaphore_Arrays.Pointer;
        Values: Semaphore_Value_Arrays.Pointer;
    end record
        with Convention => C;

    type Semaphore_Wait_Info_C_Access is access Semaphore_Wait_Info_C
        with Convention => C;

    type Semaphore_Signal_Info_C is
    record
        Record_Type: In_Structure_Type := Semaphore_Signal_Info_Type;
        Next: C.In_Structure_C_Access;
        Semaphore: Vulkan.Semaphore;
        Value: Semaphore_Value;
    end record
        with Convention => C;

    type Semaphore_Signal_Info_C_Access is access Semaphore_Signal_Info_C
        with Convention => C;

    type Physical_Device_Buffer_Device_Address_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Buffer_Device_Address_Features_Type;
        Next: C.Out_Structure_C_Access;
        Buffer_Device_Address: Interfaces.Unsigned_32;
        Buffer_Device_Address_Capture_Replay: Interfaces.Unsigned_32;
        Buffer_Device_Address_Multi_Device: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Buffer_Device_Address_Features_C_Access is
        access Physical_Device_Buffer_Device_Address_Features_C
        with Convention => C;

    type Buffer_Device_Address_Info_C is
    record
        Record_Type: In_Structure_Type := Buffer_Device_Address_Info_Type;
        Next: C.In_Structure_C_Access;
        Buffer: Vulkan.Buffer;
    end record
        with Convention => C;

    type Buffer_Device_Address_Info_C_Access is
        access Buffer_Device_Address_Info_C
        with Convention => C;

    type Buffer_Opaque_Capture_Address_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Buffer_Opaque_Capture_Address_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Opaque_Capture_Address: Interfaces.Unsigned_64;
    end record
        with Convention => C;

    type Buffer_Opaque_Capture_Address_Create_Info_C_Access is
        access Buffer_Opaque_Capture_Address_Create_Info_C
        with Convention => C;

    type Memory_Opaque_Capture_Address_Allocate_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Memory_Opaque_Capture_Address_Allocate_Info_Type;
        Next: C.In_Structure_C_Access;
        Opaque_Capture_Address: Interfaces.Unsigned_64;
    end record
        with Convention => C;

    type Memory_Opaque_Capture_Address_Allocate_Info_C_Access is
        access Memory_Opaque_Capture_Address_Allocate_Info_C
        with Convention => C;

    type Device_Memory_Opaque_Capture_Address_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Device_Memory_Opaque_Capture_Address_Info_Type;
        Next: C.In_Structure_C_Access;
        Memory: Device_Memory;
    end record
        with Convention => C;

    type Device_Memory_Opaque_Capture_Address_Info_C_Access is
        access Device_Memory_Opaque_Capture_Address_Info_C
        with Convention => C;

    -- Load all the function pointers.
    procedure Load(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    type vkCmdDrawIndirectCount_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Buffer: in Vulkan.Buffer;
                         Offset: in Device_Size;
                         Count_Buffer: in Vulkan.Buffer;
                         Count_Buffer_Offset: in Device_Size;
                         Max_Draw_Count, Stride: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdDrawIndirectCount: vkCmdDrawIndirectCount_Access;

    type vkCmdDrawIndexedIndirectCount_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Buffer: in Vulkan.Buffer;
                         Offset: in Device_Size;
                         Count_Buffer: in Vulkan.Buffer;
                         Count_Buffer_Offset: in Device_Size;
                         Max_Draw_Count, Stride: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdDrawIndexedIndirectCount: vkCmdDrawIndexedIndirectCount_Access;

    type vkCreateRenderPass2_Access is
        access function(Device: Vulkan.Device;
                        Create_Info: in Render_Pass_Create_Info_2_C;
                        Allocator: access constant Allocation_Callbacks;
                        Render_Pass: out Vulkan.Render_Pass) return Result
        with Convention => C;

    vkCreateRenderPass2: vkCreateRenderPass2_Access;

    type vkCmdBeginRenderPass2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Render_Pass_Begin: in C.Render_Pass_Begin_Info_C;
                         Subpass_Begin_Info: in Subpass_Begin_Info_C)
        with Convention => C;

    vkCmdBeginRenderPass2: vkCmdBeginRenderPass2_Access;

    type vkCmdNextSubpass2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Subpass_Begin_Info: in Subpass_Begin_Info_C;
                         Subpass_End_Info: in Subpass_End_Info_C)
        with Convention => C;

    vkCmdNextSubpass2: vkCmdNextSubpass2_Access;

    type vkCmdEndRenderPass2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Subpass_End_Info: in Subpass_End_Info_C)
        with Convention => C;

    vkCmdEndRenderPass2: vkCmdEndRenderPass2_Access;

    type vkResetQueryPool_Access is
        access procedure(Device: in Vulkan.Device;
                         Query_Pool: in Vulkan.Query_Pool;
                         First_Query, Query_Count: in Interfaces.Unsigned_32)
        with Convention => C;

    vkResetQueryPool: vkResetQueryPool_Access;

    type vkGetSemaphoreCounterValue_Access is
        access function(Device: in Vulkan.Device;
                        Semaphore: in Vulkan.Semaphore;
                        Value: out Semaphore_Value) return Result
        with Convention => C;

    vkGetSemaphoreCounterValue: vkGetSemaphoreCounterValue_Access;

    type vkWaitSemaphores_Access is
        access function(Device: in Vulkan.Device;
                        Wait_Info: in Semaphore_Wait_Info_C;
                        Timeout: in Interfaces.Unsigned_64) return Result
        with Convention => C;

    vkWaitSemaphores: vkWaitSemaphores_Access;

    type vkSignalSemaphore_Access is
        access function(Device: in Vulkan.Device;
                        Signal_Info: in Semaphore_Signal_Info_C) return Result
        with Convention => C;

    vkSignalSemaphore: vkSignalSemaphore_Access;

    type vkGetBufferDeviceAddress_Access is
        access function(Device: in Vulkan.Device;
                        Info: in Buffer_Device_Address_Info_C)
        return Device_Address
        with Convention => C;

    vkGetBufferDeviceAddress: vkGetBufferDeviceAddress_Access;

    type vkGetBufferOpaqueCaptureAddress_Access is
        access function(Device: in Vulkan.Device;
                        Info: in Buffer_Device_Address_Info_C)
        return Interfaces.Unsigned_64
        with Convention => C;

    vkGetBufferOpaqueCaptureAddress: vkGetBufferOpaqueCaptureAddress_Access;

    type vkGetDeviceMemoryOpaqueCaptureAddress_Access is
        access function(Device: in Vulkan.Device;
                        Info: in Device_Memory_Opaque_Capture_Address_Info_C)
        return Interfaces.Unsigned_64
        with Convention => C;

    vkGetDeviceMemoryOpaqueCaptureAddress:
        vkGetDeviceMemoryOpaqueCaptureAddress_Access;

    -- Record conversions and management.
    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_1_Features;
                     C_Struct: in Physical_Device_Vulkan_1_1_Features_C);
 
    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_1_Properties;
                     C_Struct: in Physical_Device_Vulkan_1_1_Properties_C);
   
    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_2_Features;
                     C_Struct: in Physical_Device_Vulkan_1_2_Features_C);
 
    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_2_Properties;
                     C_Struct: in Physical_Device_Vulkan_1_2_Properties_C);

    function To_C(Struct: in Image_Format_List_Create_Info)
        return Image_Format_List_Create_Info_C;
    procedure Free(Struct: in out Image_Format_List_Create_Info_C);

    function To_C(Struct: in Attachment_Description_2)
        return Attachment_Description_2_C;
    procedure Free(Struct: in out Attachment_Description_2_C);

    function To_C(Struct: in Attachment_Reference_2)
        return Attachment_Reference_2_C;
    procedure Free(Struct: in out Attachment_Reference_2_C);

    function To_C(Struct: in Subpass_Description_2)
        return Subpass_Description_2_C;
    procedure Free(Struct: in out Subpass_Description_2_C);

    function To_C(Struct: in Subpass_Dependency_2)
        return Subpass_Dependency_2_C;
    procedure Free(Struct: in out Subpass_Dependency_2_C);

    function To_C(Struct: in Render_Pass_Create_Info_2)
        return Render_Pass_Create_Info_2_C;
    procedure Free(Struct: in out Render_Pass_Create_Info_2_C);

    function To_C(Struct: in Subpass_Begin_Info) return Subpass_Begin_Info_C;
    procedure Free(Struct: in out Subpass_Begin_Info_C);

    function To_C(Struct: in Subpass_End_Info) return Subpass_End_Info_C;
    procedure Free(Struct: in out Subpass_End_Info_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_8Bit_Storage_Features;
                     C_Struct: in Physical_Device_8Bit_Storage_Features_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Driver_Properties;
                     C_Struct: in Physical_Device_Driver_Properties_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Atomic_Int64_Features;
         C_Struct: in Physical_Device_Shader_Atomic_Int64_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Float16_Int8_Features;
         C_Struct: in Physical_Device_Shader_Float16_Int8_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Float_Controls_Properties;
         C_Struct: in Physical_Device_Float_Controls_Properties_C);

    function To_C(Struct: in Descriptor_Set_Layout_Binding_Flags_Create_Info)
        return Descriptor_Set_Layout_Binding_Flags_Create_Info_C;
    procedure Free
        (Struct: in out Descriptor_Set_Layout_Binding_Flags_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Descriptor_Indexing_Features;
         C_Struct: in Physical_Device_Descriptor_Indexing_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Descriptor_Indexing_Properties;
         C_Struct: in Physical_Device_Descriptor_Indexing_Properties_C);

    function To_C
        (Struct: in Descriptor_Set_Variable_Descriptor_Count_Allocate_Info)
        return Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C;
    procedure Free
        (Struct:
            in out Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Descriptor_Set_Variable_Descriptor_Count_Layout_Support;
         C_Struct:
            in Descriptor_Set_Variable_Descriptor_Count_Layout_Support_C);

    function To_C(Struct: in Subpass_Description_Depth_Stencil_Resolve)
        return Subpass_Description_Depth_Stencil_Resolve_C;
    procedure Free(Struct: in out Subpass_Description_Depth_Stencil_Resolve_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Depth_Stencil_Resolve_Properties;
         C_Struct: in Physical_Device_Depth_Stencil_Resolve_Properties_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Scalar_Block_Layout_Features;
         C_Struct: in Physical_Device_Scalar_Block_Layout_Features_C);

    function To_C(Struct: in Image_Stencil_Usage_Create_Info)
        return Image_Stencil_Usage_Create_Info_C;
    procedure Free(Struct: in out Image_Stencil_Usage_Create_Info_C);

    function To_C(Struct: in Sampler_Reduction_Mode_Create_Info)
        return Sampler_Reduction_Mode_Create_Info_C;
    procedure Free(Struct: in out Sampler_Reduction_Mode_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Sampler_Filter_Minmax_Properties;
         C_Struct: in Physical_Device_Sampler_Filter_Minmax_Properties_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vulkan_Memory_Model_Features;
         C_Struct: in Physical_Device_Vulkan_Memory_Model_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Imageless_Framebuffer_Features;
         C_Struct: in Physical_Device_Imageless_Framebuffer_Features_C);

    function To_C(Struct: in Framebuffer_Attachment_Image_Info)
        return Framebuffer_Attachment_Image_Info_C;
    procedure Free(Struct: in out Framebuffer_Attachment_Image_Info_C);

    function To_C(Struct: in Framebuffer_Attachments_Create_Info)
        return Framebuffer_Attachments_Create_Info_C;
    procedure Free(Struct: in out Framebuffer_Attachments_Create_Info_C);

    function To_C(Struct: in Render_Pass_Attachment_Begin_Info)
        return Render_Pass_Attachment_Begin_Info_C;
    procedure Free(Struct: in out Render_Pass_Attachment_Begin_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Uniform_Buffer_Standard_Layout_Features;
         C_Struct:
            in Physical_Device_Uniform_Buffer_Standard_Layout_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Shader_Subgroup_Extended_Types_Features;
         C_Struct:
            in Physical_Device_Shader_Subgroup_Extended_Types_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Separate_Depth_Stencil_Layouts_Features;
         C_Struct:
            in Physical_Device_Separate_Depth_Stencil_Layouts_Features_C);

    procedure To_Ada(Ada_Struct: in out Attachment_Reference_Stencil_Layout;
                     C_Struct: in Attachment_Reference_Stencil_Layout_C);

    procedure To_Ada(Ada_Struct: in out Attachment_Description_Stencil_Layout;
                     C_Struct: in Attachment_Description_Stencil_Layout_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Host_Query_Reset_Features;
         C_Struct: in Physical_Device_Host_Query_Reset_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Timeline_Semaphore_Features;
         C_Struct: in Physical_Device_Timeline_Semaphore_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Timeline_Semaphore_Properties;
         C_Struct: in Physical_Device_Timeline_Semaphore_Properties_C);

    function To_C(Struct: in Semaphore_Type_Create_Info)
        return Semaphore_Type_Create_Info_C;
    procedure Free(Struct: in out Semaphore_Type_Create_Info_C);

    function To_C(Struct: in Timeline_Semaphore_Submit_Info)
        return Timeline_Semaphore_Submit_Info_C;
    procedure Free(Struct: in out Timeline_Semaphore_Submit_Info_C);

    function To_C(Struct: in Semaphore_Wait_Info) return Semaphore_Wait_Info_C;
    procedure Free(Struct: in out Semaphore_Wait_Info_C);

    function To_C(Struct: in Semaphore_Signal_Info)
        return Semaphore_Signal_Info_C;
    procedure Free(Struct: in out Semaphore_Signal_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Buffer_Device_Address_Features;
         C_Struct: in Physical_Device_Buffer_Device_Address_Features_C);

    function To_C(Struct: in Buffer_Device_Address_Info)
        return Buffer_Device_Address_Info_C;
    procedure Free(Struct: in out Buffer_Device_Address_Info_C);

    function To_C(Struct: in Buffer_Opaque_Capture_Address_Create_Info)
        return Buffer_Opaque_Capture_Address_Create_Info_C;
    procedure Free(Struct: in out Buffer_Opaque_Capture_Address_Create_Info_C);

    function To_C(Struct: in Memory_Opaque_Capture_Address_Allocate_Info)
        return Memory_Opaque_Capture_Address_Allocate_Info_C;
    procedure Free
        (Struct: in out Memory_Opaque_Capture_Address_Allocate_Info_C);

    function To_C(Struct: in Device_Memory_Opaque_Capture_Address_Info)
        return Device_Memory_Opaque_Capture_Address_Info_C;
    procedure Free(Struct: in out Device_Memory_Opaque_Capture_Address_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_V1_2;


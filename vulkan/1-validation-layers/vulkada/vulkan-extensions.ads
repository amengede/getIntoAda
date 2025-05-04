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

-- Constant extension names

package Vulkan.Extensions is
    Surface: constant String := "VK_KHR_surface";
    Swapchain: constant String := "VK_KHR_swapchain";
    Display: constant String := "VK_KHR_display";
    Display_Swapchain: constant String := "VK_KHR_display_swapchain";
    Sampler_Mirror_Clamp_To_Edge: constant String :=
        "VK_KHR_sampler_mirror_clamp_to_edge";
    Video_Queue: constant String := "VK_KHR_video_queue";
    Video_Decode_Queue: constant String := "VK_KHR_video_decode_queue";
    Video_Codec_H264_Decode: constant String :=
        "VK_STD_vulkan_video_codec_h264_decode";
    Video_Decode_H264: constant String := "VK_KHR_video_decode_h264";
    Dynamic_Rendering: constant String := "VK_KHR_dynamic_rendering";
    Multiview: constant String := "VK_KHR_multview";
    Get_Physical_Device_Properties_2: constant String :=
        "VK_KHR_get_physical_device_properties2";
    Device_Group: constant String := "VK_KHR_device_group";
    Shader_Draw_Parameters: constant String := "VK_KHR_shader_draw_parameters";
    Maintenance_1: constant String := "VK_KHR_maintenance1";
    Device_Group_Creation: constant String := "VK_KHR_device_group_creation";
    External_Memory_Capabilities: constant String :=
        "VK_KHR_external_memory_capabilities";
    External_Memory: constant String := "VK_KHR_external_memory";
    External_Memory_FD: constant String := "VK_KHR_external_memory_fd";
    External_Semaphore_Capabilities: constant String :=
        "VK_KHR_external_semaphore_capabilities";
    External_Semaphore: constant String := "VK_KHR_external_semaphore";
    External_Semaphore_FD: constant String := "VK_KHR_external_semaphore_fd";
    Push_Descriptor: constant String := "VK_KHR_push_descriptor";
    Shader_Float16_Int8: constant String := "VK_KHR_shader_float16_int8";
    Storage_16Bit: constant String := "VK_KHR_16bit_storage";
    Incremental_Present: constant String := "VK_KHR_incremental_present";
    Descriptor_Update_Template: constant String :=
        "VK_KHR_descriptor_update_template";
    Imageless_Framebuffer: constant String := "VK_KHR_imageless_framebuffer";
    Create_Render_Pass_2: constant String := "VK_KHR_create_renderpass2";
    Shared_Presentable_Image: constant String :=
        "VK_KHR_shared_presentable_image";
    External_Fence_Capabilities: constant String :=
        "VK_KHR_external_fence_capabilities";
    External_Fence: constant String := "VK_KHR_external_fence";
    External_Fence_FD: constant String := "VK_KHR_external_fence_fd";
    Performance_Query: constant String := "VK_KHR_performance_query";
    Maintenance_2: constant String := "VK_KHR_maintenance2";
    Get_Surface_Capabilities_2: constant String :=
        "VK_KHR_get_surface_capabilities2";
    Variable_Pointers: constant String := "VK_KHR_variable_pointers";
    Get_Display_Properties_2: constant String :=
        "VK_KHR_get_display_properties2";
    Dedicated_Allocation: constant String := "VK_KHR_dedicated_allocation";
    Storage_Buffer_Storage_Class: constant String :=
        "VK_KHR_storage_buffer_storage_class";
    Relaxed_Block_Layout: constant String := "VK_KHR_relaxed_block_layout";
    Get_Memory_Requirements_2: constant String :=
        "VK_KHR_get_memory_requirements_2";
    Image_Format_List: constant String := "VK_KHR_image_format_list";
    Sampler_YCbCr_Conversion: constant String :=
        "VK_KHR_sampler_ycbcr_conversion";
    Bind_Memory_2: constant String := "VK_KHR_bind_memory2";
    Maintenance_3: constant String := "VK_KHR_maintenance3";
    Draw_Indirect_Count: constant String := "VK_KHR_draw_indirect_count";
    Shader_Subgroup_Extended_Types: constant String :=
        "VK_KHR_shader_subgroup_extended_types";
    Storage_8Bit: constant String := "VK_KHR_8bit_storage";
    Shader_Atomic_Int64: constant String := "VK_KHR_shader_atomic_int64";
    Shader_Clock: constant String := "VK_KHR_shader_clock";
    Video_Codec_H265_Decode: constant String :=
        "VK_STD_vulkan_video_codec_h265_decode";
    Video_Decode_H265: constant String := "VK_KHR_video_decode_h265";
    Global_Priority: constant String := "VK_KHR_global_priority";
    Driver_Properties: constant String := "VK_KHR_driver_properties";
    Shader_Float_Controls: constant String := "VK_KHR_shader_float_controls";
    Depth_Stencil_Resolve: constant String := "VK_KHR_depth_stencil_resolve";
    Swapchain_Mutable_Format: constant String :=
        "VK_KHR_swapchain_mutable_format";
    Timeline_Semaphore: constant String := "VK_KHR_timeline_semaphore";
    Memory_Model: constant String := "VK_KHR_vulkan_memory_model";
    Shader_Terminate_Invocation: constant String :=
        "VK_KHR_shader_terminate_invocation";
    Fragment_Shading_Rate: constant String := "VK_KHR_fragment_shading_rate";
    SPIRV_1_4: constant String := "VK_KHR_spirv_1_4";
    Surface_Protected_Capabilities: constant String :=
        "VK_KHR_surface_protected_capabilities";
    Separate_Depth_Stencil_Layouts: constant String :=
        "VK_KHR_separate_depth_stencil_layouts";
    Present_Wait: constant String := "VK_KHR_present_wait";
    Uniform_Buffer_Standard_Layout: constant String :=
        "VK_KHR_uniform_buffer_standard_layout";
    Buffer_Device_Address: constant String := "VK_KHR_buffer_device_address";
    Deferred_Host_Operations: constant String :=
        "VK_KHR_deferred_host_operations";
    Pipeline_Executable_Properties: constant String :=
        "VK_KHR_pipeline_executable_properties";
    Map_Memory_2: constant String := "VK_KHR_map_memory2";
    Shader_Integer_Dot_Product: constant String :=
        "VK_KHR_shader_integer_dot_product";
    Pipeline_Library: constant String := "VK_KHR_pipeline_library";
    Shader_Non_Semantic_Info: constant String :=
        "VK_KHR_shader_non_semantic_info";
    Present_ID: constant String := "VK_KHR_present_id";
    Synchronization_2: constant String := "VK_KHR_synchronization2";
    Fragment_Shader_Barycentric: constant String :=
        "VK_KHR_fragment_shader_barycentric";
    Shader_Subgroup_Uniform_Control_Flow: constant String :=
        "VK_KHR_shader_subgroup_uniform_control_flow";
    Zero_Initialize_Workgroup_Memory: constant String :=
        "VK_KHR_zero_initialize_workgroup_memory";
    Workgroup_Memory_Explicit_Layout: constant String :=
        "VK_KHR_workgroup_memory_explicit_layout";
    Copy_Commands_2: constant String := "VK_KHR_copy_commands2";
    Format_Feature_Flags_2: constant String := "VK_KHR_format_feature_flags2";
    Ray_Tracing_Maintenance_1: constant String :=
        "VK_KHR_ray_tracing_maintenance1";
    Portability_Enumeration: constant String :=
        "VK_KHR_portability_enumeration";
    Maintenance_4: constant String := "VK_KHR_maintenance4";
    Maintenance_5: constant String := "VK_KHR_maintenance5";
    Ray_Tracing_Position_Fetch: constant String :=
        "VK_KHR_ray_tracing_position_fetch";
    Cooperative_Matrix: constant String := "VK_KHR_cooperative_matrix";
    Video_Maintenance_1: constant String := "VK_KHR_video_maintenance1";
    Vertex_Attribute_Divisor: constant String :=
        "VK_KHR_vertex_attribute_divisor";
    Calibrated_Timestamps: constant String := "VK_KHR_calibrated_timestamps";
    Maintenance_6: constant String := "VK_KHR_maintenance6";
    Debug_Report: constant String := "VK_EXT_debug_report";
    GLSL_Shader: constant String := "VK_NV_glsl_shader";
    Depth_Range_Unrestricted: constant String :=
        "VK_EXT_depth_range_unrestricted";
    Filter_Cubic: constant String := "VK_IMG_filter_cubic";
    Rasterization_Order: constant String := "VK_AMD_rasterization_order";
    Shader_Trinary_Minmax: constant String := "VK_AMD_shader_trinary_minmax";
    Shader_Explicit_Vertex_Parameter: constant String :=
        "VK_AMD_shader_explicit_vertex_parameter";
    Debug_Marker: constant String := "VK_EXT_debug_marker";
    GCN_Shader: constant String := "VK_AMD_gcn_shader";
    NV_Dedicated_Allocation: constant String := "VK_NV_dedicated_allocation";
    Transform_Feedback: constant String := "VK_EXT_transform_feedback";
    Binary_Import: constant String := "VK_NVX_binary_import";
    Image_View_Handle: constant String := "VK_NVX_image_view_handle";
    AMD_Draw_Indirect_Count: constant String := "VK_AMD_draw_indirect_count";
    Negative_Viewport_Height: constant String :=
        "VK_AMD_negative_viewport_height";
    GPU_Shader_Half_Float: constant String := "VK_AMD_gpu_shader_half_float";
    Shader_Ballot: constant String := "VK_AMD_shader_ballot";
    Texture_Gather_Bias_LOD: constant String :=
        "VK_AMD_texture_gather_bias_lod";
    Shader_Info: constant String := "VK_AMD_shader_info";
    Shader_Image_Load_Store_LOD: constant String :=
        "VK_AMD_shader_image_load_store_lod";
    Corner_Sampled_Image: constant String := "VK_NV_corner_sampled_image";
    Format_PVRTC: constant String := "VK_IMG_format_pvrtc";
    NV_External_Memory_Capabilities: constant String :=
        "VK_NV_external_memory_capabilities";
    NV_External_Memory: constant String := "VK_NV_external_memory";
    Validation_Flags: constant String := "VK_EXT_validation_flags";
    Shader_Subgroup_Ballot: constant String := "VK_EXT_shader_subgroup_ballot";
    Shader_Subgroup_Vote: constant String := "VK_EXT_shader_subgroup_vote";
    Texture_Compression_ASTC_HDR: constant String :=
        "VK_EXT_texture_compression_astc_hdr";
    ASTC_Decode_Mode: constant String := "VK_EXT_astc_decode_mode";
    Pipeline_Robustness: constant String := "VK_EXT_pipeline_robustness";
    Conditional_Rendering: constant String := "VK_EXT_conditional_rendering";
    Debug_Utils: constant String := "VK_EXT_debug_utils";
    -- Platform specific extensions.
    Xlib_Surface: constant String := "VK_KHR_xlib_surface";
    Xcb_Surface: constant String := "VK_KHR_xcb_surface";
    Wayland_Surface: constant String := "VK_KHR_wayland_surface";
    Win32_Surface: constant String := "VK_KHR_win32_surface";
    MacOS_Surface: constant String := "VK_MVK_macos_surface";
    Metal_Surface: constant String := "VK_EXT_metal_surface";
end Vulkan.Extensions;


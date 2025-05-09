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

-- Constant extension names

package Vulkan.Extensions is
    Surface: constant String := "VK_KHR_surface";
    Swapchain: constant String := "VK_KHR_swapchain";
    Display: constant String := "VK_KHR_display";
    Display_Swapchain: constant String := "VK_KHR_display_swapchain";
    Video_Queue: constant String := "VK_KHR_video_queue";
    Video_Codec_H264_Encode: constant String :=
        "VK_STD_vulkan_video_codec_h264_encode";
    Video_Codec_H265_Encode: constant String :=
        "VK_STD_vulkan_video_codec_h265_encode";
    Video_Encode_H264: constant String := "VK_KHR_video_encode_h264";
    Video_Encode_H265: constant String := "VK_KHR_video_encode_h265";
    Video_Decode_Queue: constant String := "VK_KHR_video_decode_queue";
    Video_Codec_H264_Decode: constant String :=
        "VK_STD_vulkan_video_codec_h264_decode";
    Video_Decode_H264: constant String := "VK_KHR_video_decode_h264";
    External_Memory_FD: constant String := "VK_KHR_external_memory_fd";
    External_Semaphore_FD: constant String := "VK_KHR_external_semaphore_fd";
    Incremental_Present: constant String := "VK_KHR_incremental_present";
    Shared_Presentable_Image: constant String :=
        "VK_KHR_shared_presentable_image";
    External_Fence_FD: constant String := "VK_KHR_external_fence_fd";
    Performance_Query: constant String := "VK_KHR_performance_query";
    Get_Surface_Capabilities_2: constant String :=
        "VK_KHR_get_surface_capabilities2";
    Get_Display_Properties_2: constant String :=
        "VK_KHR_get_display_properties2";
    Shader_Clock: constant String := "VK_KHR_shader_clock";
    Video_Codec_H265_Decode: constant String :=
        "VK_STD_vulkan_video_codec_h265_decode";
    Video_Decode_H265: constant String := "VK_KHR_video_decode_h265";
    Swapchain_Mutable_Format: constant String :=
        "VK_KHR_swapchain_mutable_format";
    Fragment_Shading_Rate: constant String := "VK_KHR_fragment_shading_rate";
    Shader_Quad_Control: constant String := "VK_KHR_shader_quad_control";
    Surface_Protected_Capabilities: constant String :=
        "VK_KHR_surface_protected_capabilities";
    Present_Wait: constant String := "VK_KHR_present_wait";
    Deferred_Host_Operations: constant String :=
        "VK_KHR_deferred_host_operations";
    Pipeline_Executable_Properties: constant String :=
        "VK_KHR_pipeline_executable_properties";
    Pipeline_Library: constant String := "VK_KHR_pipeline_library";
    Present_ID: constant String := "VK_KHR_present_id";
    Video_Encode_Queue: constant String := "VK_KHR_video_encode_queue";
    Fragment_Shader_Barycentric: constant String :=
        "VK_KHR_fragment_shader_barycentric";
    Shader_Subgroup_Uniform_Control_Flow: constant String :=
        "VK_KHR_shader_subgroup_uniform_control_flow";
    Workgroup_Memory_Explicit_Layout: constant String :=
        "VK_KHR_workgroup_memory_explicit_layout";
    Ray_Tracing_Maintenance_1: constant String :=
        "VK_KHR_ray_tracing_maintenance1";
    Portability_Enumeration: constant String :=
        "VK_KHR_portability_enumeration";
    Shader_Maximal_Reconvergence: constant String :=
        "VK_KHR_shader_maximal_reconvergence";
    Ray_Tracing_Position_Fetch: constant String :=
        "VK_KHR_ray_tracing_position_fetch";
    Cooperative_Matrix: constant String := "VK_KHR_cooperative_matrix";
    Codec_AV1_Decode: constant String := "VK_STD_vulkan_video_codec_av1_decode";
    Video_Decode_AV1: constant String := "VK_KHR_video_decode_av1";
    Video_Maintenance_1: constant String := "VK_KHR_video_maintenance1";
    Calibrated_Timestamps: constant String := "VK_KHR_calibrated_timestamps";
    Maintenance_6: constant String := "VK_KHR_maintenance6";
    Depth_Range_Unrestricted: constant String :=
        "VK_EXT_depth_range_unrestricted";
    Filter_Cubic: constant String := "VK_IMG_filter_cubic";
    Rasterization_Order: constant String := "VK_AMD_rasterization_order";
    Shader_Trinary_Minmax: constant String := "VK_AMD_shader_trinary_minmax";
    Shader_Explicit_Vertex_Parameter: constant String :=
        "VK_AMD_shader_explicit_vertex_parameter";
    GCN_Shader: constant String := "VK_AMD_gcn_shader";
    Transform_Feedback: constant String := "VK_EXT_transform_feedback";
    Binary_Import: constant String := "VK_NVX_binary_import";
    Image_View_Handle: constant String := "VK_NVX_image_view_handle";
    Shader_Ballot: constant String := "VK_AMD_shader_ballot";
    Texture_Gather_Bias_LOD: constant String :=
        "VK_AMD_texture_gather_bias_lod";
    Shader_Info: constant String := "VK_AMD_shader_info";
    Shader_Image_Load_Store_LOD: constant String :=
        "VK_AMD_shader_image_load_store_lod";
    Corner_Sampled_Image: constant String := "VK_NV_corner_sampled_image";
    ASTC_Decode_Mode: constant String := "VK_EXT_astc_decode_mode";
    Conditional_Rendering: constant String := "VK_EXT_conditional_rendering";
    Clip_Space_W_Scaling: constant String := "VK_NV_clip_space_w_scaling";
    Direct_Mode_Display: constant String := "VK_EXT_direct_mode_display";
    Display_Surface_Counter: constant String :=
        "VK_EXT_display_surface_counter";
    Display_Control: constant String := "VK_EXT_display_control";
    Display_Timing: constant String := "VK_GOOGLE_display_timing";
    Sample_Mask_Override_Coverage: constant String :=
        "VK_NV_sample_mask_override_coverage";
    Geometry_Shader_Passthrough: constant String :=
        "VK_NV_geometry_shader_passthrough";
    Viewporit_Array_2: constant String := "VK_NV_viewport_array2";
    Multiview_Per_View_Attributes: constant String :=
        "VK_NVX_multiview_per_view_attributes";
    Viewport_Swizzle: constant String := "VK_NV_viewport_swizzle";
    Discard_Rectangles: constant String := "VK_EXT_discard_rectangles";
    Conservative_Rasterization: constant String :=
        "VK_EXT_conservative_rasterization";
    Depth_Clip_Enable: constant String := "VK_EXT_depth_clip_enable";
    Swapchain_Colorspace: constant String := "VK_EXT_swapchain_colorspace";
    HDR_Metadata: constant String := "VK_EXT_hdr_metadata";
    Relaxed_Line_Rasterization: constant String :=
        "VK_IMG_relaxed_line_rasterization";
    External_Memory_DMA_Buf: constant String :=
        "VK_EXT_external_memory_dma_buf";
    Queue_Family_Foreign: constant String := "VK_EXT_queue_family_foreign";
    Debug_Utils: constant String := "VK_EXT_debug_utils";
    Mixed_Attachment_Samples: constant String :=
        "VK_AMD_mixed_attachment_samples";
    Shader_Fragment_Mask: constant String := "VK_AMD_shader_fragment_mask";
    Shader_Stencil_Export: constant String := "VK_EXT_shader_stencil_export";
    -- Platform specific extensions.
    Xlib_Surface: constant String := "VK_KHR_xlib_surface";
    Xcb_Surface: constant String := "VK_KHR_xcb_surface";
    Wayland_Surface: constant String := "VK_KHR_wayland_surface";
    Win32_Surface: constant String := "VK_KHR_win32_surface";
    Metal_Surface: constant String := "VK_EXT_metal_surface";
end Vulkan.Extensions;


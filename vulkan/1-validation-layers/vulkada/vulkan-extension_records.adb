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

-- Package for working with extension record lists

with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.C_V1_1;
with Vulkan.C_V1_2;
with Vulkan.C_V1_3;
with Vulkan.Displays_C;
with Vulkan.Display_Swapchains_C;
with Vulkan.Video_Queues_C;
with Vulkan.Video_Decode_Queues_C;
with Vulkan.Video_Decode_H264_C;
with Vulkan.Dynamic_Rendering_C;
with Vulkan.External_Memory_FDs_C;
with Vulkan.External_Semaphore_FDs_C;
with Vulkan.Push_Descriptors_C;
with Vulkan.Incremental_Presents_C;
with Vulkan.Shared_Presentable_Images_C;
with Vulkan.External_Fence_FDs_C;
with Vulkan.Performance_Queries_C;
with Vulkan.Get_Surface_Capabilities_2_C;
with Vulkan.Get_Display_Properties_2_C;
with Vulkan.Shader_Clocks_C;
with Vulkan.Video_Decode_H265_C;
with Vulkan.Global_Priorities_C;
with Vulkan.Fragment_Shading_Rates_C;
with Vulkan.Surface_Protected_Capabilities_C;
with Vulkan.Present_Waits_C;
with Vulkan.Pipeline_Executable_Properties_C;
with Vulkan.Map_Memory_2_C;
with Vulkan.Pipeline_Libraries_C;
with Vulkan.Present_IDs_C;
with Vulkan.Synchronization_2_C;
with Vulkan.Fragment_Shader_Barycentrics_C;
with Vulkan.Shader_Subgroup_Uniform_Control_Flows_C;
with Vulkan.Workgroup_Memory_Explicit_Layouts_C;
with Vulkan.Ray_Tracing_Maintenance_1_C;
with Vulkan.Maintenance_5_C;
with Vulkan.Ray_Tracing_Position_Fetches_C;
with Vulkan.Cooperative_Matrices_C;
with Vulkan.Video_Maintenance_1_C;
with Vulkan.Vertex_Attribute_Divisors_C;
with Vulkan.Calibrated_Timestamps_C;
with Vulkan.Maintenance_6_C;
with Vulkan.Debug_Reports_C;
with Vulkan.Rasterization_Orders_C;
with Vulkan.Debug_Markers_C;
with Vulkan.Dedicated_Allocations_C;
with Vulkan.Transform_Feedbacks_C;
with Vulkan.Binary_Imports_C;
with Vulkan.Image_View_Handles_C;
with Vulkan.Texture_Gather_Bias_LODs_C;
with Vulkan.Corner_Sampled_Images_C;
with Vulkan.External_Memories_C;
with Vulkan.Validation_Flags_C;
with Vulkan.ASTC_Decode_Modes_C;
with Vulkan.Pipeline_Robustness_C;
with Vulkan.Conditional_Rendering_C;
with Vulkan.Debug_Utils_C;
with Vulkan.Xlib_Surfaces_C;
with Vulkan.Xcb_Surfaces_C;
with Vulkan.Wayland_Surfaces_C;
with Vulkan.Win32_Surfaces_C;
with Vulkan.MacOS_Surfaces_C;
with Vulkan.Metal_Surfaces_C;

package body Vulkan.Extension_Records is
    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Next.Record_Type is
            when Application_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Application_Info,
                         C.Application_Info_C,
                         C.Application_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Instance_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Instance_Create_Info,
                         C.Instance_Create_Info_C,
                         C.Instance_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Queue_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Queue_Create_Info,
                         C.Device_Queue_Create_Info_C,
                         C.Device_Queue_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Create_Info,
                         C.Device_Create_Info_C,
                         C.Device_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Submit_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Submit_Info,
                         C.Submit_Info_C,
                         C.Submit_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Allocate_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_Allocate_Info,
                         C.Memory_Allocate_Info_C,
                         C.Memory_Allocate_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Mapped_Memory_Range_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Mapped_Memory_Range,
                         C.Mapped_Memory_Range_C,
                         C.Mapped_Memory_Range_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Sparse_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Sparse_Info,
                         C.Bind_Sparse_Info_C,
                         C.Bind_Sparse_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Fence_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Fence_Create_Info,
                         C.Fence_Create_Info_C,
                         C.Fence_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Semaphore_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Semaphore_Create_Info,
                         C.Semaphore_Create_Info_C,
                         C.Semaphore_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Event_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Event_Create_Info,
                         C.Event_Create_Info_C,
                         C.Event_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Query_Pool_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Query_Pool_Create_Info,
                         C.Query_Pool_Create_Info_C,
                         C.Query_Pool_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Create_Info,
                         C.Buffer_Create_Info_C,
                         C.Buffer_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_View_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_View_Create_Info,
                         C.Buffer_View_Create_Info_C,
                         C.Buffer_View_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Create_Info,
                         C.Image_Create_Info_C,
                         C.Image_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Image_View_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_View_Create_Info,
                         C.Image_View_Create_Info_C,
                         C.Image_View_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Shader_Module_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Shader_Module_Create_Info,
                         C.Shader_Module_Create_Info_C,
                         C.Shader_Module_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Cache_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Cache_Create_Info,
                         C.Pipeline_Cache_Create_Info_C,
                         C.Pipeline_Cache_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Shader_Stage_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Shader_Stage_Create_Info,
                         C.Pipeline_Shader_Stage_Create_Info_C,
                         C.Pipeline_Shader_Stage_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Vertex_Input_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Vertex_Input_State_Create_Info,
                         C.Pipeline_Vertex_Input_State_Create_Info_C,
                         C.Pipeline_Vertex_Input_State_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Input_Assembly_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Input_Assembly_State_Create_Info,
                         C.Pipeline_Input_Assembly_State_Create_Info_C,
                         C.Pipeline_Input_Assembly_State_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Tessellation_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Tessellation_State_Create_Info,
                         C.Pipeline_Tessellation_State_Create_Info_C,
                         C.Pipeline_Tessellation_State_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Viewport_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Viewport_State_Create_Info,
                         C.Pipeline_Viewport_State_Create_Info_C,
                         C.Pipeline_Viewport_State_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Rasterization_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Rasterization_State_Create_Info,
                         C.Pipeline_Rasterization_State_Create_Info_C,
                         C.Pipeline_Rasterization_State_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Multisample_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Multisample_State_Create_Info,
                         C.Pipeline_Multisample_State_Create_Info_C,
                         C.Pipeline_Multisample_State_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Depth_Stencil_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Depth_Stencil_State_Create_Info,
                         C.Pipeline_Depth_Stencil_State_Create_Info_C,
                         C.Pipeline_Depth_Stencil_State_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Color_Blend_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Color_Blend_State_Create_Info,
                         C.Pipeline_Color_Blend_State_Create_Info_C,
                         C.Pipeline_Color_Blend_State_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Dynamic_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Dynamic_State_Create_Info,
                         C.Pipeline_Dynamic_State_Create_Info_C,
                         C.Pipeline_Dynamic_State_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Graphics_Pipeline_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Graphics_Pipeline_Create_Info,
                         C.Graphics_Pipeline_Create_Info_C,
                         C.Graphics_Pipeline_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Compute_Pipeline_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Compute_Pipeline_Create_Info,
                         C.Compute_Pipeline_Create_Info_C,
                         C.Compute_Pipeline_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Layout_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Layout_Create_Info,
                         C.Pipeline_Layout_Create_Info_C,
                         C.Pipeline_Layout_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Sampler_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Sampler_Create_Info,
                         C.Sampler_Create_Info_C,
                         C.Sampler_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Descriptor_Set_Layout_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Descriptor_Set_Layout_Create_Info,
                         C.Descriptor_Set_Layout_Create_Info_C,
                         C.Descriptor_Set_Layout_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Descriptor_Pool_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Descriptor_Pool_Create_Info,
                         C.Descriptor_Pool_Create_Info_C,
                         C.Descriptor_Pool_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Descriptor_Set_Allocate_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Descriptor_Set_Allocate_Info,
                         C.Descriptor_Set_Allocate_Info_C,
                         C.Descriptor_Set_Allocate_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Write_Descriptor_Set_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Write_Descriptor_Set,
                         C.Write_Descriptor_Set_C,
                         C.Write_Descriptor_Set_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Copy_Descriptor_Set_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Copy_Descriptor_Set,
                         C.Copy_Descriptor_Set_C,
                         C.Copy_Descriptor_Set_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Framebuffer_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Framebuffer_Create_Info,
                         C.Framebuffer_Create_Info_C,
                         C.Framebuffer_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Render_Pass_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Render_Pass_Create_Info,
                         C.Render_Pass_Create_Info_C,
                         C.Render_Pass_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Command_Pool_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Command_Pool_Create_Info,
                         C.Command_Pool_Create_Info_C,
                         C.Command_Pool_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Command_Buffer_Allocate_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Command_Buffer_Allocate_Info,
                         C.Command_Buffer_Allocate_Info_C,
                         C.Command_Buffer_Allocate_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Command_Buffer_Inheritance_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Command_Buffer_Inheritance_Info,
                         C.Command_Buffer_Inheritance_Info_C,
                         C.Command_Buffer_Inheritance_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Command_Buffer_Begin_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Command_Buffer_Begin_Info,
                         C.Command_Buffer_Begin_Info_C,
                         C.Command_Buffer_Begin_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Barrier_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_Barrier,
                         C.Memory_Barrier_C,
                         C.Memory_Barrier_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Memory_Barrier_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Memory_Barrier,
                         C.Buffer_Memory_Barrier_C,
                         C.Buffer_Memory_Barrier_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Memory_Barrier_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Memory_Barrier,
                         C.Image_Memory_Barrier_C,
                         C.Image_Memory_Barrier_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Render_Pass_Begin_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Render_Pass_Begin_Info,
                         C.Render_Pass_Begin_Info_C,
                         C.Render_Pass_Begin_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Swapchain_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Swapchain_Create_Info,
                         C.Swapchain_Create_Info_C,
                         C.Swapchain_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Present_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Present_Info,
                         C.Present_Info_C,
                         C.Present_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Swapchain_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Swapchain_Create_Info,
                         C.Image_Swapchain_Create_Info_C,
                         C.Image_Swapchain_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Image_Memory_Swapchain_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Image_Memory_Swapchain_Info,
                         C.Bind_Image_Memory_Swapchain_Info_C,
                         C.Bind_Image_Memory_Swapchain_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Acquire_Next_Image_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Acquire_Next_Image_Info,
                         C.Acquire_Next_Image_Info_C,
                         C.Acquire_Next_Image_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Group_Present_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Group_Present_Info,
                         C.Device_Group_Present_Info_C,
                         C.Device_Group_Present_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Group_Swapchain_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Group_Swapchain_Create_Info,
                         C.Device_Group_Swapchain_Create_Info_C,
                         C.Device_Group_Swapchain_Create_Info_C_Access,
                         C.To_C);
                begin
                    return Make_Struct(Next);
                end;
            when C_V1_1.In_Structure =>
                return C_V1_1.To_C(Next);
            when C_V1_2.In_Structure =>
                return C_V1_2.To_C(Next);
            when C_V1_3.In_Structure =>
                return C_V1_3.To_C(Next);
            when Displays_C.Structure =>
                return Displays_C.To_C(Next);
            when Display_Swapchains_C.Structure =>
                return Display_Swapchains_C.To_C(Next);
            when Video_Queues_C.In_Structure =>
                return Video_Queues_C.To_C(Next);
            when Video_Decode_Queues_C.In_Structure =>
                return Video_Decode_Queues_C.To_C(Next);
            when Video_Decode_H264_C.In_Structure =>
                return Video_Decode_H264_C.To_C(Next);
            when Dynamic_Rendering_C.Structure =>
                return Dynamic_Rendering_C.To_C(Next);
            when External_Memory_FDs_C.In_Structure =>
                return External_Memory_FDs_C.To_C(Next);
            when External_Semaphore_FDs_C.Structure =>
                return External_Semaphore_FDs_C.To_C(Next);
            when Incremental_Presents_C.Structure =>
                return Incremental_Presents_C.To_C(Next);
            when External_Fence_FDs_C.Structure =>
                return External_Fence_FDs_C.To_C(Next);
            when Performance_Queries_C.In_Structure =>
                return Performance_Queries_C.To_C(Next);
            when Get_Surface_Capabilities_2_C.In_Structure =>
                return Get_Surface_Capabilities_2_C.To_C(Next);
            when Get_Display_Properties_2_C.In_Structure =>
                return Get_Display_Properties_2_C.To_C(Next);
            when Video_Decode_H265_C.In_Structure =>
                return Video_Decode_H265_C.To_C(Next);
            when Global_Priorities_C.In_Structure =>
                return Global_Priorities_C.To_C(Next);
            when Fragment_Shading_Rates_C.In_Structure =>
                return Fragment_Shading_Rates_C.To_C(Next);
            when Surface_Protected_Capabilities_C.Structure =>
                return Surface_Protected_Capabilities_C.To_C(Next);
            when Pipeline_Executable_Properties_C.In_Structure =>
                return Pipeline_Executable_Properties_C.To_C(Next);
            when Map_Memory_2_C.Structure =>
                return Map_Memory_2_C.To_C(Next);
            when Pipeline_Libraries_C.Structure =>
                return Pipeline_Libraries_C.To_C(Next);
            when Present_IDs_C.In_Structure =>
                return Present_IDs_C.To_C(Next);
            when Maintenance_5_C.In_Structure =>
                return Maintenance_5_C.To_C(Next);
            when Video_Maintenance_1_C.In_Structure =>
                return Video_Maintenance_1_C.To_C(Next);
            when Vertex_Attribute_Divisors_C.In_Structure =>
                return Vertex_Attribute_Divisors_C.To_C(Next);
            when Calibrated_Timestamps_C.Structure =>
                return Calibrated_Timestamps_C.To_C(Next);
            when Maintenance_6_C.In_Structure =>
                return Maintenance_6_C.To_C(Next);
            when Debug_Reports_C.Structure =>
                return Debug_Reports_C.To_C(Next);
            when Rasterization_Orders_C.Structure =>
                return Rasterization_Orders_C.To_C(Next);
            when Debug_Markers_C.Structure =>
                return Debug_Markers_C.To_C(Next);
            when Dedicated_Allocations_C.Structure =>
                return Dedicated_Allocations_C.To_C(Next);
            when Transform_Feedbacks_C.In_Structure =>
                return Transform_Feedbacks_C.To_C(Next);
            when Binary_Imports_C.Structure =>
                return Binary_Imports_C.To_C(Next);
            when Image_View_Handles_C.In_Structure =>
                return Image_View_Handles_C.To_C(Next);
            when External_Memories_C.Structure =>
                return External_Memories_C.To_C(Next);
            when Validation_Flags_C.Structure =>
                return Validation_Flags_C.To_C(Next);
            when ASTC_Decode_Modes_C.In_Structure =>
                return ASTC_Decode_Modes_C.To_C(Next);
            when Pipeline_Robustness_C.In_Structure =>
                return Pipeline_Robustness_C.To_C(Next);
            when Conditional_Rendering_C.In_Structure =>
                return Conditional_Rendering_C.To_C(Next);
            when Debug_Utils_C.Structure =>
                return Debug_Utils_C.To_C(Next);
            when Xlib_Surfaces_C.Structure =>
                return Xlib_Surfaces_C.To_C(Next);
            when Xcb_Surfaces_C.Structure =>
                return Xcb_Surfaces_C.To_C(Next);
            when Wayland_Surfaces_C.Structure =>
                return Wayland_Surfaces_C.To_C(Next);
            when Win32_Surfaces_C.Structure =>
                return Win32_Surfaces_C.To_C(Next);
            when MacOS_Surfaces_C.Structure =>
                return MacOS_Surfaces_C.To_C(Next);
            when Metal_Surfaces_C.Structure =>
                return Metal_Surfaces_C.To_C(Next);
        end case;
    end To_C;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Next.Record_Type is
            when Device_Group_Present_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Device_Group_Present_Capabilities,
                         C.Device_Group_Present_Capabilities_C,
                         C.Device_Group_Present_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when C_V1_1.Out_Structure =>
                return C_V1_1.To_C(Next);
            when C_V1_2.Out_Structure =>
                return C_V1_2.To_C(Next);
            when C_V1_3.Out_Structure =>
                return C_V1_3.To_C(Next);
            when Video_Queues_C.Out_Structure =>
                return Video_Queues_C.To_C(Next);
            when Video_Decode_Queues_C.Out_Structure =>
                return Video_Decode_Queues_C.To_C(Next);
            when Video_Decode_H264_C.Out_Structure =>
                return Video_Decode_H264_C.To_C(Next);
            when External_Memory_FDs_C.Out_Structure =>
                return External_Memory_FDs_C.To_C(Next);
            when Push_Descriptors_C.Structure =>
                return Push_Descriptors_C.To_C(Next);
            when Shared_Presentable_Images_C.Structure =>
                return Shared_Presentable_Images_C.To_C(Next);
            when Performance_Queries_C.Out_Structure =>
                return Performance_Queries_C.To_C(Next);
            when Get_Surface_Capabilities_2_C.Out_Structure =>
                return Get_Surface_Capabilities_2_C.To_C(Next);
            when Get_Display_Properties_2_C.Out_Structure =>
                return Get_Display_Properties_2_C.To_C(Next);
            when Shader_Clocks_C.Structure =>
                return Shader_Clocks_C.To_C(Next);
            when Video_Decode_H265_C.Out_Structure =>
                return Video_Decode_H265_C.To_C(Next);
            when Global_Priorities_C.Out_Structure =>
                return Global_Priorities_C.To_C(Next);
            when Fragment_Shading_Rates_C.Out_Structure =>
                return Fragment_Shading_Rates_C.To_C(Next);
            when Present_Waits_C.Structure =>
                return Present_Waits_C.To_C(Next);
            when Pipeline_Executable_Properties_C.Out_Structure =>
                return Pipeline_Executable_Properties_C.To_C(Next);
            when Present_IDs_C.Out_Structure =>
                return Present_IDs_C.To_C(Next);
            when Synchronization_2_C.Structure =>
                return Synchronization_2_C.To_C(Next);
            when Fragment_Shader_Barycentrics_C.Structure =>
                return Fragment_Shader_Barycentrics_C.To_C(Next);
            when Shader_Subgroup_Uniform_Control_Flows_C.Structure =>
                return Shader_Subgroup_Uniform_Control_Flows_C.To_C(Next);
            when Workgroup_Memory_Explicit_Layouts_C.Structure =>
                return Workgroup_Memory_Explicit_Layouts_C.To_C(Next);
            when Ray_Tracing_Maintenance_1_C.Structure =>
                return Ray_Tracing_Maintenance_1_C.To_C(Next);
            when Maintenance_5_C.Out_Structure =>
                return Maintenance_5_C.To_C(Next);
            when Ray_Tracing_Position_Fetches_C.Structure =>
                return Ray_Tracing_Position_Fetches_C.To_C(Next);
            when Cooperative_Matrices_C.Structure =>
                return Cooperative_Matrices_C.To_C(Next);
            when Video_Maintenance_1_C.Out_Structure =>
                return Video_Maintenance_1_C.To_C(Next);
            when Vertex_Attribute_Divisors_C.Out_Structure =>
                return Vertex_Attribute_Divisors_C.To_C(Next);
            when Maintenance_6_C.Out_Structure =>
                return Maintenance_6_C.To_C(Next);
            when Transform_Feedbacks_C.Out_Structure =>
                return Transform_Feedbacks_C.To_C(Next);
            when Image_View_Handles_C.Out_Structure =>
                return Image_View_Handles_C.To_C(Next);
            when Texture_Gather_Bias_LODs_C.Structure =>
                return Texture_Gather_Bias_LODs_C.To_C(Next);
            when Corner_Sampled_Images_C.Structure =>
                return Corner_Sampled_Images_C.To_C(Next);
            when ASTC_Decode_Modes_C.Out_Structure =>
                return ASTC_Decode_Modes_C.To_C(Next);
            when Pipeline_Robustness_C.Out_Structure =>
                return Pipeline_Robustness_C.To_C(Next);
            when Conditional_Rendering_C.Out_Structure =>
                return Conditional_Rendering_C.To_C(Next);
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Out_Structure_Access;
                     Next: in C.Out_Structure_C_Access) is
    begin
        if Ada_Struct = null then
            return;
        end if;

        case Ada_Struct.Record_Type is
            when Device_Group_Present_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         C.Device_Group_Present_Capabilities_C_Access);
                begin
                    C.To_Ada(Device_Group_Present_Capabilities(Ada_Struct.all),
                             To_Access(Next).all);
                end;
            when C_V1_1.Out_Structure =>
                C_V1_1.To_Ada(Ada_Struct.all, Next);
            when C_V1_2.Out_Structure =>
                C_V1_2.To_Ada(Ada_Struct.all, Next);
            when C_V1_3.Out_Structure =>
                C_V1_3.To_Ada(Ada_Struct.all, Next);
            when Video_Queues_C.Out_Structure =>
                Video_Queues_C.To_Ada(Ada_Struct.all, Next);
            when Video_Decode_Queues_C.Out_Structure =>
                Video_Decode_Queues_C.To_Ada(Ada_Struct.all, Next);
            when Video_Decode_H264_C.Out_Structure =>
                Video_Decode_H264_C.To_Ada(Ada_Struct.all, Next);
            when External_Memory_FDs_C.Out_Structure =>
                External_Memory_FDs_C.To_Ada(Ada_Struct.all, Next);
            when Push_Descriptors_C.Structure =>
                Push_Descriptors_C.To_Ada(Ada_Struct.all, Next);
            when Shared_Presentable_Images_C.Structure =>
                Shared_Presentable_Images_C.To_Ada(Ada_Struct.all, Next);
            when Performance_Queries_C.Out_Structure =>
                Performance_Queries_C.To_Ada(Ada_Struct.all, Next);
            when Get_Surface_Capabilities_2_C.Out_Structure =>
                Get_Surface_Capabilities_2_C.To_Ada(Ada_Struct.all, Next);
            when Get_Display_Properties_2_C.Out_Structure =>
                Get_Display_Properties_2_C.To_Ada(Ada_Struct.all, Next);
            when Shader_Clocks_C.Structure =>
                Shader_Clocks_C.To_Ada(Ada_Struct.all, Next);
            when Video_Decode_H265_C.Out_Structure =>
                Video_Decode_H265_C.To_Ada(Ada_Struct.all, Next);
            when Global_Priorities_C.Out_Structure =>
                Global_Priorities_C.To_Ada(Ada_Struct.all, Next);
            when Fragment_Shading_Rates_C.Out_Structure =>
                Fragment_Shading_Rates_C.To_Ada(Ada_Struct.all, Next);
            when Present_Waits_C.Structure =>
                Present_Waits_C.To_Ada(Ada_Struct.all, Next);
            when Pipeline_Executable_Properties_C.Out_Structure =>
                Pipeline_Executable_Properties_C.To_Ada(Ada_Struct.all, Next);
            when Present_IDs_C.Out_Structure =>
                Present_IDs_C.To_Ada(Ada_Struct.all, Next);
            when Synchronization_2_C.Structure =>
                Synchronization_2_C.To_Ada(Ada_Struct.all, Next);
            when Fragment_Shader_Barycentrics_C.Structure =>
                Fragment_Shader_Barycentrics_C.To_Ada(Ada_Struct.all, Next);
            when Shader_Subgroup_Uniform_Control_Flows_C.Structure =>
                Shader_Subgroup_Uniform_Control_Flows_C.To_Ada(Ada_Struct.all,
                                                               Next);
            when Workgroup_Memory_Explicit_Layouts_C.Structure =>
                Workgroup_Memory_Explicit_Layouts_C.To_Ada(Ada_Struct.all,
                                                           Next);
            when Ray_Tracing_Maintenance_1_C.Structure =>
                Ray_Tracing_Maintenance_1_C.To_Ada(Ada_Struct.all, Next);
            when Maintenance_5_C.Out_Structure =>
                Maintenance_5_C.To_Ada(Ada_Struct.all, Next);
            when Ray_Tracing_Position_Fetches_C.Structure =>
                Ray_Tracing_Position_Fetches_C.To_Ada(Ada_Struct.all, Next);
            when Cooperative_Matrices_C.Structure =>
                Cooperative_Matrices_C.To_Ada(Ada_Struct.all, Next);
            when Video_Maintenance_1_C.Out_Structure =>
                Video_Maintenance_1_C.To_Ada(Ada_Struct.all, Next);
            when Vertex_Attribute_Divisors_C.Out_Structure =>
                Vertex_Attribute_Divisors_C.To_Ada(Ada_Struct.all, Next);
            when Maintenance_6_C.Out_Structure =>
                Maintenance_6_C.To_Ada(Ada_Struct.all, Next);
            when Transform_Feedbacks_C.Out_Structure =>
                Transform_Feedbacks_C.To_Ada(Ada_Struct.all, Next);
            when Image_View_Handles_C.Out_Structure =>
                Image_View_Handles_C.To_Ada(Ada_Struct.all, Next);
            when Texture_Gather_Bias_LODs_C.Structure =>
                Texture_Gather_Bias_LODs_C.To_Ada(Ada_Struct.all, Next);
            when Corner_Sampled_Images_C.Structure =>
                Corner_Sampled_Images_C.To_Ada(Ada_Struct.all, Next);
            when ASTC_Decode_Modes_C.Out_Structure =>
                ASTC_Decode_Modes_C.To_Ada(Ada_Struct.all, Next);
            when Pipeline_Robustness_C.Out_Structure =>
                Pipeline_Robustness_C.To_Ada(Ada_Struct.all, Next);
            when Conditional_Rendering_C.Out_Structure =>
                Conditional_Rendering_C.To_Ada(Ada_Struct.all, Next);
        end case;
    end To_Ada;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Next.Record_Type is
            when Application_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Application_Info_C,
                         C.Application_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Instance_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Instance_Create_Info_C,
                         C.Instance_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Device_Queue_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Device_Queue_Create_Info_C,
                         C.Device_Queue_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Device_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Device_Create_Info_C,
                         C.Device_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Submit_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Submit_Info_C,
                         C.Submit_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Allocate_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Memory_Allocate_Info_C,
                         C.Memory_Allocate_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Mapped_Memory_Range_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Mapped_Memory_Range_C,
                         C.Mapped_Memory_Range_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Sparse_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Bind_Sparse_Info_C,
                         C.Bind_Sparse_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Fence_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Fence_Create_Info_C,
                         C.Fence_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Semaphore_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Semaphore_Create_Info_C,
                         C.Semaphore_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Event_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Event_Create_Info_C,
                         C.Event_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Query_Pool_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Query_Pool_Create_Info_C,
                         C.Query_Pool_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Buffer_Create_Info_C,
                         C.Buffer_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_View_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Buffer_View_Create_Info_C,
                         C.Buffer_View_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Image_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Image_Create_Info_C,
                         C.Image_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Image_View_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Image_View_Create_Info_C,
                         C.Image_View_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Shader_Module_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Shader_Module_Create_Info_C,
                         C.Shader_Module_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Cache_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Cache_Create_Info_C,
                         C.Pipeline_Cache_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Shader_Stage_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Shader_Stage_Create_Info_C,
                         C.Pipeline_Shader_Stage_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Vertex_Input_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Vertex_Input_State_Create_Info_C,
                         C.Pipeline_Vertex_Input_State_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Input_Assembly_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Input_Assembly_State_Create_Info_C,
                         C.Pipeline_Input_Assembly_State_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Tessellation_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Tessellation_State_Create_Info_C,
                         C.Pipeline_Tessellation_State_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Viewport_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Viewport_State_Create_Info_C,
                         C.Pipeline_Viewport_State_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Rasterization_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Rasterization_State_Create_Info_C,
                         C.Pipeline_Rasterization_State_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Multisample_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Multisample_State_Create_Info_C,
                         C.Pipeline_Multisample_State_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Depth_Stencil_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Depth_Stencil_State_Create_Info_C,
                         C.Pipeline_Depth_Stencil_State_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Color_Blend_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Color_Blend_State_Create_Info_C,
                         C.Pipeline_Color_Blend_State_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Dynamic_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Dynamic_State_Create_Info_C,
                         C.Pipeline_Dynamic_State_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Graphics_Pipeline_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Graphics_Pipeline_Create_Info_C,
                         C.Graphics_Pipeline_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Compute_Pipeline_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Compute_Pipeline_Create_Info_C,
                         C.Compute_Pipeline_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Layout_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Pipeline_Layout_Create_Info_C,
                         C.Pipeline_Layout_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Sampler_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Sampler_Create_Info_C,
                         C.Sampler_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Descriptor_Set_Layout_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Descriptor_Set_Layout_Create_Info_C,
                         C.Descriptor_Set_Layout_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Descriptor_Pool_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Descriptor_Pool_Create_Info_C,
                         C.Descriptor_Pool_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Descriptor_Set_Allocate_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Descriptor_Set_Allocate_Info_C,
                         C.Descriptor_Set_Allocate_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Write_Descriptor_Set_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Write_Descriptor_Set_C,
                         C.Write_Descriptor_Set_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Copy_Descriptor_Set_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Copy_Descriptor_Set_C,
                         C.Copy_Descriptor_Set_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Framebuffer_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Framebuffer_Create_Info_C,
                         C.Framebuffer_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Render_Pass_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Render_Pass_Create_Info_C,
                         C.Render_Pass_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Command_Pool_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Command_Pool_Create_Info_C,
                         C.Command_Pool_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Command_Buffer_Allocate_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Command_Buffer_Allocate_Info_C,
                         C.Command_Buffer_Allocate_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Command_Buffer_Inheritance_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Command_Buffer_Inheritance_Info_C,
                         C.Command_Buffer_Inheritance_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Command_Buffer_Begin_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Command_Buffer_Begin_Info_C,
                         C.Command_Buffer_Begin_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Barrier_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Memory_Barrier_C,
                         C.Memory_Barrier_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Memory_Barrier_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Buffer_Memory_Barrier_C,
                         C.Buffer_Memory_Barrier_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Image_Memory_Barrier_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Image_Memory_Barrier_C,
                         C.Image_Memory_Barrier_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Render_Pass_Begin_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Render_Pass_Begin_Info_C,
                         C.Render_Pass_Begin_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Swapchain_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Swapchain_Create_Info_C,
                         C.Swapchain_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Present_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Present_Info_C,
                         C.Present_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Image_Swapchain_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Image_Swapchain_Create_Info_C,
                         C.Image_Swapchain_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Image_Memory_Swapchain_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Bind_Image_Memory_Swapchain_Info_C,
                         C.Bind_Image_Memory_Swapchain_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Acquire_Next_Image_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Acquire_Next_Image_Info_C,
                         C.Acquire_Next_Image_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Device_Group_Present_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Device_Group_Present_Info_C,
                         C.Device_Group_Present_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when Device_Group_Swapchain_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (C.Device_Group_Swapchain_Create_Info_C,
                         C.Device_Group_Swapchain_Create_Info_C_Access,
                         C.Free);
                begin
                    Free_Struct(Next);
                end;
            when C_V1_1.In_Structure =>
                C_V1_1.Free(Next);
            when C_V1_2.In_Structure =>
                C_V1_2.Free(Next);
            when C_V1_3.In_Structure =>
                C_V1_3.Free(Next);
            when Displays_C.Structure =>
                Displays_C.Free(Next);
            when Display_Swapchains_C.Structure =>
                Display_Swapchains_C.Free(Next);
            when Video_Queues_C.In_Structure =>
                Video_Queues_C.Free(Next);
            when Video_Decode_Queues_C.In_Structure =>
                Video_Decode_Queues_C.Free(Next);
            when Video_Decode_H264_C.In_Structure =>
                Video_Decode_H264_C.Free(Next);
            when Dynamic_Rendering_C.Structure =>
                Dynamic_Rendering_C.Free(Next);
            when External_Memory_FDs_C.In_Structure =>
                External_Memory_FDs_C.Free(Next);
            when External_Semaphore_FDs_C.Structure =>
                External_Semaphore_FDs_C.Free(Next);
            when Incremental_Presents_C.Structure =>
                Incremental_Presents_C.Free(Next);
            when External_Fence_FDs_C.Structure =>
                External_Fence_FDs_C.Free(Next);
            when Performance_Queries_C.In_Structure =>
                Performance_Queries_C.Free(Next);
            when Get_Surface_Capabilities_2_C.In_Structure =>
                Get_Surface_Capabilities_2_C.Free(Next);
            when Get_Display_Properties_2_C.In_Structure =>
                Get_Display_Properties_2_C.Free(Next);
            when Video_Decode_H265_C.In_Structure =>
                Video_Decode_H265_C.Free(Next);
            when Global_Priorities_C.In_Structure =>
                Global_Priorities_C.Free(Next);
            when Fragment_Shading_Rates_C.In_Structure =>
                Fragment_Shading_Rates_C.Free(Next);
            when Surface_Protected_Capabilities_C.Structure =>
                Surface_Protected_Capabilities_C.Free(Next);
            when Pipeline_Executable_Properties_C.In_Structure =>
                Pipeline_Executable_Properties_C.Free(Next);
            when Map_Memory_2_C.Structure =>
                Map_Memory_2_C.Free(Next);
            when Pipeline_Libraries_C.Structure =>
                Pipeline_Libraries_C.Free(Next);
            when Present_IDs_C.In_Structure =>
                Present_IDs_C.Free(Next);
            when Maintenance_5_C.In_Structure =>
                Maintenance_5_C.Free(Next);
            when Video_Maintenance_1_C.In_Structure =>
                Video_Maintenance_1_C.Free(Next);
            when Vertex_Attribute_Divisors_C.In_Structure =>
                Vertex_Attribute_Divisors_C.Free(Next);
            when Calibrated_Timestamps_C.Structure =>
                Calibrated_Timestamps_C.Free(Next);
            when Maintenance_6_C.In_Structure =>
                Maintenance_6_C.Free(Next);
            when Debug_Reports_C.Structure =>
                Debug_Reports_C.Free(Next);
            when Rasterization_Orders_C.Structure =>
                Rasterization_Orders_C.Free(Next);
            when Debug_Markers_C.Structure =>
                Debug_Markers_C.Free(Next);
            when Dedicated_Allocations_C.Structure =>
                Dedicated_Allocations_C.Free(Next);
            when Transform_Feedbacks_C.In_Structure =>
                Transform_Feedbacks_C.Free(Next);
            when Binary_Imports_C.Structure =>
                Binary_Imports_C.Free(Next);
            when Image_View_Handles_C.In_Structure =>
                Image_View_Handles_C.Free(Next);
            when External_Memories_C.Structure =>
                External_Memories_C.Free(Next);
            when Validation_Flags_C.Structure =>
                Validation_Flags_C.Free(Next);
            when ASTC_Decode_Modes_C.In_Structure =>
                ASTC_Decode_Modes_C.Free(Next);
            when Pipeline_Robustness_C.In_Structure =>
                Pipeline_Robustness_C.Free(Next);
            when Conditional_Rendering_C.In_Structure =>
                Conditional_Rendering_C.Free(Next);
            when Debug_Utils_C.Structure =>
                Debug_Utils_C.Free(Next);
            when Xlib_Surfaces_C.Structure =>
                Xlib_Surfaces_C.Free(Next);
            when Xcb_Surfaces_C.Structure =>
                Xcb_Surfaces_C.Free(Next);
            when Wayland_Surfaces_C.Structure =>
                Wayland_Surfaces_C.Free(Next);
            when Win32_Surfaces_C.Structure =>
                Win32_Surfaces_C.Free(Next);
            when MacOS_Surfaces_C.Structure =>
                MacOS_Surfaces_C.Free(Next);
            when Metal_Surfaces_C.Structure =>
                Metal_Surfaces_C.Free(Next);
        end case;
    end Free;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        Free(Next.Next);

        case Next.Record_Type is
            when Device_Group_Present_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (C.Device_Group_Present_Capabilities_C,
                         C.Device_Group_Present_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when C_V1_1.Out_Structure =>
                C_V1_1.Free(Next);
            when C_V1_2.Out_Structure =>
                C_V1_2.Free(Next);
            when C_V1_3.Out_Structure =>
                C_V1_3.Free(Next);
            when Video_Queues_C.Out_Structure =>
                Video_Queues_C.Free(Next);
            when Video_Decode_Queues_C.Out_Structure =>
                Video_Decode_Queues_C.Free(Next);
            when Video_Decode_H264_C.Out_Structure =>
                Video_Decode_H264_C.Free(Next);
            when External_Memory_FDs_C.Out_Structure =>
                External_Memory_FDs_C.Free(Next);
            when Push_Descriptors_C.Structure =>
                Push_Descriptors_C.Free(Next);
            when Shared_Presentable_Images_C.Structure =>
                Shared_Presentable_Images_C.Free(Next);
            when Performance_Queries_C.Out_Structure =>
                Performance_Queries_C.Free(Next);
            when Get_Surface_Capabilities_2_C.Out_Structure =>
                Get_Surface_Capabilities_2_C.Free(Next);
            when Get_Display_Properties_2_C.Out_Structure =>
                Get_Display_Properties_2_C.Free(Next);
            when Shader_Clocks_C.Structure =>
                Shader_Clocks_C.Free(Next);
            when Video_Decode_H265_C.Out_Structure =>
                Video_Decode_H265_C.Free(Next);
            when Global_Priorities_C.Out_Structure =>
                Global_Priorities_C.Free(Next);
            when Fragment_Shading_Rates_C.Out_Structure =>
                Fragment_Shading_Rates_C.Free(Next);
            when Present_Waits_C.Structure =>
                Present_Waits_C.Free(Next);
            when Pipeline_Executable_Properties_C.Out_Structure =>
                Pipeline_Executable_Properties_C.Free(Next);
            when Present_IDs_C.Out_Structure =>
                Present_IDs_C.Free(Next);
            when Synchronization_2_C.Structure =>
                Synchronization_2_C.Free(Next);
            when Fragment_Shader_Barycentrics_C.Structure =>
                Fragment_Shader_Barycentrics_C.Free(Next);
            when Shader_Subgroup_Uniform_Control_Flows_C.Structure =>
                Shader_Subgroup_Uniform_Control_Flows_C.Free(Next);
            when Workgroup_Memory_Explicit_Layouts_C.Structure =>
                Workgroup_Memory_Explicit_Layouts_C.Free(Next);
            when Ray_Tracing_Maintenance_1_C.Structure =>
                Ray_Tracing_Maintenance_1_C.Free(Next);
            when Maintenance_5_C.Out_Structure =>
                Maintenance_5_C.Free(Next);
            when Ray_Tracing_Position_Fetches_C.Structure =>
                Ray_Tracing_Position_Fetches_C.Free(Next);
            when Cooperative_Matrices_C.Structure =>
                Cooperative_Matrices_C.Free(Next);
            when Video_Maintenance_1_C.Out_Structure =>
                Video_Maintenance_1_C.Free(Next);
            when Vertex_Attribute_Divisors_C.Out_Structure =>
                Vertex_Attribute_Divisors_C.Free(Next);
            when Maintenance_6_C.Out_Structure =>
                Maintenance_6_C.Free(Next);
            when Transform_Feedbacks_C.Out_Structure =>
                Transform_Feedbacks_C.Free(Next);
            when Image_View_Handles_C.Out_Structure =>
                Image_View_Handles_C.Free(Next);
            when Texture_Gather_Bias_LODs_C.Structure =>
                Texture_Gather_Bias_LODs_C.Free(Next);
            when Corner_Sampled_Images_C.Structure =>
                Corner_Sampled_Images_C.Free(Next);
            when ASTC_Decode_Modes_C.Out_Structure =>
                ASTC_Decode_Modes_C.Free(Next);
            when Pipeline_Robustness_C.Out_Structure =>
                Pipeline_Robustness_C.Free(Next);
            when Conditional_Rendering_C.Out_Structure =>
                Conditional_Rendering_C.Free(Next);
        end case;
    end Free;
end Vulkan.Extension_Records;


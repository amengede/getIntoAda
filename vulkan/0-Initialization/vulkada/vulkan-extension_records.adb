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

with Vulkan.Utilities;
with Vulkan.C_V1_1;
with Vulkan.C_V1_2;
with Vulkan.C_V1_3;
with Vulkan.C_KHR;
with Vulkan.C_EXT;
with Vulkan.C_AMD;
with Vulkan.C_NV;
with Vulkan.C_NVX;
with Vulkan.C_GOOGLE;
with Vulkan.C_IMG;

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
            when C_V1_1.In_Structure =>
                return C_V1_1.To_C(Next);
            when C_V1_2.In_Structure =>
                return C_V1_2.To_C(Next);
            when C_V1_3.In_Structure =>
                return C_V1_3.To_C(Next);
            when C_KHR.In_Structure =>
                return C_KHR.To_C(Next);
            when C_EXT.In_Structure =>
                return C_EXT.To_C(Next);
            when C_AMD.In_Structure =>
                return C_AMD.To_C(Next);
            when C_NV.In_Structure =>
                return C_NV.To_C(Next);
            when C_NVX.In_Structure =>
                return C_NVX.To_C(Next);
            when C_GOOGLE.In_Structure =>
                return C_GOOGLE.To_C(Next);
            when C_IMG.In_Structure =>
                return C_IMG.To_C(Next);
        end case;
    end To_C;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Next.Record_Type is
            when C_V1_1.Out_Structure =>
                return C_V1_1.To_C(Next);
            when C_V1_2.Out_Structure =>
                return C_V1_2.To_C(Next);
            when C_V1_3.Out_Structure =>
                return C_V1_3.To_C(Next);
            when C_KHR.Out_Structure =>
                return C_KHR.To_C(Next);
            when C_EXT.Out_Structure =>
                return C_EXT.To_C(Next);
            when C_AMD.Out_Structure =>
                return C_AMD.To_C(Next);
            when C_NV.Out_Structure =>
                return C_NV.To_C(Next);
            when C_NVX.Out_Structure =>
                return C_NVX.To_C(Next);
            when C_GOOGLE.Out_Structure =>
                return C_GOOGLE.To_C(Next);
            when C_IMG.Out_Structure =>
                return C_IMG.To_C(Next);
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Out_Structure_Access;
                     Next: in C.Out_Structure_C_Access) is
    begin
        if Ada_Struct = null then
            return;
        end if;

        case Ada_Struct.Record_Type is
            when C_V1_1.Out_Structure =>
                C_V1_1.To_Ada(Ada_Struct.all, Next);
            when C_V1_2.Out_Structure =>
                C_V1_2.To_Ada(Ada_Struct.all, Next);
            when C_V1_3.Out_Structure =>
                C_V1_3.To_Ada(Ada_Struct.all, Next);
            when C_KHR.Out_Structure =>
                C_KHR.To_Ada(Ada_Struct.all, Next);
            when C_EXT.Out_Structure =>
                C_EXT.To_Ada(Ada_Struct.all, Next);
            when C_AMD.Out_Structure =>
                C_AMD.To_Ada(Ada_Struct.all, Next);
            when C_NV.Out_Structure =>
                C_NV.To_Ada(Ada_Struct.all, Next);
            when C_NVX.Out_Structure =>
                C_NVX.To_Ada(Ada_Struct.all, Next);
            when C_GOOGLE.Out_Structure =>
                C_GOOGLE.To_Ada(Ada_Struct.all, Next);
            when C_IMG.Out_Structure =>
                C_IMG.To_Ada(Ada_Struct.all, Next);
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
            when C_V1_1.In_Structure =>
                C_V1_1.Free(Next);
            when C_V1_2.In_Structure =>
                C_V1_2.Free(Next);
            when C_V1_3.In_Structure =>
                C_V1_3.Free(Next);
            when C_KHR.In_Structure =>
                C_KHR.Free(Next);
            when C_EXT.In_Structure =>
                C_EXT.Free(Next);
            when C_AMD.In_Structure =>
                C_AMD.Free(Next);
            when C_NV.In_Structure =>
                C_NV.Free(Next);
            when C_NVX.In_Structure =>
                C_NVX.Free(Next);
            when C_GOOGLE.In_Structure =>
                C_GOOGLE.Free(Next);
            when C_IMG.In_Structure =>
                C_IMG.Free(Next);
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
            when C_V1_1.Out_Structure =>
                C_V1_1.Free(Next);
            when C_V1_2.Out_Structure =>
                C_V1_2.Free(Next);
            when C_V1_3.Out_Structure =>
                C_V1_3.Free(Next);
            when C_KHR.Out_Structure =>
                C_KHR.Free(Next);
            when C_EXT.Out_Structure =>
                C_EXT.Free(Next);
            when C_AMD.Out_Structure =>
                C_AMD.Free(Next);
            when C_NV.Out_Structure =>
                C_NV.Free(Next);
            when C_NVX.Out_Structure =>
                C_NVX.Free(Next);
            when C_GOOGLE.Out_Structure =>
                C_GOOGLE.Free(Next);
            when C_IMG.Out_Structure =>
                C_IMG.Free(Next);
        end case;
    end Free;
end Vulkan.Extension_Records;


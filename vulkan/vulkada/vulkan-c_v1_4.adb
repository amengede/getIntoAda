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

-- Subprogram access for Vulkan 1.4

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;
with Vulkan.Core;

package body Vulkan.C_V1_4 is
    procedure Load(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdSetLineStipple_Access);
        procedure Load is new Load_Pointer(vkMapMemory2_Access);
        procedure Load is new Load_Pointer(vkUnmapMemory2_Access);
        procedure Load is new Load_Pointer(vkCmdBindIndexBuffer2_Access);
        procedure Load is
            new Load_Pointer(vkGetRenderingAreaGranularity_Access);
        procedure Load is
            new Load_Pointer(vkGetDeviceImageSubresourceLayout_Access);
        procedure Load is new Load_Pointer(vkCmdPushDescriptorSet_Access);
        procedure Load is
            new Load_Pointer(vkCmdPushDescriptorSetWithTemplate_Access);
        procedure Load is new Load_Pointer
            (vkCmdSetRenderingAttachmentLocations_Access);
        procedure Load is new Load_Pointer
            (vkCmdSetRenderingInputAttachmentIndices_Access);
        procedure Load is new Load_Pointer(vkCmdBindDescriptorSets2_Access);
        procedure Load is new Load_Pointer(vkCmdPushConstants2_Access);
        procedure Load is new Load_Pointer(vkCmdPushDescriptorSet2_Access);
        procedure Load is
            new Load_Pointer(vkCmdPushDescriptorSetWithTemplate2_Access);
        procedure Load is new Load_Pointer(vkCopyMemoryToImage_Access);
        procedure Load is new Load_Pointer(vkCopyImageToMemory_Access);
        procedure Load is new Load_Pointer(vkCopyImageToImage_Access);
        procedure Load is new Load_Pointer(vkTransitionImageLayout_Access);
    begin
        Load(vkCmdSetLineStipple, "vkCmdSetLineStipple");
        Load(vkMapMemory2, "vkMapMemory2");
        Load(vkUnmapMemory2, "vkUnmapMemory2");
        Load(vkCmdBindIndexBuffer2, "vkCmdBindIndexBuffer2");
        Load(vkGetRenderingAreaGranularity, "vkGetRenderingAreaGranularity");
        Load(vkGetDeviceImageSubresourceLayout,
             "vkGetDeviceImageSubresourceLayout");
        Load(vkCmdPushDescriptorSet, "vkCmdPushDescriptorSet");
        Load(vkCmdPushDescriptorSetWithTemplate,
             "vkCmdPushDescriptorSetWithTemplate");
        Load(vkCmdSetRenderingAttachmentLocations,
             "vkCmdSetRenderingAttachmentLocations");
        Load(vkCmdSetRenderingInputAttachmentIndices,
             "vkCmdSetRenderingInputAttachmentIndices");
        Load(vkCmdBindDescriptorSets2, "vkCmdBindDescriptorSets2");
        Load(vkCmdPushConstants2, "vkCmdPushConstants2");
        Load(vkCmdPushDescriptorSet2, "vkCmdPushDescriptorSet2");
        Load(vkCmdPushDescriptorSetWithTemplate2,
             "vkCmdPushDescriptorSetWithTemplate2");
        Load(vkCopyMemoryToImage, "vkCopyMemoryToImage");
        Load(vkCopyImageToMemory, "vkCopyImageToMemory");
        Load(vkCopyImageToImage, "vkCopyImageToImage");
        Load(vkTransitionImageLayout, "vkTransitionImageLayout");
    end Load;
    
    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_4_Features;
                     C_Struct: in Physical_Device_Vulkan_1_4_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Global_Priority_Query :=
            Utilities.To_Ada(C_Struct.Global_Priority_Query);
        Ada_Struct.Shader_Subgroup_Rotate :=
            Utilities.To_Ada(C_Struct.Shader_Subgroup_Rotate);
        Ada_Struct.Shader_Subgroup_Rotate_Clustered :=
            Utilities.To_Ada(C_Struct.Shader_Subgroup_Rotate_Clustered);
        Ada_Struct.Shader_Float_Controls_2 :=
            Utilities.To_Ada(C_Struct.Shader_Float_Controls_2);
        Ada_Struct.Shader_Expect_Assume :=
            Utilities.To_Ada(C_Struct.Shader_Expect_Assume);
        Ada_Struct.Rectangular_Lines :=
            Utilities.To_Ada(C_Struct.Rectangular_Lines);
        Ada_Struct.Bresenham_Lines :=
            Utilities.To_Ada(C_Struct.Bresenham_Lines);
        Ada_Struct.Smooth_Lines := Utilities.To_Ada(C_Struct.Smooth_Lines);
        Ada_Struct.Stippled_Rectangular_Lines :=
            Utilities.To_Ada(C_Struct.Stippled_Rectangular_Lines);
        Ada_Struct.Stippled_Bresenham_Lines :=
            Utilities.To_Ada(C_Struct.Stippled_Bresenham_Lines);
        Ada_Struct.Stippled_Smooth_Lines :=
            Utilities.To_Ada(C_Struct.Stippled_Smooth_Lines);
        Ada_Struct.Vertex_Attribute_Instance_Rate_Divisor :=
            Utilities.To_Ada(C_Struct.Vertex_Attribute_Instance_Rate_Divisor);
        Ada_Struct.Vertex_Attribute_Instance_Rate_Zero_Divisor :=
            Utilities.To_Ada
                (C_Struct.Vertex_Attribute_Instance_Rate_Zero_Divisor);
        Ada_Struct.Index_Type_Uint8 :=
            Utilities.To_Ada(C_Struct.Index_Type_Uint8);
        Ada_Struct.Dynamic_Rendering_Local_Read :=
            Utilities.To_Ada(C_Struct.Dynamic_Rendering_Local_Read);
        Ada_Struct.Maintenance_5 := Utilities.To_Ada(C_Struct.Maintenance_5);
        Ada_Struct.Maintenance_6 := Utilities.To_Ada(C_Struct.Maintenance_6);
        Ada_Struct.Pipeline_Protected_Access :=
            Utilities.To_Ada(C_Struct.Pipeline_Protected_Access);
        Ada_Struct.Pipeline_Robustness :=
            Utilities.To_Ada(C_Struct.Pipeline_Robustness);
        Ada_Struct.Host_Image_Copy :=
            Utilities.To_Ada(C_Struct.Host_Image_Copy);
        Ada_Struct.Push_Descriptor :=
            Utilities.To_Ada(C_Struct.Push_Descriptor);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_4_Properties;
                     C_Struct: in Physical_Device_Vulkan_1_4_Properties_C) is
        Layout: Image_Layout_Pointers.Pointer;
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Line_Sub_Pixel_Precision_Bits :=
            C_Struct.Line_Sub_Pixel_Precision_Bits;
        Ada_Struct.Max_Vertex_Attrib_Divisor :=
            C_Struct.Max_Vertex_Attrib_Divisor;
        Ada_Struct.Supports_Non_Zero_First_Instance :=
            Utilities.To_Ada(C_Struct.Supports_Non_Zero_First_Instance);
        Ada_Struct.Max_Push_Descriptors := C_Struct.Max_Push_Descriptors;
        Ada_Struct.Dynamic_Rendering_Local_Read_Depth_Stencil_Attachments :=
            Utilities.To_Ada
                (C_Struct.
                    Dynamic_Rendering_Local_Read_Depth_Stencil_Attachments);
        Ada_Struct.Dynamic_Rendering_Local_Read_Multisampled_Attachments :=
            Utilities.To_Ada
                (C_Struct.
                    Dynamic_Rendering_Local_Read_Multisampled_Attachments);
        Ada_Struct.Early_Fragment_Multisample_Coverage_After_Sample_Counting :=
            Utilities.To_Ada
                (C_Struct.
                    Early_Fragment_Multisample_Coverage_After_Sample_Counting);
        Ada_Struct.Early_Fragment_Sample_Mask_Test_Before_Sample_Counting :=
            Utilities.To_Ada
                (C_Struct.
                    Early_Fragment_Sample_Mask_Test_Before_Sample_Counting);
        Ada_Struct.Depth_Stencil_Swizzle_One_Support :=
            Utilities.To_Ada(C_Struct.Depth_Stencil_Swizzle_One_Support);
        Ada_Struct.Polygon_Mode_Point_Size :=
            Utilities.To_Ada(C_Struct.Polygon_Mode_Point_Size);
        Ada_Struct.Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram :=
            Utilities.To_Ada
                (C_Struct.
                    Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram);
        Ada_Struct.Non_Strict_Wide_Lines_Use_Parallelogram :=
            Utilities.To_Ada(C_Struct.Non_Strict_Wide_Lines_Use_Parallelogram);
        Ada_Struct.Block_Texel_View_Compatible_Multiple_Layers :=
            Utilities.To_Ada
                (C_Struct.Block_Texel_View_Compatible_Multiple_Layers);
        Ada_Struct.Max_Combined_Image_Sampler_Descriptor_Count :=
            C_Struct.Max_Combined_Image_Sampler_Descriptor_Count;
        Ada_Struct.Fragment_Shading_Rate_Clamp_Combiner_Inputs :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_Clamp_Combiner_Inputs);
        Ada_Struct.Default_Robustness_Storage_Buffers :=
            C_Struct.Default_Robustness_Storage_Buffers;
        Ada_Struct.Default_Robustness_Uniform_Buffers :=
            C_Struct.Default_Robustness_Uniform_Buffers;
        Ada_Struct.Default_Robustness_Vertex_Inputs :=
            C_Struct.Default_Robustness_Vertex_Inputs;
        Ada_Struct.Default_Robustness_Images :=
            C_Struct.Default_Robustness_Images;

        Ada_Struct.Copy_Src_Layouts.Clear;
        Ada_Struct.Copy_Src_Layouts.Reserve_Capacity
            (Ada.Containers.Count_Type(C_Struct.Copy_Src_Layout_Count));
        Layout := C_Struct.Copy_Src_Layouts;

        for X in 1 .. C_Struct.Copy_Src_Layout_Count loop
            Ada_Struct.Copy_Src_Layouts.Append(Layout.all);
            Image_Layout_Pointers.Increment(Layout);
        end loop;

        Ada_Struct.Copy_Dst_Layouts.Clear;
        Ada_Struct.Copy_Dst_Layouts.Reserve_Capacity
            (Ada.Containers.Count_Type(C_Struct.Copy_Dst_Layout_Count));
        Layout := C_Struct.Copy_Dst_Layouts;

        for X in 1 .. C_Struct.Copy_Dst_Layout_Count loop
            Ada_Struct.Copy_Dst_Layouts.Append(Layout.all);
            Image_Layout_Pointers.Increment(Layout);
        end loop;

        Ada_Struct.Optimal_Tiling_Layout_UUID :=
            C_Struct.Optimal_Tiling_Layout_UUID;
        Ada_Struct.Identical_Memory_Type_Requirements :=
            Utilities.To_Ada(C_Struct.Identical_Memory_Type_Requirements);
    end To_Ada;

    function To_C(Struct: in Device_Queue_Global_Priority_Create_Info)
        return Device_Queue_Global_Priority_Create_Info_C is
        DQGPCIC: Device_Queue_Global_Priority_Create_Info_C;
    begin
        DQGPCIC.Next := Extension_Records.To_C(Struct.Next);
        DQGPCIC.Global_Priority := Struct.Global_Priority;

        return DQGPCIC;
    end To_C;

    procedure Free(Struct: in out Device_Queue_Global_Priority_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Global_Priority_Query_Features;
         C_Struct: in Physical_Device_Global_Priority_Query_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Global_Priority_Query :=
            Utilities.To_Ada(C_Struct.Global_Priority_Query);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Queue_Family_Global_Priority_Properties;
                     C_Struct: in Queue_Family_Global_Priority_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Priority_Count := C_Struct.Priority_Count;
        Ada_Struct.Priorities := C_Struct.Priorities;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Subgroup_Rotate_Features;
         C_Struct: in Physical_Device_Shader_Subgroup_Rotate_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Subgroup_Rotate :=
            Utilities.To_Ada(C_Struct.Shader_Subgroup_Rotate);
        Ada_Struct.Shader_Subgroup_Rotate_Clustered :=
            Utilities.To_Ada(C_Struct.Shader_Subgroup_Rotate_Clustered);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Float_Controls_2_Features;
         C_Struct: in Physical_Device_Shader_Float_Controls_2_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Float_Controls_2 :=
            Utilities.To_Ada(C_Struct.Shader_Float_Controls_2);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Expect_Assume_Features;
         C_Struct: in Physical_Device_Shader_Expect_Assume_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Expect_Assume :=
            Utilities.To_Ada(C_Struct.Shader_Expect_Assume);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Line_Rasterization_Features;
         C_Struct: in Physical_Device_Line_Rasterization_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Rectangular_Lines :=
            Utilities.To_Ada(C_Struct.Rectangular_Lines);
        Ada_Struct.Bresenham_Lines :=
            Utilities.To_Ada(C_Struct.Bresenham_Lines);
        Ada_Struct.Smooth_Lines :=  Utilities.To_Ada(C_Struct.Smooth_Lines);
        Ada_Struct.Stippled_Rectangular_Lines :=
            Utilities.To_Ada(C_Struct.Stippled_Rectangular_Lines);
        Ada_Struct.Stippled_Bresenham_Lines :=
            Utilities.To_Ada(C_Struct.Stippled_Bresenham_Lines);
        Ada_Struct.Stippled_Smooth_Lines :=
            Utilities.To_Ada(C_Struct.Stippled_Smooth_Lines);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Line_Rasterization_Properties;
         C_Struct: in Physical_Device_Line_Rasterization_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Line_Sub_Pixel_Precision_Bits :=
            C_Struct.Line_Sub_Pixel_Precision_Bits;
    end To_Ada;

    function To_C(Struct: in Pipeline_Rasterization_Line_State_Create_Info)
        return Pipeline_Rasterization_Line_State_Create_Info_C is
        PRLSCIC: Pipeline_Rasterization_Line_State_Create_Info_C;
    begin
        PRLSCIC.Next := Extension_Records.To_C(Struct.Next);
        PRLSCIC.Line_Rasterization_Mode := Struct.Line_Rasterization_Mode;
        PRLSCIC.Stippled_Line_Enable :=
            Utilities.To_C(Struct.Stippled_Line_Enable);
        PRLSCIC.Line_Stipple_Factor := Struct.Line_Stipple_Factor;
        PRLSCIC.Line_Stipple_Pattern := Struct.Line_Stipple_Pattern;

        return PRLSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Rasterization_Line_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vertex_Attribute_Divisor_Properties;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Vertex_Attrib_Divisor :=
            C_Struct.Max_Vertex_Attrib_Divisor;
        Ada_Struct.Supports_Non_Zero_First_Instance :=
            Utilities.To_Ada(C_Struct.Supports_Non_Zero_First_Instance);
    end To_Ada;

    function To_C(Struct: in Pipeline_Vertex_Input_Divisor_State_Create_Info)
        return Pipeline_Vertex_Input_Divisor_State_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Vertex_Input_Binding_Divisor_Description_Arrays,
             Vertex_Input_Binding_Divisor_Description_Vectors);

        PVIDSCIC: Pipeline_Vertex_Input_Divisor_State_Create_Info_C;
    begin
        PVIDSCIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(PVIDSCIC.Vertex_Binding_Divisor_Count,
                   Struct.Vertex_Binding_Divisors,
                   PVIDSCIC.Vertex_Binding_Divisors);

        return PVIDSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Vertex_Input_Divisor_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Vertex_Input_Binding_Divisor_Description_Arrays.Free
            (Struct.Vertex_Binding_Divisors);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vertex_Attribute_Divisor_Features;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Vertex_Attribute_Instance_Rate_Divisor :=
            Utilities.To_Ada(C_Struct.Vertex_Attribute_Instance_Rate_Divisor);
        Ada_Struct.Vertex_Attribute_Instance_Rate_Zero_Divisor :=
            Utilities.To_Ada
                (C_Struct.Vertex_Attribute_Instance_Rate_Zero_Divisor);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Index_Type_Uint8_Features;
         C_Struct: in Physical_Device_Index_Type_Uint8_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Index_Type_Uint8 :=
            Utilities.To_Ada(C_Struct.Index_Type_Uint8);
    end To_Ada;

    function To_C(Struct: in Memory_Map_Info) return Memory_Map_Info_C is
        MMIC: Memory_Map_Info_C;
    begin
        MMIC.Next := Extension_Records.To_C(Struct.Next);
        MMIC.Flags := Struct.Flags;
        MMIC.Memory := Struct.Memory;
        MMIC.Offset := Struct.Offset;
        MMIC.Size := Struct.Size;

        return MMIC;
    end To_C;

    procedure Free(Struct: in out Memory_Map_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Memory_Unmap_Info) return Memory_Unmap_Info_C is
        MUIC: Memory_Unmap_Info_C;
    begin
        MUIC.Next := Extension_Records.To_C(Struct.Next);
        MUIC.Flags := Struct.Flags;
        MUIC.Memory := Struct.Memory;

        return MUIC;
    end To_C;

    procedure Free(Struct: in out Memory_Unmap_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Maintenance_5_Features;
                     C_Struct: in Physical_Device_Maintenance_5_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Maintenance_5 := Utilities.To_Ada(C_Struct.Maintenance_5);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_5_Properties;
         C_Struct: in Physical_Device_Maintenance_5_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Early_Fragment_Multisample_Coverage_After_Sample_Counting :=
            Utilities.To_Ada
                (C_Struct.
                    Early_Fragment_Multisample_Coverage_After_Sample_Counting);
        Ada_Struct.Early_Fragment_Sample_Mask_Test_Before_Sample_Counting :=
            Utilities.To_Ada
                (C_Struct.
                    Early_Fragment_Sample_Mask_Test_Before_Sample_Counting);
        Ada_Struct.Depth_Stencil_Swizzle_One_Support :=
            Utilities.To_Ada(C_Struct.Depth_Stencil_Swizzle_One_Support);
        Ada_Struct.Polygon_Mode_Point_Size :=
            Utilities.To_Ada(C_Struct.Polygon_Mode_Point_Size);
        Ada_Struct.Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram :=
            Utilities.To_Ada
                (C_Struct.Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram);
        Ada_Struct.Non_Strict_Wide_Lines_Use_Parallelogram :=
            Utilities.To_Ada(C_Struct.Non_Strict_Wide_Lines_Use_Parallelogram);
    end To_Ada;

    function To_C(Struct: in Rendering_Area_Info) return Rendering_Area_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C_V1_2.Format_Arrays,
                                                         Format_Vectors);

        RAIC: Rendering_Area_Info_C;
    begin
        RAIC.Next := Extension_Records.To_C(Struct.Next);
        RAIC.View_Mask := Struct.View_Mask;
        To_C_Array(RAIC.Color_Attachment_Count,
                   Struct.Color_Attachment_Formats,
                   RAIC.Color_Attachment_Formats);
        RAIC.Depth_Attachment_Format := Struct.Depth_Attachment_Format;
        RAIC.Stencil_Attachment_Format := Struct.Stencil_Attachment_Format;

        return RAIC;
    end To_C;

    procedure Free(Struct: in out Rendering_Area_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C_V1_2.Format_Arrays.Free(Struct.Color_Attachment_Formats);
    end Free;

    procedure To_Ada(Ada_Struct: in out Image_Subresource_2;
                     C_Struct: in Image_Subresource_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Image_Subresource := C_Struct.Image_Subresource;
    end To_Ada;

    function To_C(Struct: in Device_Image_Subresource_Info)
        return Device_Image_Subresource_Info_C is
        DISIC: Device_Image_Subresource_Info_C;
    begin
        DISIC.Next := Extension_Records.To_C(Struct.Next);
        DISIC.Create_Info :=
            new C.Image_Create_Info_C'(C.To_C(Struct.Create_Info.all));
        DISIC.Subresource := new Image_Subresource_2_C;
        DISIC.Subresource.Image_Subresource :=
            Struct.Subresource.Image_Subresource;

        return DISIC;
    end To_C;

    procedure Free(Struct: in out Device_Image_Subresource_Info_C) is
        use type C.Image_Create_Info_C_Access;
        
        procedure Free is new Ada.Unchecked_Deallocation
            (C.Image_Create_Info_C, C.Image_Create_Info_C_Access);
        procedure Free is new Ada.Unchecked_Deallocation
            (Image_Subresource_2_C, Image_Subresource_2_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Create_Info /= null then
            C.Free(Struct.Create_Info.all);
            Free(Struct.Create_Info);
        end if;

        Free(Struct.Subresource);
    end Free;

    procedure To_Ada(Ada_Struct: in out Subresource_Layout_2;
                     C_Struct: in Subresource_Layout_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Subresource_Layout := C_Struct.Subresource_Layout;
    end To_Ada;

    function To_C(Struct: in Pipeline_Create_Flags_2_Create_Info)
        return Pipeline_Create_Flags_2_Create_Info_C is
        PCF2CIC: Pipeline_Create_Flags_2_Create_Info_C;
    begin
        PCF2CIC.Next := Extension_Records.To_C(Struct.Next);
        PCF2CIC.Flags := Struct.Flags;

        return PCF2CIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Create_Flags_2_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Buffer_Usage_Flags_2_Create_Info)
        return Buffer_Usage_Flags_2_Create_Info_C is
        BUF2CIC: Buffer_Usage_Flags_2_Create_Info_C;
    begin
        BUF2CIC.Next := Extension_Records.To_C(Struct.Next);
        BUF2CIC.Flags := Struct.Flags;

        return BUF2CIC;
    end To_C;

    procedure Free(Struct: in out Buffer_Usage_Flags_2_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Push_Descriptor_Properties;
        C_Struct: in Physical_Device_Push_Descriptor_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Push_Descriptors := C_Struct.Max_Push_Descriptors;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Dynamic_Rendering_Local_Read_Features;
         C_Struct:
            in Physical_Device_Dynamic_Rendering_Local_Read_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Dynamic_Rendering_Local_Read :=
            Utilities.To_Ada(C_Struct.Dynamic_Rendering_Local_Read);
    end To_Ada;

    function To_C(Struct: in Rendering_Attachment_Location_Info)
        return Rendering_Attachment_Location_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        RALIC: Rendering_Attachment_Location_Info_C;
    begin
        RALIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(RALIC.Color_Attachment_Count,
                   Struct.Color_Attachment_Locations,
                   RALIC.Color_Attachment_Locations);

        return RALIC;
    end To_C;

    procedure Free(Struct: in out Rendering_Attachment_Location_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Color_Attachment_Locations);
    end Free;
                  
    function To_C(Struct: in Rendering_Input_Attachment_Index_Info)
        return Rendering_Input_Attachment_Index_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        RIAIIC: Rendering_Input_Attachment_Index_Info_C;
    begin
        RIAIIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(RIAIIC.Color_Attachment_Count,
                   Struct.Color_Attachment_Input_Indices,
                   RIAIIC.Color_Attachment_Input_Indices);
        RIAIIC.Depth_Input_Attachment_Index :=
            Struct.Depth_Input_Attachment_Index;
        RIAIIC.Stencil_Input_Attachment_Index :=
            Struct.Stencil_Input_Attachment_Index;

        return RIAIIC;
    end To_C;

    procedure Free(Struct: in out Rendering_Input_Attachment_Index_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Color_Attachment_Input_Indices);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_6_Features;
         C_Struct: in Physical_Device_Maintenance_6_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Maintenance_6 := Utilities.To_Ada(C_Struct.Maintenance_6);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_6_Properties;
         C_Struct: in Physical_Device_Maintenance_6_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Block_Texel_View_Compatible_Multiple_Layers :=
            Utilities.To_Ada
                (C_Struct.Block_Texel_View_Compatible_Multiple_Layers);
        Ada_Struct.Max_Combined_Image_Sampler_Descriptor_Count :=
            C_Struct.Max_Combined_Image_Sampler_Descriptor_Count;
        Ada_Struct.Fragment_Shading_Rate_Clamp_Combiner_Inputs :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_Clamp_Combiner_Inputs);
    end To_Ada;

    function To_C(Struct: in Bind_Memory_Status) return Bind_Memory_Status_C is
        BMSC: Bind_Memory_Status_C;
    begin
        BMSC.Next := Extension_Records.To_C(Struct.Next);
        BMSC.Result := Struct.Result;

        return BMSC;
    end To_C;

    procedure Free(Struct: in out Bind_Memory_Status_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Bind_Descriptor_Sets_Info)
        return Bind_Descriptor_Sets_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Descriptor_Set_Arrays, Descriptor_Set_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        BDSIC: Bind_Descriptor_Sets_Info_C;
    begin
        BDSIC.Next := Extension_Records.To_C(Struct.Next);
        BDSIC.Stage_Flags := Struct.Stage_Flags;
        BDSIC.Layout := Struct.Layout;
        BDSIC.First_Set := Struct.First_Set;
        To_C_Array(BDSIC.Descriptor_Set_Count,
                   Struct.Descriptor_Sets,
                   BDSIC.Descriptor_Sets);
        To_C_Array(BDSIC.Dynamic_Offset_Count,
                   Struct.Dynamic_Offsets,
                   BDSIC.Dynamic_Offsets);

        return BDSIC;
    end To_C;

    procedure Free(Struct: in out Bind_Descriptor_Sets_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Descriptor_Set_Arrays.Free(Struct.Descriptor_Sets);
        C.Uint32_t_Arrays.Free(Struct.Dynamic_Offsets);
    end Free;

    function To_C(Struct: in Push_Constants_Info)
        return Push_Constants_Info_C is
        PCIC: Push_Constants_Info_C;
    begin
        PCIC.Next := Extension_Records.To_C(Struct.Next);
        PCIC.Layout := Struct.Layout;
        PCIC.Stage_Flags := Struct.Stage_Flags;
        PCIC.Offset := Struct.Offset;
        PCIC.Size := Struct.Size;
        PCIC.Values := Struct.Values;

        return PCIC;
    end To_C;

    procedure Free(Struct: in out Push_Constants_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Push_Descriptor_Set_Info)
        return Push_Descriptor_Set_Info_C is
        procedure To_C_Array is
            new Utilities.To_C_Array_Convert(Write_Descriptor_Set_C_Arrays,
                                             Write_Descriptor_Set_Vectors,
                                             C.To_C);

        PDSIC: Push_Descriptor_Set_Info_C;
    begin
        PDSIC.Next := Extension_Records.To_C(Struct.Next);
        PDSIC.Stage_Flags := Struct.Stage_Flags;
        PDSIC.Layout := Struct.Layout;
        PDSIC.Set := Struct.Set;
        To_C_Array(PDSIC.Descriptor_Write_Count,
                   Struct.Descriptor_Writes,
                   PDSIC.Descriptor_Writes);

        return PDSIC;
    end To_C;

    procedure Free(Struct: in out Push_Descriptor_Set_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Write_Descriptor_Set_C_Arrays.Free(Struct.Descriptor_Writes,
                                           C.Free'Access);
    end Free;

    function To_C(Struct: in Push_Descriptor_Set_With_Template_Info)
        return Push_Descriptor_Set_With_Template_Info_C is
        PDSWTIC: Push_Descriptor_Set_With_Template_Info_C;
    begin
        PDSWTIC.Next := Extension_Records.To_C(Struct.Next);
        PDSWTIC.Descriptor_Update_Template := Struct.Descriptor_Update_Template;
        PDSWTIC.Layout := Struct.Layout;
        PDSWTIC.Set := Struct.Set;
        PDSWTIC.Data := Struct.Data;

        return PDSWTIC;
    end To_C;

    procedure Free(Struct: in out Push_Descriptor_Set_With_Template_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Protected_Access_Features;
         C_Struct: in Physical_Device_Pipeline_Protected_Access_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Pipeline_Protected_Access :=
            Utilities.To_Ada(C_Struct.Pipeline_Protected_Access);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Robustness_Features;
         C_Struct: in Physical_Device_Pipeline_Robustness_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Pipeline_Robustness :=
            Utilities.To_Ada(C_Struct.Pipeline_Robustness);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Robustness_Properties;
         C_Struct: in Physical_Device_Pipeline_Robustness_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Default_Robustness_Storage_Buffers :=
            C_Struct.Default_Robustness_Storage_Buffers;
        Ada_Struct.Default_Robustness_Uniform_Buffers :=
            C_Struct.Default_Robustness_Uniform_Buffers;
        Ada_Struct.Default_Robustness_Vertex_Inputs :=
            C_Struct.Default_Robustness_Vertex_Inputs;
        Ada_Struct.Default_Robustness_Images :=
            C_Struct.Default_Robustness_Images;
    end To_Ada;

    function To_C(Struct: in Pipeline_Robustness_Create_Info)
        return Pipeline_Robustness_Create_Info_C is
        PRCIC: Pipeline_Robustness_Create_Info_C;
    begin
        PRCIC.Next := Extension_Records.To_C(Struct.Next);
        PRCIC.Storage_Buffers := Struct.Storage_Buffers;
        PRCIC.Uniform_Buffers := Struct.Uniform_Buffers;
        PRCIC.Vertex_Inputs := Struct.Vertex_Inputs;
        PRCIC.Images := Struct.Images;

        return PRCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Robustness_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Host_Image_Copy_Features;
         C_Struct: in Physical_Device_Host_Image_Copy_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Host_Image_Copy :=
            Utilities.To_Ada(C_Struct.Host_Image_Copy);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Host_Image_Copy_Properties;
         C_Struct: in Physical_Device_Host_Image_Copy_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Copy_Src_Layout_Count := C_Struct.Copy_Src_Layout_Count;
        Ada_Struct.Copy_Src_Layouts := C_Struct.Copy_Src_Layouts;
        Ada_Struct.Copy_Dst_Layout_Count := C_Struct.Copy_Dst_Layout_Count;
        Ada_Struct.Copy_Dst_Layouts := C_Struct.Copy_Dst_Layouts;
        Ada_Struct.Optimal_Tiling_Layout_UUID :=
            C_Struct.Optimal_Tiling_Layout_UUID;
        Ada_Struct.Identical_Memory_Type_Requirements :=
            Utilities.To_Ada(C_Struct.Identical_Memory_Type_Requirements);
    end To_Ada;

    function To_C(Struct: in Memory_To_Image_Copy)
        return Memory_To_Image_Copy_C is
        MTICC: Memory_To_Image_Copy_C;
    begin
        MTICC.Next := Extension_Records.To_C(Struct.Next);
        MTICC.Host_Pointer := Struct.Host_Pointer;
        MTICC.Memory_Row_Length := Struct.Memory_Row_Length;
        MTICC.Memory_Image_Height := Struct.Memory_Image_Height;
        MTICC.Image_Subresource := Struct.Image_Subresource;
        MTICC.Image_Offset := Struct.Image_Offset;
        MTICC.Image_Extent := Struct.Image_Extent;

        return MTICC;
    end To_C;

    procedure Free(Struct: in out Memory_To_Image_Copy_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Image_To_Memory_Copy)
        return Image_To_Memory_Copy_C is
        MTICC: Image_To_Memory_Copy_C;
    begin
        MTICC.Next := Extension_Records.To_C(Struct.Next);
        MTICC.Host_Pointer := Struct.Host_Pointer;
        MTICC.Memory_Row_Length := Struct.Memory_Row_Length;
        MTICC.Memory_Image_Height := Struct.Memory_Image_Height;
        MTICC.Image_Subresource := Struct.Image_Subresource;
        MTICC.Image_Offset := Struct.Image_Offset;
        MTICC.Image_Extent := Struct.Image_Extent;

        return MTICC;
    end To_C;

    procedure Free(Struct: in out Image_To_Memory_Copy_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Copy_Memory_To_Image_Info)
        return Copy_Memory_To_Image_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Memory_To_Image_Copy_C_Arrays, Memory_To_Image_Copy_Vectors);

        CMTIIC: Copy_Memory_To_Image_Info_C;
    begin
        CMTIIC.Next := Extension_Records.To_C(Struct.Next);
        CMTIIC.Flags := Struct.Flags;
        CMTIIC.Dst_Image := Struct.Dst_Image;
        CMTIIC.Dst_Image_Layout := Struct.Dst_Image_Layout;
        To_C_Array(CMTIIC.Region_Count, Struct.Regions, CMTIIC.Regions);

        return CMTIIC;
    end To_C;

    procedure Free(Struct: in out Copy_Memory_To_Image_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Memory_To_Image_Copy_C_Arrays.Free(Struct.Regions, Free'Access);
    end Free;

    function To_C(Struct: in Copy_Image_To_Memory_Info)
        return Copy_Image_To_Memory_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Image_To_Memory_Copy_C_Arrays, Image_To_Memory_Copy_Vectors);

        CMTIIC: Copy_Image_To_Memory_Info_C;
    begin
        CMTIIC.Next := Extension_Records.To_C(Struct.Next);
        CMTIIC.Flags := Struct.Flags;
        CMTIIC.Src_Image := Struct.Src_Image;
        CMTIIC.Src_Image_Layout := Struct.Src_Image_Layout;
        To_C_Array(CMTIIC.Region_Count, Struct.Regions, CMTIIC.Regions);

        return CMTIIC;
    end To_C;

    procedure Free(Struct: in out Copy_Image_To_Memory_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Image_To_Memory_Copy_C_Arrays.Free(Struct.Regions, Free'Access);
    end Free;

    function To_C(Struct: in Copy_Image_To_Image_Info)
        return Copy_Image_To_Image_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (C_V1_3.Image_Copy_2_C_Arrays, Image_Copy_2_Vectors, C_V1_3.To_C);

        CITIIC: Copy_Image_To_Image_Info_C;
    begin
        CITIIC.Next := Extension_Records.To_C(Struct.Next);
        CITIIC.Flags := Struct.Flags;
        CITIIC.Src_Image := Struct.Src_Image;
        CITIIC.Src_Image_Layout := Struct.Src_Image_Layout;
        CITIIC.Dst_Image := Struct.Dst_Image;
        CITIIC.Dst_Image_Layout := Struct.Dst_Image_Layout;
        To_C_Array(CITIIC.Region_Count, Struct.Regions, CITIIC.Regions);

        return CITIIC;
    end To_C;

    procedure Free(Struct: in out Copy_Image_To_Image_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C_V1_3.Image_Copy_2_C_Arrays.Free(Struct.Regions, C_V1_3.Free'Access);
    end Free;

    function To_C(Struct: in Host_Image_Layout_Transition_Info)
        return Host_Image_Layout_Transition_Info_C is
        HILTIC: Host_Image_Layout_Transition_Info_C;
    begin
        HILTIC.Next := Extension_Records.To_C(Struct.Next);
        HILTIC.Image := Struct.Image;
        HILTIC.Old_Layout := Struct.Old_Layout;
        HILTIC.New_Layout := Struct.New_Layout;
        HILTIC.Subresource_Range := Struct.Subresource_Range;
        
        return HILTIC;
    end To_C;

    procedure Free(Struct: in out Host_Image_Layout_Transition_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Subresource_Host_Memcpy_Size;
                     C_Struct: in Subresource_Host_Memcpy_Size_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Size := C_Struct.Size;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Host_Image_Copy_Device_Performance_Query;
         C_Struct: in Host_Image_Copy_Device_Performance_Query_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Optimal_Device_Access :=
            Utilities.To_Ada(C_Struct.Optimal_Device_Access);
        Ada_Struct.Identical_Memory_Layout :=
            Utilities.To_Ada(C_Struct.Identical_Memory_Layout);
    end To_Ada;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Device_Queue_Global_Priority_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                       (Device_Queue_Global_Priority_Create_Info,
                        Device_Queue_Global_Priority_Create_Info_C,
                        Device_Queue_Global_Priority_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Rasterization_Line_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                       (Pipeline_Rasterization_Line_State_Create_Info,
                        Pipeline_Rasterization_Line_State_Create_Info_C,
                        Pipeline_Rasterization_Line_State_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Vertex_Input_Divisor_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                     (Pipeline_Vertex_Input_Divisor_State_Create_Info,
                      Pipeline_Vertex_Input_Divisor_State_Create_Info_C,
                      Pipeline_Vertex_Input_Divisor_State_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Map_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_Map_Info,
                         Memory_Map_Info_C,
                         Memory_Map_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Unmap_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_Unmap_Info,
                         Memory_Unmap_Info_C,
                         Memory_Unmap_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Rendering_Area_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Rendering_Area_Info,
                         Rendering_Area_Info_C,
                         Rendering_Area_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Image_Subresource_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Image_Subresource_Info,
                         Device_Image_Subresource_Info_C,
                         Device_Image_Subresource_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Create_Flags_2_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Create_Flags_2_Create_Info,
                         Pipeline_Create_Flags_2_Create_Info_C,
                         Pipeline_Create_Flags_2_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Usage_Flags_2_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Usage_Flags_2_Create_Info,
                         Buffer_Usage_Flags_2_Create_Info_C,
                         Buffer_Usage_Flags_2_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Rendering_Attachment_Location_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Rendering_Attachment_Location_Info,
                         Rendering_Attachment_Location_Info_C,
                         Rendering_Attachment_Location_Info_C_Access);
                    begin
                        return Make_Struct(Next);
                    end;
            when Rendering_Input_Attachment_Index_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Rendering_Input_Attachment_Index_Info,
                         Rendering_Input_Attachment_Index_Info_C,
                         Rendering_Input_Attachment_Index_Info_C_Access);
                    begin
                        return Make_Struct(Next);
                    end;
            when Bind_Memory_Status_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Memory_Status,
                         Bind_Memory_Status_C,
                         Bind_Memory_Status_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Descriptor_Sets_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Descriptor_Sets_Info,
                         Bind_Descriptor_Sets_Info_C,
                         Bind_Descriptor_Sets_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Push_Constants_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Push_Constants_Info,
                         Push_Constants_Info_C,
                         Push_Constants_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Push_Descriptor_Set_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Push_Descriptor_Set_Info,
                         Push_Descriptor_Set_Info_C,
                         Push_Descriptor_Set_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Push_Descriptor_Set_With_Template_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Push_Descriptor_Set_With_Template_Info,
                         Push_Descriptor_Set_With_Template_Info_C,
                         Push_Descriptor_Set_With_Template_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Robustness_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Robustness_Create_Info,
                         Pipeline_Robustness_Create_Info_C,
                         Pipeline_Robustness_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_To_Image_Copy_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_To_Image_Copy,
                         Memory_To_Image_Copy_C,
                         Memory_To_Image_Copy_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_To_Memory_Copy_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_To_Memory_Copy,
                         Image_To_Memory_Copy_C,
                         Image_To_Memory_Copy_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Copy_Memory_To_Image_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Copy_Memory_To_Image_Info,
                         Copy_Memory_To_Image_Info_C,
                         Copy_Memory_To_Image_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Copy_Image_To_Memory_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Copy_Image_To_Memory_Info,
                         Copy_Image_To_Memory_Info_C,
                         Copy_Image_To_Memory_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Copy_Image_To_Image_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Copy_Image_To_Image_Info,
                         Copy_Image_To_Image_Info_C,
                         Copy_Image_To_Image_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Host_Image_Layout_Transition_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Host_Image_Layout_Transition_Info,
                         Host_Image_Layout_Transition_Info_C,
                         Host_Image_Layout_Transition_Info_C_Access);
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
            when Physical_Device_Vulkan_1_4_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Vulkan_1_4_Features,
                         Physical_Device_Vulkan_1_4_Features_C,
                         Physical_Device_Vulkan_1_4_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_4_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Vulkan_1_4_Properties,
                         Physical_Device_Vulkan_1_4_Properties_C,
                         Physical_Device_Vulkan_1_4_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Global_Priority_Query_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                      (Physical_Device_Global_Priority_Query_Features,
                       Physical_Device_Global_Priority_Query_Features_C,
                       Physical_Device_Global_Priority_Query_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Queue_Family_Global_Priority_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Queue_Family_Global_Priority_Properties,
                         Queue_Family_Global_Priority_Properties_C,
                         Queue_Family_Global_Priority_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Subgroup_Rotate_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                     (Physical_Device_Shader_Subgroup_Rotate_Features,
                      Physical_Device_Shader_Subgroup_Rotate_Features_C,
                      Physical_Device_Shader_Subgroup_Rotate_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Float_Controls_2_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Physical_Device_Shader_Float_Controls_2_Features,
                     Physical_Device_Shader_Float_Controls_2_Features_C,
                     Physical_Device_Shader_Float_Controls_2_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Expect_Assume_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Physical_Device_Shader_Expect_Assume_Features,
                        Physical_Device_Shader_Expect_Assume_Features_C,
                        Physical_Device_Shader_Expect_Assume_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Line_Rasterization_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Line_Rasterization_Features,
                         Physical_Device_Line_Rasterization_Features_C,
                         Physical_Device_Line_Rasterization_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Line_Rasterization_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Physical_Device_Line_Rasterization_Properties,
                        Physical_Device_Line_Rasterization_Properties_C,
                        Physical_Device_Line_Rasterization_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Vertex_Attribute_Divisor_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                 (Physical_Device_Vertex_Attribute_Divisor_Properties,
                  Physical_Device_Vertex_Attribute_Divisor_Properties_C,
                  Physical_Device_Vertex_Attribute_Divisor_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Vertex_Attribute_Divisor_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Physical_Device_Vertex_Attribute_Divisor_Features,
                    Physical_Device_Vertex_Attribute_Divisor_Features_C,
                    Physical_Device_Vertex_Attribute_Divisor_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Index_Type_Uint8_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Index_Type_Uint8_Features,
                         Physical_Device_Index_Type_Uint8_Features_C,
                         Physical_Device_Index_Type_Uint8_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Maintenance_5_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_5_Features,
                         Physical_Device_Maintenance_5_Features_C,
                         Physical_Device_Maintenance_5_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Maintenance_5_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_5_Properties,
                         Physical_Device_Maintenance_5_Properties_C,
                         Physical_Device_Maintenance_5_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Subresource_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Image_Subresource_2,
                         Image_Subresource_2_C,
                         Image_Subresource_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Subresource_Layout_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Subresource_Layout_2,
                         Subresource_Layout_2_C,
                         Subresource_Layout_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Push_Descriptor_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Push_Descriptor_Properties,
                         Physical_Device_Push_Descriptor_Properties_C,
                         Physical_Device_Push_Descriptor_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Dynamic_Rendering_Local_Read_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
               (Physical_Device_Dynamic_Rendering_Local_Read_Features,
                Physical_Device_Dynamic_Rendering_Local_Read_Features_C,
                Physical_Device_Dynamic_Rendering_Local_Read_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Maintenance_6_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_6_Features,
                         Physical_Device_Maintenance_6_Features_C,
                         Physical_Device_Maintenance_6_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Maintenance_6_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Physical_Device_Maintenance_6_Properties,
                        Physical_Device_Maintenance_6_Properties_C,
                        Physical_Device_Maintenance_6_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Pipeline_Protected_Access_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                  (Physical_Device_Pipeline_Protected_Access_Features,
                   Physical_Device_Pipeline_Protected_Access_Features_C,
                   Physical_Device_Pipeline_Protected_Access_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Pipeline_Robustness_Features,
                         Physical_Device_Pipeline_Robustness_Features_C,
                         Physical_Device_Pipeline_Robustness_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                      (Physical_Device_Pipeline_Robustness_Properties,
                       Physical_Device_Pipeline_Robustness_Properties_C,
                       Physical_Device_Pipeline_Robustness_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Host_Image_Copy_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Host_Image_Copy_Features,
                         Physical_Device_Host_Image_Copy_Features_C,
                         Physical_Device_Host_Image_Copy_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Host_Image_Copy_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Host_Image_Copy_Properties,
                         Physical_Device_Host_Image_Copy_Properties_C,
                         Physical_Device_Host_Image_Copy_Properties_C_Access);

                    function To_C_Struct is
                        new Ada.Unchecked_Conversion
                          (C.Out_Structure_C_Access,
                           Physical_Device_Host_Image_Copy_Properties_C_Access);
                    Ada_Struct:
                        Physical_Device_Host_Image_Copy_Properties renames
                            Physical_Device_Host_Image_Copy_Properties
                                (Next.all);
                    C_Struct_Access: C.Out_Structure_C_Access :=
                        Make_Struct(Next);
                    C_Struct:
                        Physical_Device_Host_Image_Copy_Properties_C_Access :=
                            To_C_Struct(C_Struct_Access);
                begin
                    C_Struct.Copy_Src_Layout_Count :=
                        Ada_Struct.Copy_Src_Layout_Count;
                    C_Struct.Copy_Src_Layouts := Ada_Struct.Copy_Src_Layouts;
                    C_Struct.Copy_Dst_Layout_Count :=
                        Ada_Struct.Copy_Dst_Layout_Count;
                    C_Struct.Copy_Dst_Layouts := Ada_Struct.Copy_Dst_Layouts;

                    return C_Struct_Access;
                end;
            when Subresource_Host_Memcpy_Size_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Subresource_Host_Memcpy_Size,
                         Subresource_Host_Memcpy_Size_C,
                         Subresource_Host_Memcpy_Size_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Host_Image_Copy_Device_Performance_Query_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Host_Image_Copy_Device_Performance_Query,
                         Host_Image_Copy_Device_Performance_Query_C,
                         Host_Image_Copy_Device_Performance_Query_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Vulkan_1_4_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Vulkan_1_4_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Vulkan_1_4_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Vulkan_1_4_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Vulkan_1_4_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Vulkan_1_4_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Global_Priority_Query_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Global_Priority_Query_Features_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Global_Priority_Query_Features
                            (Ada_Struct),
                         To_Access(Next).all);
                end;
            when Queue_Family_Global_Priority_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Queue_Family_Global_Priority_Properties_C_Access);
                begin
                   To_Ada(Queue_Family_Global_Priority_Properties(Ada_Struct),
                          To_Access(Next).all);
                end;
            when Physical_Device_Shader_Subgroup_Rotate_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                     (C.Out_Structure_C_Access,
                      Physical_Device_Shader_Subgroup_Rotate_Features_C_Access);
                begin
                   To_Ada(Physical_Device_Shader_Subgroup_Rotate_Features
                       (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Shader_Float_Controls_2_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                    (C.Out_Structure_C_Access,
                     Physical_Device_Shader_Float_Controls_2_Features_C_Access);
                begin
                   To_Ada(Physical_Device_Shader_Float_Controls_2_Features
                       (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Shader_Expect_Assume_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Shader_Expect_Assume_Features_C_Access);
                begin
                   To_Ada(Physical_Device_Shader_Expect_Assume_Features
                       (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Line_Rasterization_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Line_Rasterization_Features_C_Access);
                begin
                   To_Ada(Physical_Device_Line_Rasterization_Features
                       (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Line_Rasterization_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Line_Rasterization_Properties_C_Access);
                begin
                   To_Ada(Physical_Device_Line_Rasterization_Properties
                       (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Vertex_Attribute_Divisor_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                 (C.Out_Structure_C_Access,
                  Physical_Device_Vertex_Attribute_Divisor_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Vertex_Attribute_Divisor_Properties
                        (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Vertex_Attribute_Divisor_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                   (C.Out_Structure_C_Access,
                    Physical_Device_Vertex_Attribute_Divisor_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Vertex_Attribute_Divisor_Features
                        (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Index_Type_Uint8_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Index_Type_Uint8_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Index_Type_Uint8_Features
                        (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Maintenance_5_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_5_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_5_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Maintenance_5_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_5_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_5_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Image_Subresource_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Image_Subresource_2_C_Access);
                begin
                    To_Ada(Image_Subresource_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Subresource_Layout_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Subresource_Layout_2_C_Access);
                begin
                    To_Ada(Subresource_Layout_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Push_Descriptor_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Push_Descriptor_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Push_Descriptor_Properties
                            (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Dynamic_Rendering_Local_Read_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
               (C.Out_Structure_C_Access,
                Physical_Device_Dynamic_Rendering_Local_Read_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Dynamic_Rendering_Local_Read_Features
                            (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Maintenance_6_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_6_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_6_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Maintenance_6_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_6_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_6_Properties(Ada_Struct),
                            To_Access(Next).all);
                end;
            when Physical_Device_Pipeline_Protected_Access_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                  (C.Out_Structure_C_Access,
                   Physical_Device_Pipeline_Protected_Access_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Pipeline_Protected_Access_Features
                            (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Pipeline_Robustness_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Pipeline_Robustness_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Pipeline_Robustness_Properties
                            (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Pipeline_Robustness_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Pipeline_Robustness_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Pipeline_Robustness_Features
                            (Ada_Struct), To_Access(Next).all);
                end;
            when Physical_Device_Host_Image_Copy_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Host_Image_Copy_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Host_Image_Copy_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Host_Image_Copy_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Host_Image_Copy_Properties_C_Access);
                begin
                  To_Ada(Physical_Device_Host_Image_Copy_Properties(Ada_Struct),
                          To_Access(Next).all);
                end;
            when Subresource_Host_Memcpy_Size_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Subresource_Host_Memcpy_Size_C_Access);
                begin
                    To_Ada(Subresource_Host_Memcpy_Size(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Host_Image_Copy_Device_Performance_Query_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Host_Image_Copy_Device_Performance_Query_C_Access);
                begin
                    To_Ada(Host_Image_Copy_Device_Performance_Query(Ada_Struct),
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
            when Device_Queue_Global_Priority_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Queue_Global_Priority_Create_Info_C,
                         Device_Queue_Global_Priority_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Rasterization_Line_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Pipeline_Rasterization_Line_State_Create_Info_C,
                        Pipeline_Rasterization_Line_State_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Vertex_Input_Divisor_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Pipeline_Vertex_Input_Divisor_State_Create_Info_C,
                      Pipeline_Vertex_Input_Divisor_State_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Map_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Memory_Map_Info_C, Memory_Map_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Unmap_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Memory_Unmap_Info_C, Memory_Unmap_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Rendering_Area_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Rendering_Area_Info_C,
                         Rendering_Area_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Image_Subresource_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Image_Subresource_Info_C,
                         Device_Image_Subresource_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Create_Flags_2_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Create_Flags_2_Create_Info_C,
                         Pipeline_Create_Flags_2_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Usage_Flags_2_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Buffer_Usage_Flags_2_Create_Info_C,
                         Buffer_Usage_Flags_2_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Rendering_Attachment_Location_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Rendering_Attachment_Location_Info_C,
                         Rendering_Attachment_Location_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Rendering_Input_Attachment_Index_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Rendering_Input_Attachment_Index_Info_C,
                         Rendering_Input_Attachment_Index_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Memory_Status_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Memory_Status_C,
                         Bind_Memory_Status_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Descriptor_Sets_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Descriptor_Sets_Info_C,
                         Bind_Descriptor_Sets_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Push_Constants_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Push_Constants_Info_C, Push_Constants_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Push_Descriptor_Set_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Push_Descriptor_Set_Info_C,
                         Push_Descriptor_Set_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Push_Descriptor_Set_With_Template_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Push_Descriptor_Set_With_Template_Info_C,
                         Push_Descriptor_Set_With_Template_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Robustness_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Robustness_Create_Info_C,
                         Pipeline_Robustness_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_To_Image_Copy_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Memory_To_Image_Copy_C,
                         Memory_To_Image_Copy_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_To_Memory_Copy_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_To_Memory_Copy_C,
                         Image_To_Memory_Copy_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Copy_Memory_To_Image_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Copy_Memory_To_Image_Info_C,
                         Copy_Memory_To_Image_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Copy_Image_To_Memory_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Copy_Image_To_Memory_Info_C,
                         Copy_Image_To_Memory_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Copy_Image_To_Image_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Copy_Image_To_Image_Info_C,
                         Copy_Image_To_Image_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Host_Image_Layout_Transition_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Host_Image_Layout_Transition_Info_C,
                         Host_Image_Layout_Transition_Info_C_Access);
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
            when Physical_Device_Vulkan_1_4_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Vulkan_1_4_Features_C,
                         Physical_Device_Vulkan_1_4_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Vulkan_1_4_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Vulkan_1_4_Properties_C,
                         Physical_Device_Vulkan_1_4_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Global_Priority_Query_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Global_Priority_Query_Features_C,
                       Physical_Device_Global_Priority_Query_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Queue_Family_Global_Priority_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Queue_Family_Global_Priority_Properties_C,
                         Queue_Family_Global_Priority_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Subgroup_Rotate_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                     (Physical_Device_Shader_Subgroup_Rotate_Features_C,
                      Physical_Device_Shader_Subgroup_Rotate_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Float_Controls_2_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                    (Physical_Device_Shader_Float_Controls_2_Features_C,
                     Physical_Device_Shader_Float_Controls_2_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Expect_Assume_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Physical_Device_Shader_Expect_Assume_Features_C,
                        Physical_Device_Shader_Expect_Assume_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Line_Rasterization_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Line_Rasterization_Features_C,
                         Physical_Device_Line_Rasterization_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Line_Rasterization_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Physical_Device_Line_Rasterization_Properties_C,
                        Physical_Device_Line_Rasterization_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Vertex_Attribute_Divisor_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                 (Physical_Device_Vertex_Attribute_Divisor_Properties_C,
                  Physical_Device_Vertex_Attribute_Divisor_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Vertex_Attribute_Divisor_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                   (Physical_Device_Vertex_Attribute_Divisor_Features_C,
                    Physical_Device_Vertex_Attribute_Divisor_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Index_Type_Uint8_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Index_Type_Uint8_Features_C,
                         Physical_Device_Index_Type_Uint8_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Maintenance_5_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_5_Features_C,
                         Physical_Device_Maintenance_5_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Maintenance_5_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_5_Properties_C,
                         Physical_Device_Maintenance_5_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Subresource_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Image_Subresource_2_C, Image_Subresource_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Subresource_Layout_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Subresource_Layout_2_C, Subresource_Layout_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Push_Descriptor_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Push_Descriptor_Properties_C,
                         Physical_Device_Push_Descriptor_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Dynamic_Rendering_Local_Read_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
               (Physical_Device_Dynamic_Rendering_Local_Read_Features_C,
                Physical_Device_Dynamic_Rendering_Local_Read_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Maintenance_6_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_6_Features_C,
                         Physical_Device_Maintenance_6_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Maintenance_6_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_6_Properties_C,
                         Physical_Device_Maintenance_6_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Pipeline_Protected_Access_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                  (Physical_Device_Pipeline_Protected_Access_Features_C,
                   Physical_Device_Pipeline_Protected_Access_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Pipeline_Robustness_Features_C,
                         Physical_Device_Pipeline_Robustness_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Pipeline_Robustness_Properties_C,
                       Physical_Device_Pipeline_Robustness_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Host_Image_Copy_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Host_Image_Copy_Features_C,
                         Physical_Device_Host_Image_Copy_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Host_Image_Copy_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Host_Image_Copy_Properties_C,
                         Physical_Device_Host_Image_Copy_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Subresource_Host_Memcpy_Size_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Subresource_Host_Memcpy_Size_C,
                         Subresource_Host_Memcpy_Size_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Host_Image_Copy_Device_Performance_Query_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Host_Image_Copy_Device_Performance_Query_C,
                         Host_Image_Copy_Device_Performance_Query_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.C_V1_4;


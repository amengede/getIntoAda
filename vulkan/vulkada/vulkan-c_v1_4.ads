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

with Interfaces.C.Pointers;
with Vulkan.C;
with Vulkan.C_V1_2;
with Vulkan.C_V1_3;
with Vulkan.C_Arrays;

private package Vulkan.C_V1_4 is
    -- Structure classification.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Vulkan_1_4_Features_Type |
            Physical_Device_Vulkan_1_4_Properties_Type |
            Device_Queue_Global_Priority_Create_Info_Type |
            Physical_Device_Global_Priority_Query_Features_Type |
            Queue_Family_Global_Priority_Properties_Type |
            Physical_Device_Shader_Subgroup_Rotate_Features_Type |
            Physical_Device_Shader_Float_Controls_2_Features_Type |
            Physical_Device_Shader_Expect_Assume_Features_Type |
            Physical_Device_Line_Rasterization_Features_Type |
            Physical_Device_Line_Rasterization_Properties_Type |
            Pipeline_Rasterization_Line_State_Create_Info_Type |
            Physical_Device_Vertex_Attribute_Divisor_Properties_Type |
            Pipeline_Vertex_Input_Divisor_State_Create_Info_Type |
            Physical_Device_Vertex_Attribute_Divisor_Features_Type |
            Physical_Device_Index_Type_Uint8_Features_Type |
            Memory_Map_Info_Type |
            Memory_Unmap_Info_Type |
            Physical_Device_Maintenance_5_Features_Type |
            Physical_Device_Maintenance_5_Properties_Type |
            Rendering_Area_Info_Type |
            Image_Subresource_2_Type |
            Device_Image_Subresource_Info_Type |
            Subresource_Layout_2_Type |
            Pipeline_Create_Flags_2_Create_Info_Type |
            Buffer_Usage_Flags_2_Create_Info_Type |
            Physical_Device_Push_Descriptor_Properties_Type |
            Physical_Device_Dynamic_Rendering_Local_Read_Features_Type |
            Rendering_Attachment_Location_Info_Type |
            Rendering_Input_Attachment_Index_Info_Type |
            Physical_Device_Maintenance_6_Features_Type |
            Physical_Device_Maintenance_6_Properties_Type |
            Bind_Memory_Status_Type |
            Bind_Descriptor_Sets_Info_Type |
            Push_Constants_Info_Type |
            Push_Descriptor_Set_Info_Type |
            Push_Descriptor_Set_With_Template_Info_Type |
            Physical_Device_Pipeline_Protected_Access_Features_Type |
            Physical_Device_Pipeline_Robustness_Features_Type |
            Physical_Device_Pipeline_Robustness_Properties_Type |
            Pipeline_Robustness_Create_Info_Type |
            Physical_Device_Host_Image_Copy_Features_Type |
            Physical_Device_Host_Image_Copy_Properties_Type |
            Memory_To_Image_Copy_Type |
            Image_To_Memory_Copy_Type |
            Copy_Memory_To_Image_Info_Type |
            Copy_Image_To_Memory_Info_Type |
            Copy_Image_To_Image_Info_Type |
            Host_Image_Layout_Transition_Info_Type |
            Subresource_Host_Memcpy_Size_Type |
            Host_Image_Copy_Device_Performance_Query_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;
            
    -- C compatible records.
    type Physical_Device_Vulkan_1_4_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vulkan_1_4_Features_Type;
        Next: C.Out_Structure_C_Access;
        Global_Priority_Query: Interfaces.Unsigned_32;
        Shader_Subgroup_Rotate: Interfaces.Unsigned_32;
        Shader_Subgroup_Rotate_Clustered: Interfaces.Unsigned_32;
        Shader_Float_Controls_2: Interfaces.Unsigned_32;
        Shader_Expect_Assume: Interfaces.Unsigned_32;
        Rectangular_Lines: Interfaces.Unsigned_32;
        Bresenham_Lines: Interfaces.Unsigned_32;
        Smooth_Lines: Interfaces.Unsigned_32;
        Stippled_Rectangular_Lines: Interfaces.Unsigned_32;
        Stippled_Bresenham_Lines: Interfaces.Unsigned_32;
        Stippled_Smooth_Lines: Interfaces.Unsigned_32;
        Vertex_Attribute_Instance_Rate_Divisor: Interfaces.Unsigned_32;
        Vertex_Attribute_Instance_Rate_Zero_Divisor: Interfaces.Unsigned_32;
        Index_Type_Uint8: Interfaces.Unsigned_32;
        Dynamic_Rendering_Local_Read: Interfaces.Unsigned_32;
        Maintenance_5: Interfaces.Unsigned_32;
        Maintenance_6: Interfaces.Unsigned_32;
        Pipeline_Protected_Access: Interfaces.Unsigned_32;
        Pipeline_Robustness: Interfaces.Unsigned_32;
        Host_Image_Copy: Interfaces.Unsigned_32;
        Push_Descriptor: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_1_4_Features_C_Access is
        access Physical_Device_Vulkan_1_4_Features_C
        with Convention => C;

    type Image_Layout_Array is array (Interfaces.Unsigned_32 range <>)
        of aliased Image_Layout;

    package Image_Layout_Pointers is
        new Interfaces.C.Pointers(Interfaces.Unsigned_32,
                                  Image_Layout,
                                  Image_Layout_Array,
                                  Image_Layout'First);

    type Physical_Device_Vulkan_1_4_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vulkan_1_4_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Line_Sub_Pixel_Precision_Bits: Interfaces.Unsigned_32;
        Max_Vertex_Attrib_Divisor: Interfaces.Unsigned_32;
        Supports_Non_Zero_First_Instance: Interfaces.Unsigned_32;
        Max_Push_Descriptors: Interfaces.Unsigned_32;
        Dynamic_Rendering_Local_Read_Depth_Stencil_Attachments:
            Interfaces.Unsigned_32;
        Dynamic_Rendering_Local_Read_Multisampled_Attachments:
            Interfaces.Unsigned_32;
        Early_Fragment_Multisample_Coverage_After_Sample_Counting:
            Interfaces.Unsigned_32;
        Early_Fragment_Sample_Mask_Test_Before_Sample_Counting:
            Interfaces.Unsigned_32;
        Depth_Stencil_Swizzle_One_Support: Interfaces.Unsigned_32;
        Polygon_Mode_Point_Size: Interfaces.Unsigned_32;
        Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram:
            Interfaces.Unsigned_32;
        Non_Strict_Wide_Lines_Use_Parallelogram: Interfaces.Unsigned_32;
        Block_Texel_View_Compatible_Multiple_Layers: Interfaces.Unsigned_32;
        Max_Combined_Image_Sampler_Descriptor_Count: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Clamp_Combiner_Inputs: Interfaces.Unsigned_32;
        Default_Robustness_Storage_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Uniform_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Vertex_Inputs: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Images: Pipeline_Robustness_Image_Behavior;
        Copy_Src_Layout_Count: Interfaces.Unsigned_32;
        Copy_Src_Layouts: Image_Layout_Pointers.Pointer;
        Copy_Dst_Layout_Count: Interfaces.Unsigned_32;
        Copy_Dst_Layouts: Image_Layout_Pointers.Pointer;
        Optimal_Tiling_Layout_UUID: UUID;
        Identical_Memory_Type_Requirements: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_1_4_Properties_C_Access is
        access Physical_Device_Vulkan_1_4_Properties_C
        with Convention => C;

    type Device_Queue_Global_Priority_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Device_Queue_Global_Priority_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Global_Priority: Queue_Global_Priority;
    end record
        with Convention => C;

    type Device_Queue_Global_Priority_Create_Info_C_Access is
        access Device_Queue_Global_Priority_Create_Info_C
        with Convention => C;

    type Physical_Device_Global_Priority_Query_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Global_Priority_Query_Features_Type;
        Next: C.Out_Structure_C_Access;
        Global_Priority_Query: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Global_Priority_Query_Features_C_Access is
        access Physical_Device_Global_Priority_Query_Features_C
        with Convention => C;

    type Queue_Family_Global_Priority_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Queue_Family_Global_Priority_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Priority_Count: Interfaces.Unsigned_32;
        Priorities: Queue_Global_Priority_Array;
    end record
        with Convention => C;

    type Queue_Family_Global_Priority_Properties_C_Access is
        access Queue_Family_Global_Priority_Properties_C
        with Convention => C;

    type Physical_Device_Shader_Subgroup_Rotate_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Subgroup_Rotate_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Subgroup_Rotate: Interfaces.Unsigned_32;
        Shader_Subgroup_Rotate_Clustered: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Subgroup_Rotate_Features_C_Access is
        access Physical_Device_Shader_Subgroup_Rotate_Features_C
        with Convention => C;

    type Physical_Device_Shader_Float_Controls_2_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Float_Controls_2_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Float_Controls_2: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Float_Controls_2_Features_C_Access is
        access Physical_Device_Shader_Float_Controls_2_Features_C
        with Convention => C;

    type Physical_Device_Shader_Expect_Assume_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Expect_Assume_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Expect_Assume: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Expect_Assume_Features_C_Access is
        access Physical_Device_Shader_Expect_Assume_Features_C
        with Convention => C;

    type Physical_Device_Line_Rasterization_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Line_Rasterization_Features_Type;
        Next: C.Out_Structure_C_Access;
        Rectangular_Lines: Interfaces.Unsigned_32;
        Bresenham_Lines: Interfaces.Unsigned_32;
        Smooth_Lines: Interfaces.Unsigned_32;
        Stippled_Rectangular_Lines: Interfaces.Unsigned_32;
        Stippled_Bresenham_Lines: Interfaces.Unsigned_32;
        Stippled_Smooth_Lines: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Line_Rasterization_Features_C_Access is
        access Physical_Device_Line_Rasterization_Features_C
        with Convention => C;

    type Physical_Device_Line_Rasterization_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Line_Rasterization_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Line_Sub_Pixel_Precision_Bits: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Line_Rasterization_Properties_C_Access is
        access Physical_Device_Line_Rasterization_Properties_C
        with Convention => C;

    type Pipeline_Rasterization_Line_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Rasterization_Line_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Line_Rasterization_Mode: Vulkan.Line_Rasterization_Mode;
        Stippled_Line_Enable: Interfaces.Unsigned_32;
        Line_Stipple_Factor: Interfaces.Unsigned_32;
        Line_Stipple_Pattern: Interfaces.Unsigned_16;
    end record
        with Convention => C;

    type Pipeline_Rasterization_Line_State_Create_Info_C_Access is
        access Pipeline_Rasterization_Line_State_Create_Info_C
        with Convention => C;
     
    type Physical_Device_Vertex_Attribute_Divisor_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vertex_Attribute_Divisor_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Vertex_Attrib_Divisor: Interfaces.Unsigned_32;
        Supports_Non_Zero_First_Instance: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Properties_C_Access is
        access Physical_Device_Vertex_Attribute_Divisor_Properties_C
        with Convention => C;
   
    package Vertex_Input_Binding_Divisor_Description_Arrays is new C_Arrays
        (Vertex_Input_Binding_Divisor_Description);

    type Pipeline_Vertex_Input_Divisor_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Vertex_Input_Divisor_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Vertex_Binding_Divisor_Count: Interfaces.Unsigned_32;
        Vertex_Binding_Divisors:
            Vertex_Input_Binding_Divisor_Description_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Vertex_Input_Divisor_State_Create_Info_C_Access is
        access Pipeline_Vertex_Input_Divisor_State_Create_Info_C
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vertex_Attribute_Divisor_Features_Type;
        Next: C.Out_Structure_C_Access;
        Vertex_Attribute_Instance_Rate_Divisor: Interfaces.Unsigned_32;
        Vertex_Attribute_Instance_Rate_Zero_Divisor: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Features_C_Access is
        access Physical_Device_Vertex_Attribute_Divisor_Features_C
        with Convention => C;

    type Physical_Device_Index_Type_Uint8_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Index_Type_Uint8_Features_Type;
        Next: C.Out_Structure_C_Access;
        Index_Type_Uint8: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Index_Type_Uint8_Features_C_Access is
        access Physical_Device_Index_Type_Uint8_Features_C
        with Convention => C;

    type Memory_Map_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Map_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Memory_Map_Flags;
        Memory: Device_Memory;
        Offset: Device_Size;
        Size: Device_Size;
    end record
        with Convention => C;

    type Memory_Map_Info_C_Access is access Memory_Map_Info_C
        with Convention => C;

    type Memory_Unmap_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Unmap_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Memory_Unmap_Flags;
        Memory: Device_Memory;
    end record
        with Convention => C;

    type Memory_Unmap_Info_C_Access is access Memory_Unmap_Info_C
        with Convention => C;

    type Physical_Device_Maintenance_5_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_5_Features_Type;
        Next: C.Out_Structure_C_Access;
        Maintenance_5: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_5_Features_C_Access is
        access Physical_Device_Maintenance_5_Features_C
        with Convention => C;

    type Physical_Device_Maintenance_5_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_5_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Early_Fragment_Multisample_Coverage_After_Sample_Counting:
            Interfaces.Unsigned_32;
        Early_Fragment_Sample_Mask_Test_Before_Sample_Counting:
            Interfaces.Unsigned_32;
        Depth_Stencil_Swizzle_One_Support: Interfaces.Unsigned_32;
        Polygon_Mode_Point_Size: Interfaces.Unsigned_32;
        Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram:
            Interfaces.Unsigned_32;
        Non_Strict_Wide_Lines_Use_Parallelogram: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_5_Properties_C_Access is
        access Physical_Device_Maintenance_5_Properties_C
        with Convention => C;

    type Rendering_Area_Info_C is
    record
        Record_Type: In_Structure_Type := Rendering_Area_Info_Type;
        Next: C.In_Structure_C_Access;
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Formats: C_V1_2.Format_Arrays.Pointer;
        Depth_Attachment_Format: Format;
        Stencil_Attachment_Format: Format;
    end record
        with Convention => C;

    type Rendering_Area_Info_C_Access is access Rendering_Area_Info_C
        with Convention => C;

    type Image_Subresource_2_C is
    record
        Record_Type: Out_Structure_Type := Image_Subresource_2_Type;
        Next: C.Out_Structure_C_Access;
        Image_Subresource: Vulkan.Image_Subresource;
    end record
        with Convention => C;

    type Image_Subresource_2_C_Access is access Image_Subresource_2_C
        with Convention => C;

    type Device_Image_Subresource_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Image_Subresource_Info_Type;
        Next: C.In_Structure_C_Access;
        Create_Info: C.Image_Create_Info_C_Access;
        Subresource: Image_Subresource_2_C_Access;
    end record
        with Convention => C;

    type Device_Image_Subresource_Info_C_Access is
        access Device_Image_Subresource_Info_C
        with Convention => C;

    type Subresource_Layout_2_C is
    record
        Record_Type: Out_Structure_Type := Subresource_Layout_2_Type;
        Next: C.Out_Structure_C_Access;
        Subresource_Layout: Vulkan.Subresource_Layout;
    end record
        with Convention => C;

    type Subresource_Layout_2_C_Access is access Subresource_Layout_2_C
        with Convention => C;

    type Pipeline_Create_Flags_2_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Create_Flags_2_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Pipeline_Create_Flags_2;
    end record
        with Convention => C;

    type Pipeline_Create_Flags_2_Create_Info_C_Access is
        access Pipeline_Create_Flags_2_Create_Info_C
        with Convention => C;

    type Buffer_Usage_Flags_2_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Buffer_Usage_Flags_2_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Buffer_Usage_Flags_2;
    end record
        with Convention => C;

    type Buffer_Usage_Flags_2_Create_Info_C_Access is
        access Buffer_Usage_Flags_2_Create_Info_C
        with Convention => C;

    type Physical_Device_Push_Descriptor_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Push_Descriptor_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Push_Descriptors: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Push_Descriptor_Properties_C_Access is
        access Physical_Device_Push_Descriptor_Properties_C
        with Convention => C;

    type Physical_Device_Dynamic_Rendering_Local_Read_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Dynamic_Rendering_Local_Read_Features_Type;
        Next: C.Out_Structure_C_Access;
        Dynamic_Rendering_Local_Read: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Dynamic_Rendering_Local_Read_Features_C_Access is
        access Physical_Device_Dynamic_Rendering_Local_Read_Features_C
        with Convention => C;

    type Rendering_Attachment_Location_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Rendering_Attachment_Location_Info_Type;
        Next: C.In_Structure_C_Access;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Locations: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Rendering_Attachment_Location_Info_C_Access is
        access Rendering_Attachment_Location_Info_C
        with Convention => C;

    type Rendering_Input_Attachment_Index_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Rendering_Input_Attachment_Index_Info_Type;
        Next: C.In_Structure_C_Access;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Input_Indices: C.Uint32_t_Arrays.Pointer;
        Depth_Input_Attachment_Index: Unsigned_32_Access;
        Stencil_Input_Attachment_Index: Unsigned_32_Access;
    end record
        with Convention => C;

    type Rendering_Input_Attachment_Index_Info_C_Access is
        access Rendering_Input_Attachment_Index_Info_C
        with Convention => C;

    type Physical_Device_Maintenance_6_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_6_Features_Type;
        Next: C.Out_Structure_C_Access;
        Maintenance_6: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_6_Features_C_Access is
        access Physical_Device_Maintenance_6_Features_C
        with Convention => C;

    type Physical_Device_Maintenance_6_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_6_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Block_Texel_View_Compatible_Multiple_Layers: Interfaces.Unsigned_32;
        Max_Combined_Image_Sampler_Descriptor_Count: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Clamp_Combiner_Inputs: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_6_Properties_C_Access is
        access Physical_Device_Maintenance_6_Properties_C
        with Convention => C;

    type Bind_Memory_Status_C is
    record
        Record_Type: In_Structure_Type := Bind_Memory_Status_Type;
        Next: C.In_Structure_C_Access;
        Result: Result_Access;
    end record
        with Convention => C;

    type Bind_Memory_Status_C_Access is access Bind_Memory_Status_C
        with Convention => C;

    package Descriptor_Set_Arrays is new C_Arrays(Descriptor_Set);

    type Bind_Descriptor_Sets_Info_C is
    record
        Record_Type: In_Structure_Type := Bind_Descriptor_Sets_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        First_Set: Interfaces.Unsigned_32;
        Descriptor_Set_Count: Interfaces.Unsigned_32;
        Descriptor_Sets: Descriptor_Set_Arrays.Pointer;
        Dynamic_Offset_Count: Interfaces.Unsigned_32;
        Dynamic_Offsets: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Bind_Descriptor_Sets_Info_C_Access is
        access Bind_Descriptor_Sets_Info_C
        with Convention => C;

    type Push_Constants_Info_C is
    record
        Record_Type: In_Structure_Type := Push_Constants_Info_Type;
        Next: C.In_Structure_C_Access;
        Layout: Pipeline_Layout;
        Stage_Flags: Shader_Stage_Flags;
        Offset: Interfaces.Unsigned_32;
        Size: Interfaces.Unsigned_32;
        Values: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Push_Constants_Info_C_Access is access Push_Constants_Info_C
        with Convention => C;

    package Write_Descriptor_Set_C_Arrays is
        new C_Arrays(C.Write_Descriptor_Set_C);

    type Push_Descriptor_Set_Info_C is
    record
        Record_Type: In_Structure_Type := Push_Descriptor_Set_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
        Descriptor_Write_Count: Interfaces.Unsigned_32;
        Descriptor_Writes: Write_Descriptor_Set_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Push_Descriptor_Set_Info_C_Access is
        access Push_Descriptor_Set_Info_C
        with Convention => C;

    type Push_Descriptor_Set_With_Template_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Push_Descriptor_Set_With_Template_Info_Type;
        Next: C.In_Structure_C_Access;
        Descriptor_Update_Template: Vulkan.Descriptor_Update_Template;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
        Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Push_Descriptor_Set_With_Template_Info_C_Access is
        access Push_Descriptor_Set_With_Template_Info_C
        with Convention => C;

    type Physical_Device_Pipeline_Protected_Access_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Protected_Access_Features_Type;
        Next: C.Out_Structure_C_Access;
        Pipeline_Protected_Access: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Protected_Access_Features_C_Access is
        access Physical_Device_Pipeline_Protected_Access_Features_C
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Robustness_Features_Type;
        Next: C.Out_Structure_C_Access;
        Pipeline_Robustness: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Features_C_Access is
        access Physical_Device_Pipeline_Robustness_Features_C
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Robustness_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Default_Robustness_Storage_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Uniform_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Vertex_Inputs: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Images: Pipeline_Robustness_Image_Behavior;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Properties_C_Access is
        access Physical_Device_Pipeline_Robustness_Properties_C
        with Convention => C;

    type Pipeline_Robustness_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Robustness_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Storage_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Uniform_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Vertex_Inputs: Pipeline_Robustness_Buffer_Behavior;
        Images: Pipeline_Robustness_Image_Behavior;
    end record
        with Convention => C;

    type Pipeline_Robustness_Create_Info_C_Access is
        access Pipeline_Robustness_Create_Info_C
        with Convention => C;

    type Physical_Device_Host_Image_Copy_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Host_Image_Copy_Features_Type;
        Next: C.Out_Structure_C_Access;
        Host_Image_Copy: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Host_Image_Copy_Features_C_Access is
        access Physical_Device_Host_Image_Copy_Features_C
        with Convention => C;

    type Physical_Device_Host_Image_Copy_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Host_Image_Copy_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Copy_Src_Layout_Count: Interfaces.Unsigned_32;
        Copy_Src_Layouts: Image_Layout_Access;
        Copy_Dst_Layout_Count: Interfaces.Unsigned_32;
        Copy_Dst_Layouts: Image_Layout_Access;
        Optimal_Tiling_Layout_UUID: UUID;
        Identical_Memory_Type_Requirements: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Host_Image_Copy_Properties_C_Access is
        access Physical_Device_Host_Image_Copy_Properties_C
        with Convention => C;

    type Memory_To_Image_Copy_C is
    record
        Record_Type: In_Structure_Type := Memory_To_Image_Copy_Type;
        Next: C.In_Structure_C_Access;
        Host_Pointer: Interfaces.C.Extensions.void_ptr;
        Memory_Row_Length: Interfaces.Unsigned_32;
        Memory_Image_Height: Interfaces.Unsigned_32;
        Image_Subresource: Image_Subresource_Layers;
        Image_Offset: Offset_3D;
        Image_Extent: Extent_3D;
    end record
        with Convention => C;

    type Memory_To_Image_Copy_C_Access is access Memory_To_Image_Copy_C
        with Convention => C;

    type Image_To_Memory_Copy_C is
    record
        Record_Type: In_Structure_Type := Image_To_Memory_Copy_Type;
        Next: C.In_Structure_C_Access;
        Host_Pointer: Interfaces.C.Extensions.void_ptr;
        Memory_Row_Length: Interfaces.Unsigned_32;
        Memory_Image_Height: Interfaces.Unsigned_32;
        Image_Subresource: Image_Subresource_Layers;
        Image_Offset: Offset_3D;
        Image_Extent: Extent_3D;
    end record
        with Convention => C;

    type Image_To_Memory_Copy_C_Access is access Image_To_Memory_Copy_C
        with Convention => C;

    package Memory_To_Image_Copy_C_Arrays is
        new C_Arrays(Memory_To_Image_Copy_C);

    type Copy_Memory_To_Image_Info_C is
    record
        Record_Type: In_Structure_Type := Copy_Memory_To_Image_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Host_Image_Copy_Flags;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Region_Count: Interfaces.Unsigned_32;
        Regions: Memory_To_Image_Copy_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Copy_Memory_To_Image_Info_C_Access is
        access Copy_Memory_To_Image_Info_C
        with Convention => C;

    package Image_To_Memory_Copy_C_Arrays is
        new C_Arrays(Image_To_Memory_Copy_C);

    type Copy_Image_To_Memory_Info_C is
    record
        Record_Type: In_Structure_Type := Copy_Image_To_Memory_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Host_Image_Copy_Flags;
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Region_Count: Interfaces.Unsigned_32;
        Regions: Image_To_Memory_Copy_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Copy_Image_To_Memory_Info_C_Access is
        access Copy_Image_To_Memory_Info_C
        with Convention => C;

    type Copy_Image_To_Image_Info_C is
    record
        Record_Type: In_Structure_Type := Copy_Image_To_Image_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Host_Image_Copy_Flags;
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Region_Count: Interfaces.Unsigned_32;
        Regions: C_V1_3.Image_Copy_2_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Copy_Image_To_Image_Info_C_Access is access Copy_Image_To_Image_Info_C
        with Convention => C;

    type Host_Image_Layout_Transition_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Host_Image_Layout_Transition_Info_Type;
        Next: C.In_Structure_C_Access;
        Image: Vulkan.Image;
        Old_Layout: Image_Layout;
        New_Layout: Image_Layout;
        Subresource_Range: Image_Subresource_Range;
    end record
        with Convention => C;

    type Host_Image_Layout_Transition_Info_C_Access is
        access Host_Image_Layout_Transition_Info_C
        with Convention => C;

    type Subresource_Host_Memcpy_Size_C is
    record
        Record_Type: Out_Structure_Type := Subresource_Host_Memcpy_Size_Type;
        Next: C.Out_Structure_C_Access;
        Size: Device_Size;
    end record
        with Convention => C;

    type Subresource_Host_Memcpy_Size_C_Access is
        access Subresource_Host_Memcpy_Size_C
        with Convention => C;

    type Host_Image_Copy_Device_Performance_Query_C is
    record
        Record_Type: Out_Structure_Type :=
            Host_Image_Copy_Device_Performance_Query_Type;
        Next: C.Out_Structure_C_Access;
        Optimal_Device_Access: Interfaces.Unsigned_32;
        Identical_Memory_Layout: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Host_Image_Copy_Device_Performance_Query_C_Access is
        access Host_Image_Copy_Device_Performance_Query_C
        with Convention => C;

    -- Load all the function pointers.
    procedure Load(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    type vkCmdSetLineStipple_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Line_Stipple_Factor: in Interfaces.Unsigned_32;
                         Line_Stipple_Pattern: in Interfaces.Unsigned_16)
        with Convention => C;

    vkCmdSetLineStipple: vkCmdSetLineStipple_Access;

    type vkMapMemory2_Access is
        access function(Device: in Vulkan.Device;
                        Memory_Map_Info: in Memory_Map_Info_C;
                        Data: in Interfaces.C.Extensions.void_ptr) return Result
        with Convention => C;

    vkMapMemory2: vkMapMemory2_Access;

    type vkUnmapMemory2_Access is
        access function(Device: in Vulkan.Device;
                        Memory_Unmap_Info: in Memory_Unmap_Info_C) return Result
        with Convention => C;

    vkUnmapMemory2: vkUnmapMemory2_Access;

    type vkCmdBindIndexBuffer2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Buffer: in Vulkan.Buffer;
                         Offset, Size: in Device_Size;
                         Index_Type: in Vulkan.Index_Type)
        with Convention => C;

    vkCmdBindIndexBuffer2: vkCmdBindIndexBuffer2_Access;

    type vkGetRenderingAreaGranularity_Access is
        access procedure(Device: in Vulkan.Device;
                         Rendering_Area_Info: in C_V1_4.Rendering_Area_Info_C;
                         Granularity: out Extent_2D)
        with Convention => C;

    vkGetRenderingAreaGranularity: vkGetRenderingAreaGranularity_Access;

    type vkGetDeviceImageSubresourceLayout_Access is
        access procedure(Device: in Vulkan.Device;
                         Info: in C_V1_4.Device_Image_Subresource_Info_C;
                         Layout: out C_V1_4.Subresource_Layout_2_C)
        with Convention => C;

    vkGetDeviceImageSubresourceLayout: vkGetDeviceImageSubresourceLayout_Access;

    type vkGetImageSubresourceLayout2_Access is
        access procedure(Device: in Vulkan.Device;
                         Image: in Vulkan.Image;
                         Subresource: in C_V1_4.Image_Subresource_2_C;
                         Layout: out C_V1_4.Subresource_Layout_2_C)
        with Convention => C;

    vkGetImageSubresourceLayout2: vkGetImageSubresourceLayout2_Access;

    type vkCmdPushDescriptorSet_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
             Layout: in Pipeline_Layout;
             Set, Descriptor_Write_Count: in Interfaces.Unsigned_32;
             Descriptor_Writes: access constant C.Write_Descriptor_Set_C)
        with Convention => C;

    vkCmdPushDescriptorSet: vkCmdPushDescriptorSet_Access;

    type vkCmdPushDescriptorSetWithTemplate_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Descriptor_Update_Template: in Vulkan.Descriptor_Update_Template;
             Layout: in Pipeline_Layout;
             Set: in Interfaces.Unsigned_32;
             Data: in Interfaces.C.Extensions.void_ptr)
        with Convention => C;

    vkCmdPushDescriptorSetWithTemplate:
        vkCmdPushDescriptorSetWithTemplate_Access;

    type vkCmdSetRenderingAttachmentLocations_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Location_Info: in C_V1_4.Rendering_Attachment_Location_Info_C)
        with Convention => C;

    vkCmdSetRenderingAttachmentLocations:
        vkCmdSetRenderingAttachmentLocations_Access;

    type vkCmdSetRenderingInputAttachmentIndices_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Input_Attachment_Index_Info:
                in C_V1_4.Rendering_Input_Attachment_Index_Info_C)
        with Convention => C;

    vkCmdSetRenderingInputAttachmentIndices:
        vkCmdSetRenderingInputAttachmentIndices_Access;

    type vkCmdBindDescriptorSets2_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Bind_Descriptor_Sets_Info: in C_V1_4.Bind_Descriptor_Sets_Info_C)
        with Convention => C;

    vkCmdBindDescriptorSets2: vkCmdBindDescriptorSets2_Access;

    type vkCmdPushConstants2_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Push_Constants_Info: in C_V1_4.Push_Constants_Info_C)
        with Convention => C;

    vkCmdPushConstants2: vkCmdPushConstants2_Access;
    
    type vkCmdPushDescriptorSet2_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Push_Descriptor_Set_Info: in C_V1_4.Push_Descriptor_Set_Info_C)
        with Convention => C;

    vkCmdPushDescriptorSet2: vkCmdPushDescriptorSet2_Access;

    type vkCmdPushDescriptorSetWithTemplate2_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Push_Descriptor_Set_With_Template_Info:
                in C_V1_4.Push_Descriptor_Set_With_Template_Info_C)
        with Convention => C;

    vkCmdPushDescriptorSetWithTemplate2:
        vkCmdPushDescriptorSetWithTemplate2_Access;

    type vkCopyMemoryToImage_Access is
        access function(Device: in Vulkan.Device;
                        Copy_Memory_To_Image_Info:
                            in Copy_Memory_To_Image_Info_C) return Result
        with Convention => C;

    vkCopyMemoryToImage: vkCopyMemoryToImage_Access;

    type vkCopyImageToMemory_Access is
        access function(Device: in Vulkan.Device;
                        Copy_Image_To_Memory_Info:
                            in Copy_Image_To_Memory_Info_C) return Result
        with Convention => C;

    vkCopyImageToMemory: vkCopyImageToMemory_Access;

    type vkCopyImageToImage_Access is
        access function(Device: in Vulkan.Device;
                        Copy_Image_To_Image_Info:
                            in Copy_Image_To_Image_Info_C) return Result
        with Convention => C;

    vkCopyImageToImage: vkCopyImageToImage_Access;

    type vkTransitionImageLayout_Access is
        access function
            (Device: in Vulkan.Device;
             Transition_Count: Interfaces.Unsigned_32;
             Transitions: access constant Host_Image_Layout_Transition_Info_C)
        return Result
        with Convention => C;

    vkTransitionImageLayout: vkTransitionImageLayout_Access;

    -- Record conversions and management.
    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_4_Features;
                     C_Struct: in Physical_Device_Vulkan_1_4_Features_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_4_Properties;
                     C_Struct: in Physical_Device_Vulkan_1_4_Properties_C);

    function To_C(Struct: in Device_Queue_Global_Priority_Create_Info)
        return Device_Queue_Global_Priority_Create_Info_C;
    procedure Free(Struct: in out Device_Queue_Global_Priority_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Global_Priority_Query_Features;
         C_Struct: in Physical_Device_Global_Priority_Query_Features_C);

    procedure To_Ada(Ada_Struct: in out Queue_Family_Global_Priority_Properties;
                     C_Struct: in Queue_Family_Global_Priority_Properties_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Subgroup_Rotate_Features;
         C_Struct: in Physical_Device_Shader_Subgroup_Rotate_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Float_Controls_2_Features;
         C_Struct: in Physical_Device_Shader_Float_Controls_2_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Expect_Assume_Features;
         C_Struct: in Physical_Device_Shader_Expect_Assume_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Line_Rasterization_Features;
         C_Struct: in Physical_Device_Line_Rasterization_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Line_Rasterization_Properties;
         C_Struct: in Physical_Device_Line_Rasterization_Properties_C);

    function To_C(Struct: in Pipeline_Rasterization_Line_State_Create_Info)
        return Pipeline_Rasterization_Line_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Rasterization_Line_State_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vertex_Attribute_Divisor_Properties;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Properties_C);

    function To_C(Struct: in Pipeline_Vertex_Input_Divisor_State_Create_Info)
        return Pipeline_Vertex_Input_Divisor_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Vertex_Input_Divisor_State_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vertex_Attribute_Divisor_Features;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Index_Type_Uint8_Features;
         C_Struct: in Physical_Device_Index_Type_Uint8_Features_C);

    function To_C(Struct: in Memory_Map_Info) return Memory_Map_Info_C;
    procedure Free(Struct: in out Memory_Map_Info_C);

    function To_C(Struct: in Memory_Unmap_Info) return Memory_Unmap_Info_C;
    procedure Free(Struct: in out Memory_Unmap_Info_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Maintenance_5_Features;
                     C_Struct: in Physical_Device_Maintenance_5_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_5_Properties;
         C_Struct: in Physical_Device_Maintenance_5_Properties_C);

    function To_C(Struct: in Rendering_Area_Info) return Rendering_Area_Info_C;
    procedure Free(Struct: in out Rendering_Area_Info_C);

    procedure To_Ada(Ada_Struct: in out Image_Subresource_2;
                     C_Struct: in Image_Subresource_2_C);

    function To_C(Struct: in Device_Image_Subresource_Info)
        return Device_Image_Subresource_Info_C;
    procedure Free(Struct: in out Device_Image_Subresource_Info_C);

    procedure To_Ada(Ada_Struct: in out Subresource_Layout_2;
                     C_Struct: in Subresource_Layout_2_C);

    function To_C(Struct: in Pipeline_Create_Flags_2_Create_Info)
        return Pipeline_Create_Flags_2_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Create_Flags_2_Create_Info_C);

    function To_C(Struct: in Buffer_Usage_Flags_2_Create_Info)
        return Buffer_Usage_Flags_2_Create_Info_C;
    procedure Free(Struct: in out Buffer_Usage_Flags_2_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Push_Descriptor_Properties;
         C_Struct: in Physical_Device_Push_Descriptor_Properties_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Dynamic_Rendering_Local_Read_Features;
         C_Struct: in Physical_Device_Dynamic_Rendering_Local_Read_Features_C);

    function To_C(Struct: in Rendering_Attachment_Location_Info)
        return Rendering_Attachment_Location_Info_C;
    procedure Free(Struct: in out Rendering_Attachment_Location_Info_C);
 
    function To_C(Struct: in Rendering_Input_Attachment_Index_Info)
        return Rendering_Input_Attachment_Index_Info_C;
    procedure Free(Struct: in out Rendering_Input_Attachment_Index_Info_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Maintenance_6_Features;
                     C_Struct: in Physical_Device_Maintenance_6_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_6_Properties;
         C_Struct: in Physical_Device_Maintenance_6_Properties_C);

    function To_C(Struct: in Bind_Memory_Status) return Bind_Memory_Status_C;
    procedure Free(Struct: in out Bind_Memory_Status_C);

    function To_C(Struct: in Bind_Descriptor_Sets_Info)
        return Bind_Descriptor_Sets_Info_C;
    procedure Free(Struct: in out Bind_Descriptor_Sets_Info_C);

    function To_C(Struct: in Push_Constants_Info) return Push_Constants_Info_C;
    procedure Free(Struct: in out Push_Constants_Info_C);

    function To_C(Struct: in Push_Descriptor_Set_Info)
        return Push_Descriptor_Set_Info_C;
    procedure Free(Struct: in out Push_Descriptor_Set_Info_C);

    function To_C(Struct: in Push_Descriptor_Set_With_Template_Info)
        return Push_Descriptor_Set_With_Template_Info_C;
    procedure Free(Struct: in out Push_Descriptor_Set_With_Template_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Protected_Access_Features;
         C_Struct: in Physical_Device_Pipeline_Protected_Access_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Robustness_Features;
         C_Struct: in Physical_Device_Pipeline_Robustness_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Robustness_Properties;
         C_Struct: in Physical_Device_Pipeline_Robustness_Properties_C);

    function To_C(Struct: in Pipeline_Robustness_Create_Info)
        return Pipeline_Robustness_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Robustness_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Host_Image_Copy_Features;
         C_Struct: in Physical_Device_Host_Image_Copy_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Host_Image_Copy_Properties;
         C_Struct: in Physical_Device_Host_Image_Copy_Properties_C);

    function To_C(Struct: in Memory_To_Image_Copy)
        return Memory_To_Image_Copy_C;
    procedure Free(Struct: in out Memory_To_Image_Copy_C);

    function To_C(Struct: in Image_To_Memory_Copy)
        return Image_To_Memory_Copy_C;
    procedure Free(Struct: in out Image_To_Memory_Copy_C);

    function To_C(Struct: in Copy_Memory_To_Image_Info)
        return Copy_Memory_To_Image_Info_C;
    procedure Free(Struct: in out Copy_Memory_To_Image_Info_C);

    function To_C(Struct: in Copy_Image_To_Memory_Info)
        return Copy_Image_To_Memory_Info_C;
    procedure Free(Struct: in out Copy_Image_To_Memory_Info_C);

    function To_C(Struct: in Copy_Image_To_Image_Info)
        return Copy_Image_To_Image_Info_C;
    procedure Free(Struct: in out Copy_Image_To_Image_Info_C);

    function To_C(Struct: in Host_Image_Layout_Transition_Info)
        return Host_Image_Layout_Transition_Info_C;
    procedure Free(Struct: in out Host_Image_Layout_Transition_Info_C);

    procedure To_Ada(Ada_Struct: in out Subresource_Host_Memcpy_Size;
                     C_Struct: in Subresource_Host_Memcpy_Size_C);

    procedure To_Ada
        (Ada_Struct: in out Host_Image_Copy_Device_Performance_Query;
         C_Struct: in Host_Image_Copy_Device_Performance_Query_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_V1_4;


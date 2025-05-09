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

-- Interface to Vulkan C functions

with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.C is
    procedure Free is
        new Ada.Unchecked_Deallocation(Application_Info_C,
                                       Application_Info_C_Access);
    procedure Free is
        new Ada.Unchecked_Deallocation(Physical_Device_Features_C,
                                       Physical_Device_Features_C_Access); 

    function To_C(Struct: in Application_Info) return Application_Info_C is
        use type Ada.Strings.Unbounded.Unbounded_String;

        AIC: Application_Info_C;
    begin
        AIC.Next := Extension_Records.To_C(Struct.Next);

        if Struct.Application_Name /= "" then
            AIC.Application_Name :=
                Interfaces.C.Strings.New_String
                    (Ada.Strings.Unbounded.To_String(Struct.Application_Name));
        end if;

        if Struct.Engine_Name /= "" then
            AIC.Engine_Name :=
                Interfaces.C.Strings.New_String
                    (Ada.Strings.Unbounded.To_String(Struct.Engine_Name));
        end if;

        AIC.Application_Version := Struct.Application_Version;
        AIC.Engine_Version := Struct.Engine_Version;
        AIC.API_Version := Struct.API_Version;

        return AIC;
    end To_C;

    procedure Free(Struct: in out Application_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Application_Name);
        Interfaces.C.Strings.Free(Struct.Engine_Name);
    end Free;

    function To_C(Struct: in Instance_Create_Info)
        return Instance_Create_Info_C is
        use type Interfaces.Unsigned_32;

        ICIC: Instance_Create_Info_C;
    begin
        ICIC.Next := Extension_Records.To_C(Struct.Next);
        ICIC.Flags := Struct.Flags;
        
        if Struct.Application_Info /= null then
            ICIC.Application_Info :=
                new Application_Info_C'(To_C(Struct.Application_Info.all));
        end if;

        ICIC.Enabled_Layer_Count :=
            Interfaces.Unsigned_32(Struct.Enabled_Layer_Names.Length);
        ICIC.Enabled_Extension_Count :=
            Interfaces.Unsigned_32(Struct.Enabled_Extension_Names.Length);

        return ICIC;
    end To_C;

    procedure Free(Struct: in out Instance_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Free(Struct.Application_Info);
    end Free;
   
    function To_C(Struct: in Physical_Device_Features)
        return Physical_Device_Features_C is
        PDFC: Physical_Device_Features_C;
    begin
        PDFC.Robust_Buffer_Access :=
            Utilities.To_C(Struct.Robust_Buffer_Access);
        PDFC.Full_Draw_Index_Uint32 :=
            Utilities.To_C(Struct.Full_Draw_Index_UInt32);
        PDFC.Image_Cube_Array := Utilities.To_C(Struct.Image_Cube_Array);
        PDFC.Independent_Blend := Utilities.To_C(Struct.Independent_Blend);
        PDFC.Geometry_Shader := Utilities.To_C(Struct.Geometry_Shader);
        PDFC.Tessellation_Shader := Utilities.To_C(Struct.Tessellation_Shader);
        PDFC.Sample_Rate_Shading := Utilities.To_C(Struct.Sample_Rate_Shading);
        PDFC.Dual_Src_Blend := Utilities.To_C(Struct.Dual_Src_Blend);
        PDFC.Logic_Op := Utilities.To_C(Struct.Logic_Op);
        PDFC.Multi_Draw_Indirect := Utilities.To_C(Struct.Multi_Draw_Indirect);
        PDFC.Draw_Indirect_First_Instance :=
            Utilities.To_C(Struct.Draw_Indirect_First_Instance);
        PDFC.Depth_Clamp := Utilities.To_C(Struct.Depth_Clamp);
        PDFC.Depth_Bias_Clamp := Utilities.To_C(Struct.Depth_Bias_Clamp);
        PDFC.Fill_Mode_Non_Solid := Utilities.To_C(Struct.Fill_Mode_Non_Solid);
        PDFC.Depth_Bounds := Utilities.To_C(Struct.Depth_Bounds);
        PDFC.Wide_Lines := Utilities.To_C(Struct.Wide_Lines);
        PDFC.Large_Points := Utilities.To_C(Struct.Large_Points);
        PDFC.Alpha_To_One := Utilities.To_C(Struct.Alpha_To_One);
        PDFC.Multi_Viewport := Utilities.To_C(Struct.Multi_Viewport);
        PDFC.Sampler_Anisotropy := Utilities.To_C(Struct.Sampler_Anisotropy);
        PDFC.Texture_Compression_ETC2 :=
            Utilities.To_C(Struct.Texture_Compression_ETC2);
        PDFC.Texture_Compression_ASTC_LDR :=
            Utilities.To_C(Struct.Texture_Compression_ASTC_LDR);
        PDFC.Texture_Compression_BC :=
            Utilities.To_C(Struct.Texture_Compression_BC);
        PDFC.Occlusion_Query_Precise :=
            Utilities.To_C(Struct.Occlusion_Query_Precise);
        PDFC.Pipeline_Statistics_Query :=
            Utilities.To_C(Struct.Pipeline_Statistics_Query);
        PDFC.Vertex_Pipeline_Stores_And_Atomics :=
            Utilities.To_C(Struct.Vertex_Pipeline_Stores_And_Atomics);
        PDFC.Fragment_Stores_And_Atomics :=
            Utilities.To_C(Struct.Fragment_Stores_And_Atomics);
        PDFC.Shader_Tessellation_And_Geometry_Point_Size :=
            Utilities.To_C(Struct.Shader_Tessellation_And_Geometry_Point_Size);
        PDFC.Shader_Image_Gather_Extended :=
            Utilities.To_C(Struct.Shader_Image_Gather_Extended);
        PDFC.Shader_Storage_Image_Extended_Formats :=
            Utilities.To_C(Struct.Shader_Storage_Image_Extended_Formats);
        PDFC.Shader_Storage_Image_Multisample :=
            Utilities.To_C(Struct.Shader_Storage_Image_Multisample);
        PDFC.Shader_Storage_Image_Read_Without_Format :=
            Utilities.To_C(Struct.Shader_Storage_Image_Read_Without_Format);
        PDFC.Shader_Storage_Image_Write_Without_Format :=
            Utilities.To_C(Struct.Shader_Storage_Image_Write_Without_Format);
        PDFC.Shader_Uniform_Buffer_Array_Dynamic_Indexing :=
            Utilities.To_C(Struct.Shader_Uniform_Buffer_Array_Dynamic_Indexing);
        PDFC.Shader_Sampled_Image_Array_Dynamic_Indexing :=
            Utilities.To_C(Struct.Shader_Sampled_Image_Array_Dynamic_Indexing);
        PDFC.Shader_Storage_Buffer_Array_Dynamic_Indexing :=
            Utilities.To_C(Struct.Shader_Storage_Buffer_Array_Dynamic_Indexing);
        PDFC.Shader_Storage_Image_Array_Dynamic_Indexing :=
            Utilities.To_C(Struct.Shader_Storage_Image_Array_Dynamic_Indexing);
        PDFC.Shader_Clip_Distance :=
            Utilities.To_C(Struct.Shader_Clip_Distance);
        PDFC.Shader_Cull_Distance :=
            Utilities.To_C(Struct.Shader_Cull_Distance);
        PDFC.Shader_Float64 := Utilities.To_C(Struct.Shader_Float64);
        PDFC.Shader_Int64 := Utilities.To_C(Struct.Shader_Int64);
        PDFC.Shader_Int16 := Utilities.To_C(Struct.Shader_Int16);
        PDFC.Shader_Resource_Residency :=
            Utilities.To_C(Struct.Shader_Resource_Residency);
        PDFC.Shader_Resource_Min_Lod :=
            Utilities.To_C(Struct.Shader_Resource_Min_Lod);
        PDFC.Sparse_Binding := Utilities.To_C(Struct.Sparse_Binding);
        PDFC.Sparse_Residency_Buffer :=
            Utilities.To_C(Struct.Sparse_Residency_Buffer);
        PDFC.Sparse_Residency_Image_2D :=
            Utilities.To_C(Struct.Sparse_Residency_Image_2D);
        PDFC.Sparse_Residency_Image_3D :=
            Utilities.To_C(Struct.Sparse_Residency_Image_3D);
        PDFC.Sparse_Residency_2_Samples :=
            Utilities.To_C(Struct.Sparse_Residency_2_Samples);
        PDFC.Sparse_Residency_4_Samples :=
            Utilities.To_C(Struct.Sparse_Residency_4_Samples);
        PDFC.Sparse_Residency_8_Samples :=
            Utilities.To_C(Struct.Sparse_Residency_8_Samples);
        PDFC.Sparse_Residency_16_Samples :=
            Utilities.To_C(Struct.Sparse_Residency_16_Samples);
        PDFC.Sparse_Residency_Aliased :=
            Utilities.To_C(Struct.Sparse_Residency_Aliased);
        PDFC.Variable_Multisample_Rate :=
            Utilities.To_C(Struct.Variable_Multisample_Rate);
        PDFC.Inherited_Queries := Utilities.To_C(Struct.Inherited_Queries);

        return PDFC;
    end To_C;
   
    function To_C(Struct: in Physical_Device_Limits)
        return Physical_Device_Limits_C is
        PDLC: Physical_Device_Limits_C;
    begin
        PDLC.Max_Image_Dimension_1D := Struct.Max_Image_Dimension_1D;
        PDLC.Max_Image_Dimension_2D := Struct.Max_Image_Dimension_2D;
        PDLC.Max_Image_Dimension_3D := Struct.Max_Image_Dimension_3D;
        PDLC.Max_Image_Dimension_Cube := Struct.Max_Image_Dimension_Cube;
        PDLC.Max_Image_Array_Layers := Struct.Max_Image_Array_Layers;
        PDLC.Max_Texel_Buffer_Elements := Struct.Max_Texel_Buffer_Elements;
        PDLC.Max_Uniform_Buffer_Range := Struct.Max_Uniform_Buffer_Range;
        PDLC.Max_Storage_Buffer_Range := Struct.Max_Storage_Buffer_Range;
        PDLC.Max_Push_Constants_Size := Struct.Max_Push_Constants_Size;
        PDLC.Max_Memory_Allocation_Count := Struct.Max_Memory_Allocation_Count;
        PDLC.Max_Sampler_Allocation_Count :=
            Struct.Max_Sampler_Allocation_Count;
        PDLC.Buffer_Image_Granularity := Struct.Buffer_Image_Granularity;
        PDLC.Sparse_Address_Space_Size := Struct.Sparse_Address_Space_Size;
        PDLC.Max_Bound_Descriptor_Sets := Struct.Max_Bound_Descriptor_Sets;
        PDLC.Max_Per_Stage_Descriptor_Samplers :=
            Struct.Max_Per_Stage_Descriptor_Samplers;
        PDLC.Max_Per_Stage_Descriptor_Uniform_Buffers :=
            Struct.Max_Per_Stage_Descriptor_Uniform_Buffers;
        PDLC.Max_Per_Stage_Descriptor_Storage_Buffers :=
            Struct.Max_Per_Stage_Descriptor_Storage_Buffers;
        PDLC.Max_Per_Stage_Descriptor_Sampled_Images :=
            Struct.Max_Per_Stage_Descriptor_Sampled_Images;
        PDLC.Max_Per_Stage_Descriptor_Storage_Images :=
            Struct.Max_Per_Stage_Descriptor_Storage_Images;
        PDLC.Max_Per_Stage_Descriptor_Input_Attachments :=
            Struct.Max_Per_Stage_Descriptor_Input_Attachments;
        PDLC.Max_Per_Stage_Resources := Struct.Max_Per_Stage_Resources;
        PDLC.Max_Descriptor_Set_Samplers := Struct.Max_Descriptor_Set_Samplers;
        PDLC.Max_Descriptor_Set_Uniform_Buffers :=
            Struct.Max_Descriptor_Set_Uniform_Buffers;
        PDLC.Max_Descriptor_Set_Uniform_Buffers_Dynamic :=
            Struct.Max_Descriptor_Set_Uniform_Buffers_Dynamic;
        PDLC.Max_Descriptor_Set_Storage_Buffers :=
            Struct.Max_Descriptor_Set_Storage_Buffers;
        PDLC.Max_Descriptor_Set_Storage_Buffers_Dynamic :=
            Struct.Max_Descriptor_Set_Storage_Buffers_Dynamic;
        PDLC.Max_Descriptor_Set_Sampled_Images :=
            Struct.Max_Descriptor_Set_Sampled_Images;
        PDLC.Max_Descriptor_Set_Storage_Images :=
            Struct.Max_Descriptor_Set_Storage_Images;
        PDLC.Max_Descriptor_Set_Input_Attachments :=
            Struct.Max_Descriptor_Set_Input_Attachments;
        PDLC.Max_Vertex_Input_Attributes := Struct.Max_Vertex_Input_Attributes;
        PDLC.Max_Vertex_Input_Bindings := Struct.Max_Vertex_Input_Bindings;
        PDLC.Max_Vertex_Input_Attribute_Offset :=
            Struct.Max_Vertex_Input_Attribute_Offset;
        PDLC.Max_Vertex_Input_Binding_Stride :=
            Struct.Max_Vertex_Input_Binding_Stride;
        PDLC.Max_Vertex_Output_Components :=
            Struct.Max_Vertex_Output_Components;
        PDLC.Max_Tessellation_Generation_Level :=
            Struct.Max_Tessellation_Generation_Level;
        PDLC.Max_Tessellation_Patch_Size := Struct.Max_Tessellation_Patch_Size;
        PDLC.Max_Tessellation_Control_Per_Vertex_Input_Components :=
            Struct.Max_Tessellation_Control_Per_Vertex_Input_Components;
        PDLC.Max_Tessellation_Control_Per_Vertex_Output_Components :=
            Struct.Max_Tessellation_Control_Per_Vertex_Output_Components;
        PDLC.Max_Tessellation_Control_Per_Patch_Output_Components :=
            Struct.Max_Tessellation_Control_Per_Patch_Output_Components;
        PDLC.Max_Tessellation_Control_Total_Output_Components :=
            Struct.Max_Tessellation_Control_Total_Output_Components;
        PDLC.Max_Tessellation_Evaluation_Input_Components :=
            Struct.Max_Tessellation_Evaluation_Input_Components;
        PDLC.Max_Tessellation_Evaluation_Output_Components :=
            Struct.Max_Tessellation_Evaluation_Output_Components;
        PDLC.Max_Geometry_Shader_Invocations :=
            Struct.Max_Geometry_Shader_Invocations;
        PDLC.Max_Geometry_Input_Components :=
            Struct.Max_Geometry_Input_Components;
        PDLC.Max_Geometry_Output_Components :=
            Struct.Max_Geometry_Output_Components;
        PDLC.Max_Geometry_Output_Vertices :=
            Struct.Max_Geometry_Output_Vertices;
        PDLC.Max_Geometry_Total_Output_Components :=
            Struct.Max_Geometry_Total_Output_Components;
        PDLC.Max_Fragment_Input_Components :=
            Struct.Max_Fragment_Input_Components;
        PDLC.Max_Fragment_Output_Attachments :=
            Struct.Max_Fragment_Output_Attachments;
        PDLC.Max_Fragment_Dual_Src_Attachments :=
            Struct.Max_Fragment_Dual_Src_Attachments;
        PDLC.Max_Fragment_Combined_Output_Resources :=
            Struct.Max_Fragment_Combined_Output_Resources;
        PDLC.Max_Compute_Shared_Memory_Size :=
            Struct.Max_Compute_Shared_Memory_Size;
        PDLC.Max_Compute_Work_Group_Count :=
            Struct.Max_Compute_Work_Group_Count;
        PDLC.Max_Compute_Work_Group_Invocations :=
            Struct.Max_Compute_Work_Group_Invocations;
        PDLC.Max_Compute_Work_Group_Size := Struct.Max_Compute_Work_Group_Size;
        PDLC.Sub_Pixel_Precision_Bits := Struct.Sub_Pixel_Precision_Bits;
        PDLC.Sub_Texel_Precision_Bits := Struct.Sub_Texel_Precision_Bits;
        PDLC.Mipmap_Precision_Bits := Struct.Mipmap_Precision_Bits;
        PDLC.Max_Draw_Indexed_Index_Value :=
            Struct.Max_Draw_Indexed_Index_Value;
        PDLC.Max_Draw_Indirect_Count := Struct.Max_Draw_Indirect_Count;
        PDLC.Max_Sampler_Lod_Bias :=
            Interfaces.C.C_float(Struct.Max_Sampler_Lod_Bias);
        PDLC.Max_Sampler_Anisotropy :=
            Interfaces.C.C_float(Struct.Max_Sampler_Anisotropy);
        PDLC.Max_Viewports := Struct.Max_Viewports;
        PDLC.Max_Viewport_Dimensions := Struct.Max_Viewport_Dimensions;
        PDLC.Viewport_Bounds_Range :=
            (Interfaces.C.C_float(Struct.Viewport_Bounds_Range(1)),
             Interfaces.C.C_float(Struct.Viewport_Bounds_Range(2)));
        PDLC.Viewport_Sub_Pixel_Bits := Struct.Viewport_Sub_Pixel_Bits;
        PDLC.Min_Memory_Map_Alignment := Struct.Min_Memory_Map_Alignment;
        PDLC.Min_Texel_Buffer_Offset_Alignment :=
            Struct.Min_Texel_Buffer_Offset_Alignment;
        PDLC.Min_Uniform_Buffer_Offset_Alignment :=
            Struct.Min_Uniform_Buffer_Offset_Alignment;
        PDLC.Min_Storage_Buffer_Offset_Alignment :=
            Struct.Min_Storage_Buffer_Offset_Alignment;
        PDLC.Min_Texel_Offset := Struct.Min_Texel_Offset;
        PDLC.Max_Texel_Offset := Struct.Max_Texel_Offset;
        PDLC.Min_Texel_Gather_Offset := Struct.Min_Texel_Gather_Offset;
        PDLC.Max_Texel_Gather_Offset := Struct.Max_Texel_Gather_Offset;
        PDLC.Min_Interpolation_Offset :=
            Interfaces.C.C_float(Struct.Min_Interpolation_Offset);
        PDLC.Max_Interpolation_Offset :=
            Interfaces.C.C_float(Struct.Max_Interpolation_Offset);
        PDLC.Sub_Pixel_Interpolation_Offset_Bits :=
            Struct.Sub_Pixel_Interpolation_Offset_Bits;
        PDLC.Max_Framebuffer_Width := Struct.Max_Framebuffer_Width;
        PDLC.Max_Framebuffer_Height := Struct.Max_Framebuffer_Height;
        PDLC.Max_Framebuffer_Layers := Struct.Max_Framebuffer_Layers;
        PDLC.Framebuffer_Color_Sample_Counts :=
            Struct.Framebuffer_Color_Sample_Counts;
        PDLC.Framebuffer_Depth_Sample_Counts :=
            Struct.Framebuffer_Depth_Sample_Counts;
        PDLC.Framebuffer_Stencil_Sample_Counts :=
            Struct.Framebuffer_Stencil_Sample_Counts;
        PDLC.Framebuffer_No_Attachments_Sample_Counts :=
            Struct.Framebuffer_No_Attachments_Sample_Counts;
        PDLC.Max_Color_Attachments := Struct.Max_Color_Attachments;
        PDLC.Sampled_Image_Color_Sample_Counts :=
            Struct.Sampled_Image_Color_Sample_Counts;
        PDLC.Sampled_Image_Integer_Sample_Counts :=
            Struct.Sampled_Image_Integer_Sample_Counts;
        PDLC.Sampled_Image_Depth_Sample_Counts :=
            Struct.Sampled_Image_Depth_Sample_Counts;
        PDLC.Sampled_Image_Stencil_Sample_Counts :=
            Struct.Sampled_Image_Stencil_Sample_Counts;
        PDLC.Storage_Image_Sample_Counts := Struct.Storage_Image_Sample_Counts;
        PDLC.Max_Sample_Mask_Words := Struct.Max_Sample_Mask_Words;
        PDLC.Timestamp_Compute_And_Graphics :=
            Utilities.To_C(Struct.Timestamp_Compute_And_Graphics);
        PDLC.Timestamp_Period := Interfaces.C.C_float(Struct.Timestamp_Period);
        PDLC.Max_Clip_Distances := Struct.Max_Clip_Distances;
        PDLC.Max_Cull_Distances := Struct.Max_Cull_Distances;
        PDLC.Max_Combined_Clip_And_Cull_Distances :=
            Struct.Max_Combined_Clip_And_Cull_Distances;
        PDLC.Discrete_Queue_Priorities := Struct.Discrete_Queue_Priorities;
        PDLC.Point_Size_Range :=
            (Interfaces.C.C_float(Struct.Point_Size_Range(1)),
             Interfaces.C.C_float(Struct.Point_Size_Range(2)));
        PDLC.Line_Width_Range :=
            (Interfaces.C.C_float(Struct.Line_Width_Range(1)),
             Interfaces.C.C_float(Struct.Line_Width_Range(2)));
        PDLC.Line_Width_Granularity :=
            Interfaces.C.C_float(Struct.Point_Size_Granularity);
        PDLC.Line_Width_Granularity :=
            Interfaces.C.C_float(Struct.Line_Width_Granularity);
        PDLC.Strict_Lines := Utilities.To_C(Struct.Strict_Lines);
        PDLC.Standard_Sample_Locations :=
            Utilities.To_C(Struct.Standard_Sample_Locations);
        PDLC.Optimal_Buffer_Copy_Offset_Alignment :=
            Struct.Optimal_Buffer_Copy_Offset_Alignment;
        PDLC.Optimal_Buffer_Copy_Row_Pitch_Alignment :=
            Struct.Optimal_Buffer_Copy_Row_Pitch_Alignment;
        PDLC.Non_Coherent_Atom_Size := Struct.Non_Coherent_Atom_Size;

        return PDLC;
    end To_C;

    function To_C(Struct: in Physical_Device_Sparse_Properties)
        return Physical_Device_Sparse_Properties_C is
        PDSPC: Physical_Device_Sparse_Properties_C;
    begin
        PDSPC.Residency_Standard_2D_Block_Shape :=
            Utilities.To_C(Struct.Residency_Standard_2D_Block_Shape);
        PDSPC.Residency_Standard_2D_Multisample_Block_Shape :=
            Utilities.To_C
                (Struct.Residency_Standard_2D_Multisample_Block_Shape);
        PDSPC.Residency_Standard_3D_Block_Shape :=
            Utilities.To_C(Struct.Residency_Standard_3D_Block_Shape);
        PDSPC.Residency_Aligned_Mip_Size :=
            Utilities.To_C(Struct.Residency_Aligned_Mip_Size);
        PDSPC.Residency_Non_Resident_Strict :=
            Utilities.To_C(Struct.Residency_Non_Resident_Strict);

        return PDSPC;
    end To_C;

    function To_C(Struct: in Physical_Device_Properties)
        return Physical_Device_Properties_C is
        use type Interfaces.C.size_t;

        PDPC: Physical_Device_Properties_C;
    begin
        PDPC.API_Version := Struct.API_Version;
        PDPC.Driver_Version := Struct.Driver_Version;
        PDPC.Vendor_ID := Struct.Vendor_ID;
        PDPC.Device_ID := Struct.Device_ID;
        PDPC.Device_Type := Struct.Device_Type;
        PDPC.Device_Name := (others => Interfaces.C.nul);

        for X in PDPC.Device_Name'First .. PDPC.Device_Name'Last - 1 loop
            exit when Natural(X) =
                      Ada.Strings.Unbounded.Length(Struct.Device_Name);

            PDPC.Device_Name(X) :=
                Interfaces.C.char
                    (Ada.Strings.Unbounded.Element(Struct.Device_Name,
                                                   Integer(X)));
        end loop;

        PDPC.Pipeline_Cache_UUID := Struct.Pipeline_Cache_UUID;
        PDPC.Limits := To_C(Struct.Limits);
        PDPC.Sparse_Properties := To_C(Struct.Sparse_Properties);

        return PDPC;
    end To_C;

    function To_C(Struct: in Device_Queue_Create_Info)
        return Device_Queue_Create_Info_C is
        DQCIC: Device_Queue_Create_Info_C;
        Priorities_Array: Float_Arrays.Array_Access;
    begin
        DQCIC.Next := Extension_Records.To_C(Struct.Next);
        DQCIC.Flags := Struct.Flags;
        DQCIC.Queue_Family_Index := Struct.Queue_Family_Index;
        DQCIC.Count := Interfaces.Unsigned_32(Struct.Priorities.Length);
        Priorities_Array :=
            Float_Arrays.Allocate(Positive(Struct.Priorities.Length));

        for X in Priorities_Array'Range loop
            Priorities_Array(X) :=
                Interfaces.C.C_Float(Struct.Priorities.Element(X));
        end loop;

        DQCIC.Priorities := Priorities_Array(1)'Access;

        return DQCIC;
    end To_C;

    procedure Free(Struct: in out Device_Queue_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Float_Arrays.Free(Struct.Priorities);
    end Free;

    function To_C(Struct: in Device_Create_Info) return Device_Create_Info_C is
        use type Interfaces.Unsigned_32;

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Device_Queue_Create_Info_C_Arrays,
             Device_Queue_Create_Info_Vectors);

        DCIC: Device_Create_Info_C;
    begin
        DCIC.Next := Extension_Records.To_C(Struct.Next);
        DCIC.Flags := Struct.Flags;
        To_C_Array(DCIC.Queue_Create_Info_Count,
                   Struct.Queue_Create_Infos,
                   DCIC.Queue_Create_Infos);

        DCIC.Enabled_Layer_Count :=
            Interfaces.Unsigned_32(Struct.Enabled_Layer_Names.Length);

        DCIC.Enabled_Extension_Count :=
            Interfaces.Unsigned_32(Struct.Enabled_Extension_Names.Length);

        if Struct.Enabled_Features /= null then
            DCIC.Enabled_Features :=
                new Physical_Device_Features_C'
                    (To_C(Struct.Enabled_Features.all));
        end if;

        return DCIC;
    end To_C;

    procedure Free(Struct: in out Device_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Device_Queue_Create_Info_C_Arrays.Free(Struct.Queue_Create_Infos,
                                               Free'Access);
        Free(Struct.Enabled_Features);
    end Free;

    function To_C(Struct: in Submit_Info) return Submit_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Command_Buffer_Arrays, Command_Buffer_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array(Semaphore_Arrays,
                                                         Semaphore_Vectors);

        SIC: Submit_Info_C;
        Wait_Semaphore_Array: Semaphore_Arrays.Array_Access;
        Wait_Stage_Mask_Array: Pipeline_Stage_Flags_Arrays.Array_Access;
    begin
        SIC.Next := Extension_Records.To_C(Struct.Next);
        SIC.Wait_Semaphore_Count :=
            Interfaces.Unsigned_32(Struct.Wait_Semaphores.Length);

        if not Struct.Wait_Semaphores.Is_Empty then
            Wait_Semaphore_Array :=
                Semaphore_Arrays.Allocate(Positive(SIC.Wait_Semaphore_Count));
            SIC.Wait_Semaphores := Wait_Semaphore_Array(1)'Access;

            Wait_Stage_Mask_Array :=
                Pipeline_Stage_Flags_Arrays.Allocate
                    (Positive(SIC.Wait_Semaphore_Count));
            SIC.Wait_Dst_Stage_Mask := Wait_Stage_Mask_Array(1)'Access;

            for X in Wait_Semaphore_Array'Range loop
                Wait_Semaphore_Array(X) := Struct.Wait_Semaphores(X);
                Wait_Stage_Mask_Array(X) := Struct.Wait_Dst_Stage_Mask(X);
            end loop;
        end if;

        To_C_Array(SIC.Command_Buffer_Count,
                   Struct.Command_Buffers,
                   SIC.Command_Buffers);
        To_C_Array(SIC.Signal_Semaphore_Count,
                   Struct.Signal_Semaphores,
                   SIC.Signal_Semaphores);

        return SIC;
    end To_C;

    procedure Free(Struct: in out Submit_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Semaphore_Arrays.Free(Struct.Wait_Semaphores);
        Pipeline_Stage_Flags_Arrays.Free(Struct.Wait_Dst_Stage_Mask);
        Command_Buffer_Arrays.Free(Struct.Command_Buffers);
        Semaphore_Arrays.Free(Struct.Signal_Semaphores);
    end Free;

    function To_C(Struct: in Memory_Allocate_Info)
        return Memory_Allocate_Info_C is
        MAIC: Memory_Allocate_Info_C;
    begin
        MAIC.Next := Extension_Records.To_C(Struct.Next);
        MAIC.Allocation_Size := Struct.Allocation_Size;
        MAIC.Memory_Type_Index := Struct.Memory_Type_Index;

        return MAIC;
    end To_C;

    procedure Free(Struct: in out Memory_Allocate_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Mapped_Memory_Range)
        return Mapped_Memory_Range_C is
        MMRC: Mapped_Memory_Range_C;
    begin
        MMRC.Next := Extension_Records.To_C(Struct.Next);
        MMRC.Memory := Struct.Memory;
        MMRC.Offset := Struct.Offset;
        MMRC.Size := Struct.Size;

        return MMRC;
    end To_C;

    procedure Free(Struct: in out Mapped_Memory_Range_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Sparse_Buffer_Memory_Bind_Info)
        return Sparse_Buffer_Memory_Bind_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Sparse_Memory_Bind_Arrays, Sparse_Memory_Bind_Vectors);

        SBMBIC: Sparse_Buffer_Memory_Bind_Info_C;
    begin
        SBMBIC.Buffer := Struct.Buffer;
        To_C_Array(SBMBIC.Bind_Count, Struct.Binds, SBMBIC.Binds);

        return SBMBIC;
    end To_C;

    procedure Free(Struct: in out Sparse_Buffer_Memory_Bind_Info_C) is
    begin
        Sparse_Memory_Bind_Arrays.Free(Struct.Binds);
    end Free;

    function To_C(Struct: in Sparse_Image_Opaque_Memory_Bind_Info)
        return Sparse_Image_Opaque_Memory_Bind_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Sparse_Memory_Bind_Arrays, Sparse_Memory_Bind_Vectors);

        SIOMBIC: Sparse_Image_Opaque_Memory_Bind_Info_C;
    begin
        SIOMBIC.Image := Struct.Image;
        To_C_Array(SIOMBIC.Bind_Count, Struct.Binds, SIOMBIC.Binds);

        return SIOMBIC;
    end To_C;

    procedure Free(Struct: in out Sparse_Image_Opaque_Memory_Bind_Info_C) is
    begin
        Sparse_Memory_Bind_Arrays.Free(Struct.Binds);
    end Free;

    function To_C(Struct: in Sparse_Image_Memory_Bind_Info)
        return Sparse_Image_Memory_Bind_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Sparse_Image_Memory_Bind_Arrays, Sparse_Image_Memory_Bind_Vectors);

        SIMBIC: Sparse_Image_Memory_Bind_Info_C;
    begin
        SIMBIC.Image := Struct.Image;
        To_C_Array(SIMBIC.Bind_Count, Struct.Binds, SIMBIC.Binds);

        return SIMBIC;
    end To_C;

    procedure Free(Struct: in out Sparse_Image_Memory_Bind_Info_C) is
    begin
        Sparse_Image_Memory_Bind_Arrays.Free(Struct.Binds);
    end Free;

    function To_C(Struct: in Bind_Sparse_Info) return Bind_Sparse_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(Semaphore_Arrays,
                                                         Semaphore_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Sparse_Buffer_Memory_Bind_Info_Arrays,
             Sparse_Buffer_Memory_Bind_Info_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Sparse_Image_Opaque_Memory_Bind_Info_Arrays,
             Sparse_Image_Opaque_Memory_Bind_Info_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Sparse_Image_Memory_Bind_Info_Arrays,
             Sparse_Image_Memory_Bind_Info_Vectors);

        BSIC: Bind_Sparse_Info_C;
    begin
        BSIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(BSIC.Wait_Semaphore_Count,
                   Struct.Wait_Semaphores,
                   BSIC.Wait_Semaphores);
        To_C_Array(BSIC.Buffer_Bind_Count,
                   Struct.Buffer_Binds,
                   BSIC.Buffer_Binds);
        To_C_Array(BSIC.Image_Opaque_Bind_Count,
                   Struct.Image_Opaque_Binds,
                   BSIC.Image_Opaque_Binds);
        To_C_Array(BSIC.Image_Bind_Count,
                   Struct.Image_Binds,
                   BSIC.Image_Binds);
        To_C_Array(BSIC.Signal_Semaphore_Count,
                   Struct.Signal_Semaphores,
                   BSIC.Signal_Semaphores);

        return BSIC;
    end To_C;

    procedure Free(Struct: in out Bind_Sparse_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Semaphore_Arrays.Free(Struct.Wait_Semaphores);
        Sparse_Buffer_Memory_Bind_Info_Arrays.Free(Struct.Buffer_Binds,
                                                   Free'Access);
        Sparse_Image_Opaque_Memory_Bind_Info_Arrays.Free
            (Struct.Image_Opaque_Binds, Free'Access);
        Sparse_Image_Memory_Bind_Info_Arrays.Free(Struct.Image_Binds,
                                                  Free'Access);
        Semaphore_Arrays.Free(Struct.Signal_Semaphores);
    end Free;

    function To_C(Struct: in Fence_Create_Info) return Fence_Create_Info_C is
        FCIC: Fence_Create_Info_C;
    begin
        FCIC.Next := Extension_Records.To_C(Struct.Next);
        FCIC.Flags := Struct.Flags;

        return FCIC;
    end To_C;

    procedure Free(Struct: in out Fence_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Semaphore_Create_Info)
        return Semaphore_Create_Info_C is
        SCIC: Semaphore_Create_Info_C;
    begin
        SCIC.Next := Extension_Records.To_C(Struct.Next);
        SCIC.Flags := Struct.Flags;

        return SCIC;
    end To_C;

    procedure Free(Struct: in out Semaphore_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: Event_Create_Info) return Event_Create_Info_C is
        ECIC: Event_Create_Info_C;
    begin
        ECIC.Next := Extension_Records.To_C(Struct.Next);
        ECIC.Flags := Struct.Flags;
        
        return ECIC;
    end To_C;

    procedure Free(Struct: in out Event_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Query_Pool_Create_Info)
        return Query_Pool_Create_Info_C is
        QPCIC: Query_Pool_Create_Info_C;
    begin
        QPCIC.Next := Extension_Records.To_C(Struct.Next);
        QPCIC.Flags := Struct.Flags;
        QPCIC.Query_Type := Struct.Query_Type;
        QPCIC.Query_Count := Struct.Query_Count;
        QPCIC.Pipeline_Statistics := Struct.Pipeline_Statistics;

        return QPCIC;
    end To_C;

    procedure Free(Struct: in out Query_Pool_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Buffer_Create_Info) return Buffer_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Queue_Family_Index_Arrays, Queue_Family_Index_Vectors);

        BCIC: Buffer_Create_Info_C;
    begin
        BCIC.Next := Extension_Records.To_C(Struct.Next);
        BCIC.Flags := Struct.Flags;
        BCIC.Size := Struct.Size;
        BCIC.Usage := Struct.Usage;
        BCIC.Sharing_Mode := Struct.Sharing_Mode;
        To_C_Array(BCIC.Queue_Family_Index_Count,
                   Struct.Queue_Family_Indices,
                   BCIC.Queue_Family_Indices);

        return BCIC;
    end To_C;

    procedure Free(Struct: in out Buffer_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Queue_Family_Index_Arrays.Free(Struct.Queue_Family_Indices);
    end Free;

    function To_C(Struct: in Buffer_View_Create_Info)
        return Buffer_View_Create_Info_C is
        BVCIC: Buffer_View_Create_Info_C;
    begin
        BVCIC.Next := Extension_Records.To_C(Struct.Next);
        BVCIC.Flags := Struct.Flags;
        BVCIC.Buffer := Struct.Buffer;
        BVCIC.Format := Struct.Format;
        BVCIC.Offset := Struct.Offset;
        BVCIC.View_Range := Struct.View_Range;

        return BVCIC;
    end To_C;

    procedure Free(Struct: in out Buffer_View_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;
 
    function To_C(Struct: in Image_Create_Info) return Image_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Queue_Family_Index_Arrays, Queue_Family_Index_Vectors);

        ICIC: Image_Create_Info_C;
    begin
        ICIC.Next := Extension_Records.To_C(Struct.Next);
        ICIC.Flags := Struct.Flags;
        ICIC.Image_Type := Struct.Image_Type;
        ICIC.Format := Struct.Format;
        ICIC.Extent := Struct.Extent;
        ICIC.Mip_Levels := Interfaces.Unsigned_32(Struct.Mip_Levels);
        ICIC.Array_Layers := Interfaces.Unsigned_32(Struct.Array_Layers);
        ICIC.Samples := Struct.Samples;
        ICIC.Tiling := Struct.Tiling;
        ICIC.Usage := Struct.Usage;
        ICIC.Sharing_Mode := Struct.Sharing_Mode;
        To_C_Array(ICIC.Queue_Family_Index_Count,
                   Struct.Queue_Family_Indices,
                   ICIC.Queue_Family_Indices);
        ICIC.Initial_Layout := Struct.Initial_Layout;

        return ICIC;
    end To_C;

    procedure Free(Struct: in out Image_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Queue_Family_Index_Arrays.Free(Struct.Queue_Family_Indices);
    end Free;

    function To_C(Struct: in Image_View_Create_Info)
        return Image_View_Create_Info_C is
        IVCIC: Image_View_Create_Info_C;
    begin
        IVCIC.Next := Extension_Records.To_C(Struct.Next);
        IVCIC.Flags := Struct.Flags;
        IVCIC.Image := Struct.Image;
        IVCIC.View_Type := Struct.View_Type;
        IVCIC.Format := Struct.Format;
        IVCIC.Components := Struct.Components;
        IVCIC.Subresource_Range := Struct.Subresource_Range;
        
        return IVCIC;
    end To_C;

    procedure Free(Struct: in out Image_View_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Shader_Module_Create_Info)
        return Shader_Module_Create_Info_C is
        use type Ada.Containers.Count_Type;
        
        SMCIC: Shader_Module_Create_Info_C;
    begin
        SMCIC.Next := Extension_Records.To_C(Struct.Next);
        SMCIC.Flags := Struct.Flags;
        SMCIC.Code_Size := Interfaces.C.size_t(Struct.Code.Length * 4);
        
        declare
            Array_Access: Uint32_t_Arrays.Array_Access;
        begin
            Array_Access :=
                Uint32_t_Arrays.Allocate(Positive(Struct.Code.Length));
            SMCIC.Code := Array_Access(1)'Access;

            for X in Array_Access'Range loop
                Array_Access(X) := Struct.Code(X);
            end loop;
        end;

        return SMCIC;
    end To_C;

    procedure Free(Struct: in out Shader_Module_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Uint32_t_Arrays.Free(Struct.Code);
    end Free;

    function To_C(Struct: in Pipeline_Cache_Create_Info)
        return Pipeline_Cache_Create_Info_C is
        PCCIC: Pipeline_Cache_Create_Info_C;
    begin
        PCCIC.Next := Extension_Records.To_C(Struct.Next);
        PCCIC.Flags := Struct.Flags;
        PCCIC.Initial_Data_Size := Struct.Initial_Data_Size;
        PCCIC.Initial_Data := Struct.Initial_Data;

        return PCCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Cache_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Specialization_Info)
        return Specialization_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Specialization_Map_Entry_Arrays, Specialization_Map_Entry_Vectors);

        SIC: Specialization_Info_C;
    begin
        To_C_Array(SIC.Map_Entry_Count,
                   Struct.Map_Entries,
                   SIC.Map_Entries);
        SIC.Data_Size := Struct.Data_Size;
        SIC.Data := Struct.Data;

        return SIC;
    end To_C;

    procedure Free(Struct: in out Specialization_Info_C) is
    begin
        Specialization_Map_Entry_Arrays.Free(Struct.Map_Entries);
    end Free;

    function To_C(Struct: in Pipeline_Shader_Stage_Create_Info)
        return Pipeline_Shader_Stage_Create_Info_C is
        PSSCIC: Pipeline_Shader_Stage_Create_Info_C;
    begin
        PSSCIC.Next := Extension_Records.To_C(Struct.Next);
        PSSCIC.Flags := Struct.Flags;
        PSSCIC.Stage := Struct.Stage;
        PSSCIC.Module := Struct.Module;
        PSSCIC.Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Name));
        
        if Struct.Specialization_Info /= null then
            PSSCIC.Specialization_Info := new Specialization_Info_C'
                (To_C(Struct.Specialization_Info.all));
        end if;

        return PSSCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Shader_Stage_Create_Info_C) is
        procedure Free is
            new Ada.Unchecked_Deallocation(Specialization_Info_C,
                                           Specialization_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Name);
        Free(Struct.Specialization_Info);
    end Free;

    function To_C(Struct: in Pipeline_Vertex_Input_State_Create_Info)
        return Pipeline_Vertex_Input_State_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Vertex_Input_Binding_Description_Arrays,
             Vertex_Input_Binding_Description_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (Vertex_Input_Attribute_Description_Arrays,
             Vertex_Input_Attribute_Description_Vectors);

        PVISCIC: Pipeline_Vertex_Input_State_Create_Info_C;
    begin
        PVISCIC.Next := Extension_Records.To_C(Struct.Next);
        PVISCIC.Flags := Struct.Flags;
        To_C_Array(PVISCIC.Vertex_Binding_Description_Count,
                   Struct.Vertex_Binding_Descriptions,
                   PVISCIC.Vertex_Binding_Descriptions);
        To_C_Array(PVISCIC.Vertex_Attribute_Description_Count,
                   Struct.Vertex_Attribute_Descriptions,
                   PVISCIC.Vertex_Attribute_Descriptions);

        return PVISCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Vertex_Input_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Vertex_Input_Binding_Description_Arrays.Free
            (Struct.Vertex_Binding_Descriptions);
        Vertex_Input_Attribute_Description_Arrays.Free
            (Struct.Vertex_Attribute_Descriptions);
    end Free;

    function To_C(Struct: in Pipeline_Input_Assembly_State_Create_Info)
        return Pipeline_Input_Assembly_State_Create_Info_C is
        PIASCIC: Pipeline_Input_Assembly_State_Create_Info_C;
    begin
        PIASCIC.Next := Extension_Records.To_C(Struct.Next);
        PIASCIC.Flags := Struct.Flags;
        PIASCIC.Topology := Struct.Topology;
        PIASCIC.Primitive_Restart_Enable :=
            Utilities.To_C(Struct.Primitive_Restart_Enable);

        return PIASCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Input_Assembly_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Pipeline_Tessellation_State_Create_Info)
        return Pipeline_Tessellation_State_Create_Info_C is
        PTSCIC: Pipeline_Tessellation_State_Create_Info_C;
    begin
        PTSCIC.Next := Extension_Records.To_C(Struct.Next);
        PTSCIC.Flags := Struct.Flags;
        PTSCIC.Patch_Control_Points := Struct.Patch_Control_Points;

        return PTSCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Tessellation_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Pipeline_Viewport_State_Create_Info)
        return Pipeline_Viewport_State_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(Viewport_Arrays,
                                                         Viewport_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array(Rect_2D_Arrays,
                                                         Rect_2D_Vectors);

        PVSCIC: Pipeline_Viewport_State_Create_Info_C;
    begin
        PVSCIC.Next := Extension_Records.To_C(Struct.Next);
        PVSCIC.Flags := Struct.Flags;
        To_C_Array(PVSCIC.Viewport_Count,
                   Struct.Viewports,
                   PVSCIC.Viewports);
        To_C_Array(PVSCIC.Scissor_Count,
                   Struct.Scissors,
                   PVSCIC.Scissors);

        return PVSCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Viewport_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Viewport_Arrays.Free(Struct.Viewports);
        Rect_2D_Arrays.Free(Struct.Scissors);
    end Free;

    function To_C(Struct: in Pipeline_Rasterization_State_Create_Info)
        return Pipeline_Rasterization_State_Create_Info_C is
        PRSCIC: Pipeline_Rasterization_State_Create_Info_C;
    begin
        PRSCIC.Next := Extension_Records.To_C(Struct.Next);
        PRSCIC.Flags := Struct.Flags;
        PRSCIC.Depth_Clamp_Enable := Utilities.To_C(Struct.Depth_Clamp_Enable);
        PRSCIC.Rasterizer_Discard_Enable :=
            Utilities.To_C(Struct.Rasterizer_Discard_Enable);
        PRSCIC.Polygon_Mode := Struct.Polygon_Mode;
        PRSCIC.Cull_Mode := Struct.Cull_Mode;
        PRSCIC.Front_Face := Struct.Front_Face;
        PRSCIC.Depth_Bias_Enable := Utilities.To_C(Struct.Depth_Bias_Enable);
        PRSCIC.Depth_Bias_Constant_Factor := Struct.Depth_Bias_Constant_Factor;
        PRSCIC.Depth_Bias_Clamp := Struct.Depth_Bias_Clamp;
        PRSCIC.Depth_Bias_Slope_Factor :=
            Struct.Depth_Bias_Slope_Factor;
        PRSCIC.Line_Width := Struct.Line_Width;

        return PRSCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Rasterization_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Pipeline_Multisample_State_Create_Info)
        return Pipeline_Multisample_State_Create_Info_C is
        PMSCIC: Pipeline_Multisample_State_Create_Info_C;
    begin
        PMSCIC.Next := Extension_Records.To_C(Struct.Next);
        PMSCIC.Flags := Struct.Flags;
        PMSCIC.Rasterization_Samples := Struct.Rasterization_Samples;
        PMSCIC.Sample_Shading_Enable :=
            Utilities.To_C(Struct.Sample_Shading_Enable);
        PMSCIC.Min_Sample_Shading := Struct.Min_Sample_Shading;

        if not Struct.Sample_Masks.Is_Empty then
            declare
                Sample_Mask: Sample_Mask_Arrays.Array_Access;
            begin
                Sample_Mask := Sample_Mask_Arrays.Allocate
                                (Positive(Struct.Sample_Masks.Length));
                PMSCIC.Sample_Mask := Sample_Mask(1)'Access;

                for X in Sample_Mask'Range loop
                    Sample_Mask(X) := Struct.Sample_Masks(X);
                end loop;
            end;
        end if;

        PMSCIC.Alpha_To_Converage_Enable :=
            Utilities.To_C(Struct.Alpha_To_Coverage_Enable);
        PMSCIC.Alpha_To_One_Enable :=
            Utilities.To_C(Struct.Alpha_To_One_Enable);

        return PMSCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Multisample_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Sample_Mask_Arrays.Free(Struct.Sample_Mask);
    end Free;

    function To_C(Struct: in Pipeline_Depth_Stencil_State_Create_Info)
        return Pipeline_Depth_Stencil_State_Create_Info_C is
        PDSSCIC: Pipeline_Depth_Stencil_State_Create_Info_C;
    begin
        PDSSCIC.Next := Extension_Records.To_C(Struct.Next);
        PDSSCIC.Flags := Struct.Flags;
        PDSSCIC.Depth_Test_Enable := Utilities.To_C(Struct.Depth_Test_Enable);
        PDSSCIC.Depth_Write_Enable := Utilities.To_C(Struct.Depth_Write_Enable);
        PDSSCIC.Depth_Compare_Op := Struct.Depth_Compare_Op;
        PDSSCIC.Depth_Bounds_Test_Enable :=
            Utilities.To_C(Struct.Depth_Bounds_Test_Enable);
        PDSSCIC.Stencil_Test_Enable :=
            Utilities.To_C(Struct.Stencil_Test_Enable);
        PDSSCIC.Front := Struct.Front;
        PDSSCIC.Back := Struct.Back;
        PDSSCIC.Min_Depth_Bounds := Struct.Min_Depth_Bounds;
        PDSSCIC.Max_Depth_Bounds := Struct.Max_Depth_Bounds;

        return PDSSCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Depth_Stencil_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Pipeline_Color_Blend_Attachment_State)
        return Pipeline_Color_Blend_Attachment_State_C is
        PCBASC: Pipeline_Color_Blend_Attachment_State_C;
    begin
        PCBASC.Blend_Enable := Utilities.To_C(Struct.Blend_Enable);
        PCBASC.Src_Color_Blend_Factor := Struct.Src_Color_Blend_Factor;
        PCBASC.Dst_Color_Blend_Factor := Struct.Dst_Color_Blend_Factor;
        PCBASC.Color_Blend_Op := Struct.Color_Blend_Op;
        PCBASC.Src_Alpha_Blend_Factor := Struct.Src_Alpha_Blend_Factor;
        PCBASC.Dst_Alpha_Blend_Factor := Struct.Dst_Alpha_Blend_Factor;
        PCBASC.Alpha_Blend_Op := Struct.Alpha_Blend_Op;
        PCBASC.Color_Write_Mask := Struct.Color_Write_Mask;

        return PCBASC;
    end To_C;

    function To_C(Struct: in Pipeline_Color_Blend_State_Create_Info)
        return Pipeline_Color_Blend_State_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Pipeline_Color_Blend_Attachment_State_C_Arrays,
             Pipeline_Color_Blend_Attachment_State_Vectors);

        PCBSCIC: Pipeline_Color_Blend_State_Create_Info_C;
    begin
        PCBSCIC.Next := Extension_Records.To_C(Struct.Next);
        PCBSCIC.Flags := Struct.Flags;
        PCBSCIC.Logic_Op_Enable := Utilities.To_C(Struct.Logic_Op_Enable);
        PCBSCIC.Logic_Op := Struct.Logic_Op;
        To_C_Array(PCBSCIC.Attachment_Count,
                   Struct.Attachments,
                   PCBSCIC.Attachments);
        PCBSCIC.Blend_Constants := Struct.Blend_Constants;

        return PCBSCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Color_Blend_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Pipeline_Color_Blend_Attachment_State_C_Arrays.Free(Struct.Attachments);
    end Free;

    function To_C(Struct: in Pipeline_Dynamic_State_Create_Info)
        return Pipeline_Dynamic_State_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(Dynamic_State_Arrays,
                                                         Dynamic_State_Vectors);

        PDSCIC: Pipeline_Dynamic_State_Create_Info_C;
    begin
        PDSCIC.Next := Extension_Records.To_C(Struct.Next);
        PDSCIC.Flags := Struct.Flags;
        To_C_Array(PDSCIC.Dynamic_State_Count,
                   Struct.Dynamic_States,
                   PDSCIC.Dynamic_States);

        return PDSCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Dynamic_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Dynamic_State_Arrays.Free(Struct.Dynamic_States);
    end Free;

    function To_C(Struct: in Graphics_Pipeline_Create_Info)
        return Graphics_Pipeline_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Pipeline_Shader_Stage_Create_Info_C_Arrays,
             Pipeline_Shader_Stage_Create_Info_Vectors);

        generic
            type Ada_Struct(<>) is limited private;
            type C_Struct is limited private;
            type Ada_Struct_Access is access constant Ada_Struct;
            type C_Struct_Access is access C_Struct;
            with function To_C(Struct: in Ada_Struct) return C_Struct is <>;
        function Generic_Make_Struct(Ada: in Ada_Struct_Access)
            return C_Struct_Access;

        function Generic_Make_Struct(Ada: in Ada_Struct_Access)
            return C_Struct_Access is
        begin
            if Ada = null then
                return null;
            end if;

            return new C_Struct'(To_C(Ada.all));
        end Generic_Make_Struct;

        function Make_Struct is new Generic_Make_Struct
            (Pipeline_Vertex_Input_State_Create_Info,
             Pipeline_Vertex_Input_State_Create_Info_C,
             Pipeline_Vertex_Input_State_Create_Info_Access,
             Pipeline_Vertex_Input_State_Create_Info_C_Access);

        function Make_Struct is new Generic_Make_Struct
            (Pipeline_Input_Assembly_State_Create_Info,
             Pipeline_Input_Assembly_State_Create_Info_C,
             Pipeline_Input_Assembly_State_Create_Info_Access,
             Pipeline_Input_Assembly_State_Create_Info_C_Access);

        function Make_Struct is new Generic_Make_Struct
            (Pipeline_Tessellation_State_Create_Info,
             Pipeline_Tessellation_State_Create_Info_C,
             Pipeline_Tessellation_State_Create_Info_Access,
             Pipeline_Tessellation_State_Create_Info_C_Access);

        function Make_Struct is new Generic_Make_Struct
            (Pipeline_Viewport_State_Create_Info,
             Pipeline_Viewport_State_Create_Info_C,
             Pipeline_Viewport_State_Create_Info_Access,
             Pipeline_Viewport_State_Create_Info_C_Access);

        function Make_Struct is new Generic_Make_Struct
            (Pipeline_Rasterization_State_Create_Info,
             Pipeline_Rasterization_State_Create_Info_C,
             Pipeline_Rasterization_State_Create_Info_Access,
             Pipeline_Rasterization_State_Create_Info_C_Access);

        function Make_Struct is new Generic_Make_Struct
            (Pipeline_Multisample_State_Create_Info,
             Pipeline_Multisample_State_Create_Info_C,
             Pipeline_Multisample_State_Create_Info_Access,
             Pipeline_Multisample_State_Create_Info_C_Access);

        function Make_Struct is new Generic_Make_Struct
            (Pipeline_Depth_Stencil_State_Create_Info,
             Pipeline_Depth_Stencil_State_Create_Info_C,
             Pipeline_Depth_Stencil_State_Create_Info_Access,
             Pipeline_Depth_Stencil_State_Create_Info_C_Access);

        function Make_Struct is new Generic_Make_Struct
            (Pipeline_Color_Blend_State_Create_Info,
             Pipeline_Color_Blend_State_Create_Info_C,
             Pipeline_Color_Blend_State_Create_Info_Access,
             Pipeline_Color_Blend_State_Create_Info_C_Access);

        function Make_Struct is new Generic_Make_Struct
            (Pipeline_Dynamic_State_Create_Info,
             Pipeline_Dynamic_State_Create_Info_C,
             Pipeline_Dynamic_State_Create_Info_Access,
             Pipeline_Dynamic_State_Create_Info_C_Access);

        GPCIC: Graphics_Pipeline_Create_Info_C;
    begin
        GPCIC.Next := Extension_Records.To_C(Struct.Next);
        GPCIC.Flags := Struct.Flags;
        To_C_Array(GPCIC.Stage_Count, Struct.Stages, GPCIC.Stages);
        GPCIC.Vertex_Input_State := Make_Struct(Struct.Vertex_Input_State);
        GPCIC.Input_Assembly_State := Make_Struct(Struct.Input_Assembly_State);
        GPCIC.Tessellation_State := Make_Struct(Struct.Tessellation_State);
        GPCIC.Viewport_State := Make_Struct(Struct.Viewport_State);
        GPCIC.Rasterization_State := Make_Struct(Struct.Rasterization_State);
        GPCIC.Multisample_State := Make_Struct(Struct.Multisample_State);
        GPCIC.Depth_Stencil_State := Make_Struct(Struct.Depth_Stencil_State);
        GPCIC.Color_Blend_State := Make_Struct(Struct.Color_Blend_State);
        GPCIC.Dynamic_State := Make_Struct(Struct.Dynamic_State);
        GPCIC.Layout := Struct.Layout;
        GPCIC.Render_Pass := Struct.Render_Pass;
        GPCIC.Subpass := Struct.Subpass;
        GPCIC.Base_Pipeline_Handle := Struct.Base_Pipeline_Handle;
        GPCIC.Base_Pipeline_Index := Struct.Base_Pipeline_Index;

        return GPCIC;
    end To_C;

    procedure Free(Struct: in out Graphics_Pipeline_Create_Info_C) is
        procedure Free is
            new Ada.Unchecked_Deallocation
                (Pipeline_Vertex_Input_State_Create_Info_C,
                 Pipeline_Vertex_Input_State_Create_Info_C_Access);

        procedure Free is
            new Ada.Unchecked_Deallocation
                (Pipeline_Input_Assembly_State_Create_Info_C,
                 Pipeline_Input_Assembly_State_Create_Info_C_Access);

        procedure Free is
            new Ada.Unchecked_Deallocation
                (Pipeline_Tessellation_State_Create_Info_C,
                 Pipeline_Tessellation_State_Create_Info_C_Access);

        procedure Free is
            new Ada.Unchecked_Deallocation
                (Pipeline_Viewport_State_Create_Info_C,
                 Pipeline_Viewport_State_Create_Info_C_Access);

        procedure Free is
            new Ada.Unchecked_Deallocation
                (Pipeline_Rasterization_State_Create_Info_C,
                 Pipeline_Rasterization_State_Create_Info_C_Access);

        procedure Free is
            new Ada.Unchecked_Deallocation
                (Pipeline_Multisample_State_Create_Info_C,
                 Pipeline_Multisample_State_Create_Info_C_Access);

        procedure Free is
            new Ada.Unchecked_Deallocation
                (Pipeline_Depth_Stencil_State_Create_Info_C,
                 Pipeline_Depth_Stencil_State_Create_Info_C_Access);

        procedure Free is
            new Ada.Unchecked_Deallocation
                (Pipeline_Color_Blend_State_Create_Info_C,
                 Pipeline_Color_Blend_State_Create_Info_C_Access);

        procedure Free is
            new Ada.Unchecked_Deallocation
                (Pipeline_Dynamic_State_Create_Info_C,
                 Pipeline_Dynamic_State_Create_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        Pipeline_Shader_Stage_Create_Info_C_Arrays.Free(Struct.Stages,
                                                        Free'Access);
        Free(Struct.Vertex_Input_State);
        Free(Struct.Input_Assembly_State);
        Free(Struct.Tessellation_State);
        Free(Struct.Viewport_State);
        Free(Struct.Rasterization_State);
        Free(Struct.Multisample_State);
        Free(Struct.Depth_Stencil_State);
        Free(Struct.Color_Blend_State);
        Free(Struct.Dynamic_State);
    end Free;

    function To_C(Struct: in Compute_Pipeline_Create_Info)
        return Compute_Pipeline_Create_Info_C is
        CPCIC: Compute_Pipeline_Create_Info_C;
    begin
        CPCIC.Next := Extension_Records.To_C(Struct.Next);
        CPCIC.Flags := Struct.Flags;
        CPCIC.Stage := To_C(Struct.Stage);
        CPCIC.Layout := Struct.Layout;
        CPCIC.Base_Pipeline_Handle := Struct.Base_Pipeline_Handle;
        CPCIC.Base_Pipeline_Index := Struct.Base_Pipeline_Index;

        return CPCIC;
    end To_C;

    procedure Free(Struct: in out Compute_Pipeline_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Free(Struct.Stage);
    end Free;

    function To_C(Struct: in Pipeline_Layout_Create_Info)
        return Pipeline_Layout_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Descriptor_Set_Layout_Arrays, Descriptor_Set_Layout_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (Push_Constant_Range_Arrays, Push_Constant_Range_Vectors);

        PLCIC: Pipeline_Layout_Create_Info_C;
    begin
        PLCIC.Next := Extension_Records.To_C(Struct.Next);
        PLCIC.Flags := Struct.Flags;
        To_C_Array(PLCIC.Set_Layout_Count,
                   Struct.Set_Layouts,
                   PLCIC.Set_Layouts);
        To_C_Array(PLCIC.Push_Constant_Range_Count,
                   Struct.Push_Constant_Ranges,
                   PLCIC.Push_Constant_Ranges);

        return PLCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Layout_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Descriptor_Set_Layout_Arrays.Free(Struct.Set_Layouts);
        Push_Constant_Range_Arrays.Free(Struct.Push_Constant_Ranges);
    end Free;

    function To_C(Struct: in Sampler_Create_Info)
        return Sampler_Create_Info_C is
        SCIC: Sampler_Create_Info_C;
    begin
        SCIC.Next := Extension_Records.To_C(Struct.Next);
        SCIC.Flags := Struct.Flags;
        SCIC.Mag_Filter := Struct.Mag_Filter;
        SCIC.Min_Filter := Struct.Min_Filter;
        SCIC.Mipmap_Mode := Struct.Mipmap_Mode;
        SCIC.Address_Mode_U := Struct.Address_Mode_U;
        SCIC.Address_Mode_V := Struct.Address_Mode_V;
        SCIC.Address_Mode_W := Struct.Address_Mode_W;
        SCIC.Mip_Lod_Bias := Struct.Mip_Lod_Bias;
        SCIC.Anisotropy_Enable := Utilities.To_C(Struct.Anisotropy_Enable);
        SCIC.Max_Anisotropy := Struct.Max_Anisotropy;
        SCIC.Compare_Enable := Utilities.To_C(Struct.Compare_Enable);
        SCIC.Compare_Op := Struct.Compare_Op;
        SCIC.Min_Lod := Struct.Min_Lod;
        SCIC.Max_Lod := Struct.Max_Lod;
        SCIC.Border_Color := Struct.Border_Color;
        SCIC.Unnormalized_Coordinates :=
            Utilities.To_C(Struct.Unnormalized_Coordinates);

        return SCIC;
    end To_C;

    procedure Free(Struct: in out Sampler_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Descriptor_Set_Layout_Binding)
        return Descriptor_Set_Layout_Binding_C is
        DSLBC: Descriptor_Set_Layout_Binding_C;
    begin
        DSLBC.Binding := Struct.Binding;
        DSLBC.Descriptor_Type := Struct.Descriptor_Type;
        DSLBC.Descriptor_Count := Struct.Descriptor_Count;
        DSLBC.Stage_Flags := Struct.Stage_Flags;

        if not Struct.Immutable_Samplers.Is_Empty then
            declare
                Immutable_Samplers: Sampler_Arrays.Array_Access;
            begin
                Immutable_Samplers := Sampler_Arrays.Allocate
                    (Positive(Struct.Immutable_Samplers.Length));
                DSLBC.Immutable_Samplers := Immutable_Samplers(1)'Access;

                for X in Immutable_Samplers'Range loop
                    Immutable_Samplers(X) := Struct.Immutable_Samplers(X);
                end loop;
            end;
        end if;

        return DSLBC;
    end To_C;

    procedure Free(Struct: in out Descriptor_Set_Layout_Binding_C) is
    begin
        Sampler_Arrays.Free(Struct.Immutable_Samplers);
    end Free;

    function To_C(Struct: in Descriptor_Set_Layout_Create_Info)
        return Descriptor_Set_Layout_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Descriptor_Set_Layout_Binding_C_Arrays,
             Descriptor_Set_Layout_Binding_Vectors);

        DSLCIC: Descriptor_Set_Layout_Create_Info_C;
    begin
        DSLCIC.Next := Extension_Records.To_C(Struct.Next);
        DSLCIC.Flags := Struct.Flags;
        To_C_Array(DSLCIC.Binding_Count,
                   Struct.Bindings,
                   DSLCIC.Bindings);

        return DSLCIC;
    end To_C;

    procedure Free(Struct: in out Descriptor_Set_Layout_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Descriptor_Set_Layout_Binding_C_Arrays.Free(Struct.Bindings,
                                                    Free'Access);
    end Free;
 
    function To_C(Struct: in Descriptor_Pool_Create_Info)
        return Descriptor_Pool_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Descriptor_Pool_Size_Arrays, Descriptor_Pool_Size_Vectors);

        DPCIC: Descriptor_Pool_Create_Info_C;
    begin
        DPCIC.Next := Extension_Records.To_C(Struct.Next);
        DPCIC.Flags := Struct.Flags;
        DPCIC.Max_Sets := Struct.Max_Sets;
        To_C_Array(DPCIC.Pool_Size_Count, Struct.Pool_Sizes, DPCIC.Pool_Sizes);

        return DPCIC;
    end To_C;

    procedure Free(Struct: in out Descriptor_Pool_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Descriptor_Pool_Size_Arrays.Free(Struct.Pool_Sizes);
    end Free;

    function To_C(Struct: in Descriptor_Set_Allocate_Info)
        return Descriptor_Set_Allocate_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Descriptor_Set_Layout_Arrays, Descriptor_Set_Layout_Vectors);

        DSAIC: Descriptor_Set_Allocate_Info_C;
    begin
        DSAIC.Next := Extension_Records.To_C(Struct.Next);
        DSAIC.Descriptor_Pool := Struct.Descriptor_Pool;
        To_C_Array(DSAIC.Descriptor_Set_Count,
                   Struct.Set_Layouts,
                   DSAIC.Set_Layouts);

        return DSAIC;
    end To_C;

    procedure Free(Struct: in out Descriptor_Set_Allocate_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Descriptor_Set_Layout_Arrays.Free(Struct.Set_Layouts);
    end Free;

    function To_C(Struct: in Write_Descriptor_Set)
        return Write_Descriptor_Set_C is
        generic
            with package C_Array is new C_Arrays(<>);
            with package Vectors is new Ada.Containers.Vectors(Positive,
                                                               C_Array.Element,
                                                               <>);
        procedure Generic_To_C_Array(C_Pointer: out C_Array.Pointer;
                                     Vector: in Vectors.Vector);

        procedure Generic_To_C_Array(C_Pointer: out C_Array.Pointer;
                                     Vector: in Vectors.Vector) is
        begin
            if not Vector.Is_Empty then
                declare
                    C_Access: C_Array.Array_Access;
                begin
                    C_Access := C_Array.Allocate(Positive(Vector.Length));
                    C_Pointer := C_Access(1)'Access;

                    for X in C_Access'Range loop
                        C_Access(X) := Vector(X);
                    end loop;
                end;
            end if;
        end Generic_To_C_Array;

        procedure To_C_Array is
            new Generic_To_C_Array(Descriptor_Image_Info_Arrays,
                                   Descriptor_Image_Info_Vectors);
        procedure To_C_Array is
            new Generic_To_C_Array(Descriptor_Buffer_Info_Arrays,
                                   Descriptor_Buffer_Info_Vectors);
        procedure To_C_Array is
            new Generic_To_C_Array(Buffer_View_Arrays,
                                   Buffer_View_Vectors);

        WDSC: Write_Descriptor_Set_C;
    begin
        WDSC.Next := Extension_Records.To_C(Struct.Next);
        WDSC.Dst_Set := Struct.Dst_Set;
        WDSC.Dst_Binding := Struct.Dst_Binding;
        WDSC.Dst_Array_Element := Struct.Dst_Array_Element;
        WDSC.Descriptor_Count := Struct.Descriptor_Count;
        WDSC.Descriptor_Type := Struct.Descriptor_Type;
        To_C_Array(WDSC.Image_Info, Struct.Image_Info);
        To_C_Array(WDSC.Buffer_Info, Struct.Buffer_Info);
        To_C_Array(WDSC.Texel_Buffer_View, Struct.Texel_Buffer_View);

        return WDSC;
    end To_C;

    procedure Free(Struct: in out Write_Descriptor_Set_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Descriptor_Image_Info_Arrays.Free(Struct.Image_Info);
        Descriptor_Buffer_Info_Arrays.Free(Struct.Buffer_Info);
        Buffer_View_Arrays.Free(Struct.Texel_Buffer_View);
    end Free;

    function To_C(Struct: in Copy_Descriptor_Set)
        return Copy_Descriptor_Set_C is
        CDSC: Copy_Descriptor_Set_C;
    begin
        CDSC.Next := Extension_Records.To_C(Struct.Next);
        CDSC.Src_Set := Struct.Src_Set;
        CDSC.Src_Binding := Struct.Src_Binding;
        CDSC.Src_Array_Element := Struct.Src_Array_Element;
        CDSC.Dst_Set := Struct.Dst_Set;
        CDSC.Dst_Binding := Struct.Dst_Binding;
        CDSC.Dst_Array_Element := Struct.Dst_Array_Element;
        CDSC.Descriptor_Count := Struct.Descriptor_Count;

        return CDSC;
    end To_C;

    procedure Free(Struct: in out Copy_Descriptor_Set_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Framebuffer_Create_Info)
        return Framebuffer_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(Image_View_Arrays,
                                                         Image_View_Vectors);

        FCIC: Framebuffer_Create_Info_C;
    begin
        FCIC.Next := Extension_Records.To_C(Struct.Next);
        FCIC.Flags := Struct.Flags;
        FCIC.Render_Pass := Struct.Render_Pass;
        To_C_Array(FCIC.Attachment_Count,
                   Struct.Attachments,
                   FCIC.Attachments);
        FCIC.Width := Struct.Width;
        FCIC.Height := Struct.Height;
        FCIC.Layers := Struct.Layers;

        return FCIC;
    end To_C;

    procedure Free(Struct: in out Framebuffer_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Image_View_Arrays.Free(Struct.Attachments);
    end Free;

    function To_C(Struct: in Subpass_Description)
        return Subpass_Description_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Attachment_Reference_Arrays, Attachment_Reference_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array(Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        SDC: Subpass_Description_C;
        Dummy: Interfaces.Unsigned_32;
    begin
        SDC.Flags := Struct.Flags;
        To_C_Array(SDC.Input_Attachment_Count,
                   Struct.Input_Attachments,
                   SDC.Input_Attachments);
        To_C_Array(SDC.Color_Attachment_Count,
                   Struct.Color_Attachments,
                   SDC.Color_Attachments);
        To_C_Array(Dummy,
                   Struct.Resolve_Attachments,
                   SDC.Resolve_Attachments);

        SDC.Depth_Stencil_Attachment := Struct.Depth_Stencil_Attachment;

        To_C_Array(SDC.Preserve_Attachment_Count,
                   Struct.Preserve_Attachments,
                   SDC.Preserve_Attachments);

        return SDC;
    end To_C;

    procedure Free(Struct: in out Subpass_Description_C) is
    begin
        Attachment_Reference_Arrays.Free(Struct.Input_Attachments);
        Attachment_Reference_Arrays.Free(Struct.Color_Attachments);
        Attachment_Reference_Arrays.Free(Struct.Resolve_Attachments);
        Uint32_t_Arrays.Free(Struct.Preserve_Attachments);
    end Free;

    function To_C(Struct: in Render_Pass_Create_Info)
        return Render_Pass_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Attachment_Description_Arrays, Attachment_Description_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Subpass_Description_C_Arrays, Subpass_Description_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (Subpass_Dependency_Arrays, Subpass_Dependency_Vectors);

        RPCIC: Render_Pass_Create_Info_C;
    begin
        RPCIC.Next := Extension_Records.To_C(Struct.Next);
        RPCIC.Flags := Struct.Flags;
        To_C_Array(RPCIC.Attachment_Count,
                   Struct.Attachments,
                   RPCIC.Attachments);
        To_C_Array(RPCIC.Subpass_Count,
                   Struct.Subpasses,
                   RPCIC.Subpasses);
        To_C_Array(RPCIC.Dependency_Count,
                   Struct.Dependencies,
                   RPCIC.Dependencies);

        return RPCIC;
    end To_C;

    procedure Free(Struct: in out Render_Pass_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Attachment_Description_Arrays.Free(Struct.Attachments);
        Subpass_Description_C_Arrays.Free(Struct.Subpasses, Free'Access);
        Subpass_Dependency_Arrays.Free(Struct.Dependencies);
    end Free;

    function To_C(Struct: in Command_Pool_Create_Info)
        return Command_Pool_Create_Info_C is
        CPCIC: Command_Pool_Create_Info_C;
    begin
        CPCIC.Next := Extension_Records.To_C(Struct.Next);
        CPCIC.Flags := Struct.Flags;
        CPCIC.Queue_Family_Index := Struct.Queue_Family_Index;

        return CPCIC;
    end To_C;

    procedure Free(Struct: in out Command_Pool_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Command_Buffer_Allocate_Info)
        return Command_Buffer_Allocate_Info_C is
        CBAIC: Command_Buffer_Allocate_Info_C;
    begin
        CBAIC.Next := Extension_Records.To_C(Struct.Next);
        CBAIC.Command_Pool := Struct.Command_Pool;
        CBAIC.Level := Struct.Level;
        CBAIC.Command_Buffer_Count := Struct.Command_Buffer_Count;

        return CBAIC;
    end To_C;

    procedure Free(Struct: in out Command_Buffer_Allocate_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Command_Buffer_Inheritance_Info)
        return Command_Buffer_Inheritance_Info_C is
        CBIIC: Command_Buffer_Inheritance_Info_C;
    begin
        CBIIC.Next := Extension_Records.To_C(Struct.Next);
        CBIIC.Render_Pass := Struct.Render_Pass;
        CBIIC.Subpass := Struct.Subpass;
        CBIIC.Framebuffer := Struct.Framebuffer;
        CBIIC.Occlusion_Query_Enable :=
            Utilities.To_C(Struct.Occlusion_Query_Enable);
        CBIIC.Query_Flags := Struct.Query_Flags;
        CBIIC.Pipeline_Statistics := Struct.Pipeline_Statistics;

        return CBIIC;
    end To_C;

    procedure Free(Struct: in out Command_Buffer_Inheritance_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Command_Buffer_Begin_Info)
        return Command_Buffer_Begin_Info_C is
        CBBIC: Command_Buffer_Begin_Info_C;
    begin
        CBBIC.Next := Extension_Records.To_C(Struct.Next);
        CBBIC.Flags := Struct.Flags;

        if Struct.Inheritance_Info /= null then
            CBBIC.Inheritance_Info :=
                new Command_Buffer_Inheritance_Info_C'
                    (To_C(Struct.Inheritance_Info.all));
        end if;

        return CBBIC;
    end To_C;

    procedure Free(Struct: in out Command_Buffer_Begin_Info_C) is
        procedure Free is
            new Ada.Unchecked_Deallocation
                (Command_Buffer_Inheritance_Info_C,
                 Command_Buffer_Inheritance_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        Free(Struct.Inheritance_Info);
    end Free;

    function To_C(Struct: in Clear_Color_Value) return Clear_Color_Value_C is
    begin
        case Struct.Color_Type is
            when Clear_Color_Float =>
                return CCVC: Clear_Color_Value_C(Clear_Color_Float) do
                    CCVC.Float_Color := Struct.Float_Color;
                end return;
            when Clear_Color_Integer =>
                return CCVC: Clear_Color_Value_C(Clear_Color_Integer) do
                    CCVC.Integer_Color := Struct.Integer_Color;
                end return;
            when Clear_Color_Unsigned =>
                return CCVC: Clear_Color_Value_C(Clear_Color_Unsigned) do
                    CCVC.Unsigned_Color := Struct.Unsigned_Color;
                end return;
        end case;
    end To_C;

    function To_C(Struct: in Clear_Value) return Clear_Value_C is
    begin
        case Struct.Clear_Type is
            when Clear_Color =>
                case Struct.Color_Type is
                    when Clear_Color_Float =>
                        return CVC: Clear_Value_C(Clear_Color,
                                                  Clear_Color_Float) do
                            CVC.Color_Float := To_C(Struct.Color);
                        end return;
                    when Clear_Color_Integer =>
                        return CVC: Clear_Value_C(Clear_Color,
                                                  Clear_Color_Integer) do
                            CVC.Color_Integer := To_C(Struct.Color);
                        end return;
                    when Clear_Color_Unsigned =>
                        return CVC: Clear_Value_C(Clear_Color,
                                                  Clear_Color_Unsigned) do
                            CVC.Color_Unsigned := To_C(Struct.Color);
                        end return;
                end case;
            when Clear_Depth_Stencil =>
                return CVC: Clear_Value_C(Clear_Depth_Stencil,
                                          Clear_Color_Float) do
                    CVC.Depth_Stencil := Struct.Depth_Stencil;
                end return;
        end case;
    end To_C;

    function To_C(Struct: in Clear_Attachment) return Clear_Attachment_C is
        CAC: Clear_Attachment_C(Struct.Clear_Type, Struct.Color_Type);
    begin
        CAC.Aspect_Mask := Struct.Aspect_Mask;
        CAC.Color_Attachment := Struct.Color_Attachment;

        case Struct.Clear_Type is
            when Clear_Color =>
                CAC.Color_Clear_Value := To_C(Struct.Clear_Value);
            when Clear_Depth_Stencil =>
                CAC.Depth_Stencil_Clear_Value := To_C(Struct.Clear_Value);
        end case;

        return CAC;
    end To_C;

    function To_C(Struct: in Memory_Barrier) return Memory_Barrier_C is
        MBC: Memory_Barrier_C;
    begin
        MBC.Next := Extension_Records.To_C(Struct.Next);
        MBC.Src_Access_Mask := Struct.Src_Access_Mask;
        MBC.Dst_Access_Mask := Struct.Dst_Access_Mask;

        return MBC;
    end To_C;

    procedure Free(Struct: in out Memory_Barrier_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Buffer_Memory_Barrier)
        return Buffer_Memory_Barrier_C is
        BMBC: Buffer_Memory_Barrier_C;
    begin
        BMBC.Next := Extension_Records.To_C(Struct.Next);
        BMBC.Src_Access_Mask := Struct.Src_Access_Mask;
        BMBC.Dst_Access_Mask := Struct.Dst_Access_Mask;
        BMBC.Src_Queue_Family_Index := Struct.Src_Queue_Family_Index;
        BMBC.Dst_Queue_Family_Index := Struct.Dst_Queue_Family_Index;
        BMBC.Buffer := Struct.Buffer;
        BMBC.Offset := Struct.Offset;
        BMBC.Size := Struct.Size;

        return BMBC;
    end To_C;

    procedure Free(Struct: in out Buffer_Memory_Barrier_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Image_Memory_Barrier)
        return Image_Memory_Barrier_C is
        IMBC: Image_Memory_Barrier_C;
    begin
        IMBC.Next := Extension_Records.To_C(Struct.Next);
        IMBC.Src_Access_Mask := Struct.Src_Access_Mask;
        IMBC.Dst_Access_Mask := Struct.Dst_Access_Mask;
        IMBC.Old_Layout := Struct.Old_Layout;
        IMBC.New_Layout := Struct.New_Layout;
        IMBC.Src_Queue_Family_Index := Struct.Src_Queue_Family_Index;
        IMBC.Dst_Queue_Family_Index := Struct.Dst_Queue_Family_Index;
        IMBC.Image := Struct.Image;
        IMBC.Subresource_Range := Struct.Subresource_Range;

        return IMBC;
    end To_C;

    procedure Free(Struct: in out Image_Memory_Barrier_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Render_Pass_Begin_Info)
        return Render_Pass_Begin_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert_Indefinite
            (Clear_Value_C_Arrays, Clear_Value_Vectors);

        RPBIC: Render_Pass_Begin_Info_C;
    begin
        RPBIC.Next := Extension_Records.To_C(Struct.Next);
        RPBIC.Render_Pass := Struct.Render_Pass;
        RPBIC.Framebuffer := Struct.Framebuffer;
        RPBIC.Render_Area := Struct.Render_Area;
        To_C_Array(RPBIC.Clear_Value_Count,
                   Struct.Clear_Values,
                   RPBIC.Clear_Values);

        return RPBIC;
    end To_C;

    procedure Free(Struct: in out Render_Pass_Begin_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Clear_Value_C_Arrays.Free(Struct.Clear_Values);
    end Free;         
end Vulkan.C;


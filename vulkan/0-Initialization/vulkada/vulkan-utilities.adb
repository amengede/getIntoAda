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

-- Internal utilities package

pragma No_Strict_Aliasing;

with Ada.Unchecked_Deallocation;
with Vulkan.Extension_Records;

package body Vulkan.Utilities is
    function To_Ada(Properties: in C_Extension_Properties_Array)
        return Extension_Properties_Vectors.Vector is
        EPV: Extension_Properties_Vectors.Vector;
    begin
        for X in Properties'Range loop
            declare
                EP: Extension_Properties;
            begin
                EP.Name := Ada.Strings.Unbounded.
                    To_Unbounded_String
                        (Interfaces.C.To_Ada(Properties(X).Extension_Name));
                EP.Spec_Version := Properties(X).Spec_Version;
                EPV.Append(EP);
            end;
        end loop;

        return EPV;
    end To_Ada;

    function To_Ada(Properties: in C_Layer_Properties_Array)
        return Layer_Properties_Vectors.Vector is
        LPV: Layer_Properties_Vectors.Vector;
    begin
        for X in Properties'Range loop
            declare
                P: Layer_Properties;
            begin
                P.Name := Ada.Strings.Unbounded.
                    To_Unbounded_String
                        (Interfaces.C.To_Ada(Properties(X).Layer_Name));
                P.Spec_Version := Properties(X).Spec_Version;
                P.Implementation_Version := Properties(X).Implementation_Version;
                P.Description := Ada.Strings.Unbounded.
                    To_Unbounded_String
                        (Interfaces.C.To_Ada(Properties(X).Description));
                LPV.Append(P);
            end;
        end loop;

        return LPV;
    end To_Ada;

    function To_C(Strings: in String_Vectors.Vector)
        return Interfaces.C.Strings.chars_ptr_array is
        use type Interfaces.C.size_t;

        C_Strings: Interfaces.C.Strings.chars_ptr_array
            (1 .. Interfaces.C.size_t(Strings.Length));
        Index: Interfaces.C.size_t := 1;
    begin
        for S of Strings loop
            C_Strings(Index) := Interfaces.C.Strings.New_String(S);
            Index := Index + 1;
        end loop;

        return C_Strings;
    end To_C;

    procedure Free(Strings: in out Interfaces.C.Strings.chars_ptr_array) is
    begin
        for S of Strings loop
            Interfaces.C.Strings.Free(S);
        end loop;
    end Free;

    function To_Unbounded_String(C_String: in Interfaces.C.Strings.chars_ptr)
        return Ada.Strings.Unbounded.Unbounded_String is
    begin
        if C_String = Interfaces.C.Strings.Null_Ptr then
            return Ada.Strings.Unbounded.Null_Unbounded_String;
        end if;

        return Ada.Strings.Unbounded.To_Unbounded_String
            (Interfaces.C.Strings.Value(C_String));
    end To_Unbounded_String;

    function Get_Array_1(First: in Arg_1) return Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
    begin
        if C_Get_Array(First, Count, null) /= Success then
            return Vectors.Empty_Vector;
        end if;

        if Count = 0 then
            return Vectors.Empty_Vector;
        end if;

        declare
            Item_Array: array (1 .. Count) of aliased Vectors.Element_Type
                with Convention => C;
            Items: Vectors.Vector;
        begin
            if C_Get_Array(First, Count, Item_Array(1)'Access) = Success then
                Items.Reserve_Capacity(Ada.Containers.Count_Type(Count));

                for Item of Item_Array loop
                    Items.Append(Item);
                end loop;
            end if;

            return Items;
        end;
    end Get_Array_1;

    function Get_Array_1_Proc(First: in Arg_1) return Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
    begin
        C_Get_Array(First, Count, null);

        if Count = 0 then
            return Vectors.Empty_Vector;
        end if;

        declare
            Item_Array: array (1 .. Count) of aliased Vectors.Element_Type
                with Convention => C;
            Items: Vectors.Vector;
        begin
            C_Get_Array(First, Count, Item_Array(1)'Access);
            Items.Reserve_Capacity(Ada.Containers.Count_Type(Count));

            for Item of Item_Array loop
                Items.Append(Item);
            end loop;

            return Items;
        end;
    end Get_Array_1_Proc;

    function Get_Array_2(First: in Arg_1;
                         Second: in Arg_2) return Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
    begin
        if C_Get_Array(First, Second, Count, null) /= Success then
            return Vectors.Empty_Vector;
        end if;

        if Count = 0 then
            return Vectors.Empty_Vector;
        end if;

        declare
            Item_Array: array (1 .. Count) of aliased Vectors.Element_Type
                with Convention => C;
            Items: Vectors.Vector;
        begin
            if C_Get_Array(First,
                           Second,
                           Count,
                           Item_Array(1)'Access) = Success then
                Items.Reserve_Capacity(Ada.Containers.Count_Type(Count));

                for Item of Item_Array loop
                    Items.Append(Item);
                end loop;
            end if;

            return Items;
        end;
    end Get_Array_2;

    function Get_Array_2_Proc(First: in Arg_1;
                              Second: in Arg_2) return Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
    begin
        C_Get_Array(First, Second, Count, null);

        if Count = 0 then
            return Vectors.Empty_Vector;
        end if;

        declare
            Item_Array: array (1 .. Count) of aliased Vectors.Element_Type
                with Convention => C;
            Items: Vectors.Vector;
        begin
            C_Get_Array(First, Second, Count, Item_Array(1)'Access);
            Items.Reserve_Capacity(Ada.Containers.Count_Type(Count));

            for Item of Item_Array loop
                Items.Append(Item);
            end loop;

            return Items;
        end;
    end Get_Array_2_Proc;

    function To_Ada(PDFC: in C.Physical_Device_Features_C)
        return Physical_Device_Features is
        PDF: Physical_Device_Features;
    begin
        PDF.Robust_Buffer_Access := To_Ada(PDFC.Robust_Buffer_Access);
        PDF.Full_Draw_Index_Uint32 := To_Ada(PDFC.Full_Draw_Index_Uint32);
        PDF.Image_Cube_Array := To_Ada(PDFC.Image_Cube_Array);
        PDF.Independent_Blend := To_Ada(PDFC.Independent_Blend);
        PDF.Geometry_Shader := To_Ada(PDFC.Geometry_Shader);
        PDF.Tessellation_Shader := To_Ada(PDFC.Tessellation_Shader);
        PDF.Sample_Rate_Shading := To_Ada(PDFC.Sample_Rate_Shading);
        PDF.Dual_Src_Blend := To_Ada(PDFC.Dual_Src_Blend);
        PDF.Logic_Op := To_Ada(PDFC.Logic_Op);
        PDF.Multi_Draw_Indirect := To_Ada(PDFC.Multi_Draw_Indirect);
        PDF.Draw_Indirect_First_Instance :=
            To_Ada(PDFC.Draw_Indirect_First_Instance);
        PDF.Depth_Clamp := To_Ada(PDFC.Depth_Clamp);
        PDF.Depth_Bias_Clamp := To_Ada(PDFC.Depth_Bias_Clamp);
        PDF.Fill_Mode_Non_Solid := To_Ada(PDFC.Fill_Mode_Non_Solid);
        PDF.Depth_Bounds := To_Ada(PDFC.Depth_Bounds);
        PDF.Wide_Lines := To_Ada(PDFC.Wide_Lines);
        PDF.Large_Points := To_Ada(PDFC.Large_Points);
        PDF.Alpha_To_One := To_Ada(PDFC.Alpha_To_One);
        PDF.Multi_Viewport := To_Ada(PDFC.Multi_Viewport);
        PDF.Sampler_Anisotropy := To_Ada(PDFC.Sampler_Anisotropy);
        PDF.Texture_Compression_ETC2 := To_Ada(PDFC.Texture_Compression_ETC2);
        PDF.Texture_Compression_ASTC_LDR :=
            To_Ada(PDFC.Texture_Compression_ASTC_LDR);
        PDF.Texture_Compression_BC := To_Ada(PDFC.Texture_Compression_BC);
        PDF.Occlusion_Query_Precise := To_Ada(PDFC.Occlusion_Query_Precise);
        PDF.Pipeline_Statistics_Query := To_Ada(PDFC.Pipeline_Statistics_Query);
        PDF.Vertex_Pipeline_Stores_And_Atomics :=
            To_Ada(PDFC.Vertex_Pipeline_Stores_And_Atomics);
        PDF.Fragment_Stores_And_Atomics :=
            To_Ada(PDFC.Fragment_Stores_And_Atomics);
        PDF.Shader_Tessellation_And_Geometry_Point_Size :=
            To_Ada(PDFC.Shader_Tessellation_And_Geometry_Point_Size);
        PDF.Shader_Image_Gather_Extended :=
            To_Ada(PDFC.Shader_Image_Gather_Extended);
        PDF.Shader_Storage_Image_Extended_Formats :=
            To_Ada(PDFC.Shader_Storage_Image_Extended_Formats);
        PDF.Shader_Storage_Image_Multisample :=
            To_Ada(PDFC.Shader_Storage_Image_Multisample);
        PDF.Shader_Storage_Image_Read_Without_Format :=
            To_Ada(PDFC.Shader_Storage_Image_Read_Without_Format);
        PDF.Shader_Storage_Image_Write_Without_Format :=
            To_Ada(PDFC.Shader_Storage_Image_Write_Without_Format);
        PDF.Shader_Uniform_Buffer_Array_Dynamic_Indexing :=
            To_Ada(PDFC.Shader_Uniform_Buffer_Array_Dynamic_Indexing);
        PDF.Shader_Sampled_Image_Array_Dynamic_Indexing :=
            To_Ada(PDFC.Shader_Sampled_Image_Array_Dynamic_Indexing);
        PDF.Shader_Storage_Buffer_Array_Dynamic_Indexing :=
            To_Ada(PDFC.Shader_Storage_Buffer_Array_Dynamic_Indexing);
        PDF.Shader_Storage_Image_Array_Dynamic_Indexing :=
            To_Ada(PDFC.Shader_Storage_Image_Array_Dynamic_Indexing);
        PDF.Shader_Clip_Distance := To_Ada(PDFC.Shader_Clip_Distance);
        PDF.Shader_Cull_Distance := To_Ada(PDFC.Shader_Cull_Distance);
        PDF.Shader_Float64 := To_Ada(PDFC.Shader_Float64);
        PDF.Shader_Int64 := To_Ada(PDFC.Shader_Int64);
        PDF.Shader_Int16 := To_Ada(PDFC.Shader_Int16);
        PDF.Shader_Resource_Residency := To_Ada(PDFC.Shader_Resource_Residency);
        PDF.Shader_Resource_Min_Lod := To_Ada(PDFC.Shader_Resource_Min_Lod);
        PDF.Sparse_Binding := To_Ada(PDFC.Sparse_Binding);
        PDF.Sparse_Residency_Buffer := To_Ada(PDFC.Sparse_Residency_Buffer);
        PDF.Sparse_Residency_Image_2D := To_Ada(PDFC.Sparse_Residency_Image_2D);
        PDF.Sparse_Residency_Image_3D := To_Ada(PDFC.Sparse_Residency_Image_3D);
        PDF.Sparse_Residency_2_Samples :=
            To_Ada(PDFC.Sparse_Residency_2_Samples);
        PDF.Sparse_Residency_4_Samples :=
            To_Ada(PDFC.Sparse_Residency_4_Samples);
        PDF.Sparse_Residency_8_Samples :=
            To_Ada(PDFC.Sparse_Residency_8_Samples);
        PDF.Sparse_Residency_16_Samples :=
            To_Ada(PDFC.Sparse_Residency_16_Samples);
        PDF.Sparse_Residency_Aliased := To_Ada(PDFC.Sparse_Residency_Aliased);
        PDF.Variable_Multisample_Rate := To_Ada(PDFC.Variable_Multisample_Rate);
        PDF.Inherited_Queries := To_Ada(PDFC.Inherited_Queries);

        return PDF;
    end To_Ada;

    function To_Ada(PDPC: in C.Physical_Device_Properties_C)
        return Physical_Device_Properties is
        use type Interfaces.Unsigned_32;

        function To_Ada(FRC: in C.Float_Range_C) return Float_Range is
            FR: Float_Range;
        begin
            for X in FR'Range loop
                FR(X) := Float(FRC(X));
            end loop;

            return FR;
        end To_Ada;
        
        PDP: Physical_Device_Properties;
        CL: C.Physical_Device_Limits_C renames PDPC.Limits;
        L: Physical_Device_Limits renames PDP.Limits;
        SPC: C.Physical_Device_Sparse_Properties_C
            renames PDPC.Sparse_Properties;
        SP: Physical_Device_Sparse_Properties renames PDP.Sparse_Properties;
    begin
        PDP.API_Version := PDPC.API_Version;
        PDP.Driver_Version := PDPC.Driver_Version;
        PDP.Vendor_ID := PDPC.Vendor_ID;
        PDP.Device_ID := PDPC.Device_ID;
        PDP.Device_Type := PDPC.Device_Type;
        PDP.Device_Name := Ada.Strings.Unbounded.To_Unbounded_String
                            (Interfaces.C.To_Ada(PDPC.Device_Name));
        PDP.Pipeline_Cache_UUID := PDPC.Pipeline_Cache_UUID;
       
        L.Max_Image_Dimension_1D := CL.Max_Image_Dimension_1D;
        L.Max_Image_Dimension_2D := CL.Max_Image_Dimension_2D;
        L.Max_Image_Dimension_3D := CL.Max_Image_Dimension_3D;
        L.Max_Image_Dimension_Cube := CL.Max_Image_Dimension_Cube;
        L.Max_Image_Array_Layers := CL.Max_Image_Array_Layers;
        L.Max_Texel_Buffer_Elements := CL.Max_Texel_Buffer_Elements;
        L.Max_Uniform_Buffer_Range := CL.Max_Uniform_Buffer_Range;
        L.Max_Storage_Buffer_Range := CL.Max_Storage_Buffer_Range;
        L.Max_Push_Constants_Size := CL.Max_Push_Constants_Size;
        L.Max_Memory_Allocation_Count := CL.Max_Memory_Allocation_Count;
        L.Max_Sampler_Allocation_Count := CL.Max_Sampler_Allocation_Count;
        L.Buffer_Image_Granularity := CL.Buffer_Image_Granularity;
        L.Sparse_Address_Space_Size := CL.Sparse_Address_Space_Size;
        L.Max_Bound_Descriptor_Sets := CL.Max_Bound_Descriptor_Sets;
        L.Max_Per_Stage_Descriptor_Samplers :=
            CL.Max_Per_Stage_Descriptor_Samplers;
        L.Max_Per_Stage_Descriptor_Uniform_Buffers :=
            CL.Max_Per_Stage_Descriptor_Uniform_Buffers;
        L.Max_Per_Stage_Descriptor_Storage_Buffers :=
            CL.Max_Per_Stage_Descriptor_Storage_Buffers;
        L.Max_Per_Stage_Descriptor_Sampled_Images :=
            CL.Max_Per_Stage_Descriptor_Sampled_Images;
        L.Max_Per_Stage_Descriptor_Storage_Images :=
            CL.Max_Per_Stage_Descriptor_Storage_Images;
        L.Max_Per_Stage_Descriptor_Input_Attachments :=
            CL.Max_Per_Stage_Descriptor_Input_Attachments;
        L.Max_Per_Stage_Resources := CL.Max_Per_Stage_Resources;
        L.Max_Descriptor_Set_Samplers := CL.Max_Descriptor_Set_Samplers;
        L.Max_Descriptor_Set_Uniform_Buffers :=
            CL.Max_Descriptor_Set_Uniform_Buffers;
        L.Max_Descriptor_Set_Uniform_Buffers_Dynamic :=
            CL.Max_Descriptor_Set_Uniform_Buffers_Dynamic;
        L.Max_Descriptor_Set_Storage_Buffers :=
            CL.Max_Descriptor_Set_Storage_Buffers;
        L.Max_Descriptor_Set_Storage_Buffers_Dynamic :=
            CL.Max_Descriptor_Set_Storage_Buffers_Dynamic;
        L.Max_Descriptor_Set_Sampled_Images :=
            CL.Max_Descriptor_Set_Sampled_Images;
        L.Max_Descriptor_Set_Storage_Images :=
            CL.Max_Descriptor_Set_Storage_Images;
        L.Max_Descriptor_Set_Input_Attachments :=
            CL.Max_Descriptor_Set_Input_Attachments;
        L.Max_Vertex_Input_Attributes := CL.Max_Vertex_Input_Attributes;
        L.Max_Vertex_Input_Bindings := CL.Max_Vertex_Input_Bindings;
        L.Max_Vertex_Input_Attribute_Offset :=
            CL.Max_Vertex_Input_Attribute_Offset;
        L.Max_Vertex_Input_Binding_Stride := CL.Max_Vertex_Input_Binding_Stride;
        L.Max_Vertex_Output_Components := CL.Max_Vertex_Output_Components;
        L.Max_Tessellation_Generation_Level :=
            CL.Max_Tessellation_Generation_Level;
        L.Max_Tessellation_Patch_Size := CL.Max_Tessellation_Patch_Size;
        L.Max_Tessellation_Control_Per_Vertex_Input_Components :=
            CL.Max_Tessellation_Control_Per_Vertex_Input_Components;
        L.Max_Tessellation_Control_Per_Vertex_Output_Components :=
            CL.Max_Tessellation_COntrol_Per_Vertex_Output_Components;
        L.Max_Tessellation_Control_Per_Patch_Output_Components :=
            CL.Max_Tessellation_Control_Per_Patch_Output_Components;
        L.Max_Tessellation_Control_Total_Output_Components :=
            CL.Max_Tessellation_Control_Total_Output_Components;
        L.Max_Tessellation_Evaluation_Input_Components :=
            CL.Max_Tessellation_Evaluation_Input_Components;
        L.Max_Tessellation_Evaluation_Output_Components :=
            CL.Max_Tessellation_Evaluation_Output_Components;
        L.Max_Geometry_Shader_Invocations := CL.Max_Geometry_Shader_Invocations;
        L.Max_Geometry_Input_Components := CL.Max_Geometry_Input_Components;
        L.Max_Geometry_Output_Components := CL.Max_Geometry_Output_Components;
        L.Max_Geometry_Output_Vertices := CL.Max_Geometry_Output_Vertices;
        L.Max_Geometry_Total_Output_Components :=
            CL.Max_Geometry_Total_Output_Components;
        L.Max_Fragment_Input_Components := CL.Max_Fragment_Input_Components;
        L.Max_Fragment_Output_Attachments := CL.Max_Fragment_Output_Attachments;
        L.Max_Fragment_Dual_Src_Attachments :=
            CL.Max_Fragment_Dual_Src_Attachments;
        L.Max_Fragment_Combined_Output_Resources :=
            CL.Max_Fragment_Combined_Output_Resources;
        L.Max_Compute_Shared_Memory_Size := CL.Max_Compute_Shared_Memory_Size;
        L.Max_Compute_Work_Group_Count :=
            CL.Max_Compute_Work_Group_Count(1 .. 3);
        L.Max_Compute_Work_Group_Invocations :=
            CL.Max_Compute_Work_Group_Invocations;
        L.Max_Compute_Work_Group_Size := CL.Max_Compute_Work_Group_Size(1 .. 3);
        L.Sub_Pixel_Precision_Bits := CL.Sub_Pixel_Precision_Bits;
        L.Sub_Texel_Precision_Bits := CL.Sub_Texel_Precision_Bits;
        L.Mipmap_Precision_Bits := CL.Mipmap_Precision_Bits;
        L.Max_Draw_Indexed_Index_Value := CL.Max_Draw_Indexed_Index_Value;
        L.Max_Draw_Indirect_Count := CL.Max_Draw_Indirect_Count;
        L.Max_Sampler_Lod_Bias := Float(CL.Max_Sampler_Lod_Bias);
        L.Max_Sampler_Anisotropy := Float(CL.Max_Sampler_Anisotropy);
        L.Max_Viewports := CL.Max_Viewports;
        L.Max_Viewport_Dimensions := CL.Max_Viewport_Dimensions;
        L.Viewport_Bounds_Range := To_Ada(CL.Viewport_Bounds_Range);
        L.Viewport_Sub_Pixel_Bits := CL.Viewport_Sub_Pixel_Bits;
        L.Min_Memory_Map_Alignment := CL.Min_Memory_Map_Alignment;
        L.Min_Texel_Buffer_Offset_Alignment :=
            CL.Min_Texel_Buffer_Offset_Alignment;
        L.Min_Uniform_Buffer_Offset_Alignment :=
            CL.Min_Uniform_Buffer_Offset_Alignment;
        L.Min_Storage_Buffer_Offset_Alignment :=
            CL.Min_Storage_Buffer_Offset_Alignment;
        L.Min_Texel_Offset := CL.Min_Texel_Offset;
        L.Max_Texel_Offset := CL.Max_Texel_Offset;
        L.Min_Texel_Gather_Offset := CL.Min_Texel_Gather_Offset;
        L.Max_Texel_Gather_Offset := CL.Max_Texel_Gather_Offset;
        L.Min_Interpolation_Offset := Float(CL.Min_Interpolation_Offset);
        L.Max_Interpolation_Offset := Float(CL.Max_Interpolation_Offset);
        L.Sub_Pixel_Interpolation_Offset_Bits :=
            CL.Sub_Pixel_Interpolation_Offset_Bits;
        L.Max_Framebuffer_Width := CL.Max_Framebuffer_Width;
        L.Max_Framebuffer_Height := CL.Max_Framebuffer_Height;
        L.Max_Framebuffer_Layers := CL.Max_Framebuffer_Layers;
        L.Framebuffer_Color_Sample_Counts := CL.Framebuffer_Color_Sample_Counts;
        L.Framebuffer_Depth_Sample_Counts := CL.Framebuffer_Depth_Sample_Counts;
        L.Framebuffer_Stencil_Sample_Counts :=
            CL.Framebuffer_Stencil_Sample_Counts;
        L.Framebuffer_No_Attachments_Sample_Counts :=
            CL.Framebuffer_No_Attachments_Sample_Counts;
        L.Max_Color_Attachments := CL.Max_Color_Attachments;
        L.Sampled_Image_Color_Sample_Counts :=
            CL.Sampled_Image_Color_Sample_Counts;
        L.Sampled_Image_Integer_Sample_Counts :=
            CL.Sampled_Image_Integer_Sample_Counts;
        L.Sampled_Image_Depth_Sample_Counts :=
            CL.Sampled_Image_Depth_Sample_Counts;
        L.Sampled_Image_Stencil_Sample_Counts :=
            CL.Sampled_Image_Stencil_Sample_Counts;
        L.Storage_Image_Sample_Counts := CL.Storage_Image_Sample_Counts;
        L.Max_Sample_Mask_Words := CL.Max_Sample_Mask_Words;
        L.Timestamp_Compute_And_Graphics :=
            To_Ada(CL.Timestamp_Compute_And_Graphics);
        L.Timestamp_Period := Float(CL.Timestamp_Period);
        L.Max_Clip_Distances := CL.Max_Clip_Distances;
        L.Max_Cull_Distances := CL.Max_Cull_Distances;
        L.Max_Combined_Clip_And_Cull_Distances :=
            CL.Max_Combined_Clip_And_Cull_Distances;
        L.Discrete_Queue_Priorities := CL.Discrete_Queue_Priorities;
        L.Point_Size_Range := To_Ada(CL.Point_Size_Range);
        L.Line_Width_Range := To_Ada(CL.Line_Width_Range);
        L.Point_Size_Granularity := Float(CL.Point_Size_Granularity);
        L.Line_Width_Granularity := Float(CL.Line_Width_Granularity);
        L.Strict_Lines := To_Ada(CL.Strict_Lines);
        L.Standard_Sample_Locations := To_Ada(CL.Standard_Sample_Locations);
        L.Optimal_Buffer_Copy_Offset_Alignment :=
            CL.Optimal_Buffer_Copy_Offset_Alignment;
        L.Optimal_Buffer_Copy_Row_Pitch_Alignment :=
            CL.Optimal_Buffer_Copy_Row_Pitch_Alignment;
        L.Non_Coherent_Atom_Size := CL.Non_Coherent_Atom_Size;

        SP.Residency_Standard_2D_Block_Shape :=
            To_Ada(SPC.Residency_Standard_2D_Block_Shape);
        SP.Residency_Standard_2D_Multisample_Block_Shape :=
            To_Ada(SPC.Residency_Standard_2D_Multisample_Block_Shape);
        SP.Residency_Standard_3D_Block_Shape :=
            To_Ada(SPC.Residency_Standard_3D_Block_Shape);
        SP.Residency_Aligned_Mip_Size := To_Ada(SPC.Residency_Aligned_Mip_Size);
        SP.Residency_Non_Resident_Strict :=
            To_Ada(SPC.Residency_Non_Resident_Strict);

        return PDP;
    end To_Ada;

    function To_Ada(PDMPC: in C.Physical_Device_Memory_Properties_C)
        return Physical_Device_Memory_Properties is
        PDMP: Physical_Device_Memory_Properties;
    begin
        for X in PDMPC.Memory_Types'Range loop
            PDMP.Memory_Types.Append(PDMPC.Memory_Types(X));
        end loop;

        for X in PDMPC.Memory_Heaps'Range loop
            PDMP.Memory_Heaps.Append(PDMPC.Memory_Heaps(X));
        end loop;

        return PDMP;
    end To_Ada;

    function To_C(Flag: in Boolean) return Interfaces.Unsigned_32 is
    begin
        return (if Flag then 1 else 0);
    end To_C;

    function To_Ada(Flag: in Interfaces.Unsigned_32) return Boolean is
        use type Interfaces.Unsigned_32;
    begin
        return Flag /= 0;
    end To_Ada;

    procedure To_C_Array(C_Count: out Interfaces.Unsigned_32;
                         Vector: in Vectors.Vector;
                         C_Pointer: out C_Array.Pointer) is
    begin
        C_Count := Interfaces.Unsigned_32(Vector.Length);

        if not Vector.Is_Empty then
            declare
                C_Array_Access: C_Array.Array_Access;
            begin
                C_Array_Access := C_Array.Allocate(Positive(C_Count));
                C_Pointer := C_Array_Access(1)'Access;
                
                for X in C_Array_Access'Range loop
                    C_Array_Access(X) := Vector(X);
                end loop;
            end;
        end if;
    end To_C_Array;

    procedure To_C_Array_Convert(C_Count: out Interfaces.Unsigned_32;
                                 Vector: in Vectors.Vector;
                                 C_Pointer: out C_Array.Pointer) is
    begin
        C_Count := Interfaces.Unsigned_32(Vector.Length);

        if not Vector.Is_Empty then
            declare
                C_Array_Access: C_Array.Array_Access;
            begin
                C_Array_Access := C_Array.Allocate(Positive(C_Count));
                C_Pointer := C_Array_Access(1)'Access;
                
                for X in C_Array_Access'Range loop
                    C_Array_Access(X) := To_C(Vector(X));
                end loop;
            end;
        end if;
    end To_C_Array_Convert;
    
    procedure To_C_Array_Convert_Indefinite(C_Count: out Interfaces.Unsigned_32;
                                            Vector: in Vectors.Vector;
                                            C_Pointer: out C_Array.Pointer) is
    begin
        C_Count := Interfaces.Unsigned_32(Vector.Length);

        if not Vector.Is_Empty then
            declare
                C_Array_Access: C_Array.Array_Access;
            begin
                C_Array_Access := C_Array.Allocate(Positive(C_Count));
                C_Pointer := C_Array_Access(1)'Access;
                
                for X in C_Array_Access'Range loop
                    C_Array_Access(X) := To_C(Vector(X));
                end loop;
            end;
        end if;
    end To_C_Array_Convert_Indefinite;

    function Make_In_Struct(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
        function To_In_Structure_C_Access is new Ada.Unchecked_Conversion
            (C_Structure_Access, C.In_Structure_C_Access);

        Full_Struct: Ada_Record renames Ada_Record(Next.all);
        C_Struct: C_Structure_Access;
        C_In: C.In_Structure_C_Access;
    begin
        C_Struct := new C_Structure'(To_C(Full_Struct));
        C_In := To_In_Structure_C_Access(C_Struct);
        C_In.Next := Extension_Records.To_C(Next.Next);

        return C_In;
    end Make_In_Struct;

    function Make_Out_Struct(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
        function To_Out_Structure_C_Access is new Ada.Unchecked_Conversion
            (C_Structure_Access, C.Out_Structure_C_Access);

        Full_Struct: Ada_Record renames Ada_Record(Next.all);
        C_Struct: C_Structure_Access;
        C_Out: C.Out_Structure_C_Access;
    begin
        C_Struct := new C_Structure;
        C_Out := To_Out_Structure_C_Access(C_Struct);
        C_Out.Next := Extension_Records.To_C(Next.Next);

        return C_Out;
    end Make_Out_Struct;

    procedure Free_In_Struct(Next: in out C.In_Structure_C_Access) is
        function To_Struct is new Ada.Unchecked_Conversion
            (C.In_Structure_C_Access, Struct_Access);

        procedure Free is new Ada.Unchecked_Deallocation(Struct_Type,
                                                         Struct_Access);

        Struct: Struct_Access := To_Struct(Next);
    begin
        Free(Struct.all);
        Free(Struct);
    end Free_In_Struct;

    procedure Free_Out_Struct(Next: in out C.Out_Structure_C_Access) is
        function To_Struct is new Ada.Unchecked_Conversion
            (C.Out_Structure_C_Access, Struct_Access);

        procedure Free is new Ada.Unchecked_Deallocation(Struct_Type,
                                                         Struct_Access);

        Struct: Struct_Access := To_Struct(Next);
    begin
        Free(Struct);
    end Free_Out_Struct;

    procedure Forward(Command_Buffer: in Vulkan.Command_Buffer;
                      Input: in Input_Type) is
    begin
        C_Proc(Command_Buffer, Input);
    end Forward;

    procedure Forward_Convert(Command_Buffer: in Vulkan.Command_Buffer;
                              Input: in Input_Struct) is
        Input_C: Input_Struct_C := To_C(Input);
    begin
        C_Proc(Command_Buffer, Input_C);
        Free(Input_C);
    end Forward_Convert;

    procedure Forward_Boolean(Command_Buffer: in Vulkan.Command_Buffer;
                              Flag: in Boolean) is
    begin
        C_Proc(Command_Buffer, Utilities.To_C(Flag));
    end Forward_Boolean;
end Vulkan.Utilities;


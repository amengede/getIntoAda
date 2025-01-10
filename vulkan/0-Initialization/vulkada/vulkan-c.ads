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

-- Interface to Vulkan C functions

with Ada.Unchecked_Deallocation;
with System;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Vulkan.C_Arrays;

private package Vulkan.C is
    -- void (*)() procedure pointer.
    type Void_Function is access procedure
        with Convention => C;

    -- C compatible record types.
    type In_Structure_C;
    type In_Structure_C_Access is access In_Structure_C
        with Convention => C,
             Storage_Size => 0;

    type In_Structure_C is
    record
        Record_Type: In_Structure_Type;
        Next: In_Structure_C_Access;
    end record
        with Convention => C;

    type Out_Structure_C;
    type Out_Structure_C_Access is access Out_Structure_C
        with Convention => C,
             Storage_Size => 0;

    type Out_Structure_C is
    record
        Record_Type: Out_Structure_Type;
        Next: Out_Structure_C_Access;
    end record
        with Convention => C;

    type Application_Info_C is
    record
        Record_Type: In_Structure_Type := Application_Info_Type;
        Next: In_Structure_C_Access;
        Application_Name: Interfaces.C.Strings.chars_ptr
            := Interfaces.C.Strings.Null_Ptr;
        Application_Version: Version_Number;
        Engine_Name: Interfaces.C.Strings.chars_ptr
            := Interfaces.C.Strings.Null_Ptr;
        Engine_Version: Version_Number;
        API_Version: Version_Number;
    end record
        with Convention => C;

    type Application_Info_C_Access is access Application_Info_C
        with Convention => C;

    type Instance_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Instance_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Instance_Create_Flags;
        Application_Info: Application_Info_C_Access;
        Enabled_Layer_Count: Interfaces.Unsigned_32;
        Enabled_Layer_Names: access Interfaces.C.Strings.chars_ptr;
        Enabled_Extension_Count: Interfaces.Unsigned_32;
        Enabled_Extension_Names: access Interfaces.C.Strings.chars_ptr;
    end record
        with Convention => C;

    type Instance_Create_Info_C_Access is access Instance_Create_Info_C
        with Convention => C;

    type Physical_Device_Features_C is
    record
        Robust_Buffer_Access: Interfaces.Unsigned_32;
        Full_Draw_Index_UInt32: Interfaces.Unsigned_32;
        Image_Cube_Array: Interfaces.Unsigned_32;
        Independent_Blend: Interfaces.Unsigned_32;
        Geometry_Shader: Interfaces.Unsigned_32;
        Tessellation_Shader: Interfaces.Unsigned_32;
        Sample_Rate_Shading: Interfaces.Unsigned_32;
        Dual_Src_Blend: Interfaces.Unsigned_32;
        Logic_Op: Interfaces.Unsigned_32;
        Multi_Draw_Indirect: Interfaces.Unsigned_32;
        Draw_Indirect_First_Instance: Interfaces.Unsigned_32;
        Depth_Clamp: Interfaces.Unsigned_32;
        Depth_Bias_Clamp: Interfaces.Unsigned_32;
        Fill_Mode_Non_Solid: Interfaces.Unsigned_32;
        Depth_Bounds: Interfaces.Unsigned_32;
        Wide_Lines: Interfaces.Unsigned_32;
        Large_Points: Interfaces.Unsigned_32;
        Alpha_To_One: Interfaces.Unsigned_32;
        Multi_Viewport: Interfaces.Unsigned_32;
        Sampler_Anisotropy: Interfaces.Unsigned_32;
        Texture_Compression_ETC2: Interfaces.Unsigned_32;
        Texture_Compression_ASTC_LDR: Interfaces.Unsigned_32;
        Texture_Compression_BC: Interfaces.Unsigned_32;
        Occlusion_Query_Precise: Interfaces.Unsigned_32;
        Pipeline_Statistics_Query: Interfaces.Unsigned_32;
        Vertex_Pipeline_Stores_And_Atomics: Interfaces.Unsigned_32;
        Fragment_Stores_And_Atomics: Interfaces.Unsigned_32;
        Shader_Tessellation_And_Geometry_Point_Size: Interfaces.Unsigned_32;
        Shader_Image_Gather_Extended: Interfaces.Unsigned_32;
        Shader_Storage_Image_Extended_Formats: Interfaces.Unsigned_32;
        Shader_Storage_Image_Multisample: Interfaces.Unsigned_32;
        Shader_Storage_Image_Read_Without_Format: Interfaces.Unsigned_32;
        Shader_Storage_Image_Write_Without_Format: Interfaces.Unsigned_32;
        Shader_Uniform_Buffer_Array_Dynamic_Indexing: Interfaces.Unsigned_32;
        Shader_Sampled_Image_Array_Dynamic_Indexing: Interfaces.Unsigned_32;
        Shader_Storage_Buffer_Array_Dynamic_Indexing: Interfaces.Unsigned_32;
        Shader_Storage_Image_Array_Dynamic_Indexing: Interfaces.Unsigned_32;
        Shader_Clip_Distance: Interfaces.Unsigned_32;
        Shader_Cull_Distance: Interfaces.Unsigned_32;
        Shader_Float64: Interfaces.Unsigned_32;
        Shader_Int64: Interfaces.Unsigned_32;
        Shader_Int16: Interfaces.Unsigned_32;
        Shader_Resource_Residency: Interfaces.Unsigned_32;
        Shader_Resource_Min_Lod: Interfaces.Unsigned_32;
        Sparse_Binding: Interfaces.Unsigned_32;
        Sparse_Residency_Buffer: Interfaces.Unsigned_32;
        Sparse_Residency_Image_2D: Interfaces.Unsigned_32;
        Sparse_Residency_Image_3D: Interfaces.Unsigned_32;
        Sparse_Residency_2_Samples: Interfaces.Unsigned_32;
        Sparse_Residency_4_Samples: Interfaces.Unsigned_32;
        Sparse_Residency_8_Samples: Interfaces.Unsigned_32;
        Sparse_Residency_16_Samples: Interfaces.Unsigned_32;
        Sparse_Residency_Aliased: Interfaces.Unsigned_32;
        Variable_Multisample_Rate: Interfaces.Unsigned_32;
        Inherited_Queries: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Features_C_Access is
        access Physical_Device_Features_C
        with Convention => C;
 
    type Float_Range_C is array (1 .. 2) of Interfaces.C.C_Float
        with Convention => C;

    type Physical_Device_Limits_C is
    record
        Max_Image_Dimension_1D: Interfaces.Unsigned_32;
        Max_Image_Dimension_2D: Interfaces.Unsigned_32;
        Max_Image_Dimension_3D: Interfaces.Unsigned_32;
        Max_Image_Dimension_Cube: Interfaces.Unsigned_32;
        Max_Image_Array_Layers: Array_Layers;
        Max_Texel_Buffer_Elements: Interfaces.Unsigned_32;
        Max_Uniform_Buffer_Range: Interfaces.Unsigned_32;
        Max_Storage_Buffer_Range: Interfaces.Unsigned_32;
        Max_Push_Constants_Size: Interfaces.Unsigned_32;
        Max_Memory_Allocation_Count: Interfaces.Unsigned_32;
        Max_Sampler_Allocation_Count: Interfaces.Unsigned_32;
        Buffer_Image_Granularity: Device_Size;
        Sparse_Address_Space_Size: Device_Size;
        Max_Bound_Descriptor_Sets: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Samplers: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Uniform_Buffers: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Storage_Buffers: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Sampled_Images: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Storage_Images: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Input_Attachments: Interfaces.Unsigned_32;
        Max_Per_Stage_Resources: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Samplers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Uniform_Buffers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Uniform_Buffers_Dynamic: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Storage_Buffers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Storage_Buffers_Dynamic: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Sampled_Images: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Storage_Images: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Input_Attachments: Interfaces.Unsigned_32;
        Max_Vertex_Input_Attributes: Interfaces.Unsigned_32;
        Max_Vertex_Input_Bindings: Interfaces.Unsigned_32;
        Max_Vertex_Input_Attribute_Offset: Interfaces.Unsigned_32;
        Max_Vertex_Input_Binding_Stride: Interfaces.Unsigned_32;
        Max_Vertex_Output_Components: Interfaces.Unsigned_32;
        Max_Tessellation_Generation_Level: Interfaces.Unsigned_32;
        Max_Tessellation_Patch_Size: Interfaces.Unsigned_32;
        Max_Tessellation_Control_Per_Vertex_Input_Components:
            Interfaces.Unsigned_32;
        Max_Tessellation_Control_Per_Vertex_Output_Components:
            Interfaces.Unsigned_32;
        Max_Tessellation_Control_Per_Patch_Output_Components:
            Interfaces.Unsigned_32;
        Max_Tessellation_Control_Total_Output_Components:
            Interfaces.Unsigned_32;
        Max_Tessellation_Evaluation_Input_Components: Interfaces.Unsigned_32;
        Max_Tessellation_Evaluation_Output_Components: Interfaces.Unsigned_32;
        Max_Geometry_Shader_Invocations: Interfaces.Unsigned_32;
        Max_Geometry_Input_Components: Interfaces.Unsigned_32;
        Max_Geometry_Output_Components: Interfaces.Unsigned_32;
        Max_Geometry_Output_Vertices: Interfaces.Unsigned_32;
        Max_Geometry_Total_Output_Components: Interfaces.Unsigned_32;
        Max_Fragment_Input_Components: Interfaces.Unsigned_32;
        Max_Fragment_Output_Attachments: Interfaces.Unsigned_32;
        Max_Fragment_Dual_Src_Attachments: Interfaces.Unsigned_32;
        Max_Fragment_Combined_Output_Resources: Interfaces.Unsigned_32;
        Max_Compute_Shared_Memory_Size: Interfaces.Unsigned_32;
        Max_Compute_Work_Group_Count: Work_Group_Array;
        Max_Compute_Work_Group_Invocations: Interfaces.Unsigned_32;
        Max_Compute_Work_Group_Size: Work_Group_Array;
        Sub_Pixel_Precision_Bits: Interfaces.Unsigned_32;
        Sub_Texel_Precision_Bits: Interfaces.Unsigned_32;
        Mipmap_Precision_Bits: Interfaces.Unsigned_32;
        Max_Draw_Indexed_Index_Value: Interfaces.Unsigned_32;
        Max_Draw_Indirect_Count: Interfaces.Unsigned_32;
        Max_Sampler_Lod_Bias: Interfaces.C.C_Float;
        Max_Sampler_Anisotropy: Interfaces.C.C_Float;
        Max_Viewports: Interfaces.Unsigned_32;
        Max_Viewport_Dimensions: Viewport_Dimensions;
        Viewport_Bounds_Range: Float_Range_C;
        Viewport_Sub_Pixel_Bits: Interfaces.Unsigned_32;
        Min_Memory_Map_Alignment: Interfaces.C.size_t;
        Min_Texel_Buffer_Offset_Alignment: Device_Size;
        Min_Uniform_Buffer_Offset_Alignment: Device_Size;
        Min_Storage_Buffer_Offset_Alignment: Device_Size;
        Min_Texel_Offset: Interfaces.Integer_32;
        Max_Texel_Offset: Interfaces.Unsigned_32;
        Min_Texel_Gather_Offset: Interfaces.Integer_32;
        Max_Texel_Gather_Offset: Interfaces.Unsigned_32;
        Min_Interpolation_Offset: Interfaces.C.C_Float;
        Max_Interpolation_Offset: Interfaces.C.C_Float;
        Sub_Pixel_Interpolation_Offset_Bits: Interfaces.Unsigned_32;
        Max_Framebuffer_Width: Interfaces.Unsigned_32;
        Max_Framebuffer_Height: Interfaces.Unsigned_32;
        Max_Framebuffer_Layers: Interfaces.Unsigned_32;
        Framebuffer_Color_Sample_Counts: Sample_Count_Flags;
        Framebuffer_Depth_Sample_Counts: Sample_Count_Flags;
        Framebuffer_Stencil_Sample_Counts: Sample_Count_Flags;
        Framebuffer_No_Attachments_Sample_Counts: Sample_Count_Flags;
        Max_Color_Attachments: Interfaces.Unsigned_32;
        Sampled_Image_Color_Sample_Counts: Sample_Count_Flags;
        Sampled_Image_Integer_Sample_Counts: Sample_Count_Flags;
        Sampled_Image_Depth_Sample_Counts: Sample_Count_Flags;
        Sampled_Image_Stencil_Sample_Counts: Sample_Count_Flags;
        Storage_Image_Sample_Counts: Sample_Count_Flags;
        Max_Sample_Mask_Words: Interfaces.Unsigned_32;
        Timestamp_Compute_And_Graphics: Interfaces.Unsigned_32;
        Timestamp_Period: Interfaces.C.C_Float;
        Max_Clip_Distances: Interfaces.Unsigned_32;
        Max_Cull_Distances: Interfaces.Unsigned_32;
        Max_Combined_Clip_And_Cull_Distances: Interfaces.Unsigned_32;
        Discrete_Queue_Priorities: Interfaces.Unsigned_32;
        Point_Size_Range: Float_Range_C;
        Line_Width_Range: Float_Range_C;
        Point_Size_Granularity: Interfaces.C.C_Float;
        Line_Width_Granularity: Interfaces.C.C_Float;
        Strict_Lines: Interfaces.Unsigned_32;
        Standard_Sample_Locations: Interfaces.Unsigned_32;
        Optimal_Buffer_Copy_Offset_Alignment: Device_Size;
        Optimal_Buffer_Copy_Row_Pitch_Alignment: Device_Size;
        Non_Coherent_Atom_Size: Device_Size;
    end record
        with Convention => C;

    type Physical_Device_Sparse_Properties_C is
    record
        Residency_Standard_2D_Block_Shape: Interfaces.Unsigned_32;
        Residency_Standard_2D_Multisample_Block_Shape: Interfaces.Unsigned_32;
        Residency_Standard_3D_Block_Shape: Interfaces.Unsigned_32;
        Residency_Aligned_Mip_Size: Interfaces.Unsigned_32;
        Residency_Non_Resident_Strict: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Properties_C is
    record
        API_Version: Version_Number;
        Driver_Version: Version_Number;
        Vendor_ID: Interfaces.Unsigned_32;
        Device_ID: Interfaces.Unsigned_32;
        Device_Type: Physical_Device_Type;
        Device_Name: Interfaces.C.char_array
            (1 .. Max_Physical_Device_Name_Size);
        Pipeline_Cache_UUID: UUID;
        Limits: Physical_Device_Limits_C;
        Sparse_Properties: Physical_Device_Sparse_Properties_C;
    end record
        with Convention => C;

    type Memory_Type_Array_C is array (1 .. Max_Memory_Types) of Memory_Type
        with Convention => C;
    type Memory_Heap_Array_C is array (1 .. Max_Memory_Heaps) of Memory_Heap
        with Convention => C;

    type Physical_Device_Memory_Properties_C is
    record
        Type_Count: Interfaces.Unsigned_32;
        Memory_Types: Memory_Type_Array_C;
        Heap_Count: Interfaces.Unsigned_32;
        Memory_Heaps: Memory_Heap_Array_C;
    end record
        with Convention => C;

    package Float_Arrays is new C_Arrays(Interfaces.C.C_float);

    type Device_Queue_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Queue_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Device_Queue_Create_Flags;
        Queue_Family_Index: Vulkan.Queue_Family_Index;
        Count: Interfaces.Unsigned_32;
        Priorities: Float_Arrays.Pointer;
    end record
        with Convention => C;

    type Device_Queue_Create_Info_C_Access is
        access Device_Queue_Create_Info_C
        with Convention => C;

    package Device_Queue_Create_Info_C_Arrays is
        new C_Arrays(Device_Queue_Create_Info_C);

    type Device_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Device_Create_Flags;
        Queue_Create_Info_Count: Interfaces.Unsigned_32;
        Queue_Create_Infos: Device_Queue_Create_Info_C_Arrays.Pointer;
        Enabled_Layer_Count: Interfaces.Unsigned_32;
        Enabled_Layer_Names: access Interfaces.C.Strings.chars_ptr;
        Enabled_Extension_Count: Interfaces.Unsigned_32;
        Enabled_Extension_Names: access Interfaces.C.Strings.chars_ptr;
        Enabled_Features: Physical_Device_Features_C_Access;
    end record
        with Convention => C;

    type Device_Create_Info_C_Access is access Device_Create_Info_C
        with Convention => C;

    type Layer_Properties_C is
    record
        Layer_Name: Interfaces.C.char_array(1 .. Max_Extension_Name_Size);
        Spec_Version: Version_Number;
        Implementation_Version: Version_Number;
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
    end record
        with Convention => C;

    type Extension_Properties_C is
    record
        Extension_Name: Interfaces.C.char_array(1 .. Max_Extension_Name_Size);
        Spec_Version: Version_Number;
    end record
        with Convention => C;

    type Extension_Properties_C_Access is access Extension_Properties_C
        with Convention => C;

    package Semaphore_Arrays is new C_Arrays(Semaphore);
    package Pipeline_Stage_Flags_Arrays is new C_Arrays(Pipeline_Stage_Flags);
    package Command_Buffer_Arrays is new C_Arrays(Command_Buffer);

    type Submit_Info_C is
    record
        Record_Type: In_Structure_Type := Submit_Info_Type;
        Next: In_Structure_C_Access;
        Wait_Semaphore_Count: Interfaces.Unsigned_32;
        Wait_Semaphores: Semaphore_Arrays.Pointer;
        Wait_Dst_Stage_Mask: Pipeline_Stage_Flags_Arrays.Pointer;
        Command_Buffer_Count: Interfaces.Unsigned_32;
        Command_Buffers: Command_Buffer_Arrays.Pointer;
        Signal_Semaphore_Count: Interfaces.Unsigned_32;
        Signal_Semaphores: Semaphore_Arrays.Pointer;
    end record
        with Convention => C;

    type Submit_Info_C_Access is access Submit_Info_C
        with Convention => C;

    type Memory_Allocate_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Allocate_Info_Type;
        Next: In_Structure_C_Access;
        Allocation_Size: Device_Size;
        Memory_Type_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Memory_Allocate_Info_C_Access is access Memory_Allocate_Info_C
        with Convention => C;

    type Mapped_Memory_Range_C is
    record
        Record_Type: In_Structure_Type := Mapped_Memory_Range_Type;
        Next: In_Structure_C_Access;
        Memory: Device_Memory;
        Offset: Device_Size;
        Size: Device_Size;
    end record
        with Convention => C;

    type Mapped_Memory_Range_C_Access is access Mapped_Memory_Range_C
        with Convention => C;

    package Sparse_Memory_Bind_Arrays is new C_Arrays(Sparse_Memory_Bind);

    type Sparse_Buffer_Memory_Bind_Info_C is
    record
        Buffer: Vulkan.Buffer;
        Bind_Count: Interfaces.Unsigned_32;
        Binds: Sparse_Memory_Bind_Arrays.Pointer;
    end record
        with Convention => C;

    type Sparse_Image_Opaque_Memory_Bind_Info_C is
    record
        Image: Vulkan.Image;
        Bind_Count: Interfaces.Unsigned_32;
        Binds: Sparse_Memory_Bind_Arrays.Pointer;
    end record
        with Convention => C;

    package Sparse_Image_Memory_Bind_Arrays is
        new C_Arrays(Sparse_Image_Memory_Bind);

    type Sparse_Image_Memory_Bind_Info_C is
    record
        Image: Vulkan.Image;
        Bind_Count: Interfaces.Unsigned_32;
        Binds: Sparse_Image_Memory_Bind_Arrays.Pointer;
    end record
        with Convention => C;

    package Sparse_Buffer_Memory_Bind_Info_Arrays is
        new C_Arrays(Sparse_Buffer_Memory_Bind_Info_C);
    package Sparse_Image_Opaque_Memory_Bind_Info_Arrays is
        new C_Arrays(Sparse_Image_Opaque_Memory_Bind_Info_C);
    package Sparse_Image_Memory_Bind_Info_Arrays is
        new C_Arrays(Sparse_Image_Memory_Bind_Info_C);

    type Bind_Sparse_Info_C is
    record
        Record_Type: In_Structure_Type := Bind_Sparse_Info_Type;
        Next: In_Structure_C_Access;
        Wait_Semaphore_Count: Interfaces.Unsigned_32;
        Wait_Semaphores: Semaphore_Arrays.Pointer;
        Buffer_Bind_Count: Interfaces.Unsigned_32;
        Buffer_Binds: Sparse_Buffer_Memory_Bind_Info_Arrays.Pointer;
        Image_Opaque_Bind_Count: Interfaces.Unsigned_32;
        Image_Opaque_Binds: Sparse_Image_Opaque_Memory_Bind_Info_Arrays.Pointer;
        Image_Bind_Count: Interfaces.Unsigned_32;
        Image_Binds: Sparse_Image_Memory_Bind_Info_Arrays.Pointer;
        Signal_Semaphore_Count: Interfaces.Unsigned_32;
        Signal_Semaphores: Semaphore_Arrays.Pointer;
    end record
        with Convention => C;

    type Bind_Sparse_Info_C_Access is access Bind_Sparse_Info_C
        with Convention => C;

    type Bind_Sparse_Info_Array is
        array (Natural range <>) of aliased Bind_Sparse_Info_C
        with Convention => C;

    type Fence_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Fence_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Fence_Create_Flags;
    end record
        with Convention => C;

    type Fence_Create_Info_C_Access is access Fence_Create_Info_C
        with Convention => C;

    type Semaphore_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Semaphore_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Semaphore_Create_Flags;
    end record
        with Convention => C;

    type Semaphore_Create_Info_C_Access is access Semaphore_Create_Info_C
        with Convention => C;

    type Event_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Event_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Event_Create_Flags;
    end record
        with Convention => C;

    type Event_Create_Info_C_Access is access Event_Create_Info_C
        with Convention => C;

    type Query_Pool_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Query_Pool_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Query_Pool_Create_Flags;
        Query_Type: Vulkan.Query_Type;
        Query_Count: Interfaces.Unsigned_32;
        Pipeline_Statistics: Query_Pipeline_Statistic_Flags;
    end record
        with Convention => C;

    type Query_Pool_Create_Info_C_Access is access Query_Pool_Create_Info_C
        with Convention => C;

    package Queue_Family_Index_Arrays is new C_Arrays(Queue_Family_Index);

    type Buffer_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Buffer_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Buffer_Create_Flags;
        Size: Device_Size;
        Usage: Buffer_Usage_Flags;
        Sharing_Mode: Vulkan.Sharing_Mode;
        Queue_Family_Index_Count: Interfaces.Unsigned_32;
        Queue_Family_Indices: Queue_Family_Index_Arrays.Pointer;
    end record
        with Convention => C;

    type Buffer_Create_Info_C_Access is access Buffer_Create_Info_C
        with Convention => C;

    type Buffer_View_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Buffer_View_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Buffer_View_Create_Flags;
        Buffer: Vulkan.Buffer;
        Format: Vulkan.Format;
        Offset: Device_Size;
        View_Range: Device_Size;
    end record
        with Convention => C;

    type Buffer_View_Create_Info_C_Access is access Buffer_View_Create_Info_C
        with Convention => C;

    type Image_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Image_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Image_Create_Flags;
        Image_Type: Vulkan.Image_Type;
        Format: Vulkan.Format;
        Extent: Extent_3D;
        Mip_Levels: Interfaces.Unsigned_32;
        Array_Layers: Interfaces.Unsigned_32;
        Samples: Sample_Count_Flags;
        Tiling: Image_Tiling;
        Usage: Image_Usage_Flags;
        Sharing_Mode: Vulkan.Sharing_Mode;
        Queue_Family_Index_Count: Interfaces.Unsigned_32;
        Queue_Family_Indices: Queue_Family_Index_Arrays.Pointer;
        Initial_Layout: Image_Layout;
    end record
        with Convention => C;

    type Image_Create_Info_C_Access is access Image_Create_Info_C
        with Convention => C;

    type Image_View_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Image_View_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Image_View_Create_Flags;
        Image: Vulkan.Image;
        View_Type: Image_View_Type;
        Format: Vulkan.Format;
        Components: Component_Mapping;
        Subresource_Range: Image_Subresource_Range;
    end record
        with Convention => C;

    type Image_View_Create_Info_C_Access is
        access Image_View_Create_Info_C
        with Convention => C;

    package Uint32_t_Arrays is new C_Arrays(Interfaces.Unsigned_32);

    type Shader_Module_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Shader_Module_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Shader_Module_Create_Flags;
        Code_Size: Interfaces.C.size_t;
        Code: Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Shader_Module_Create_Info_C_Access is
        access Shader_Module_Create_Info_C
        with Convention => C;

    type Pipeline_Cache_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Cache_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Cache_Create_Flags;
        Initial_Data_Size: Interfaces.C.size_t;
        Initial_Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Pipeline_Cache_Create_Info_C_Access is
        access Pipeline_Cache_Create_Info_C
        with Convention => C;

    package Specialization_Map_Entry_Arrays is
        new C_Arrays(Specialization_Map_Entry);

    type Specialization_Info_C is
    record
        Map_Entry_Count: Interfaces.Unsigned_32;
        Map_Entries: Specialization_Map_Entry_Arrays.Pointer;
        Data_Size: Interfaces.C.size_t;
        Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Specialization_Info_C_Access is
        access Specialization_Info_C
        with Convention => C;

    type Pipeline_Shader_Stage_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Shader_Stage_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Shader_Stage_Create_Flags;
        Stage: Shader_Stage_Flags;
        Module: Shader_Module;
        Name: Interfaces.C.Strings.chars_ptr;
        Specialization_Info: Specialization_Info_C_Access;
    end record
        with Convention => C;

    type Pipeline_Shader_Stage_Create_Info_C_Access is
        access Pipeline_Shader_Stage_Create_Info_C
        with Convention => C;

    package Vertex_Input_Binding_Description_Arrays is
        new C_Arrays(Vertex_Input_Binding_Description);

    package Vertex_Input_Attribute_Description_Arrays is
        new C_Arrays(Vertex_Input_Attribute_Description);

    type Pipeline_Vertex_Input_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Vertex_Input_State_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Vertex_Input_State_Create_Flags;
        Vertex_Binding_Description_Count: Interfaces.Unsigned_32;
        Vertex_Binding_Descriptions:
            Vertex_Input_Binding_Description_Arrays.Pointer;
        Vertex_Attribute_Description_Count: Interfaces.Unsigned_32;
        Vertex_Attribute_Descriptions:
            Vertex_Input_Attribute_Description_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Vertex_Input_State_Create_Info_C_Access is
        access Pipeline_Vertex_Input_State_Create_Info_C
        with Convention => C;

    type Pipeline_Input_Assembly_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Input_Assembly_State_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Input_Assembly_State_Create_Flags;
        Topology: Primitive_Topology;
        Primitive_Restart_Enable: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Input_Assembly_State_Create_Info_C_Access is
        access Pipeline_Input_Assembly_State_Create_Info_C
        with Convention => C;

    type Pipeline_Tessellation_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Tessellation_State_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Tessellation_State_Create_Flags;
        Patch_Control_Points: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Tessellation_State_Create_Info_C_Access is
        access Pipeline_Tessellation_State_Create_Info_C
        with Convention => C;

    package Viewport_Arrays is new C_Arrays(Viewport);
    package Rect_2D_Arrays is new C_Arrays(Rect_2D);

    type Pipeline_Viewport_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Viewport_State_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Viewport_State_Create_Flags;
        Viewport_Count: Interfaces.Unsigned_32;
        Viewports: Viewport_Arrays.Pointer;
        Scissor_Count: Interfaces.Unsigned_32;
        Scissors: Rect_2D_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Viewport_State_Create_Info_C_Access is
        access Pipeline_Viewport_State_Create_Info_C
        with Convention => C;

    type Pipeline_Rasterization_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Rasterization_State_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Rasterization_State_Create_Flags;
        Depth_Clamp_Enable: Interfaces.Unsigned_32;
        Rasterizer_Discard_Enable: Interfaces.Unsigned_32;
        Polygon_Mode: Vulkan.Polygon_Mode;
        Cull_Mode: Cull_Mode_Flags;
        Front_Face: Vulkan.Front_Face;
        Depth_Bias_Enable: Interfaces.Unsigned_32;
        Depth_Bias_Constant_Factor: Interfaces.C.C_Float;
        Depth_Bias_Clamp: Interfaces.C.C_Float;
        Depth_Bias_Slope_Factor: Interfaces.C.C_Float;
        Line_Width: Interfaces.C.C_Float;
    end record
        with Convention => C;

    type Pipeline_Rasterization_State_Create_Info_C_Access is
        access Pipeline_Rasterization_State_Create_Info_C
        with Convention => C;

    package Sample_Mask_Arrays is new C_Arrays(Sample_Mask);

    type Pipeline_Multisample_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Multisample_State_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Multisample_State_Create_Flags;
        Rasterization_Samples: Sample_Count_Flags;
        Sample_Shading_Enable: Interfaces.Unsigned_32;
        Min_Sample_Shading: Interfaces.C.C_Float;
        Sample_Mask: Sample_Mask_Arrays.Pointer;
        Alpha_To_Converage_Enable: Interfaces.Unsigned_32;
        Alpha_To_One_Enable: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Multisample_State_Create_Info_C_Access is
        access Pipeline_Multisample_State_Create_Info_C
        with Convention => C;

    type Pipeline_Depth_Stencil_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Depth_Stencil_State_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Depth_Stencil_State_Create_Flags;
        Depth_Test_Enable: Interfaces.Unsigned_32;
        Depth_Write_Enable: Interfaces.Unsigned_32;
        Depth_Compare_Op: Compare_Op;
        Depth_Bounds_Test_Enable: Interfaces.Unsigned_32;
        Stencil_Test_Enable: Interfaces.Unsigned_32;
        Front: Stencil_Op_State;
        Back: Stencil_Op_State;
        Min_Depth_Bounds: Interfaces.C.C_Float;
        Max_Depth_Bounds: Interfaces.C.C_Float;
    end record
        with Convention => C;

    type Pipeline_Depth_Stencil_State_Create_Info_C_Access is
        access Pipeline_Depth_Stencil_State_Create_Info_C
        with Convention => C;

    type Pipeline_Color_Blend_Attachment_State_C is
    record
        Blend_Enable: Interfaces.Unsigned_32;
        Src_Color_Blend_Factor: Blend_Factor;
        Dst_Color_Blend_Factor: Blend_Factor;
        Color_Blend_Op: Blend_Op;
        Src_Alpha_Blend_Factor: Blend_Factor;
        Dst_Alpha_Blend_Factor: Blend_Factor;
        Alpha_Blend_Op: Blend_Op;
        Color_Write_Mask: Color_Component_Flags;
    end record
        with Convention => C;

    package Pipeline_Color_Blend_Attachment_State_C_Arrays is
        new C_Arrays(Pipeline_Color_Blend_Attachment_State_C);
    
    type Pipeline_Color_Blend_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Color_Blend_State_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Color_Blend_State_Create_Flags;
        Logic_Op_Enable: Interfaces.Unsigned_32;
        Logic_Op: Vulkan.Logic_Op;
        Attachment_Count: Interfaces.Unsigned_32;
        Attachments: Pipeline_Color_Blend_Attachment_State_C_Arrays.Pointer;
        Blend_Constants: Blend_Constants_Array;
    end record
        with Convention => C;

    type Pipeline_Color_Blend_State_Create_Info_C_Access is
        access Pipeline_Color_Blend_State_Create_Info_C
        with Convention => C;

    package Dynamic_State_Arrays is new C_Arrays(Dynamic_State);
    
    type Pipeline_Dynamic_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Dynamic_State_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Dynamic_State_Create_Flags;
        Dynamic_State_Count: Interfaces.Unsigned_32;
        Dynamic_States: Dynamic_State_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Dynamic_State_Create_Info_C_Access is
        access Pipeline_Dynamic_State_Create_Info_C
        with Convention => C;

    package Pipeline_Shader_Stage_Create_Info_C_Arrays is
        new C_Arrays(Pipeline_Shader_Stage_Create_Info_C);
    
    type Graphics_Pipeline_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Graphics_Pipeline_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Create_Flags;
        Stage_Count: Interfaces.Unsigned_32;
        Stages: Pipeline_Shader_Stage_Create_Info_C_Arrays.Pointer;
        Vertex_Input_State: Pipeline_Vertex_Input_State_Create_Info_C_Access;
        Input_Assembly_State:
            Pipeline_Input_Assembly_State_Create_Info_C_Access;
        Tessellation_State: Pipeline_Tessellation_State_Create_Info_C_Access;
        Viewport_State: Pipeline_Viewport_State_Create_Info_C_Access;
        Rasterization_State: Pipeline_Rasterization_State_Create_Info_C_Access;
        Multisample_State: Pipeline_Multisample_State_Create_Info_C_Access;
        Depth_Stencil_State: Pipeline_Depth_Stencil_State_Create_Info_C_Access;
        Color_Blend_State: Pipeline_Color_Blend_State_Create_Info_C_Access;
        Dynamic_State: Pipeline_Dynamic_State_Create_Info_C_Access;
        Layout: Pipeline_Layout;
        Render_Pass: Vulkan.Render_Pass;
        Subpass: Interfaces.Unsigned_32;
        Base_Pipeline_Handle: Pipeline;
        Base_Pipeline_Index: Interfaces.Integer_32;
    end record
        with Convention => C;

    type Graphics_Pipeline_Create_Info_C_Access is
        access Graphics_Pipeline_Create_Info_C
        with Convention => C;

    type Compute_Pipeline_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Compute_Pipeline_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Create_Flags;
        Stage: Pipeline_Shader_Stage_Create_Info_C;
        Layout: Pipeline_Layout;
        Base_Pipeline_Handle: Pipeline;
        Base_Pipeline_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Compute_Pipeline_Create_Info_C_Access is
        access Compute_Pipeline_Create_Info_C
        with Convention => C;

    package Descriptor_Set_Layout_Arrays is
        new C_Arrays(Descriptor_Set_Layout);
    
    package Push_Constant_Range_Arrays is
        new C_Arrays(Push_Constant_Range);

    type Pipeline_Layout_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Layout_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Pipeline_Layout_Create_Flags;
        Set_Layout_Count: Interfaces.Unsigned_32;
        Set_Layouts: Descriptor_Set_Layout_Arrays.Pointer;
        Push_Constant_Range_Count: Interfaces.Unsigned_32;
        Push_Constant_Ranges: Push_Constant_Range_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Layout_Create_Info_C_Access is
        access Pipeline_Layout_Create_Info_C
        with Convention => C;

    type Sampler_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Sampler_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Sampler_Create_Flags;
        Mag_Filter: Filter;
        Min_Filter: Filter;
        Mipmap_Mode: Sampler_Mipmap_Mode;
        Address_Mode_U: Sampler_Address_Mode;
        Address_Mode_V: Sampler_Address_Mode;
        Address_Mode_W: Sampler_Address_Mode;
        Mip_Lod_Bias: Interfaces.C.C_Float;
        Anisotropy_Enable: Interfaces.Unsigned_32;
        Max_Anisotropy: Interfaces.C.C_Float;
        Compare_Enable: Interfaces.Unsigned_32;
        Compare_Op: Vulkan.Compare_Op;
        Min_Lod: Interfaces.C.C_Float;
        Max_Lod: Interfaces.C.C_Float;
        Border_Color: Vulkan.Border_Color;
        Unnormalized_Coordinates: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Sampler_Create_Info_C_Access is
        access Sampler_Create_Info_C
        with Convention => C;

    package Sampler_Arrays is new C_Arrays(Sampler);

    type Descriptor_Set_Layout_Binding_C is
    record
        Binding: Interfaces.Unsigned_32;
        Descriptor_Type: Vulkan.Descriptor_Type;
        Descriptor_Count: Interfaces.Unsigned_32;
        Stage_Flags: Shader_Stage_Flags;
        Immutable_Samplers: Sampler_Arrays.Pointer;
    end record
        with Convention => C;

    package Descriptor_Set_Layout_Binding_C_Arrays is
        new C_Arrays(Descriptor_Set_Layout_Binding_C);

    type Descriptor_Set_Layout_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Descriptor_Set_Layout_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Descriptor_Set_Layout_Create_Flags;
        Binding_Count: Interfaces.Unsigned_32;
        Bindings: Descriptor_Set_Layout_Binding_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Descriptor_Set_Layout_Create_Info_C_Access is
        access Descriptor_Set_Layout_Create_Info_C
        with Convention => C;

    package Descriptor_Pool_Size_Arrays is
        new C_Arrays(Descriptor_Pool_Size);

    type Descriptor_Pool_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Descriptor_Pool_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Descriptor_Pool_Create_Flags;
        Max_Sets: Interfaces.Unsigned_32;
        Pool_Size_Count: Interfaces.Unsigned_32;
        Pool_Sizes: Descriptor_Pool_Size_Arrays.Pointer;
    end record
        with Convention => C;

    type Descriptor_Pool_Create_Info_C_Access is
        access Descriptor_Pool_Create_Info_C
        with Convention => C;

    type Descriptor_Set_Allocate_Info_C is
    record
        Record_Type: In_Structure_Type := Descriptor_Set_Allocate_Info_Type;
        Next: In_Structure_C_Access;
        Descriptor_Pool: Vulkan.Descriptor_Pool;
        Descriptor_Set_Count: Interfaces.Unsigned_32;
        Set_Layouts: Descriptor_Set_Layout_Arrays.Pointer;
    end record
        with Convention => C;

    type Descriptor_Set_Allocate_Info_C_Access is
        access Descriptor_Set_Allocate_Info_C
        with Convention => C;

    package Descriptor_Image_Info_Arrays is
        new C_Arrays(Descriptor_Image_Info);

    package Descriptor_Buffer_Info_Arrays is
        new C_Arrays(Descriptor_Buffer_Info);

    package Buffer_View_Arrays is new C_Arrays(Buffer_View);

    type Write_Descriptor_Set_C is
    record
        Record_Type: In_Structure_Type := Write_Descriptor_Set_Type;
        Next: In_Structure_C_Access;
        Dst_Set: Descriptor_Set;
        Dst_Binding: Interfaces.Unsigned_32;
        Dst_Array_Element: Interfaces.Unsigned_32;
        Descriptor_Count: Interfaces.Unsigned_32;
        Descriptor_Type: Vulkan.Descriptor_Type;
        Image_Info: Descriptor_Image_Info_Arrays.Pointer;
        Buffer_Info: Descriptor_Buffer_Info_Arrays.Pointer;
        Texel_Buffer_View: Buffer_View_Arrays.Pointer;
    end record
        with Convention => C;

    type Write_Descriptor_Set_C_Access is access Write_Descriptor_Set_C
        with Convention => C;

    type Copy_Descriptor_Set_C is
    record
        Record_Type: In_Structure_Type := Copy_Descriptor_Set_Type;
        Next: In_Structure_C_Access;
        Src_Set: Descriptor_Set;
        Src_Binding: Interfaces.Unsigned_32;
        Src_Array_Element: Interfaces.Unsigned_32;
        Dst_Set: Descriptor_Set;
        Dst_Binding: Interfaces.Unsigned_32;
        Dst_Array_Element: Interfaces.Unsigned_32;
        Descriptor_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Copy_Descriptor_Set_C_Access is access Copy_Descriptor_Set_C
        with Convention => C;

    package Image_View_Arrays is new C_Arrays(Image_View);

    type Framebuffer_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Framebuffer_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Framebuffer_Create_Flags;
        Render_Pass: Vulkan.Render_Pass;
        Attachment_Count: Interfaces.Unsigned_32;
        Attachments: Image_View_Arrays.Pointer;
        Width: Vulkan.Width;
        Height: Vulkan.Height;
        Layers: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Framebuffer_Create_Info_C_Access is
        access Framebuffer_Create_Info_C
        with Convention => C;

    package Attachment_Reference_Arrays is new C_Arrays(Attachment_Reference);

    type Subpass_Description_C is
    record
        Flags: Subpass_Description_Flags;
        Pipeline_Bind_Point: Vulkan.Pipeline_Bind_Point := Graphics;
        Input_Attachment_Count: Interfaces.Unsigned_32;
        Input_Attachments: Attachment_Reference_Arrays.Pointer;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachments: Attachment_Reference_Arrays.Pointer;
        Resolve_Attachments: Attachment_Reference_Arrays.Pointer;
        Depth_Stencil_Attachment: Attachment_Reference_Access;
        Preserve_Attachment_Count: Interfaces.Unsigned_32;
        Preserve_Attachments: Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    package Attachment_Description_Arrays is
        new C_Arrays(Attachment_Description);

    package Subpass_Description_C_Arrays is
        new C_Arrays(Subpass_Description_C);

    package Subpass_Dependency_Arrays is
        new C_Arrays(Subpass_Dependency);

    type Render_Pass_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Render_Pass_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Render_Pass_Create_Flags;
        Attachment_Count: Interfaces.Unsigned_32;
        Attachments: Attachment_Description_Arrays.Pointer;
        Subpass_Count: Interfaces.Unsigned_32;
        Subpasses: Subpass_Description_C_Arrays.Pointer;
        Dependency_Count: Interfaces.Unsigned_32;
        Dependencies: Subpass_Dependency_Arrays.Pointer;
    end record
        with Convention => C;

    type Render_Pass_Create_Info_C_Access is
        access Render_Pass_Create_Info_C
        with Convention => C;

    type Command_Pool_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Command_Pool_Create_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Command_Pool_Create_Flags;
        Queue_Family_Index: Vulkan.Queue_Family_Index;
    end record
        with Convention => C;

    type Command_Pool_Create_Info_C_Access is
        access Command_Pool_Create_Info_C
        with Convention => C;

    type Command_Buffer_Allocate_Info_C is
    record
        Record_Type: In_Structure_Type := Command_Buffer_Allocate_Info_Type;
        Next: In_Structure_C_Access;
        Command_Pool: Vulkan.Command_Pool;
        Level: Command_Buffer_Level;
        Command_Buffer_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Command_Buffer_Allocate_Info_C_Access is
        access Command_Buffer_Allocate_Info_C
        with Convention => C;
        
    type Command_Buffer_Inheritance_Info_C is
    record
        Record_Type: In_Structure_Type := Command_Buffer_Inheritance_Info_Type;
        Next: In_Structure_C_Access;
        Render_Pass: Vulkan.Render_Pass;
        Subpass: Interfaces.Unsigned_32;
        Framebuffer: Vulkan.Framebuffer;
        Occlusion_Query_Enable: Interfaces.Unsigned_32;
        Query_Flags: Query_Control_Flags;
        Pipeline_Statistics: Query_Pipeline_Statistic_Flags;
    end record
        with Convention => C;

    type Command_Buffer_Inheritance_Info_C_Access is
        access Command_Buffer_Inheritance_Info_C
        with Convention => C;

    type Command_Buffer_Begin_Info_C is
    record
        Record_Type: In_Structure_Type := Command_Buffer_Begin_Info_Type;
        Next: In_Structure_C_Access;
        Flags: Command_Buffer_Usage_Flags;
        Inheritance_Info: Command_Buffer_Inheritance_Info_C_Access;
    end record
        with Convention => C;

    type Command_Buffer_Begin_Info_C_Access is
        access Command_Buffer_Begin_Info_C
        with Convention => C;

    type Clear_Color_Value_C(Color_Type: Clear_Color_Type) is
    record
        case Color_Type is
            when Clear_Color_Float =>
                Float_Color: Float_Clear_Color;
            when Clear_Color_Integer =>
                Integer_Color: Integer_Clear_Color;
            when Clear_Color_Unsigned =>
                Unsigned_Color: Unsigned_Clear_Color;
        end case;
    end record
        with Convention => C,
             Unchecked_Union;

    type Clear_Value_C(Clear_Type: Clear_Value_Type;
                       Color_Type: Clear_Color_Type) is
    record
        case Clear_Type is
            when Clear_Color =>
                case Color_Type is
                    when Clear_Color_Float =>
                        Color_Float: Clear_Color_Value_C(Clear_Color_Float);
                    when Clear_Color_Integer =>
                        Color_Integer: Clear_Color_Value_C(Clear_Color_Integer);
                    when Clear_Color_Unsigned =>
                        Color_Unsigned:
                            Clear_Color_Value_C(Clear_Color_Unsigned);
                end case;
            when Clear_Depth_Stencil =>
                Depth_Stencil: Clear_Depth_Stencil_Value;
        end case;
    end record
        with Convention => C,
             Unchecked_Union;

    type Clear_Attachment_C(Clear_Type: Clear_Value_Type;
                            Color_Type: Clear_Color_Type) is
    record
        Aspect_Mask: Image_Aspect_Flags;
        Color_Attachment: Interfaces.Unsigned_32;

        case Clear_Type is
            when Clear_Color =>
                Color_Clear_Value: Clear_Value_C(Clear_Color, Color_Type);
            when Clear_Depth_Stencil =>
                Depth_Stencil_Clear_Value: Clear_Value_C(Clear_Depth_Stencil,
                                                         Color_Type);
        end case;
    end record
        with Convention => C,
             Unchecked_Union;

    type Memory_Barrier_C is
    record
        Record_Type: In_Structure_Type := Memory_Barrier_Type;
        Next: In_Structure_C_Access;
        Src_Access_Mask: Access_Flags;
        Dst_Access_Mask: Access_Flags;
    end record
        with Convention => C;

    type Memory_Barrier_C_Access is access Memory_Barrier_C
        with Convention => C;

    type Buffer_Memory_Barrier_C is
    record
        Record_Type: In_Structure_Type := Buffer_Memory_Barrier_Type;
        Next: In_Structure_C_Access;
        Src_Access_Mask: Access_Flags;
        Dst_Access_Mask: Access_Flags;
        Src_Queue_Family_Index: Queue_Family_Index;
        Dst_Queue_Family_Index: Queue_Family_Index;
        Buffer: Vulkan.Buffer;
        Offset: Device_Size;
        Size: Device_Size;
    end record
        with Convention => C;

    type Buffer_Memory_Barrier_C_Access is access Buffer_Memory_Barrier_C
        with Convention => C;

    type Image_Memory_Barrier_C is
    record
        Record_Type: In_Structure_Type := Image_Memory_Barrier_Type;
        Next: In_Structure_C_Access;
        Src_Access_Mask: Access_Flags;
        Dst_Access_Mask: Access_Flags;
        Old_Layout: Image_Layout;
        New_Layout: Image_Layout;
        Src_Queue_Family_Index: Queue_Family_Index;
        Dst_Queue_Family_Index: Queue_Family_Index;
        Image: Vulkan.Image;
        Subresource_Range: Image_Subresource_Range;
    end record
        with Convention => C;

    type Image_Memory_Barrier_C_Access is access Image_Memory_Barrier_C
        with Convention => C;

    subtype Arrayed_Clear_Value_C is
        Clear_Value_C(Clear_Color, Clear_Color_Float);

    package Clear_Value_C_Arrays is new C_Arrays(Arrayed_Clear_Value_C);

    type Render_Pass_Begin_Info_C is
    record
        Record_Type: In_Structure_Type := Render_Pass_Begin_Info_Type;
        Next: In_Structure_C_Access;
        Render_Pass: Vulkan.Render_Pass;
        Framebuffer: Vulkan.Framebuffer;
        Render_Area: Rect_2D;
        Clear_Value_Count: Interfaces.Unsigned_32;
        Clear_Values: Clear_Value_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Render_Pass_Begin_Info_C_Access is
        access Render_Pass_Begin_Info_C
        with Convention => C;
      
    function To_C(Struct: in Application_Info) return Application_Info_C;
    procedure Free(Struct: in out Application_Info_C);

    function To_C(Struct: in Instance_Create_Info)
        return Instance_Create_Info_C;
    procedure Free(Struct: in out Instance_Create_Info_C);

    function To_C(Struct: in Physical_Device_Features)
        return Physical_Device_Features_C;

    function To_C(Struct: in Physical_Device_Limits)
        return Physical_Device_Limits_C;

    function To_C(Struct: in Physical_Device_Sparse_Properties)
        return Physical_Device_Sparse_Properties_C;

    function To_C(Struct: in Physical_Device_Properties)
        return Physical_Device_Properties_C;

    function To_C(Struct: in Device_Queue_Create_Info)
        return Device_Queue_Create_Info_C;
    procedure Free(Struct: in out Device_Queue_Create_Info_C);

    function To_C(Struct: in Device_Create_Info) return Device_Create_Info_C;
    procedure Free(Struct: in out Device_Create_Info_C);

    function To_C(Struct: in Submit_Info) return Submit_Info_C;
    procedure Free(Struct: in out Submit_Info_C);

    function To_C(Struct: in Memory_Allocate_Info)
        return Memory_Allocate_Info_C;
    procedure Free(Struct: in out Memory_Allocate_Info_C);

    function To_C(Struct: in Mapped_Memory_Range) return Mapped_Memory_Range_C;
    procedure Free(Struct: in out Mapped_Memory_Range_C);

    function To_C(Struct: in Sparse_Buffer_Memory_Bind_Info)
        return Sparse_Buffer_Memory_Bind_Info_C;
    procedure Free(Struct: in out Sparse_Buffer_Memory_Bind_Info_C);

    function To_C(Struct: in Sparse_Image_Opaque_Memory_Bind_Info)
        return Sparse_Image_Opaque_Memory_Bind_Info_C;
    procedure Free(Struct: in out Sparse_Image_Opaque_Memory_Bind_Info_C);

    function To_C(Struct: in Sparse_Image_Memory_Bind_Info)
        return Sparse_Image_Memory_Bind_Info_C;
    procedure Free(Struct: in out Sparse_Image_Memory_Bind_Info_C);

    function To_C(Struct: in Bind_Sparse_Info) return Bind_Sparse_Info_C;
    procedure Free(Struct: in out Bind_Sparse_Info_C);

    function To_C(Struct: in Fence_Create_Info) return Fence_Create_Info_C;
    procedure Free(Struct: in out Fence_Create_Info_C);

    function To_C(Struct: in Semaphore_Create_Info)
        return Semaphore_Create_Info_C;
    procedure Free(Struct: in out Semaphore_Create_Info_C);

    function To_C(Struct: in Event_Create_Info) return Event_Create_Info_C;
    procedure Free(Struct: in out Event_Create_Info_C);

    function To_C(Struct: in Query_Pool_Create_Info)
        return Query_Pool_Create_Info_C;
    procedure Free(Struct: in out Query_Pool_Create_Info_C);

    function To_C(Struct: in Buffer_Create_Info) return Buffer_Create_Info_C;
    procedure Free(Struct: in out Buffer_Create_Info_C);

    function To_C(Struct: in Buffer_View_Create_Info)
        return Buffer_View_Create_Info_C;
    procedure Free(Struct: in out Buffer_View_Create_Info_C);

    function To_C(Struct: in Image_Create_Info) return Image_Create_Info_C;
    procedure Free(Struct: in out Image_Create_Info_C);

    function To_C(Struct: in Image_View_Create_Info) 
        return Image_View_Create_Info_C;
    procedure Free(Struct: in out Image_View_Create_Info_C);

    function To_C(Struct: in Shader_Module_Create_Info)
        return Shader_Module_Create_Info_C;
    procedure Free(Struct: in out Shader_Module_Create_Info_C);

    function To_C(Struct: in Pipeline_Cache_Create_Info)
        return Pipeline_Cache_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Cache_Create_Info_C);

    function To_C(Struct: in Specialization_Info) return Specialization_Info_C;
    procedure Free(Struct: in out Specialization_Info_C);

    function To_C(Struct: in Pipeline_Shader_Stage_Create_Info)
        return Pipeline_Shader_Stage_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Shader_Stage_Create_Info_C);

    function To_C(Struct: in Pipeline_Vertex_Input_State_Create_Info)
        return Pipeline_Vertex_Input_State_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Vertex_Input_State_Create_Info_C);

    function To_C(Struct: in Pipeline_Input_Assembly_State_Create_Info)
        return Pipeline_Input_Assembly_State_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Input_Assembly_State_Create_Info_C);

    function To_C(Struct: in Pipeline_Tessellation_State_Create_Info)
        return Pipeline_Tessellation_State_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Tessellation_State_Create_Info_C);

    function To_C(Struct: in Pipeline_Viewport_State_Create_Info)
        return Pipeline_Viewport_State_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Viewport_State_Create_Info_C);

    function To_C(Struct: in Pipeline_Rasterization_State_Create_Info)
        return Pipeline_Rasterization_State_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Rasterization_State_Create_Info_C);

    function To_C(Struct: in Pipeline_Multisample_State_Create_Info)
        return Pipeline_Multisample_State_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Multisample_State_Create_Info_C);

    function To_C(Struct: in Pipeline_Depth_Stencil_State_Create_Info)
        return Pipeline_Depth_Stencil_State_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Depth_Stencil_State_Create_Info_C);

    function To_C(Struct: in Pipeline_Color_Blend_Attachment_State)
        return Pipeline_Color_Blend_Attachment_State_C;

    function To_C(Struct: in Pipeline_Color_Blend_State_Create_Info)
        return Pipeline_Color_Blend_State_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Color_Blend_State_Create_Info_C);

    function To_C(Struct: in Pipeline_Dynamic_State_Create_Info)
        return Pipeline_Dynamic_State_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Dynamic_State_Create_Info_C);

    function To_C(Struct: in Graphics_Pipeline_Create_Info)
        return Graphics_Pipeline_Create_Info_C;
    procedure Free(Struct: in out Graphics_Pipeline_Create_Info_C);

    function To_C(Struct: in Compute_Pipeline_Create_Info)
        return Compute_Pipeline_Create_Info_C;
    procedure Free(Struct: in out Compute_Pipeline_Create_Info_C);

    function To_C(Struct: in Pipeline_Layout_Create_Info)
        return Pipeline_Layout_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Layout_Create_Info_C);

    function To_C(Struct: in Sampler_Create_Info) return Sampler_Create_Info_C;
    procedure Free(Struct: in out Sampler_Create_Info_C);

    function To_C(Struct: in Descriptor_Set_Layout_Binding)
        return Descriptor_Set_Layout_Binding_C;
    procedure Free(Struct: in out Descriptor_Set_Layout_Binding_C);

    function To_C(Struct: in Descriptor_Set_Layout_Create_Info)
        return Descriptor_Set_Layout_Create_Info_C;
    procedure Free(Struct: in out Descriptor_Set_Layout_Create_Info_C);

    function To_C(Struct: in Descriptor_Pool_Create_Info)
        return Descriptor_Pool_Create_Info_C;
    procedure Free(Struct: in out Descriptor_Pool_Create_Info_C);

    function To_C(Struct: in Descriptor_Set_Allocate_Info)
        return Descriptor_Set_Allocate_Info_C;
    procedure Free(Struct: in out Descriptor_Set_Allocate_Info_C);

    function To_C(Struct: in Write_Descriptor_Set)
        return Write_Descriptor_Set_C;
    procedure Free(Struct: in out Write_Descriptor_Set_C);

    function To_C(Struct: in Copy_Descriptor_Set) return Copy_Descriptor_Set_C;
    procedure Free(Struct: in out Copy_Descriptor_Set_C);

    function To_C(Struct: in Framebuffer_Create_Info)
        return Framebuffer_Create_Info_C;
    procedure Free(Struct: in out Framebuffer_Create_Info_C);

    function To_C(Struct: in Subpass_Description) return Subpass_Description_C;
    procedure Free(Struct: in out Subpass_Description_C);

    function To_C(Struct: in Render_Pass_Create_Info)
        return Render_Pass_Create_Info_C;
    procedure Free(Struct: in out Render_Pass_Create_Info_C);

    function To_C(Struct: in Command_Pool_Create_Info)
        return Command_Pool_Create_Info_C;
    procedure Free(Struct: in out Command_Pool_Create_Info_C);

    function To_C(Struct: in Command_Buffer_Allocate_Info)
        return Command_Buffer_Allocate_Info_C;
    procedure Free(Struct: in out Command_Buffer_Allocate_Info_C);

    function To_C(Struct: in Command_Buffer_Inheritance_Info)
        return Command_Buffer_Inheritance_Info_C;
    procedure Free(Struct: in out Command_Buffer_Inheritance_Info_C);

    function To_C(Struct: in Command_Buffer_Begin_Info)
        return Command_Buffer_Begin_Info_C;
    procedure Free(Struct: in out Command_Buffer_Begin_Info_C);

    function To_C(Struct: in Clear_Color_Value) return Clear_Color_Value_C;

    function To_C(Struct: in Clear_Value) return Clear_Value_C;

    function To_C(Struct: in Clear_Attachment) return Clear_Attachment_C;

    function To_C(Struct: in Memory_Barrier) return Memory_Barrier_C;
    procedure Free(Struct: in out Memory_Barrier_C);

    function To_C(Struct: in Buffer_Memory_Barrier)
        return Buffer_Memory_Barrier_C;
    procedure Free(Struct: in out Buffer_Memory_Barrier_C);

    function To_C(Struct: in Image_Memory_Barrier)
        return Image_Memory_Barrier_C;
    procedure Free(Struct: in out Image_Memory_Barrier_C);

    function To_C(Struct: in Render_Pass_Begin_Info)
        return Render_Pass_Begin_Info_C;
    procedure Free(Struct: in out Render_Pass_Begin_Info_C);
  
    function vkCreateInstance(Create_Info: in Instance_Create_Info_C;
                              Allocator: access constant Allocation_Callbacks;
                              Instance: in out Vulkan.Instance)
        return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateInstance";

    procedure vkDestroyInstance(Instance: in Vulkan.Instance;
                                Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyInstance";
 
    function vkEnumeratePhysicalDevices(Instance: in Vulkan.Instance;
                                        Count: in out Interfaces.Unsigned_32;
                                        Devices: access Physical_Device)
        return Result
        with Import,
             Convention => C,
             External_Name => "vkEnumeratePhysicalDevices";

    procedure vkGetPhysicalDeviceFeatures
        (Device: in Physical_Device;
         Features: in out Physical_Device_Features_C)
        with Import,
             Convention => C,
             External_Name => "vkGetPhysicalDeviceFeatures";

    procedure vkGetPhysicalDeviceFormatProperties
        (Device: in Physical_Device;
         Format: in Vulkan.Format;
         Properites: in out Format_Properties)
        with Import,
             Convention => C,
             External_Name => "vkGetPhysicalDeviceFormatProperties";

    function vkGetPhysicalDeviceImageFormatProperties
        (Device: in Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Tiling: in Image_Tiling;
         Usage: in Image_Usage_Flags;
         Create_Flags: in Image_Create_Flags;
         Properties: in out Image_Format_Properties) return Result
        with Import,
             Convention => C,
             External_Name => "vkGetPhysicalDeviceImageFormatProperties";

    procedure vkGetPhysicalDeviceProperties
        (Device: in Physical_Device;
         Properties: in out Physical_Device_Properties_C)
        with Import,
             Convention => C,
             External_Name => "vkGetPhysicalDeviceProperties";

    procedure vkGetPhysicalDeviceQueueFamilyProperties
        (Device: in Physical_Device;
         Count: in out Interfaces.Unsigned_32;
         Properties: access Queue_Family_Properties)
        with Import,
             Convention => C,
             External_Name => "vkGetPhysicalDeviceQueueFamilyProperties";

    procedure vkGetPhysicalDeviceMemoryProperties
        (Device: in Physical_Device;
         Properties: in out Physical_Device_Memory_Properties_C)
        with Import,
             Convention => C,
             External_Name => "vkGetPhysicalDeviceMemoryProperties";
    
    function vkGetInstanceProcAddr
        (Instance: in Vulkan.Instance;
         Name: in Interfaces.C.Strings.char_array_access) return Void_Function
        with Import,
             Convention => C,
             External_Name => "vkGetInstanceProcAddr";

    function vkGetDeviceProcAddr
        (Device: in Vulkan.Device;
         Name: in Interfaces.C.Strings.char_array_access)
        return Void_Function
        with Import,
             Convention => C,
             External_Name => "vkGetDeviceProcAddr";

    function vkCreateDevice(Physical_Device: in Vulkan.Physical_Device;
                            Create_Info: in Device_Create_Info_C;
                            Allocator: access constant Allocation_Callbacks;
                            Device: in out Vulkan.Device) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateDevice";

    procedure vkDestroyDevice(Device: in Vulkan.Device;
                              Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyDevice";

    function vkEnumerateInstanceExtensionProperties
        (Layer_Name: in Interfaces.C.Strings.char_array_access;
         Count: in out Interfaces.Unsigned_32;
         Properties: access Extension_Properties_C) return Result
        with Import,
             Convention => C,
             External_Name => "vkEnumerateInstanceExtensionProperties";

    function vkEnumerateDeviceExtensionProperties
        (Device: in Physical_Device;
         Layer_Name: in Interfaces.C.Strings.char_array_access;
         Count: in out Interfaces.Unsigned_32;
         Properties: access Extension_Properties_C) return Result
        with Import,
             Convention => C,
             External_Name => "vkEnumerateDeviceExtensionProperties";

    function vkEnumerateInstanceLayerProperties
        (Count: in out Interfaces.Unsigned_32;
         Properties: access Layer_Properties_C) return Result
        with Import,
             Convention => C,
             External_Name => "vkEnumerateInstanceLayerProperties";

    function vkEnumerateDeviceLayerProperties
        (Device: in Physical_Device;
         Count: in out Interfaces.Unsigned_32;
         Properties: access Layer_Properties_C) return Result
        with Import,
             Convention => C,
             External_Name => "vkEnumerateDeviceLayerProperties";

    procedure vkGetDeviceQueue
        (Device: in Vulkan.Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Queue_Index: in Interfaces.Unsigned_32;
         Queue: in out Vulkan.Queue)
        with Import,
             Convention => C,
             External_Name => "vkGetDeviceQueue";
    
    function vkQueueSubmit(Queue: in Vulkan.Queue;
                           Submit_Count: in Interfaces.Unsigned_32;
                           Submits: access Submit_Info_C;
                           Fence: in Vulkan.Fence) return Result
        with Import,
             Convention => C,
             External_Name => "vkQueueSubmit";

    function vkQueueWaitIdle(Queue: in Vulkan.Queue) return Result
        with Import,
             Convention => C,
             External_Name => "vkQueueWaitIdle";

    function vkDeviceWaitIdle(Device: in Vulkan.Device) return Result
        with Import,
             Convention => C,
             External_Name => "vkDeviceWaitIdle";

    function vkAllocateMemory(Device: in Vulkan.Device;
                              Allocate_Info: in Memory_Allocate_Info_C;
                              Allocator: access constant Allocation_Callbacks;
                              Memory: in out Device_Memory) return Result
        with Import,
             Convention => C,
             External_Name => "vkAllocateMemory";

    procedure vkFreeMemory(Device: in Vulkan.Device;
                           Memory: in Device_Memory;
                           Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkFreeMemory";

    function vkMapMemory(Device: in Vulkan.Device;
                         Memory: in Device_Memory;
                         Offset, Size: in Device_Size;
                         Flags: in Memory_Map_Flags;
                         Data: in Interfaces.C.Extensions.void_ptr)
        return Result
        with Import,
             Convention => C,
             External_Name => "vkMapMemory";

    procedure vkUnmapMemory(Device: in Vulkan.Device;
                            Memory: in Device_Memory)
        with Import,
             Convention => C,
             External_Name => "vkUnmapMemory";

    function vkFlushMappedMemoryRanges
        (Device: in Vulkan.Device;
         Memory_Range_Count: in Interfaces.Unsigned_32;
         Memory_Ranges: access constant  Mapped_Memory_Range_C) return Result
        with Import,
             Convention => C,
             External_Name => "vkFlushMappedMemoryRanges";

    function vkInvalidateMappedMemoryRanges
        (Device: in Vulkan.Device;
         Memory_Range_Count: in Interfaces.Unsigned_32;
         Memory_Ranges: access constant Mapped_Memory_Range_C) return Result
        with Import,
             Convention => C,
             External_Name => "vkInvalidateMappedMemoryRanges";

    procedure vkGetDeviceMemoryCommitment(Device: in Vulkan.Device;
                                          Memory: in Device_Memory;
                                          Committed_Memory_In_Bytes:
                                            in out Device_Size)
        with Import,
             Convention => C,
             External_Name => "vkGetDeviceMemoryCommitment";

    function vkBindBufferMemory(Device: in Vulkan.Device;
                                Buffer: in VUlkan.Buffer;
                                Memory: in Device_Memory;
                                Offset: in Device_Size) return Result
        with Import,
             Convention => C,
             External_Name => "vkBindBufferMemory";

    function vkBindImageMemory(Device: in Vulkan.Device;
                               Image: in Vulkan.Image;
                               Memory: in Device_Memory;
                               Offset: in Device_Size) return Result
        with Import,
             Convention => C,
             External_Name => "vkBindImageMemory";

    procedure vkGetBufferMemoryRequirements(Device: in Vulkan.Device;
                                            Buffer: in Vulkan.Buffer;
                                            Memory_Requirements:
                                            in out Vulkan.Memory_Requirements)
        with Import,
             Convention => C,
             External_Name => "vkGetBufferMemoryRequirements";

    procedure vkGetImageMemoryRequirements(Device: in Vulkan.Device;
                                           Image: in Vulkan.Image;
                                           Memory_Requirements:
                                            in out Vulkan.Memory_Requirements)
        with Import,
             Convention => C,
             External_Name => "vkGetImageMemoryRequirements";

    procedure vkGetImageSparseMemoryRequirements
        (Device: in Vulkan.Device;
         Image: in Vulkan.Image;
         Count: in out Interfaces.Unsigned_32;
         Requirements: access Sparse_Image_Memory_Requirements)
        with Import,
             Convention => C,
             External_Name => "vkGetImageSparseMemoryRequirements";

    procedure vkGetPhysicalDeviceSparseImageFormatProperties
        (Device: in Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Samples: in Sample_Count_Flags;
         Usage: in Image_Usage_Flags;
         Tiling: in Image_Tiling;
         Count: in out Interfaces.Unsigned_32;
         Properties: access Sparse_Image_Format_Properties)
        with Import,
             Convention => C,
             External_Name => "vkGetPhysicalDeviceSparseImageFormatProperties";

    function vkQueueBindSparse(Queue: in Vulkan.Queue;
                               Bind_Info_Count: in Interfaces.Unsigned_32;
                               Bind_Info: access Bind_Sparse_Info_C;
                               Fence: in Vulkan.Fence) return Result
        with Import,
             Convention => C,
             External_Name => "vkQueueBindSparse";

    function vkCreateFence(Device: in Vulkan.Device;
                           Create_Info: in Fence_Create_Info_C;
                           Allocator: access constant Allocation_Callbacks;
                           Fence: in out Vulkan.Fence) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateFence";

    procedure vkDestroyFence(Device: in Vulkan.Device;
                             Fence: in Vulkan.Fence;
                             Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyFence";
    
    function vkResetFences(Device: in Vulkan.Device;
                           Fence_Count: in Interfaces.Unsigned_32;
                           Fences: access constant Fence) return Result
        with Import,
             Convention => C,
             External_Name => "vkResetFences";

    function vkGetFenceStatus(Device: in Vulkan.Device;
                              Fence: in Vulkan.Fence) return Result
        with Import,
             Convention => C,
             External_Name => "vkGetFenceStatus";

    function vkWaitForFences(Device: in Vulkan.Device;
                             Fence_Count: in Interfaces.Unsigned_32;
                             Fences: access constant Fence;
                             Wait_All: in Interfaces.Unsigned_32;
                             Timeout: in Interfaces.Unsigned_64) return Result
        with Import,
             Convention => C,
             External_Name => "vkWaitForFences";

    function vkCreateSemaphore(Device: in Vulkan.Device;
                               Create_Info: in Semaphore_Create_Info_C;
                               Allocator: access constant Allocation_Callbacks;
                               Semaphore: in out Vulkan.Semaphore)
                                return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateSemaphore";

    procedure vkDestroySemaphore
        (Device: in Vulkan.Device;
         Semaphore: in Vulkan.Semaphore;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroySemaphore";

    function vkCreateEvent(Device: in Vulkan.Device;
                           Create_Info: in Event_Create_Info_C;
                           Allocator: access constant Allocation_Callbacks;
                           Event: in out Vulkan.Event) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateEvent";

    procedure vkDestroyEvent(Device: in Vulkan.Device;
                             Event: in Vulkan.Event;
                             Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyEvent";

    function vkGetEventStatus(Device: in Vulkan.Device;
                              Event: in Vulkan.Event) return Result
        with Import,
             Convention => C,
             External_Name => "vkGetEventStatus";

    function vkSetEvent(Device: in Vulkan.Device;
                        Event: in Vulkan.Event) return Result
        with Import,
             Convention => C,
             External_Name => "vkSetEvent";

    function vkResetEvent(Device: in Vulkan.Device;
                          Event: in Vulkan.Event) return Result
        with Import,
             Convention => C,
             External_Name => "vkResetEvent";

    function vkCreateQueryPool(Device: in Vulkan.Device;
                               Create_Info: in Query_Pool_Create_Info_C;
                               Allocator: access constant Allocation_Callbacks;
                               Query_Pool: in out Vulkan.Query_Pool)
                                return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateQueryPool";

    procedure vkDestroyQueryPool
        (Device: in Vulkan.Device;
         Query_Pool: in Vulkan.Query_Pool;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyQueryPool";

    function vkGetQueryPoolResults(Device: in Vulkan.Device;
                                   Query_Pool: in Vulkan.Query_Pool;
                                   First_Query,
                                   Query_Count: in Interfaces.Unsigned_32;
                                   Data_Size: in Interfaces.C.size_t;
                                   Data: in Interfaces.C.Extensions.void_ptr;
                                   Stride: in Device_Size;
                                   Flags: in Query_Result_Flags) return Result
        with Import,
             Convention => C,
             External_Name => "vkGetQueryPoolResults";

    function vkCreateBuffer(Device: in Vulkan.Device;
                            Create_Info: in Buffer_Create_Info_C;
                            Allocator: access constant Allocation_Callbacks;
                            Buffer: in out Vulkan.Buffer) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateBuffer";

    procedure vkDestroyBuffer(Device: in Vulkan.Device;
                              Buffer: in Vulkan.Buffer;
                              Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyBuffer";

    function vkCreateBufferView(Device: in Vulkan.Device;
                                Create_Info: in Buffer_View_Create_Info_C;
                                Allocator: access constant Allocation_Callbacks;
                                Buffer_View: in out Vulkan.Buffer_View)
                                    return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateBufferView";

    procedure vkDestroyBufferView
        (Device: in Vulkan.Device;
         Buffer_View: in Vulkan.Buffer_View;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyBufferView";

    function vkCreateImage(Device: in Vulkan.Device;
                           Create_Info: in Image_Create_Info_C;
                           Allocator: access constant Allocation_Callbacks;
                           Image: in out Vulkan.Image) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateImage";

    procedure vkDestroyImage(Device: in Vulkan.Device;
                             Image: in Vulkan.Image;
                             Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyImage";

    procedure vkGetImageSubresourceLayout(Device: in Vulkan.Device;
                                          Image: in Vulkan.Image;
                                          Subresource: in Image_Subresource;
                                          Layout: out Subresource_Layout)
        with Import,
             Convention => C,
             External_Name => "vkGetImageSubresourceLayout";

    function vkCreateImageView
        (Device: in Vulkan.Device;
         Create_Info: in Image_View_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Image_View: in out Vulkan.Image_View) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateImageView";

    procedure vkDestroyImageView
        (Device: in Vulkan.Device;
         Image_View: in Vulkan.Image_View;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyImageView";

    function vkCreateShaderModule
        (Device: in Vulkan.Device;
         Create_Info: in Shader_Module_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Shader_Module: in out Vulkan.Shader_Module) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateShaderModule";

    procedure vkDestroyShaderModule
        (Device: in Vulkan.Device;
         Shader_Module: in Vulkan.Shader_Module;
         Allocation: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyShaderModule";

    function vkCreatePipelineCache
        (Device: in Vulkan.Device;
         Create_Info: in Pipeline_Cache_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Pipeline_Cache: in out Vulkan.Pipeline_Cache) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreatePipelineCache";

    procedure vkDestroyPipelineCache
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyPipelineCache";

    function vkGetPipelineCacheData(Device: in Vulkan.Device;
                                    Pipeline_Cache: in Vulkan.Pipeline_Cache;
                                    Data_Size: in out Interfaces.C.size_t;
                                    Data: in Interfaces.C.Extensions.void_ptr)
                                        return Result
        with Import,
             Convention => C,
             External_Name => "vkGetPipelineCacheData";
    
    function vkMergePipelineCaches
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Cache_Count: in Interfaces.Unsigned_32;
         Caches: access constant Vulkan.Pipeline_Cache) return Result
        with Import,
             Convention => C,
             External_Name => "vkMergePipelineCaches";

    function vkCreateGraphicsPipelines
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Info_Count: in Interfaces.Unsigned_32;
         Create_Infos: access constant Graphics_Pipeline_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Pipelines: access Pipeline) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateGraphicsPipelines";

    function vkCreateComputePipelines
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Info_Count: in Interfaces.Unsigned_32;
         Create_Infos: access constant Compute_Pipeline_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Pipelines: access Pipeline) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateComputePipelines";

    procedure vkDestroyPipeline(Device: in Vulkan.Device;
                                Pipeline: in Vulkan.Pipeline;
                                Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyPipeline";

    function vkCreatePipelineLayout
        (Device: in Vulkan.Device;
         Create_Info: in Pipeline_Layout_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Pipeline_Layout: in out Vulkan.Pipeline_Layout) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreatePipelineLayout";

    procedure vkDestroyPipelineLayout
        (Device: in Vulkan.Device;
         Pipeline_Layout: in Vulkan.Pipeline_Layout;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyPipelineLayout";

    function vkCreateSampler(Device: in Vulkan.Device;
                             Create_Info: in Sampler_Create_Info_C;
                             Allocator: access constant Allocation_Callbacks;
                             Sampler: in out Vulkan.Sampler) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateSampler";

    procedure vkDestroySampler(Device: in Vulkan.Device;
                               Sampler: in Vulkan.Sampler;
                               Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroySampler";

    function vkCreateDescriptorSetLayout
        (Device: in Vulkan.Device;
         Create_Info: in Descriptor_Set_Layout_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Descriptor_Set_Layout: in out Vulkan.Descriptor_Set_Layout)
            return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateDescriptorSetLayout";

    procedure vkDestroyDescriptorSetLayout
        (Device: in Vulkan.Device;
         Descriptor_Set_Layout: in Vulkan.Descriptor_Set_Layout;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyDescriptorSetLayout";

    function vkCreateDescriptorPool
        (Device: in Vulkan.Device;
         Create_Info: in Descriptor_Pool_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Descriptor_Pool: in out Vulkan.Descriptor_Pool) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateDescriptorPool";

    procedure vkDestroyDescriptorPool
        (Device: in Vulkan.Device;
         Descriptor_Pool: in Vulkan.Descriptor_Pool;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyDescriptorPool";

    function vkResetDescriptorPool
        (Device: in Vulkan.Device;
         Descriptor_Pool: in Vulkan.Descriptor_Pool;
         Flags: in Descriptor_Pool_Reset_Flags) return Result
        with Import,
             Convention => C,
             External_Name => "vkResetDescriptorPool";

    function vkAllocateDescriptorSets
        (Device: in Vulkan.Device;
         Allocate_Info: in Descriptor_Set_Allocate_Info_C;
         Descriptor_Sets: access Descriptor_Set) return Result
        with Import,
             Convention => C,
             External_Name => "vkAllocateDescriptorSets";

    function vkFreeDescriptorSets
        (Device: in Vulkan.Device;
         Descriptor_Pool: in Vulkan.Descriptor_Pool;
         Descriptor_Set_Count: in Interfaces.Unsigned_32;
         Descriptor_Sets: access Descriptor_Set) return Result
        with Import,
             Convention => C,
             External_Name => "vkFreeDescriptorSets";

    procedure vkUpdateDescriptorSets
        (Device: in Vulkan.Device;
         Descriptor_Write_Count: in Interfaces.Unsigned_32;
         Descriptor_Writes: access Write_Descriptor_Set_C;
         Descriptor_Copy_Count: in Interfaces.Unsigned_32;
         Descriptor_Copies: access Copy_Descriptor_Set_C)
        with Import,
             Convention => C,
             External_Name => "vkUpdateDescriptorSets";

    function vkCreateFramebuffer
        (Device: in Vulkan.Device;
         Create_Info: in Framebuffer_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Framebuffer: in out Vulkan.Framebuffer) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateFramebuffer";

    procedure vkDestroyFramebuffer
        (Device: in Vulkan.Device;
         Framebuffer: in Vulkan.Framebuffer;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyFramebuffer";

    function vkCreateRenderPass
        (Device: in Vulkan.Device;
         Create_Info: in Render_Pass_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Render_Pass: in out Vulkan.Render_Pass) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateRenderPass";

    procedure vkDestroyRenderPass
        (Device: in Vulkan.Device;
         RenderPass: in Vulkan.Render_Pass;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyRenderPass";

    procedure vkGetRenderAreaGranularity(Device: in Vulkan.Device;
                                         Render_Pass: in Vulkan.Render_Pass;
                                         Granularity: in out Extent_2D)
        with Import,
             Convention => C,
             External_Name => "vkGetRenderAreaGranularity";

    function vkCreateCommandPool
        (Device: in Vulkan.Device;
         Create_Info: in Command_Pool_Create_Info_C;
         Allocator: access constant Allocation_Callbacks;
         Command_Pool: in out Vulkan.Command_Pool) return Result
        with Import,
             Convention => C,
             External_Name => "vkCreateCommandPool";

    procedure vkDestroyCommandPool
        (Device: in Vulkan.Device;
         Command_Pool: in Vulkan.Command_Pool;
         Allocator: access constant Allocation_Callbacks)
        with Import,
             Convention => C,
             External_Name => "vkDestroyCommandPool";

    function vkResetCommandPool
        (Device: in Vulkan.Device;
         Command_Pool: in Vulkan.Command_Pool;
         Flags: in Command_Pool_Reset_Flags) return Result
        with Import,
             Convention => C,
             External_Name => "vkResetCommandPool";

    function vkAllocateCommandBuffers
        (Device: in Vulkan.Device;
         Allocate_Info: in Command_Buffer_Allocate_Info_C;
         Command_Buffers: access Command_Buffer) return Result
        with Import,
             Convention => C,
             External_Name => "vkAllocateCommandBuffers";

    procedure vkFreeCommandBuffers
        (Device: in Vulkan.Device;
         Command_Pool: in Vulkan.Command_Pool;
         Command_Buffer_Count: in Interfaces.Unsigned_32;
         Command_Buffers: access Command_Buffer)
        with Import,
             Convention => C,
             External_Name => "vkFreeCommandBuffers";

    function vkBeginCommandBuffer(Command_Buffer: in Vulkan.Command_Buffer;
                                  Begin_Info: in Command_Buffer_Begin_Info_C)
        return Result
        with Import,
             Convention => C,
             External_Name => "vkBeginCommandBuffer";

    function vkEndCommandBuffer(Command_Buffer: in Vulkan.Command_Buffer)
        return Result
        with Import,
             Convention => C,
             External_Name => "vkEndCommandBuffer";

    function vkResetCommandBuffer(Command_Buffer: in Vulkan.Command_Buffer;
                                  Flags: in Command_Buffer_Reset_Flags)
        return Result
        with Import,
             Convention => C,
             External_Name => "vkResetCommandBuffer";

    procedure vkCmdBindPipeline
        (Command_Buffer: in Vulkan.Command_Buffer;
         Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
         Pipeline: in Vulkan.Pipeline)
        with Import,
             Convention => C,
             External_Name => "vkCmdBindPipeline";

    procedure vkCmdSetViewport(Command_Buffer: in Vulkan.Command_Buffer;
                               First_Viewport, Viewport_Count:
                                in Interfaces.Unsigned_32;
                               Viewports: access constant Viewport)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetViewport";

    procedure vkCmdSetScissor(Command_Buffer: in Vulkan.Command_Buffer;
                              First_Scissor, Scissor_Count:
                               in Interfaces.Unsigned_32;
                              Scissors: access constant Rect_2D)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetScissor";

    procedure vkCmdSetLineWidth(Command_Buffer: in Vulkan.Command_Buffer;
                                Line_Width: in Interfaces.C.C_Float)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetLineWidth";

    procedure vkCmdSetDepthBias(Command_Buffer: in Vulkan.Command_Buffer;
                                Depth_Bias_Constant_Factor,
                                Depth_Bias_Clamp,
                                Depth_Bias_Slope_Factor:
                                 in Interfaces.C.C_Float)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetDepthBias";

    procedure vkCmdSetBlendConstants(Command_Buffer: in Vulkan.Command_Buffer;
                                     Blend_Constants: in Blend_Constants_Array)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetBlendConstants";

    procedure vkCmdSetDepthBounds(Command_Buffer: in Vulkan.Command_Buffer;
                                  Min_Depth_Bounds,
                                  Max_Depth_Bounds: in Interfaces.C.C_Float)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetDepthBounds";

    procedure vkCmdSetStencilCompareMask
        (Command_Buffer: in Vulkan.Command_Buffer;
         Face_Mask: in Stencil_Face_Flags;
         Compare_Mask: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetStencilCompareMask";

    procedure vkCmdSetStencilWriteMask(Command_Buffer: in Vulkan.Command_Buffer;
                                       Face_Mask: in Stencil_Face_Flags;
                                       Write_Mask: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetStencilWriteMask";
        
    procedure vkCmdSetStencilReference(Command_Buffer: in Vulkan.Command_Buffer;
                                       Face_Mask: in Stencil_Face_Flags;
                                       Reference: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetStencilReference";

    procedure vkCmdBindDescriptorSets
        (Command_Buffer: in Vulkan.Command_Buffer;
         Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
         Layout: in Pipeline_Layout;
         First_Set, Descriptor_Set_Count: in Interfaces.Unsigned_32;
         Descriptor_Sets: access constant Descriptor_Set;
         Dynamic_Offset_Count: in Interfaces.Unsigned_32;
         Dynamic_Offsets: access constant Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdBindDescriptorSets";

    procedure vkCmdBindIndexBuffer(Command_Buffer: in Vulkan.Command_Buffer;
                                   Buffer: in Vulkan.Buffer;
                                   Offset: in Device_Size;
                                   Index_Type: in Vulkan.Index_Type)
        with Import,
             Convention => C,
             External_Name => "vkCmdBindIndexBuffer";

    procedure vkCmdBindVertexBuffers(Command_Buffer: in Vulkan.Command_Buffer;
                                     First_Binding,
                                     Binding_Count: in Interfaces.Unsigned_32;
                                     Buffers: access constant Buffer;
                                     Offsets: access constant Device_Size)
        with Import,
             Convention => C,
             External_Name => "vkCmdBindVertexBuffers";

    procedure vkCmdDraw(Command_Buffer: in Vulkan.Command_Buffer;
                        Vertex_Count,
                        Instance_Count,
                        First_Vertex,
                        First_Instance: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdDraw";

    procedure vkCmdDrawIndexed(Command_Buffer: in Vulkan.Command_Buffer;
                               Index_Count,
                               Instance_Count,
                               First_Index: in Interfaces.Unsigned_32;
                               Vertex_Offset: in Interfaces.Integer_32;
                               First_Instance: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdDrawIndexed";

    procedure vkCmdDrawIndirect(Command_Buffer: in Vulkan.Command_Buffer;
                                Buffer: in Vulkan.Buffer;
                                Offset: in Device_Size;
                                Draw_Count,
                                Stride: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdDrawIndirect";

    procedure vkCmdDrawIndexedIndirect(Command_Buffer: in Vulkan.Command_Buffer;
                                       Buffer: in Vulkan.Buffer;
                                       Offset: in Device_Size;
                                       Draw_Count,
                                       Stride: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdDrawIndexedIndirect";

    procedure vkCmdDispatch(Command_Buffer: in Vulkan.Command_Buffer;
                            Group_Count_X,
                            Group_Count_Y,
                            Group_Count_Z: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdDispatch";

    procedure vkCmdDispatchIndirect(Command_Buffer: in Vulkan.Command_Buffer;
                                    Buffer: in Vulkan.Buffer;
                                    Offset: in Device_Size)
        with Import,
             Convention => C,
             External_Name => "vkCmdDispatchIndirect";

    procedure vkCmdCopyBuffer(Command_Buffer: in Vulkan.Command_Buffer;
                              Src_Buffer, Dst_Buffer: in Buffer;
                              Region_Count: in Interfaces.Unsigned_32;
                              Regions: access constant Buffer_Copy)
        with Import,
             Convention => C,
             External_Name => "vkCmdCopyBuffer";

    procedure vkCmdCopyImage(Command_Buffer: in Vulkan.Command_Buffer;
                             Src_Image: in Image;
                             Src_Image_Layout: in Image_Layout;
                             Dst_Image: in Image;
                             Dst_Image_Layout: in Image_Layout;
                             Region_Count: in Interfaces.Unsigned_32;
                             Regions: access constant Image_Copy)
        with Import,
             Convention => C,
             External_Name => "vkCmdCopyImage";

    procedure vkCmdBlitImage(Command_Buffer: in Vulkan.Command_Buffer;
                             Src_Image: in Image;
                             Src_Image_Layout: in Image_Layout;
                             Dst_Image: in Image;
                             Dst_Image_Layout: in Image_Layout;
                             Region_Count: in Interfaces.Unsigned_32;
                             Regions: access constant Image_Blit;
                             Filter: in Vulkan.Filter)
        with Import,
             Convention => C,
             External_Name => "vkCmdBlitImage";

    procedure vkCmdCopyBufferToImage(Command_Buffer: in Vulkan.Command_Buffer;
                                     Src_Buffer: in Buffer;
                                     Dst_Image: in Image;
                                     Dst_Image_Layout: in Image_Layout;
                                     Region_Count: in Interfaces.Unsigned_32;
                                     Regions: access constant Buffer_Image_Copy)
        with Import,
             Convention => C,
             External_Name => "vkCmdCopyBufferToImage";

    procedure vkCmdCopyImageToBuffer(Command_Buffer: in Vulkan.Command_Buffer;
                                     Src_Image: in Image;
                                     Src_Image_Layout: in Image_Layout;
                                     Dst_Buffer: in Buffer;
                                     Region_Count: in Interfaces.Unsigned_32;
                                     Regions: access constant Buffer_Image_Copy)
        with Import,
             Convention => C,
             External_Name => "vkCmdCopyImageToBuffer";

    procedure vkCmdUpdateBuffer(Command_Buffer: in Vulkan.Command_Buffer;
                                Dst_Buffer: in Buffer;
                                Dst_Offset, Data_Size: in Device_Size;
                                Data: in Interfaces.C.Extensions.void_ptr)
        with Import,
             Convention => C,
             External_Name => "vkCmdUpdateBuffer";

    procedure vkCmdFillBuffer(Command_Buffer: in Vulkan.Command_Buffer;
                              Dst_Buffer: in Buffer;
                              Dst_Offset, Size: in Device_Size;
                              Data: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdFillBuffer";

    procedure vkCmdClearColorImage
        (Command_Buffer: in Vulkan.Command_Buffer;
         Image: in Vulkan.Image;
         Image_Layout: in Vulkan.Image_Layout;
         Color: in Interfaces.C.Extensions.void_ptr;
         Range_Count: in Interfaces.Unsigned_32;
         Ranges: access constant Image_Subresource_Range)
        with Import,
             Convention => C,
             External_Name => "vkCmdClearColorImage";

    procedure vkCmdClearDepthStencilImage
        (Command_Buffer: in Vulkan.Command_Buffer;
         Image: in Vulkan.Image;
         Image_Layout: in Vulkan.Image_Layout;
         Depth_Stencil: in Clear_Depth_Stencil_Value;
         Range_Count: in Interfaces.Unsigned_32;
         Ranges: access constant Image_Subresource_Range)
        with Import,
             Convention => C,
             External_Name => "vkCmdClearDepthStencilImage";

    procedure vkCmdClearAttachments
        (Command_Buffer: in Vulkan.Command_Buffer;
         Attachment_Count: in Interfaces.Unsigned_32;
         Attachments: access constant Clear_Attachment_C;
         Rect_Count: in Interfaces.Unsigned_32;
         Rects: access constant Clear_Rect)
        with Import,
             Convention => C,
             External_Name => "vkCmdClearAttachments";

    procedure vkCmdResolveImage(Command_Buffer: in Vulkan.Command_Buffer;
                                Src_Image: in Image;
                                Src_Image_Layout: in Image_Layout;
                                Dst_Image: in Image;
                                Dst_Image_Layout: in Image_Layout;
                                Region_Count: in Interfaces.Unsigned_32;
                                Regions: access constant Image_Resolve)
        with Import,
             Convention => C,
             External_Name => "vkCmdResolveImage";

    procedure vkCmdSetEvent(Command_Buffer: in Vulkan.Command_Buffer;
                            Event: in Vulkan.Event;
                            Stage_Mask: in Pipeline_Stage_Flags)
        with Import,
             Convention => C,
             External_Name => "vkCmdSetEvent";

    procedure vkCmdResetEvent(Command_Buffer: in Vulkan.Command_Buffer;
                              Event: in Vulkan.Event;
                              Stage_Mask: in Pipeline_Stage_Flags)
        with Import,
             Convention => C,
             External_Name => "vkCmdResetEvent";

    procedure vkCmdWaitEvents
        (Command_Buffer: in Vulkan.Command_Buffer;
         Event_Count: in Interfaces.Unsigned_32;
         Events: access constant Event;
         Src_Stage_Mask, Dst_Stage_Mask: in Pipeline_Stage_Flags;
         Memory_Barrier_Count: in Interfaces.Unsigned_32;
         Memory_Barriers: access constant Memory_Barrier_C;
         Buffer_Memory_Barrier_Count: in Interfaces.Unsigned_32;
         Buffer_Memory_Barriers: access constant Buffer_Memory_Barrier_C;
         Image_Memory_Barrier_Count: in Interfaces.Unsigned_32;
         Image_Memory_Barriers: access constant Image_Memory_Barrier_C)
        with Import,
             Convention => C,
             External_Name => "vkCmdWaitEvents";

    procedure vkCmdPipelineBarrier
        (Command_Buffer: in Vulkan.Command_Buffer;
         Src_Stage_Mask, Dst_Stage_Mask: in Pipeline_Stage_Flags;
         Dependecy_Flags: in Vulkan.Dependency_Flags;
         Memory_Barrier_Count: in Interfaces.Unsigned_32;
         Memory_Barriers: access constant Memory_Barrier_C;
         Buffer_Memory_Barrier_Count: in Interfaces.Unsigned_32;
         Buffer_Memory_Barriers: access constant Buffer_Memory_Barrier_C;
         Image_Memory_Barrier_Count: in Interfaces.Unsigned_32;
         Image_Memory_Barriers: access constant Image_Memory_Barrier_C)
        with Import,
             Convention => C,
             External_Name => "vkCmdPipelineBarrier";

    procedure vkCmdBeginQuery(Command_Buffer: in Vulkan.Command_Buffer;
                              Query_Pool: in Vulkan.Query_Pool;
                              Query: in Interfaces.Unsigned_32;
                              Flags: in Query_Control_Flags)
        with Import,
             Convention => C,
             External_Name => "vkCmdBeginQuery";

    procedure vkCmdEndQuery(Command_Buffer: in Vulkan.Command_Buffer;
                            Query_Pool: in Vulkan.Query_Pool;
                            Query: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdEndQuery";

    procedure vkCmdResetQueryPool(Command_Buffer: in Vulkan.Command_Buffer;
                                  Query_Pool: in Vulkan.Query_Pool;
                                  First_Query,
                                  Query_Count: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdResetQueryPool";

    procedure vkCmdWriteTimestamp(Command_Buffer: in Vulkan.Command_Buffer;
                                  Pipeline_Stage: in Pipeline_Stage_Flags;
                                  Query_Pool: in Vulkan.Query_Pool;
                                  Query: in Interfaces.Unsigned_32)
        with Import,
             Convention => C,
             External_Name => "vkCmdWriteTimestamp";

    procedure vkCmdCopyQueryPoolResults
        (Command_Buffer: in Vulkan.Command_Buffer;
         Query_Pool: in Vulkan.Query_Pool;
         First_Query,
         Query_Count: in Interfaces.Unsigned_32;
         Dst_Buffer: in Buffer;
         Dst_Offset,
         Stride: in Device_Size;
         Flags: in Query_Result_Flags)
        with Import,
             Convention => C,
             External_Name => "vkCmdCopyQueryPoolResults";

    procedure vkCmdPushConstants(Command_Buffer: in Vulkan.Command_Buffer;
                                 Layout: in Pipeline_Layout;
                                 Stage_Flags: in Shader_Stage_Flags;
                                 Offset, Size: in Interfaces.Unsigned_32;
                                 Values: in Interfaces.C.Extensions.void_ptr)
        with Import,
             Convention => C,
             External_Name => "vkCmdPushConstants";

    procedure vkCmdBeginRenderPass
        (Command_Buffer: in Vulkan.Command_Buffer;
         Render_Pass_Begin: in Render_Pass_Begin_Info_C;
         Contents: in Subpass_Contents)
        with Import,
             Convention => C,
             External_Name => "vkCmdBeginRenderPass";

    procedure vkCmdNextSubpass(Command_Buffer: in Vulkan.Command_Buffer;
                               Contents: in Subpass_Contents)
        with Import,
             Convention => C,
             External_Name => "vkCmdNextSubpass";

    procedure vkCmdEndRenderPass(Command_Buffer: in Vulkan.Command_Buffer)
        with Import,
             Convention => C,
             External_Name => "vkCmdEndRenderPass";

    procedure vkCmdExecuteCommands
        (Command_Buffer: in Vulkan.Command_Buffer;
         Command_Buffer_Count: in Interfaces.Unsigned_32;
         Command_Buffers: access constant Vulkan.Command_Buffer)
        with Import,
             Convention => C,
             External_Name => "vkCmdExecuteCommands";
end Vulkan.C;


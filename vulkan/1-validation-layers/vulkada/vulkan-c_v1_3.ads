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

-- Subprogram access for Vulkan 1.3

with Vulkan.C;
with Vulkan.C_V1_1;
with Vulkan.C_V1_2;
with Vulkan.C_Arrays;

private package Vulkan.C_V1_3 is
    -- Structure classification.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Vulkan_1_3_Features_Type |
            Physical_Device_Vulkan_1_3_Properties_Type |
            Pipeline_Creation_Feedback_Create_Info_Type |
            Physical_Device_Shader_Terminate_Invocation_Features_Type |
            Physical_Device_Tool_Properties_Type |
            Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type |
            Physical_Device_Private_Data_Features_Type |
            Device_Private_Data_Create_Info_Type |
            Private_Data_Slot_Create_Info_Type |
            Physical_Device_Pipeline_Creation_Cache_Control_Features_Type |
            Memory_Barrier_2_Type |
            Buffer_Memory_Barrier_2_Type |
            Image_Memory_Barrier_2_Type |
            Dependency_Info_Type |
            Semaphore_Submit_Info_Type |
            Command_Buffer_Submit_Info_Type |
            Submit_Info_2_Type |
            Physical_Device_Synchronization_2_Features_Type |
            Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type |
            Physical_Device_Image_Robustness_Features_Type |
            Buffer_Copy_2_Type |
            Copy_Buffer_Info_2_Type |
            Image_Copy_2_Type |
            Copy_Image_Info_2_Type |
            Buffer_Image_Copy_2_Type |
            Copy_Buffer_To_Image_Info_2_Type |
            Copy_Image_To_Buffer_Info_2_Type |
            Image_Blit_2_Type |
            Blit_Image_Info_2_Type |
            Image_Resolve_2_Type |
            Resolve_Image_Info_2_Type |
            Physical_Device_Subgroup_Size_Control_Features_Type |
            Physical_Device_Subgroup_Size_Control_Properties_Type |
            Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type |
            Physical_Device_Inline_Uniform_Block_Features_Type |
            Physical_Device_Inline_Uniform_Block_Properties_Type |
            Write_Descriptor_Set_Inline_Uniform_Block_Type |
            Descriptor_Pool_Inline_Uniform_Block_Create_Info_Type |
            Physical_Device_Texture_Compression_ASTC_HDR_Features_Type |
            Rendering_Attachment_Info_Type |
            Rendering_Info_Type |
            Pipeline_Rendering_Create_Info_Type |
            Physical_Device_Dynamic_Rendering_Features_Type |
            Command_Buffer_Inheritance_Rendering_Info_Type |
            Physical_Device_Shader_Integer_Dot_Product_Features_Type |
            Physical_Device_Shader_Integer_Dot_Product_Properties_Type |
            Physical_Device_Texel_Buffer_Alignment_Properties_Type |
            Format_Properties_3_Type |
            Physical_Device_Maintenance_4_Features_Type |
            Physical_Device_Maintenance_4_Properties_Type |
            Device_Buffer_Memory_Requirements_Type |
            Device_Image_Memory_Requirements_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in
            Physical_Device_Vulkan_1_3_Features_Type |
            Physical_Device_Vulkan_1_3_Properties_Type |
            Physical_Device_Shader_Terminate_Invocation_Features_Type |
            Physical_Device_Tool_Properties_Type |
            Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type |
            Physical_Device_Private_Data_Features_Type |
            Physical_Device_Pipeline_Creation_Cache_Control_Features_Type |
            Physical_Device_Synchronization_2_Features_Type |
            Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type |
            Physical_Device_Image_Robustness_Features_Type |
            Physical_Device_Subgroup_Size_Control_Features_Type |
            Physical_Device_Subgroup_Size_Control_Properties_Type |
            Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type |
            Physical_Device_Inline_Uniform_Block_Features_Type |
            Physical_Device_Inline_Uniform_Block_Properties_Type |
            Physical_Device_Texture_Compression_ASTC_HDR_Features_Type |
            Physical_Device_Dynamic_Rendering_Features_Type |
            Physical_Device_Shader_Integer_Dot_Product_Features_Type |
            Physical_Device_Shader_Integer_Dot_Product_Properties_Type |
            Physical_Device_Texel_Buffer_Alignment_Properties_Type |
            Format_Properties_3_Type |
            Physical_Device_Maintenance_4_Features_Type |
            Physical_Device_Maintenance_4_Properties_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C compatible records.
    type Physical_Device_Vulkan_1_3_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vulkan_1_3_Features_Type;
        Next: C.Out_Structure_C_Access;
        Robust_Image_Access: Interfaces.Unsigned_32;
        Inline_Uniform_Block: Interfaces.Unsigned_32;
        Descriptor_Binding_Inline_Uniform_Block_Update_After_Bind:
            Interfaces.Unsigned_32;
        Pipeline_Creation_Cache_Control: Interfaces.Unsigned_32;
        Private_Data: Interfaces.Unsigned_32;
        Shader_Demote_To_Helper_Invocation: Interfaces.Unsigned_32;
        Shader_Terminate_Invocation: Interfaces.Unsigned_32;
        Subgroup_Size_Control: Interfaces.Unsigned_32;
        Compute_Full_Subgroups: Interfaces.Unsigned_32;
        Synchronization_2: Interfaces.Unsigned_32;
        Texture_Compression_ASTC_HDR: Interfaces.Unsigned_32;
        Shader_Zero_Initialize_Workgroup_Memory: Interfaces.Unsigned_32;
        Dynamic_Rendering: Interfaces.Unsigned_32;
        Shader_Integer_Dot_Product: Interfaces.Unsigned_32;
        Maintenance_4: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_1_3_Features_C_Access is
        access Physical_Device_Vulkan_1_3_Features_C
        with Convention => C;

    type Physical_Device_Vulkan_1_3_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vulkan_1_3_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Min_Subgroup_Size: Interfaces.Unsigned_32;
        Max_Subgroup_Size: Interfaces.Unsigned_32;
        Max_Compute_Workgroup_Subgroups: Interfaces.Unsigned_32;
        Required_Subgroup_Size_Stages: Shader_Stage_Flags :=
            Shader_Stage_No_Bit;
        Max_Inline_Uniform_Block_Size: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Inline_Uniform_Blocks: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Inline_Uniform_Blocks:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Inline_Uniform_Blocks: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Inline_Uniform_Blocks:
            Interfaces.Unsigned_32;
        Max_Inline_Uniform_Total_Size: Interfaces.Unsigned_32;
        Integer_Dot_Product_8Bit_Unsigned_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_8Bit_Signed_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_8Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_4x8Bit_Packed_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_4x8Bit_Packed_Signed_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_4x8Bit_Packed_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_16Bit_Unsigned_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_16Bit_Signed_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_16Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_32Bit_Unsigned_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_32Bit_Signed_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_32Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_64Bit_Unsigned_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_64Bit_Signed_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_64Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Signed_Accelerated:
            Interfaces.Unsigned_32;
  Integer_Dot_Product_Accumulating_Saturating_8Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
 Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
   Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Signed_Accelerated:
            Interfaces.Unsigned_32;
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Signed_Accelerated:
            Interfaces.Unsigned_32;
 Integer_Dot_Product_Accumulating_Saturating_16Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Signed_Accelerated:
            Interfaces.Unsigned_32;
 Integer_Dot_Product_Accumulating_Saturating_32Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Signed_Accelerated:
            Interfaces.Unsigned_32;
 Integer_Dot_Product_Accumulating_Saturating_64Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Storage_Texel_Buffer_Offset_Alignment_Bytes: Device_Size;
        Storage_Texel_Buffer_Offset_Single_Texel_Alignment:
            Interfaces.Unsigned_32;
        Uniform_Texel_Buffer_Offset_Alignment_Bytes: Device_Size;
        Uniform_Texel_Buffer_Offset_Single_Texel_Alignment:
            Interfaces.Unsigned_32;
        Max_Buffer_Size: Device_Size;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_1_3_Properties_C_Access is
        access Physical_Device_Vulkan_1_3_Properties_C
        with Convention => C;

    type Pipeline_Creation_Feedback_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Creation_Feedback_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Pipeline_Creation_Feedback: Pipeline_Creation_Feedback_Access;
        Pipeline_Stage_Creation_Feedback_Count: Interfaces.Unsigned_32;
        Pipeline_Stage_Creation_Feedbacks: Pipeline_Creation_Feedback_Access;
    end record
        with Convention => C;

    type Pipeline_Creation_Feedback_Create_Info_C_Access is
        access Pipeline_Creation_Feedback_Create_Info_C
        with Convention => C;

    type Physical_Device_Shader_Terminate_Invocation_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Terminate_Invocation_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Terminate_Invocation: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Terminate_Invocation_Features_C_Access is
        access Physical_Device_Shader_Terminate_Invocation_Features_C
        with Convention => C;

    type Physical_Device_Tool_Properties_C is
    record
        Record_Type: Out_Structure_Type := Physical_Device_Tool_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Name: Interfaces.C.char_array(1 .. Max_Extension_Name_Size);
        Version: Interfaces.C.char_array(1 .. Max_Extension_Name_Size);
        Purposes: Tool_Purpose_Flags;
        Description: Interfaces.C.char_array(1 .. Max_Description_Size);
        Layer: Interfaces.C.char_array(1 .. Max_Extension_Name_Size);
    end record
        with Convention => C;

    type Physical_Device_Tool_Properties_C_Access is
        access Physical_Device_Tool_Properties_C
        with Convention => C;

    type Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Demote_To_Helper_Invocation: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C_Access is
        access Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C
        with Convention => C;

    type Physical_Device_Private_Data_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Private_Data_Features_Type;
        Next: C.Out_Structure_C_Access;
        Private_Data: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Private_Data_Features_C_Access is
        access Physical_Device_Private_Data_Features_C
        with Convention => C;

    type Device_Private_Data_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Private_Data_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Private_Data_Slot_Request_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Device_Private_Data_Create_Info_C_Access is
        access Device_Private_Data_Create_Info_C
        with Convention => C;

    type Private_Data_Slot_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Private_Data_Slot_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Private_Data_Slot_Create_Flags;
    end record
        with Convention => C;

    type Private_Data_Slot_Create_Info_C_Access is
        access Private_Data_Slot_Create_Info_C
        with Convention => C;

    type Physical_Device_Pipeline_Creation_Cache_Control_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Creation_Cache_Control_Features_Type;
        Next: C.Out_Structure_C_Access;
        Pipeline_Creation_Cache_Control: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Creation_Cache_Control_Features_C_Access is
        access Physical_Device_Pipeline_Creation_Cache_Control_Features_C
        with Convention => C;

    type Memory_Barrier_2_C is
    record
        Record_Type: In_Structure_Type := Memory_Barrier_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Stage_Mask: Pipeline_Stage_Flags_2;
        Src_Access_Mask: Access_Flags_2;
        Dst_Stage_Mask: Pipeline_Stage_Flags_2;
        Dst_Access_Mask: Access_Flags_2;
    end record
        with Convention => C;

    type Memory_Barrier_2_C_Access is access Memory_Barrier_2_C
        with Convention => C;

    type Buffer_Memory_Barrier_2_C is
    record
        Record_Type: In_Structure_Type := Buffer_Memory_Barrier_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Stage_Mask: Pipeline_Stage_Flags_2;
        Src_Access_Mask: Access_Flags_2;
        Dst_Stage_Mask: Pipeline_Stage_Flags_2;
        Dst_Access_Mask: Access_Flags_2;
        Src_Queue_Family_Index: Queue_Family_Index;
        Dst_Queue_Family_Index: Queue_Family_Index;
        Buffer: Vulkan.Buffer;
        Offset: Device_Size;
        Size: Device_Size;
    end record
        with Convention => C;

    type Buffer_Memory_Barrier_2_C_Access is access Buffer_Memory_Barrier_2_C
        with Convention => C;

    type Image_Memory_Barrier_2_C is
    record
        Record_Type: In_Structure_Type := Image_Memory_Barrier_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Stage_Mask: Pipeline_Stage_Flags_2;
        Src_Access_Mask: Access_Flags_2;
        Dst_Stage_Mask: Pipeline_Stage_Flags_2;
        Dst_Access_Mask: Access_Flags_2;
        Old_Layout: Image_Layout;
        New_Layout: Image_Layout;
        Src_Queue_Family_Index: Queue_Family_Index;
        Dst_Queue_Family_Index: Queue_Family_Index;
        Image: Vulkan.Image;
        Subresource_Range: Image_Subresource_Range;
    end record
        with Convention => C;

    type Image_Memory_Barrier_2_C_Access is access Image_Memory_Barrier_2_C
        with Convention => C;

    package Memory_Barrier_2_C_Arrays is new C_Arrays(Memory_Barrier_2_C);

    package Buffer_Memory_Barrier_2_C_Arrays is new C_Arrays
        (Buffer_Memory_Barrier_2_C);

    package Image_Memory_Barrier_2_C_Arrays is new C_Arrays
        (Image_Memory_Barrier_2_C);

    type Dependency_Info_C is
    record
        Record_Type: In_Structure_Type := Dependency_Info_Type;
        Next: C.In_Structure_C_Access;
        Dependency_Flags: Vulkan.Dependency_Flags;
        Memory_Barrier_Count: Interfaces.Unsigned_32;
        Memory_Barriers: Memory_Barrier_2_C_Arrays.Pointer;
        Buffer_Memory_Barrier_Count: Interfaces.Unsigned_32;
        Buffer_Memory_Barriers: Buffer_Memory_Barrier_2_C_Arrays.Pointer;
        Image_Memory_Barrier_Count: Interfaces.Unsigned_32;
        Image_Memory_Barriers: Image_Memory_Barrier_2_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Dependency_Info_C_Access is access Dependency_Info_C
        with Convention => C;

    type Semaphore_Submit_Info_C is
    record
        Record_Type: In_Structure_Type := Semaphore_Submit_Info_Type;
        Next: C.In_Structure_C_Access;
        Semaphore: Vulkan.Semaphore;
        Value: Semaphore_Value;
        Stage_Mask: Pipeline_Stage_Flags_2;
        Device_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Semaphore_Submit_Info_C_Access is access Semaphore_Submit_Info_C
        with Convention => C;

    type Command_Buffer_Submit_Info_C is
    record
        Record_Type: In_Structure_Type := Command_Buffer_Submit_Info_Type;
        Next: C.In_Structure_C_Access;
        Command_Buffer: Vulkan.Command_Buffer;
        Device_Mask: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Command_Buffer_Submit_Info_C_Access is
        access Command_Buffer_Submit_Info_C
        with Convention => C;

    package Semaphore_Submit_Info_C_Arrays is new C_Arrays
        (Semaphore_Submit_Info_C);

    package Command_Buffer_Submit_Info_C_Arrays is new C_Arrays
        (Command_Buffer_Submit_Info_C);

    type Submit_Info_2_C is
    record
        Record_Type: In_Structure_Type := Submit_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Flags: Submit_Flags;
        Wait_Semaphore_Info_Count: Interfaces.Unsigned_32;
        Wait_Semaphore_Infos: Semaphore_Submit_Info_C_Arrays.Pointer;
        Command_Buffer_Info_Count: Interfaces.Unsigned_32;
        Command_Buffer_Infos: Command_Buffer_Submit_Info_C_Arrays.Pointer;
        Signal_Semaphore_Info_Count: Interfaces.Unsigned_32;
        Signal_Semaphore_Infos: Semaphore_Submit_Info_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Submit_Info_2_C_Access is access Submit_Info_2_C
        with Convention => C;

    type Physical_Device_Synchronization_2_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Synchronization_2_Features_Type;
        Next: C.Out_Structure_C_Access;
        Synchronization_2: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Synchronization_2_Features_C_Access is
        access Physical_Device_Synchronization_2_Features_C
        with Convention => C;

    type Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Zero_Initialize_Workgroup_Memory: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C_Access is
        access Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C
        with Convention => C;

    type Physical_Device_Image_Robustness_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Image_Robustness_Features_Type;
        Next: C.Out_Structure_C_Access;
        Robust_Image_Access: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Image_Robustness_Features_C_Access is
        access Physical_Device_Image_Robustness_Features_C
        with Convention => C;

    type Buffer_Copy_2_C is
    record
        Record_Type: In_Structure_Type := Buffer_Copy_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Offset: Device_Size;
        Dst_Offset: Device_Size;
        Size: Device_Size;
    end record
        with Convention => C;

    type Buffer_Copy_2_C_Access is access Buffer_Copy_2_C
        with Convention => C;

    package Buffer_Copy_2_C_Arrays is new C_Arrays(Buffer_Copy_2_C);

    type Copy_Buffer_Info_2_C is
    record
        Record_Type: In_Structure_Type := Copy_Buffer_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Buffer: Buffer;
        Dst_Buffer: Buffer;
        Region_Count: Interfaces.Unsigned_32;
        Regions: Buffer_Copy_2_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Copy_Buffer_Info_2_C_Access is access Copy_Buffer_Info_2_C
        with Convention => C;

    type Image_Copy_2_C is
    record
        Record_Type: In_Structure_Type := Image_Copy_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Subresource: Image_Subresource_Layers;
        Src_Offset: Offset_3D;
        Dst_Subresource: Image_Subresource_Layers;
        Dst_Offset: Offset_3D;
        Extent: Extent_3D;
    end record
        with Convention => C;

    type Image_Copy_2_C_Access is access Image_Copy_2_C
        with Convention => C;

    package Image_Copy_2_C_Arrays is new C_Arrays(Image_Copy_2_C);

    type Copy_Image_Info_2_C is
    record
        Record_Type: In_Structure_Type := Copy_Image_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Region_Count: Interfaces.Unsigned_32;
        Regions: Image_Copy_2_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Copy_Image_Info_2_C_Access is access Copy_Image_Info_2_C
        with Convention => C;

    type Buffer_Image_Copy_2_C is
    record
        Record_Type: In_Structure_Type := Buffer_Image_Copy_2_Type;
        Next: C.In_Structure_C_Access;
        Buffer_Offset: Device_Size;
        Buffer_Row_Length: Width;
        Buffer_Image_Height: Height;
        Image_Subresource: Image_Subresource_Layers;
        Image_Offset: Offset_3D;
        Image_Extent: Extent_3D;
    end record
        with Convention => C;

    type Buffer_Image_Copy_2_C_Access is access Buffer_Image_Copy_2_C
        with Convention => C;

    package Buffer_Image_Copy_2_C_Arrays is new C_Arrays(Buffer_Image_Copy_2_C);

    type Copy_Buffer_To_Image_Info_2_C is
    record
        Record_Type: In_Structure_Type := Copy_Buffer_To_Image_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Buffer: Buffer;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Region_Count: Interfaces.Unsigned_32;
        Regions: Buffer_Image_Copy_2_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Copy_Buffer_To_Image_Info_2_C_Access is
        access Copy_Buffer_To_Image_Info_2_C
        with Convention => C;

    type Copy_Image_To_Buffer_Info_2_C is
    record
        Record_Type: In_Structure_Type := Copy_Image_To_Buffer_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Buffer: Buffer;
        Region_Count: Interfaces.Unsigned_32;
        Regions: Buffer_Image_Copy_2_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Copy_Image_To_Buffer_Info_2_C_Access is
        access Copy_Image_To_Buffer_Info_2_C
        with Convention => C;

    type Image_Blit_2_C is
    record
        Record_Type: In_Structure_Type := Image_Blit_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Subresource: Image_Subresource_Layers;
        Src_Offsets: Offset_3D_Array(1 .. 2);
        Dst_Subresource: Image_Subresource_Layers;
        Dst_Offsets: Offset_3D_Array(1 .. 2);
    end record
        with Convention => C;

    type Image_Blit_2_C_Access is access Image_Blit_2_C
        with Convention => C;

    package Image_Blit_2_C_Arrays is new C_Arrays(Image_Blit_2_C);

    type Blit_Image_Info_2_C is
    record
        Record_Type: In_Structure_Type := Blit_Image_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Region_Count: Interfaces.Unsigned_32;
        Regions: Image_Blit_2_C_Arrays.Pointer;
        Filter: Vulkan.Filter;
    end record
        with Convention => C;

    type Blit_Image_Info_2_C_Access is access Blit_Image_Info_2_C
        with Convention => C;

    type Image_Resolve_2_C is
    record
        Record_Type: In_Structure_Type := Image_Resolve_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Subresource: Image_Subresource_Layers;
        Src_Offset: Offset_3D;
        Dst_Subresource: Image_Subresource_Layers;
        Dst_Offset: Offset_3D;
        Extent: Extent_3D;
    end record
        with Convention => C;

    type Image_Resolve_2_C_Access is access Image_Resolve_2_C
        with Convention => C;

    package Image_Resolve_2_C_Arrays is new C_Arrays(Image_Resolve_2_C);

    type Resolve_Image_Info_2_C is
    record
        Record_Type: In_Structure_Type := Resolve_Image_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Region_Count: Interfaces.Unsigned_32;
        Regions: Image_Resolve_2_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Resolve_Image_Info_2_C_Access is access Resolve_Image_Info_2_C
        with Convention => C;

    type Physical_Device_Subgroup_Size_Control_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Subgroup_Size_Control_Features_Type;
        Next: C.Out_Structure_C_Access;
        Subgroup_Size_Control: Interfaces.Unsigned_32;
        Compute_Full_Subgroups: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Subgroup_Size_Control_Features_C_Access is
        access Physical_Device_Subgroup_Size_Control_Features_C
        with Convention => C;

    type Physical_Device_Subgroup_Size_Control_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Subgroup_Size_Control_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Min_Subgroup_Size: Interfaces.Unsigned_32;
        Max_Subgroup_Size: Interfaces.Unsigned_32;
        Max_Compute_Workgroup_Subgroups: Interfaces.Unsigned_32;
        Required_Subgroup_Size_Stages: Shader_Stage_Flags;
    end record
        with Convention => C;

    type Physical_Device_Subgroup_Size_Control_Properties_C_Access is
        access Physical_Device_Subgroup_Size_Control_Properties_C
        with Convention => C;

    type Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C is
    record
        Record_Type: Out_Structure_Type :=
            Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type;
        Next: C.Out_Structure_C_Access;
        Required_Subgroup_Size: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C_Access is
        access Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C
        with Convention => C;

    type Physical_Device_Inline_Uniform_Block_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Inline_Uniform_Block_Features_Type;
        Next: C.Out_Structure_C_Access;
        Inline_Uniform_Block: Interfaces.Unsigned_32;
        Descriptor_Binding_Inline_Uniform_Block_Update_After_Bind:
            Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Inline_Uniform_Block_Features_C_Access is
        access Physical_Device_Inline_Uniform_Block_Features_C
        with Convention => C;

    type Physical_Device_Inline_Uniform_Block_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Inline_Uniform_Block_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Inline_Uniform_Block_Size: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Inline_Uniform_Blocks: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Inline_Uniform_Blocks:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Inline_Uniform_Blocks: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Inline_Uniform_Blocks:
            Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Inline_Uniform_Block_Properties_C_Access is
        access Physical_Device_Inline_Uniform_Block_Properties_C
        with Convention => C;

    type Write_Descriptor_Set_Inline_Uniform_Block_C is
    record
        Record_Type: In_Structure_Type :=
            Write_Descriptor_Set_Inline_Uniform_Block_Type;
        Next: C.In_Structure_C_Access;
        Data_Size: Interfaces.Unsigned_32;
        Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Write_Descriptor_Set_Inline_Uniform_Block_C_Access is
        access Write_Descriptor_Set_Inline_Uniform_Block_C
        with Convention => C;

    type Descriptor_Pool_Inline_Uniform_Block_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Descriptor_Pool_Inline_Uniform_Block_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Max_Inline_Uniform_Block_Bindings: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Descriptor_Pool_Inline_Uniform_Block_Create_Info_C_Access is
        access Descriptor_Pool_Inline_Uniform_Block_Create_Info_C
        with Convention => C;

    type Physical_Device_Texture_Compression_ASTC_HDR_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Texture_Compression_ASTC_HDR_Features_Type;
        Next: C.Out_Structure_C_Access;
        Texture_Compression_ASTC_HDR: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Texture_Compression_ASTC_HDR_Features_C_Access is
        access Physical_Device_Texture_Compression_ASTC_HDR_Features_C
        with Convention => C;

    type Rendering_Attachment_Info_C(Clear_Type: Clear_Value_Type;
                                     Color_Type: Clear_Color_Type) is
    record
        Record_Type: In_Structure_Type := Rendering_Attachment_Info_Type;
        Next: C.In_Structure_C_Access;
        Image_View: Vulkan.Image_View;
        Image_Layout: Vulkan.Image_Layout;
        Resolve_Mode: Resolve_Mode_Flags;
        Resolve_Image_View: Vulkan.Image_View;
        Resolve_Image_Layout: Vulkan.Image_Layout;
        Load_Op: Attachment_Load_Op;
        Store_Op: Attachment_Store_Op;

        case Clear_Type is
            when Clear_Color =>
                Color_Clear_Value: C.Clear_Value_C(Clear_Color, Color_Type);
            when Clear_Depth_Stencil =>
                Depth_Stencil_Clear_Value: C.Clear_Value_C(Clear_Depth_Stencil,
                                                           Color_Type);
        end case;
    end record
        with Convention => C,
             Unchecked_Union;

    type Rendering_Attachment_Info_C_Access is
        access Rendering_Attachment_Info_C
        with Convention => C;

    subtype Arrayed_Rendering_Attachment_Info_C is
        Rendering_Attachment_Info_C(Clear_Color, Clear_Color_Float);

    package Rendering_Attachment_Info_C_Arrays is new C_Arrays
        (Arrayed_Rendering_Attachment_Info_C);

    type Rendering_Info_C is
    record
        Record_Type: In_Structure_Type := Rendering_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Rendering_Flags;
        Render_Area: Rect_2D;
        Layer_Count: Array_Layers;
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachments: Rendering_Attachment_Info_C_Arrays.Pointer;
        Depth_Attachment: Rendering_Attachment_Info_C_Access;
        Stencil_Attachment: Rendering_Attachment_Info_C_Access;
    end record
        with Convention => C;

    type Rendering_Info_C_Access is access Rendering_Info_C
        with Convention => C;

    type Pipeline_Rendering_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Rendering_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Formats: C_V1_2.Format_Arrays.Pointer;
        Depth_Attachment_Format: Format;
        Stencil_Attachment_Format: Format;
    end record
        with Convention => C;

    type Pipeline_Rendering_Create_Info_C_Access is
        access Pipeline_Rendering_Create_Info_C
        with Convention => C;

    type Physical_Device_Dynamic_Rendering_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Dynamic_Rendering_Features_Type;
        Next: C.Out_Structure_C_Access;
        Dynamic_Rendering: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Dynamic_Rendering_Features_C_Access is
        access Physical_Device_Dynamic_Rendering_Features_C
        with Convention => C;

    type Command_Buffer_Inheritance_Rendering_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Command_Buffer_Inheritance_Rendering_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Rendering_Flags;
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachment_Count: Interfaces.Unsigned_32;
        Color_Attachment_Formats: C_V1_2.Format_Arrays.Pointer;
        Depth_Attachment_Format: Format;
        Stencil_Attachment_Format: Format;
        Rasterization_Samples: Sample_Count_Flags;
    end record
        with Convention => C;

    type Command_Buffer_Inheritance_Rendering_Info_C_Access is
        access Command_Buffer_Inheritance_Rendering_Info_C
        with Convention => C;

    type Physical_Device_Shader_Integer_Dot_Product_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Integer_Dot_Product_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Integer_Dot_Product: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Integer_Dot_Product_Features_C_Access is
        access Physical_Device_Shader_Integer_Dot_Product_Features_C
        with Convention => C;

    type Physical_Device_Shader_Integer_Dot_Product_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Integer_Dot_Product_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Integer_Dot_Product_8Bit_Unsigned_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_8Bit_Signed_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_8Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_4x8Bit_Packed_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_4x8Bit_Packed_Signed_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_4x8Bit_Packed_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_16Bit_Unsigned_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_16Bit_Signed_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_16Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_32Bit_Unsigned_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_32Bit_Signed_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_32Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_64Bit_Unsigned_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_64Bit_Signed_Accelerated: Interfaces.Unsigned_32;
        Integer_Dot_Product_64Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Signed_Accelerated:
            Interfaces.Unsigned_32;
  Integer_Dot_Product_Accumulating_Saturating_8Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
 Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
   Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Signed_Accelerated:
            Interfaces.Unsigned_32;
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Signed_Accelerated:
            Interfaces.Unsigned_32;
 Integer_Dot_Product_Accumulating_Saturating_16Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Signed_Accelerated:
            Interfaces.Unsigned_32;
 Integer_Dot_Product_Accumulating_Saturating_32Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Unsigned_Accelerated:
            Interfaces.Unsigned_32;
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Signed_Accelerated:
            Interfaces.Unsigned_32;
 Integer_Dot_Product_Accumulating_Saturating_64Bit_Mixed_Signedness_Accelerated:
            Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Integer_Dot_Product_Properties_C_Access is
        access Physical_Device_Shader_Integer_Dot_Product_Properties_C
        with Convention => C;

    type Physical_Device_Texel_Buffer_Alignment_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Texel_Buffer_Alignment_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Storage_Texel_Buffer_Offset_Alignment_Bytes: Device_Size;
        Storage_Texel_Buffer_Offset_Single_Texel_Alignment:
            Interfaces.Unsigned_32;
        Uniform_Texel_Buffer_Offset_Alignment_Bytes: Device_Size;
        Uniform_Texel_Buffer_Offset_Single_Texel_Alignment:
            Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Texel_Buffer_Alignment_Properties_C_Access is
        access Physical_Device_Texel_Buffer_Alignment_Properties_C
        with Convention => C;

    type Format_Properties_3_C is
    record
        Record_Type: Out_Structure_Type := Format_Properties_3_Type;
        Next: C.Out_Structure_C_Access;
        Linear_Tiling_Features: Format_Feature_Flags_2;
        Optimal_Tiling_Features: Format_Feature_Flags_2;
        Buffer_Features: Format_Feature_Flags_2;
    end record
        with Convention => C;

    type Format_Properties_3_C_Access is access Format_Properties_3_C
        with Convention => C;

    type Physical_Device_Maintenance_4_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_4_Features_Type;
        Next: C.Out_Structure_C_Access;
        Maintenance_4: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_4_Features_C_Access is
        access Physical_Device_Maintenance_4_Features_C
        with Convention => C;

    type Physical_Device_Maintenance_4_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_4_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Buffer_Size: Device_Size;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_4_Properties_C_Access is
        access Physical_Device_Maintenance_4_Properties_C
        with Convention => C;

    type Device_Buffer_Memory_Requirements_C is
    record
        Record_Type: In_Structure_Type :=
            Device_Buffer_Memory_Requirements_Type;
        Next: C.In_Structure_C_Access;
        Create_Info: C.Buffer_Create_Info_C_Access;
    end record
        with Convention => C;

    type Device_Buffer_Memory_Requirements_C_Access is
        access Device_Buffer_Memory_Requirements_C
        with Convention => C;

    type Device_Image_Memory_Requirements_C is
    record
        Record_Type: In_Structure_Type := Device_Image_Memory_Requirements_Type;
        Next: C.In_Structure_C_Access;
        Create_Info: C.Image_Create_Info_C_Access;
        Plane_Aspect: Image_Aspect_Flags;
    end record
        with Convention => C;

    type Device_Image_Memory_Requirements_C_Access is
        access Device_Image_Memory_Requirements_C
        with Convention => C;

    -- Load all the function pointers.
    procedure Load(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    type vkGetPhysicalDeviceToolProperties_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Tool_Count: in out Interfaces.Unsigned_32;
             Tool_Properties: access Physical_Device_Tool_Properties_C)
                return Result
        with Convention => C;

    vkGetPhysicalDeviceToolProperties: vkGetPhysicalDeviceToolProperties_Access;
    
    type vkCreatePrivateDataSlot_Access is
        access function(Device: in Vulkan.Device;
                        Create_Info: in Private_Data_Slot_Create_Info_C;
                        Allocator: access constant Allocation_Callbacks;
                        Private_Data_Slot: out Vulkan.Private_Data_Slot)
                            return Result
        with Convention => C;

    vkCreatePrivateDataSlot: vkCreatePrivateDataSlot_Access;

    type vkDestroyPrivateDataSlot_Access is
        access procedure(Device: in Vulkan.Device;
                         Private_Data_Slot: in Vulkan.Private_Data_Slot;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroyPrivateDataSlot: vkDestroyPrivateDataSlot_Access;

    type vkSetPrivateData_Access is
        access function(Device: in Vulkan.Device;
                        Object_Type: in Vulkan.Object_Type;
                        Object_Handle: in Vulkan.Object_Handle;
                        Private_Data_Slot: in Vulkan.Private_Data_Slot;
                        Data: in Interfaces.Unsigned_64)
                            return Result
        with Convention => C;

    vkSetPrivateData: vkSetPrivateData_Access;

    type vkGetPrivateData_Access is
        access procedure(Device: in Vulkan.Device;
                         Object_Type: in Vulkan.Object_Type;
                         Object_Handle: in Vulkan.Object_Handle;
                         Private_Data_Slot: in Vulkan.Private_Data_Slot;
                         Data: out Interfaces.Unsigned_64)
        with Convention => C;

    vkGetPrivateData: vkGetPrivateData_Access;

    type vkCmdSetEvent2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Event: in Vulkan.Event;
                         Dependency_Info: in Dependency_Info_C)
        with Convention => C;

    vkCmdSetEvent2: vkCmdSetEvent2_Access;

    type vkCmdResetEvent2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Event: in Vulkan.Event;
                         Stage_Mask: in Pipeline_Stage_Flags_2)
        with Convention => C;

    vkCmdResetEvent2: vkCmdResetEvent2_Access;

    type vkCmdWaitEvents2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Event_Count: in Interfaces.Unsigned_32;
                         Events: access constant Event;
                         Dependency_Infos: access constant Dependency_Info_C)
        with Convention => C;

    vkCmdWaitEvents2: vkCmdWaitEvents2_Access;

    type vkCmdPipelineBarrier2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Dependency_Info: in Dependency_Info_C)
        with Convention => C;

    vkCmdPipelineBarrier2: vkCmdPipelineBarrier2_Access;

    type vkCmdWriteTimestamp2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Stage: in Pipeline_Stage_Flags_2;
                         Query_Pool: in Vulkan.Query_Pool;
                         Query: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdWriteTimestamp2: vkCmdWriteTimestamp2_Access;

    type vkQueueSubmit2_Access is
        access function(Queue: in Vulkan.Queue;
                        Submit_Count: in Interfaces.Unsigned_32;
                        Submits: access constant Submit_Info_2_C;
                        Fence: in Vulkan.Fence) return Result
        with Convention => C;

    vkQueueSubmit2: vkQueueSubmit2_Access;

    type vkCmdCopyBuffer2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Copy_Buffer_Info: in Copy_Buffer_Info_2_C)
        with Convention => C;

    vkCmdCopyBuffer2: vkCmdCopyBuffer2_Access;

    type vkCmdCopyImage2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Copy_Image_Info: in Copy_Image_Info_2_C)
        with Convention => C;

    vkCmdCopyImage2: vkCmdCopyImage2_Access;

    type vkCmdCopyBufferToImage2_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Copy_Buffer_To_Image_Info: in Copy_Buffer_To_Image_Info_2_C)
        with Convention => C;

    vkCmdCopyBufferToImage2: vkCmdCopyBufferToImage2_Access;

    type vkCmdCopyImageToBuffer2_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Copy_Image_To_Buffer_Info: in Copy_Image_To_Buffer_Info_2_C)
        with Convention => C;

    vkCmdCopyImageToBuffer2: vkCmdCopyImageToBuffer2_Access;

    type vkCmdBlitImage2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Blit_Image_Info: in Blit_Image_Info_2_C)
        with Convention => C;

    vkCmdBlitImage2: vkCmdBlitImage2_Access;

    type vkCmdResolveImage2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Resolve_Image_Info: in Resolve_Image_Info_2_C)
        with Convention => C;

    vkCmdResolveImage2: vkCmdResolveImage2_Access;

    type vkCmdBeginRendering_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Rendering_Info: in Rendering_Info_C)
        with Convention => C;

    vkCmdBeginRendering: vkCmdBeginRendering_Access;

    type vkCmdEndRendering_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer)
        with Convention => C;

    vkCmdEndRendering: vkCmdEndRendering_Access;

    type vkCmdSetCullMode_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Cull_Mode: in Cull_Mode_Flags)
        with Convention => C;

    vkCmdSetCullMode: vkCmdSetCullMode_Access;

    type vkCmdSetFrontFace_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Front_Face: in Vulkan.Front_Face)
        with Convention => C;

    vkCmdSetFrontFace: vkCmdSetFrontFace_Access;

    type vkCmdSetPrimitiveTopology_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Primitive_Topology: in Vulkan.Primitive_Topology)
        with Convention => C;

    vkCmdSetPrimitiveTopology: vkCmdSetPrimitiveTopology_Access;

    type vkCmdSetViewportWithCount_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Viewport_Count: in Interfaces.Unsigned_32;
                         Viewports: access constant Viewport)
        with Convention => C;

    vkCmdSetViewportWithCount: vkCmdSetViewportWithCount_Access;

    type vkCmdSetScissorWithCount_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Scissor_Count: in Interfaces.Unsigned_32;
                         Scissors: access constant Rect_2D)
        with Convention => C;

    vkCmdSetScissorWithCount: vkCmdSetScissorWithCount_Access;

    type vkCmdBindVertexBuffers2_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         First_Binding,
                         Binding_Count: in Interfaces.Unsigned_32;
                         Buffers: access constant Buffer;
                         Offsets,
                         Sizes,
                         Strides: access constant Device_Size)
        with Convention => C;

    vkCmdBindVertexBuffers2: vkCmdBindVertexBuffers2_Access;
        
    type vkCmdSetDepthTestEnable_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Depth_Test_Enable: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdSetDepthTestEnable: vkCmdSetDepthTestEnable_Access;

    type vkCmdSetDepthWriteEnable_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Depth_Write_Enable: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdSetDepthWriteEnable: vkCmdSetDepthWriteEnable_Access;

    type vkCmdSetStencilOp_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Face_Mask: in Stencil_Face_Flags;
                         Fail_Op, Pass_Op, Depth_Fail_Op: in Stencil_Op;
                         Compare_Op: in Vulkan.Compare_Op)
        with Convention => C;

    vkCmdSetStencilOp: vkCmdSetStencilOp_Access;

    type vkCmdSetRasterizerDiscardEnable_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Rasterizer_Discard_Enable: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdSetRasterizerDiscardEnable: vkCmdSetRasterizerDiscardEnable_Access;

    type vkCmdSetDepthBiasEnable_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Depth_Bias_Enable: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdSetDepthBiasEnable: vkCmdSetDepthBiasEnable_Access;

    type vkCmdSetPrimitiveRestartEnable_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Primitive_Restart_Enable: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdSetPrimitiveRestartEnable: vkCmdSetPrimitiveRestartEnable_Access;

    type vkGetDeviceBufferMemoryRequirements_Access is
        access procedure(Device: in Vulkan.Device;
                         Info: in Device_Buffer_Memory_Requirements_C;
                         Memory_Requirements:
                            in out C_V1_1.Memory_Requirements_2_C)
        with Convention => C;

    vkGetDeviceBufferMemoryRequirements:
        vkGetDeviceBufferMemoryRequirements_Access;

    type vkGetDeviceImageMemoryRequirements_Access is
        access procedure(Device: in Vulkan.Device;
                         Info: in Device_Image_Memory_Requirements_C;
                         Memory_Requirements:
                            in out C_V1_1.Memory_Requirements_2_C)
        with Convention => C;

    vkGetDeviceImageMemoryRequirements:
        vkGetDeviceImageMemoryRequirements_Access;

    type vkGetDeviceImageSparseMemoryRequirements_Access is
        access procedure
            (Device: in Vulkan.Device;
             Info: in Device_Image_Memory_Requirements_C;
             Sparse_Memory_Requirement_Count: in out Interfaces.Unsigned_32;
             Sparse_Memory_Requirements:
                access C_V1_1.Sparse_Image_Memory_Requirements_2_C)
        with Convention => C;

    vkGetDeviceImageSparseMemoryRequirements:
        vkGetDeviceImageSparseMemoryRequirements_Access;

    -- Record conversions and management.
    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_3_Features;
                     C_Struct: in Physical_Device_Vulkan_1_3_Features_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Vulkan_1_3_Properties;
                     C_Struct: in Physical_Device_Vulkan_1_3_Properties_C);

    function To_C(Struct: in Pipeline_Creation_Feedback_Create_Info)
        return Pipeline_Creation_Feedback_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Creation_Feedback_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Shader_Terminate_Invocation_Features;
         C_Struct: in Physical_Device_Shader_Terminate_Invocation_Features_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Tool_Properties;
                     C_Struct: in Physical_Device_Tool_Properties_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Shader_Demote_To_Helper_Invocation_Features;
         C_Struct:
            in Physical_Device_Shader_Demote_To_Helper_Invocation_Features_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Private_Data_Features;
                     C_Struct: in Physical_Device_Private_Data_Features_C);

    function To_C(Struct: in Device_Private_Data_Create_Info)
        return Device_Private_Data_Create_Info_C;
    procedure Free(Struct: in out Device_Private_Data_Create_Info_C);

    function To_C(Struct: in Private_Data_Slot_Create_Info)
        return Private_Data_Slot_Create_Info_C;
    procedure Free(Struct: in out Private_Data_Slot_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Pipeline_Creation_Cache_Control_Features;
         C_Struct:
            in Physical_Device_Pipeline_Creation_Cache_Control_Features_C);

    function To_C(Struct: in Memory_Barrier_2) return Memory_Barrier_2_C;
    procedure Free(Struct: in out Memory_Barrier_2_C);

    function To_C(Struct: in Buffer_Memory_Barrier_2)
        return Buffer_Memory_Barrier_2_C;
    procedure Free(Struct: in out Buffer_Memory_Barrier_2_C);

    function To_C(Struct: in Image_Memory_Barrier_2)
        return Image_Memory_Barrier_2_C;
    procedure Free(Struct: in out Image_Memory_Barrier_2_C);

    function To_C(Struct: in Dependency_Info) return Dependency_Info_C;
    procedure Free(Struct: in out Dependency_Info_C);

    function To_C(Struct: in Semaphore_Submit_Info)
        return Semaphore_Submit_Info_C;
    procedure Free(Struct: in out Semaphore_Submit_Info_C);

    function To_C(Struct: in Command_Buffer_Submit_Info)
        return Command_Buffer_Submit_Info_C;
    procedure Free(Struct: in out Command_Buffer_Submit_Info_C);

    function To_C(Struct: in Submit_Info_2) return Submit_Info_2_C;
    procedure Free(Struct: in out Submit_Info_2_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Synchronization_2_Features;
         C_Struct: in Physical_Device_Synchronization_2_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Zero_Initialize_Workgroup_Memory_Features;
         C_Struct:
            in Physical_Device_Zero_Initialize_Workgroup_Memory_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Image_Robustness_Features;
         C_Struct: in Physical_Device_Image_Robustness_Features_C);

    function To_C(Struct: in Buffer_Copy_2) return Buffer_Copy_2_C;
    procedure Free(Struct: in out Buffer_Copy_2_C);

    function To_C(Struct: in Copy_Buffer_Info_2) return Copy_Buffer_Info_2_C;
    procedure Free(Struct: in out Copy_Buffer_Info_2_C);

    function To_C(Struct: in Image_Copy_2) return Image_Copy_2_C;
    procedure Free(Struct: in out Image_Copy_2_C);
    
    function To_C(Struct: in Copy_Image_Info_2) return Copy_Image_Info_2_C;
    procedure Free(Struct: in out Copy_Image_Info_2_C);

    function To_C(Struct: in Buffer_Image_Copy_2) return Buffer_Image_Copy_2_C;
    procedure Free(Struct: in out Buffer_Image_Copy_2_C);

    function To_C(Struct: in Copy_Buffer_To_Image_Info_2)
        return Copy_Buffer_To_Image_Info_2_C;
    procedure Free(Struct: in out Copy_Buffer_To_Image_Info_2_C);

    function To_C(Struct: in Copy_Image_To_Buffer_Info_2)
        return Copy_Image_To_Buffer_Info_2_C;
    procedure Free(Struct: in out Copy_Image_To_Buffer_Info_2_C);

    function To_C(Struct: in Image_Blit_2) return Image_Blit_2_C;
    procedure Free(Struct: in out Image_Blit_2_C);

    function To_C(Struct: in Blit_Image_Info_2) return Blit_Image_Info_2_C;
    procedure Free(Struct: in out Blit_Image_Info_2_C);

    function To_C(Struct: in Image_Resolve_2) return Image_Resolve_2_C;
    procedure Free(Struct: in out Image_Resolve_2_C);

    function To_C(Struct: in Resolve_Image_Info_2)
        return Resolve_Image_Info_2_C;
    procedure Free(Struct: in out Resolve_Image_Info_2_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Subgroup_Size_Control_Features;
         C_Struct: in Physical_Device_Subgroup_Size_Control_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Subgroup_Size_Control_Properties;
         C_Struct: in Physical_Device_Subgroup_Size_Control_Properties_C);

    procedure To_Ada
        (Ada_Struct:
            in out Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info;
         C_Struct:
            in Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Inline_Uniform_Block_Features;
         C_Struct: in Physical_Device_Inline_Uniform_Block_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Inline_Uniform_Block_Properties;
         C_Struct: in Physical_Device_Inline_Uniform_Block_Properties_C);

    function To_C(Struct: in Write_Descriptor_Set_Inline_Uniform_Block)
        return Write_Descriptor_Set_Inline_Uniform_Block_C;
    procedure Free(Struct: in out Write_Descriptor_Set_Inline_Uniform_Block_C);

    function To_C(Struct: in Descriptor_Pool_Inline_Uniform_Block_Create_Info)
        return Descriptor_Pool_Inline_Uniform_Block_Create_Info_C;
    procedure Free
        (Struct: in out Descriptor_Pool_Inline_Uniform_Block_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Texture_Compression_ASTC_HDR_Features;
         C_Struct: in Physical_Device_Texture_Compression_ASTC_HDR_Features_C);

    function To_C(Struct: in Rendering_Attachment_Info)
        return Rendering_Attachment_Info_C;
    procedure Free(Struct: in out Rendering_Attachment_Info_C);

    function To_C(Struct: in Rendering_Info) return Rendering_Info_C;
    procedure Free(Struct: in out Rendering_Info_C);

    function To_C(Struct: in Pipeline_Rendering_Create_Info)
        return Pipeline_Rendering_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Rendering_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Dynamic_Rendering_Features;
         C_Struct: in Physical_Device_Dynamic_Rendering_Features_C);

    function To_C(Struct: in Command_Buffer_Inheritance_Rendering_Info)
        return Command_Buffer_Inheritance_Rendering_Info_C;
    procedure Free(Struct: in out Command_Buffer_Inheritance_Rendering_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Integer_Dot_Product_Features;
         C_Struct: in Physical_Device_Shader_Integer_Dot_Product_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Physical_Device_Shader_Integer_Dot_Product_Properties;
         C_Struct: in Physical_Device_Shader_Integer_Dot_Product_Properties_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Texel_Buffer_Alignment_Properties;
         C_Struct: in Physical_Device_Texel_Buffer_Alignment_Properties_C);

    procedure To_Ada(Ada_Struct: in out Format_Properties_3;
                     C_Struct: in Format_Properties_3_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Maintenance_4_Features;
                     C_Struct: in Physical_Device_Maintenance_4_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_4_Properties;
         C_Struct: in Physical_Device_Maintenance_4_Properties_C);

    function To_C(Struct: in Device_Buffer_Memory_Requirements)
        return Device_Buffer_Memory_Requirements_C;
    procedure Free(Struct: in out Device_Buffer_Memory_Requirements_C);

    function To_C(Struct: in Device_Image_Memory_Requirements)
        return Device_Image_Memory_Requirements_C;
    procedure Free(Struct: in out Device_Image_Memory_Requirements_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_V1_3;


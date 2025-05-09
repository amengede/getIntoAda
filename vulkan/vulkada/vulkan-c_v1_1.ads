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

-- Subprogram access for Vulkan 1.1

with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.C_V1_1 is
    -- Structure classification.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Subgroup_Properties_Type |
            Bind_Buffer_Memory_Info_Type |
            Bind_Image_Memory_Info_Type |
            Physical_Device_16Bit_Storage_Features_Type |
            Memory_Dedicated_Requirements_Type |
            Memory_Dedicated_Allocate_Info_Type |
            Memory_Allocate_Flags_Info_Type |
            Device_Group_Render_Pass_Begin_Info_Type |
            Device_Group_Command_Buffer_Begin_Info_Type |
            Device_Group_Submit_Info_Type |
            Device_Group_Bind_Sparse_Info_Type |
            Bind_Buffer_Memory_Device_Group_Info_Type |
            Bind_Image_Memory_Device_Group_Info_Type |
            Physical_Device_Group_Properties_Type |
            Device_Group_Device_Create_Info_Type |
            Buffer_Memory_Requirements_Info_2_Type |
            Image_Memory_Requirements_Info_2_Type |
            Image_Sparse_Memory_Requirements_Info_2_Type |
            Memory_Requirements_2_Type |
            Sparse_Image_Memory_Requirements_2_Type |
            Physical_Device_Features_2_Type |
            Physical_Device_Properties_2_Type |
            Format_Properties_2_Type |
            Image_Format_Properties_2_Type |
            Physical_Device_Image_Format_Info_2_Type |
            Queue_Family_Properties_2_Type |
            Physical_Device_Memory_Properties_2_Type |
            Sparse_Image_Format_Properties_2_Type |
            Physical_Device_Sparse_Image_Format_Info_2_Type |
            Physical_Device_Point_Clipping_Properties_Type |
            Render_Pass_Input_Attachment_Aspect_Create_Info_Type |
            Image_View_Usage_Create_Info_Type |
            Pipeline_Tessellation_Domain_Origin_State_Create_Info_Type |
            Render_Pass_Multiview_Create_Info_Type |
            Physical_Device_Multiview_Features_Type |
            Physical_Device_Multiview_Properties_Type |
            Physical_Device_Variable_Pointer_Features_Type |
            Physical_Device_Protected_Memory_Features_Type |
            Physical_Device_Protected_Memory_Properties_Type |
            Device_Queue_Info_2_Type |
            Protected_Submit_Info_Type |
            Sampler_YCbCr_Conversion_Create_Info_Type |
            Sampler_YCbCr_Conversion_Info_Type |
            Bind_Image_Plane_Memory_Info_Type |
            Image_Plane_Memory_Requirements_Info_Type |
            Physical_Device_Sampler_YCbCr_Conversion_Features_Type |
            Sampler_YCbCr_Conversion_Image_Format_Properties_Type |
            Descriptor_Update_Template_Create_Info_Type |
            Physical_Device_External_Image_Format_Info_Type |
            External_Image_Format_Properties_Type |
            Physical_Device_External_Buffer_Info_Type |
            External_Buffer_Properties_Type |
            Physical_Device_ID_Properties_Type |
            External_Memory_Image_Create_Info_Type |
            External_Memory_Buffer_Create_Info_Type |
            Export_Memory_Allocate_Info_Type |
            Physical_Device_External_Fence_Info_Type |
            External_Fence_Properties_Type |
            Export_Fence_Create_Info_Type |
            Export_Semaphore_Create_Info_Type |
            Physical_Device_External_Semaphore_Info_Type |
            External_Semaphore_Properties_Type |
            Physical_Device_Maintenance_3_Properties_Type |
            Descriptor_Set_Layout_Support_Type |
            Physical_Device_Shader_Draw_Parameter_Features_Type;
            
    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C compatible records.
    type Physical_Device_Subgroup_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Subgroup_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Subgroup_Size: Interfaces.Unsigned_32;
        Supported_Stages: Shader_Stage_Flags;
        Supported_Operations: Subgroup_Feature_Flags;
        Quad_Operations_In_All_Stages: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Subgroup_Properties_C_Access is
        access Physical_Device_Subgroup_Properties_C
        with Convention => C;

    type Bind_Buffer_Memory_Info_C is
    record
        Record_Type: In_Structure_Type := Bind_Buffer_Memory_Info_Type;
        Next: C.In_Structure_C_Access;
        Buffer: Vulkan.Buffer;
        Memory: Device_Memory;
        Memory_Offset: Device_Size;
    end record
        with Convention => C;

    type Bind_Buffer_Memory_Info_C_Access is
        access Bind_Buffer_Memory_Info_C
        with Convention => C;

    type Bind_Image_Memory_Info_C is
    record
        Record_Type: In_Structure_Type := Bind_Image_Memory_Info_Type;
        Next: C.In_Structure_C_Access;
        Image: Vulkan.Image;
        Memory: Device_Memory;
        Memory_Offset: Device_Size;
    end record
        with Convention => C;

    type Bind_Image_Memory_Info_C_Access is
        access Bind_Image_Memory_Info_C
        with Convention => C;

    type Physical_Device_16Bit_Storage_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_16Bit_Storage_Features_Type;
        Next: C.Out_Structure_C_Access;
        Storage_Buffer_16Bit_Access: Interfaces.Unsigned_32;
        Uniform_And_Storage_Buffer_16Bit_Access: Interfaces.Unsigned_32;
        Storage_Push_Constant_16: Interfaces.Unsigned_32;
        Storage_Input_Output_16: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_16Bit_Storage_Features_C_Access is
        access Physical_Device_16Bit_Storage_Features_C
        with Convention => C;

    type Memory_Dedicated_Requirements_C is
    record
        Record_Type: Out_Structure_Type := Memory_Dedicated_Requirements_Type;
        Next: C.Out_Structure_C_Access;
        Prefers_Dedicated_Allocation: Interfaces.Unsigned_32;
        Requires_Dedicated_Allocation: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Memory_Dedicated_Requirements_C_Access is
        access Memory_Dedicated_Requirements_C
        with Convention => C;

    type Memory_Dedicated_Allocate_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Dedicated_Allocate_Info_Type;
        Next: C.In_Structure_C_Access;
        Image: Vulkan.Image;
        Buffer: Vulkan.Buffer;
    end record
        with Convention => C;

    type Memory_Dedicated_Allocate_Info_C_Access is
        access Memory_Dedicated_Allocate_Info_C
        with Convention => C;

    type Memory_Allocate_Flags_Info_C is
    record
        Record_Type: In_Structure_Type := Memory_Allocate_Flags_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Memory_Allocate_Flags;
        Device_Mask: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Memory_Allocate_Flags_Info_C_Access is
        access Memory_Allocate_Flags_Info_C
        with Convention => C;

    type Device_Group_Render_Pass_Begin_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Device_Group_Render_Pass_Begin_Info_Type;
        Next: C.In_Structure_C_Access;
        Device_Mask: Interfaces.Unsigned_32;
        Device_Render_Area_Count: Interfaces.Unsigned_32;
        Device_Render_Areas: C.Rect_2D_Arrays.Pointer;
    end record
        with Convention => C;

    type Device_Group_Render_Pass_Begin_Info_C_Access is
        access Device_Group_Render_Pass_Begin_Info_C
        with Convention => C;

    type Device_Group_Command_Buffer_Begin_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Device_Group_Command_Buffer_Begin_Info_Type;
        Next: C.In_Structure_C_Access;
        Device_Mask: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Device_Group_Command_Buffer_Begin_Info_C_Access is
        access Device_Group_Command_Buffer_Begin_Info_C
        with Convention => C;

    type Device_Group_Submit_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Group_Submit_Info_Type;
        Next: C.In_Structure_C_Access;
        Wait_Semaphore_Count: Interfaces.Unsigned_32;
        Wait_Semaphore_Device_Indices: C.Uint32_t_Arrays.Pointer;
        Command_Buffer_Count: Interfaces.Unsigned_32;
        Command_Buffer_Device_Masks: C.Uint32_t_Arrays.Pointer;
        Signal_Semaphore_Count: Interfaces.Unsigned_32;
        Signal_Semaphore_Device_Indices: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Device_Group_Submit_Info_C_Access is
        access Device_Group_Submit_Info_C
        with Convention => C;

    type Device_Group_Bind_Sparse_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Group_Bind_Sparse_Info_Type;
        Next: C.In_Structure_C_Access;
        Resource_Device_Index: Interfaces.Unsigned_32;
        Memory_Device_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Device_Group_Bind_Sparse_Info_C_Access is
        access Device_Group_Bind_Sparse_Info_C
        with Convention => C;

    type Bind_Buffer_Memory_Device_Group_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Bind_Buffer_Memory_Device_Group_Info_Type;
        Next: C.In_Structure_C_Access;
        Device_Index_Count: Interfaces.Unsigned_32;
        Device_Indices: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Bind_Buffer_Memory_Device_Group_Info_C_Access is
        access Bind_Buffer_Memory_Device_Group_Info_C
        with Convention => C;

    type Bind_Image_Memory_Device_Group_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Bind_Image_Memory_Device_Group_Info_Type;
        Next: C.In_Structure_C_Access;
        Device_Index_Count: Interfaces.Unsigned_32;
        Device_Indices: C.Uint32_t_Arrays.Pointer;
        Split_Instance_Bind_Region_Count: Interfaces.Unsigned_32;
        Split_Instance_Bind_Regions: C.Rect_2D_Arrays.Pointer;
    end record
        with Convention => C;

    type Bind_Image_Memory_Device_Group_Info_C_Access is
        access Bind_Image_Memory_Device_Group_Info_C
        with Convention => C;

    type Physical_Device_Array is array (Positive range <>) of Physical_Device
        with Convention => C;

    type Physical_Device_Group_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Group_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Physical_Device_Count: Interfaces.Unsigned_32;
        Physical_Devices: Physical_Device_Array(1 .. Max_Device_Group_Size);
        Subset_Allocation: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Group_Properties_C_Access is
        access Physical_Device_Group_Properties_C
        with Convention => C;

    package Physical_Device_Arrays is new C_Arrays(Physical_Device);

    type Device_Group_Device_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Group_Device_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Physical_Device_Count: Interfaces.Unsigned_32;
        Physical_Devices: Physical_Device_Arrays.Pointer;
    end record
        with Convention => C;

    type Device_Group_Device_Create_Info_C_Access is
        access Device_Group_Device_Create_Info_C
        with Convention => C;

    type Buffer_Memory_Requirements_Info_2_C is
    record
        Record_Type: In_Structure_Type :=
            Buffer_Memory_Requirements_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Buffer: Vulkan.Buffer;
    end record
        with Convention => C;

    type Buffer_Memory_Requirements_Info_2_C_Access is
        access Buffer_Memory_Requirements_Info_2_C
        with Convention => C;

    type Image_Memory_Requirements_Info_2_C is
    record
        Record_Type: In_Structure_Type := Image_Memory_Requirements_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Image: Vulkan.Image;
    end record
        with Convention => C;

    type Image_Memory_Requirements_Info_2_C_Access is
        access Image_Memory_Requirements_Info_2_C
        with Convention => C;

    type Image_Sparse_Memory_Requirements_Info_2_C is
    record
        Record_Type: In_Structure_Type :=
            Image_Sparse_Memory_Requirements_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Image: Vulkan.Image;
    end record
        with Convention => C;

    type Image_Sparse_Memory_Requirements_Info_2_C_Access is
        access Image_Sparse_Memory_Requirements_Info_2_C
        with Convention => C;

    type Memory_Requirements_2_C is
    record
        Record_Type: Out_Structure_Type := Memory_Requirements_2_Type;
        Next: C.Out_Structure_C_Access;
        Memory_Requirements: Vulkan.Memory_Requirements;
    end record
        with Convention => C;

    type Memory_Requirements_2_C_Access is
        access Memory_Requirements_2_C
        with Convention => C;

    type Sparse_Image_Memory_Requirements_2_C is
    record
        Record_Type: Out_Structure_Type :=
            Sparse_Image_Memory_Requirements_2_Type;
        Next: C.Out_Structure_C_Access;
        Memory_Requirements: Sparse_Image_Memory_Requirements;
    end record
        with Convention => C;

    type Sparse_Image_Memory_Requirements_2_C_Access is
        access Sparse_Image_Memory_Requirements_2_C
        with Convention => C;

    type Physical_Device_Features_2_C is
    record
        Record_Type: Out_Structure_Type := Physical_Device_Features_2_Type;
        Next: C.Out_Structure_C_Access;
        Features: C.Physical_Device_Features_C;
    end record
        with Convention => C;

    type Physical_Device_Features_2_C_Access is
        access Physical_Device_Features_2_C
        with Convention => C;

    type Physical_Device_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Physical_Device_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Properties: C.Physical_Device_Properties_C;
    end record
        with Convention => C;

    type Physical_Device_Properties_2_C_Access is
        access Physical_Device_Properties_2_C
        with Convention => C;

    type Format_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Format_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Format_Properties: Vulkan.Format_Properties;
    end record
        with Convention => C;

    type Format_Properties_2_C_Access is access Format_Properties_2_C
        with Convention => C;

    type Image_Format_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Image_Format_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Image_Format_Properties: Vulkan.Image_Format_Properties;
    end record
        with Convention => C;

    type Image_Format_Properties_2_C_Access is
        access Image_Format_Properties_2_C
        with Convention => C;

    type Physical_Device_Image_Format_Info_2_C is
    record
        Record_Type: In_Structure_Type :=
            Physical_Device_Image_Format_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Format: Vulkan.Format;
        Image_Type: Vulkan.Image_Type;
        Tiling: Image_Tiling;
        Usage: Image_Usage_Flags;
        Flags: Image_Create_Flags;
    end record
        with Convention => C;

    type Physical_Device_Image_Format_Info_2_C_Access is
        access Physical_Device_Image_Format_Info_2_C
        with Convention => C;

    type Queue_Family_Properties_2_C is
    record
        Record_Type: Out_Structure_Type := Queue_Family_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Queue_Family_Properties: Vulkan.Queue_Family_Properties;
    end record
        with Convention => C;

    type Queue_Family_Properties_2_C_Access is
        access Queue_Family_Properties_2_C
        with Convention => C;

    type Physical_Device_Memory_Properties_2_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Memory_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Memory_Properties: C.Physical_Device_Memory_Properties_C;
    end record
        with Convention => C;

    type Physical_Device_Memory_Properties_2_C_Access is
        access Physical_Device_Memory_Properties_2_C
        with Convention => C;

    type Sparse_Image_Format_Properties_2_C is
    record
        Record_Type: Out_Structure_Type :=
            Sparse_Image_Format_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Properties: Sparse_Image_Format_Properties;
    end record
        with Convention => C;

    type Sparse_Image_Format_Properties_2_C_Access is
        access Sparse_Image_Format_Properties_2_C
        with Convention => C;

    type Physical_Device_Sparse_Image_Format_Info_2_C is
    record
        Record_Type: In_Structure_Type :=
            Physical_Device_Sparse_Image_Format_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Format: Vulkan.Format;
        Image_Type: Vulkan.Image_Type;
        Samples: Sample_Count_Flags;
        Usage: Image_Usage_Flags;
        Tiling: Image_Tiling;
    end record
        with Convention => C;

    type Physical_Device_Sparse_Image_Format_Info_2_C_Access is
        access Physical_Device_Sparse_Image_Format_Info_2_C
        with Convention => C;

    type Physical_Device_Point_Clipping_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Point_Clipping_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Point_Clipping_Behavior: Vulkan.Point_Clipping_Behavior;
    end record
        with Convention => C;

    type Physical_Device_Point_Clipping_Properties_C_Access is
        access Physical_Device_Point_Clipping_Properties_C
        with Convention => C;

    package Input_Attachment_Aspect_Reference_Arrays is
        new C_Arrays(Input_Attachment_Aspect_Reference);

    type Render_Pass_Input_Attachment_Aspect_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Render_Pass_Input_Attachment_Aspect_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Aspect_Reference_Count: Interfaces.Unsigned_32;
        Aspect_References: Input_Attachment_Aspect_Reference_Arrays.Pointer;
    end record
        with Convention => C;

    type Render_Pass_Input_Attachment_Aspect_Create_Info_C_Access is
        access Render_Pass_Input_Attachment_Aspect_Create_Info_C
        with Convention => C;

    type Image_View_Usage_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Image_View_Usage_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Usage: Image_Usage_Flags;
    end record
        with Convention => C;

    type Image_View_Usage_Create_Info_C_Access is
        access Image_View_Usage_Create_Info_C
        with Convention => C;

    type Pipeline_Tessellation_Domain_Origin_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Tessellation_Domain_Origin_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Domain_Origin: Tessellation_Domain_Origin;
    end record
        with Convention => C;

    type Pipeline_Tessellation_Domain_Origin_State_Create_Info_C_Access is
        access Pipeline_Tessellation_Domain_Origin_State_Create_Info_C
        with Convention => C;

    type Render_Pass_Multiview_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Render_Pass_Multiview_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Subpass_Count: Interfaces.Unsigned_32;
        View_Masks: C.Uint32_t_Arrays.Pointer;
        Dependency_Count: Interfaces.Unsigned_32;
        View_Offsets: C.Uint32_t_Arrays.Pointer;
        Correlation_Mask_Count: Interfaces.Unsigned_32;
        Correlation_Masks: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Render_Pass_Multiview_Create_Info_C_Access is
        access Render_Pass_Multiview_Create_Info_C
        with Convention => C;

    type Physical_Device_Multiview_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Multiview_Features_Type;
        Next: C.Out_Structure_C_Access;
        Multiview: Interfaces.Unsigned_32;
        Multiview_Geometry_Shader: Interfaces.Unsigned_32;
        Multiview_Tessellation_Shader: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Multiview_Features_C_Access is
        access Physical_Device_Multiview_Features_C
        with Convention => C;

    type Physical_Device_Multiview_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Multiview_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Multiview_View_Count: Interfaces.Unsigned_32;
        Max_Multiview_Instance_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Multiview_Properties_C_Access is
        access Physical_Device_Multiview_Properties_C
        with Convention => C;

    type Physical_Device_Variable_Pointer_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Variable_Pointer_Features_Type;
        Next: C.Out_Structure_C_Access;
        Variable_Pointers_Storage_Buffer: Interfaces.Unsigned_32;
        Variable_Pointers: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Variable_Pointer_Features_C_Access is
        access Physical_Device_Variable_Pointer_Features_C
        with Convention => C;

    type Physical_Device_Protected_Memory_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Protected_Memory_Features_Type;
        Next: C.Out_Structure_C_Access;
        Protected_Memory: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Protected_Memory_Features_C_Access is
        access Physical_Device_Protected_Memory_Features_C
        with Convention => C;

    type Physical_Device_Protected_Memory_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Protected_Memory_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Protected_No_Fault: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Protected_Memory_Properties_C_Access is
        access Physical_Device_Protected_Memory_Properties_C
        with Convention => C;

    type Device_Queue_Info_2_C is
    record
        Record_Type: In_Structure_Type := Device_Queue_Info_2_Type;
        Next: C.In_Structure_C_Access;
        Flags: Device_Queue_Create_Flags;
        Queue_Family_Index: Vulkan.Queue_Family_Index;
        Queue_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Device_Queue_Info_2_C_Access is
        access Device_Queue_Info_2_C
        with Convention => C;

    type Protected_Submit_Info_C is
    record
        Record_Type: In_Structure_Type := Protected_Submit_Info_Type;
        Next: C.In_Structure_C_Access;
        Protected_Submit: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Protected_Submit_Info_C_Access is
        access Protected_Submit_Info_C
        with Convention => C;

    type Sampler_YCbCr_Conversion_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Sampler_YCbCr_Conversion_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Format: Vulkan.Format;
        YCbCr_Model: Sampler_YCbCr_Model_Conversion;
        YCbCr_Range: Sampler_YCbCr_Range;
        Components: Component_Mapping;
        X_Chroma_Offset: Chroma_Location;
        Y_Chroma_Offset: Chroma_Location;
        Chroma_Filter: Filter;
        Force_Explicit_Reconstruction: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Sampler_YCbCr_Conversion_Create_Info_C_Access is
        access Sampler_YCbCr_Conversion_Create_Info_C
        with Convention => C;

    type Sampler_YCbCr_Conversion_Info_C is
    record
        Record_Type: In_Structure_Type := Sampler_YCbCr_Conversion_Info_Type;
        Next: C.In_Structure_C_Access;
        Conversion: Sampler_YCbCr_Conversion;
    end record
        with Convention => C;

    type Sampler_YCbCr_Conversion_Info_C_Access is
        access Sampler_YCbCr_Conversion_Info_C
        with Convention => C;

    type Bind_Image_Plane_Memory_Info_C is
    record
        Record_Type: In_Structure_Type := Bind_Image_Plane_Memory_Info_Type;
        Next: C.In_Structure_C_Access;
        Plane_Aspect: Image_Aspect_Flags;
    end record
        with Convention => C;

    type Bind_Image_Plane_Memory_Info_C_Access is
        access Bind_Image_Plane_Memory_Info_C
        with Convention => C;

    type Image_Plane_Memory_Requirements_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Image_Plane_Memory_Requirements_Info_Type;
        Next: C.In_Structure_C_Access;
        Plane_Aspect: Image_Aspect_Flags;
    end record
        with Convention => C;

    type Image_Plane_Memory_Requirements_Info_C_Access is
        access Image_Plane_Memory_Requirements_Info_C
        with Convention => C;

    type Physical_Device_Sampler_YCbCr_Conversion_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Sampler_YCbCr_Conversion_Features_Type;
        Next: C.Out_Structure_C_Access;
        Sampler_YCbCr_Conversion: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Sampler_YCbCr_Conversion_Features_C_Access is
        access Physical_Device_Sampler_YCbCr_Conversion_Features_C
        with Convention => C;

    type Sampler_YCbCr_Conversion_Image_Format_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Sampler_YCbCr_Conversion_Image_Format_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Combined_Image_Sampler_Descriptor_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Sampler_YCbCr_Conversion_Image_Format_Properties_C_Access is
        access Sampler_YCbCr_Conversion_Image_Format_Properties_C
        with Convention => C;
        
    package Descriptor_Update_Template_Entry_Arrays is
        new C_Arrays(Descriptor_Update_Template_Entry);

    type Descriptor_Update_Template_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Descriptor_Update_Template_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Descriptor_Update_Template_Create_Flags;
        Descriptor_Update_Entry_Count: Interfaces.Unsigned_32;
        Descriptor_Update_Entries:
            Descriptor_Update_Template_Entry_Arrays.Pointer;
        Template_Type: Descriptor_Update_Template_Type;
        Descriptor_Set_Layout: Vulkan.Descriptor_Set_Layout;
        Pipeline_Bind_Point: Vulkan.Pipeline_Bind_Point;
        Pipeline_Layout: Vulkan.Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Descriptor_Update_Template_Create_Info_C_Access is
        access Descriptor_Update_Template_Create_Info_C
        with Convention => C;

    type Physical_Device_External_Image_Format_Info_C is
    record
        Record_Type: In_Structure_Type
            := Physical_Device_External_Image_Format_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Type: External_Memory_Handle_Type_Flags;
    end record
        with Convention => C;

    type Physical_Device_External_Image_Format_Info_C_Access is
        access Physical_Device_External_Image_Format_Info_C
        with Convention => C;

    type External_Image_Format_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            External_Image_Format_Properties_Type;
        Next: C.Out_Structure_C_Access;
        External_Memory_Properties: Vulkan.External_Memory_Properties;
    end record
        with Convention => C;

    type External_Image_Format_Properties_C_Access is
        access External_Image_Format_Properties_C
        with Convention => C;

    type Physical_Device_External_Buffer_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Physical_Device_External_Buffer_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Buffer_Create_Flags;
        Usage: Buffer_Usage_Flags;
        Handle_Type: External_Memory_Handle_Type_Flags;
    end record
        with Convention => C;

    type Physical_Device_External_Buffer_Info_C_Access is
        access Physical_Device_External_Buffer_Info_C
        with Convention => C;

    type External_Buffer_Properties_C is
    record
        Record_Type: Out_Structure_Type := External_Buffer_Properties_Type;
        Next: C.Out_Structure_C_Access;
        External_Memory_Properties: Vulkan.External_Memory_Properties;
    end record
        with Convention => C;

    type External_Buffer_Properties_C_Access is
        access External_Buffer_Properties_C
        with Convention => C;

    type Physical_Device_ID_Properties_C is
    record
        Record_Type: Out_Structure_Type := Physical_Device_ID_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Device_UUID: UUID;
        Driver_UUID: UUID;
        Device_LUID: LUID;
        Device_Node_Mask: Interfaces.Unsigned_32;
        Device_LUID_Valid: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_ID_Properties_C_Access is
        access Physical_Device_ID_Properties_C
        with Convention => C;

    type External_Memory_Image_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            External_Memory_Image_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Types: External_Memory_Handle_Type_Flags;
    end record
        with Convention => C;

    type External_Memory_Image_Create_Info_C_Access is
        access External_Memory_Image_Create_Info_C
        with Convention => C;

    type External_Memory_Buffer_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            External_Memory_Buffer_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Types: External_Memory_Handle_Type_Flags;
    end record
        with Convention => C;

    type External_Memory_Buffer_Create_Info_C_Access is
        access External_Memory_Buffer_Create_Info_C
        with Convention => C;

    type Export_Memory_Allocate_Info_C is
    record
        Record_Type: In_Structure_Type := Export_Memory_Allocate_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Types: External_Memory_Handle_Type_Flags;
    end record
        with Convention => C;

    type Export_Memory_Allocate_Info_C_Access is
        access Export_Memory_Allocate_Info_C
        with Convention => C;

    type Physical_Device_External_Fence_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Physical_Device_External_Fence_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Type: External_Fence_Handle_Type_Flags;
    end record
        with Convention => C;

    type Physical_Device_External_Fence_Info_C_Access is
        access Physical_Device_External_Fence_Info_C
        with Convention => C;

    type External_Fence_Properties_C is
    record
        Record_Type: Out_Structure_Type := External_Fence_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Export_From_Imported_Handle_Types: External_Fence_Handle_Type_Flags;
        Compatible_Handle_Types: External_Fence_Handle_Type_Flags;
        External_Fence_Features: External_Fence_Feature_Flags;
    end record
        with Convention => C;

    type External_Fence_Properties_C_Access is
        access External_Fence_Properties_C
        with Convention => C;

    type Export_Fence_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Export_Fence_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Types: External_Fence_Handle_Type_Flags;
    end record
        with Convention => C;

    type Export_Fence_Create_Info_C_Access is
        access Export_Fence_Create_Info_C
        with Convention => C;

    type Export_Semaphore_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Export_Semaphore_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Types: External_Semaphore_Handle_Type_Flags;
    end record
        with Convention => C;

    type Export_Semaphore_Create_Info_C_Access is
        access Export_Semaphore_Create_Info_C
        with Convention => C;

    type Physical_Device_External_Semaphore_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Physical_Device_External_Semaphore_Info_Type;
        Next: C.In_Structure_C_Access;
        Handle_Type: External_Semaphore_Handle_Type_Flags;
    end record
        with Convention => C;

    type Physical_Device_External_Semaphore_Info_C_Access is
        access Physical_Device_External_Semaphore_Info_C
        with Convention => C;

    type External_Semaphore_Properties_C is
    record
        Record_Type: Out_Structure_Type := External_Semaphore_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Export_From_Imported_Handle_Types: External_Semaphore_Handle_Type_Flags;
        Compatible_Handle_Types: External_Semaphore_Handle_Type_Flags;
        External_Semaphore_Features: External_Semaphore_Feature_Flags;
    end record
        with Convention => C;

    type External_Semaphore_Properties_C_Access is
        access External_Semaphore_Properties_C
        with Convention => C;

    type Physical_Device_Maintenance_3_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_3_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Per_Set_Descriptors: Interfaces.Unsigned_32;
        Max_Memory_Allocation_Size: Device_Size;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_3_Properties_C_Access is
        access Physical_Device_Maintenance_3_Properties_C
        with Convention => C;

    type Descriptor_Set_Layout_Support_C is
    record
        Record_Type: Out_Structure_Type := Descriptor_Set_Layout_Support_Type;
        Next: C.Out_Structure_C_Access;
        Supported: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Descriptor_Set_Layout_Support_C_Access is
        access Descriptor_Set_Layout_Support_C
        with Convention => C;

    type Physical_Device_Shader_Draw_Parameter_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Shader_Draw_Parameter_Features_Type;
        Next: C.Out_Structure_C_Access;
        Shader_Draw_Parameters: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Shader_Draw_Parameter_Features_C_Access is
        access Physical_Device_Shader_Draw_Parameter_Features_C
        with Convention => C;

    -- Load all the function pointers.
    procedure Load(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    type vkEnumerateInstanceVersion_Access is
        access procedure(Version: out Version_Number)
        with Convention => C;

    vkEnumerateInstanceVersion: vkEnumerateInstanceVersion_Access;

    type vkBindBufferMemory2_Access is
        access function
            (Device: in Vulkan.Device;
             Bind_Info_Count: in Interfaces.Unsigned_32;
             Bind_Infos: access constant Bind_Buffer_Memory_Info_C)
                return Result
        with Convention => C;

    vkBindBufferMemory2: vkBindBufferMemory2_Access;

    type vkBindImageMemory2_Access is
        access function
            (Device: in Vulkan.Device;
             Bind_Info_Count: in Interfaces.Unsigned_32;
             Bind_Infos: access constant Bind_Image_Memory_Info_C)
                return Result
        with Convention => C;

    vkBindImageMemory2: vkBindImageMemory2_Access;

    type vkGetDeviceGroupPeerMemoryFeatures_Access is
        access procedure(Device: in Vulkan.Device;
                         Heap_Index,
                         Local_Device_Index,
                         Remote_Device_Index: in Interfaces.Unsigned_32;
                         Peer_Memory_Features: out Peer_Memory_Feature_Flags)
        with Convention => C;

    vkGetDeviceGroupPeerMemoryFeatures:
        vkGetDeviceGroupPeerMemoryFeatures_Access;

    type vkCmdSetDeviceMask_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Device_Mask: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdSetDeviceMask: vkCmdSetDeviceMask_Access;

    type vkCmdDispatchBase_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Base_Group_X,
                         Base_Group_Y,
                         Base_Group_Z,
                         Group_Count_X,
                         Group_Count_Y,
                         Group_Count_Z: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdDispatchBase: vkCmdDispatchBase_Access;

    type vkEnumeratePhysicalDeviceGroups_Access is
        access function(Instance: in Vulkan.Instance;
                        Physical_Device_Group_Count:
                            in out Interfaces.Unsigned_32;
                        Physical_Device_Group_Properties:
                            access Physical_Device_Group_Properties_C)
                            return Result
        with Convention => C;

    vkEnumeratePhysicalDeviceGroups: vkEnumeratePhysicalDeviceGroups_Access;

    type vkGetImageMemoryRequirements2_Access is
        access procedure
            (Device: in Vulkan.Device;
             Info: in Image_Memory_Requirements_Info_2_C;
             Memory_Requirements: out Memory_Requirements_2_C)
        with Convention => C;

    vkGetImageMemoryRequirements2: vkGetImageMemoryRequirements2_Access;

    type vkGetBufferMemoryRequirements2_Access is
        access procedure
            (Device: in Vulkan.Device;
             Info: in Buffer_Memory_Requirements_Info_2_C;
             Memory_Requirements: out Memory_Requirements_2_C)
        with Convention => C;

    vkGetBufferMemoryRequirements2: vkGetBufferMemoryRequirements2_Access;

    type vkGetImageSparseMemoryRequirements2_Access is
        access procedure
            (Device: in Vulkan.Device;
             Info: in Image_Sparse_Memory_Requirements_Info_2_C;
             Sparse_Memory_Requirement_Count: in out Interfaces.Unsigned_32;
             Sparse_Memory_Requirements:
                access Sparse_Image_Memory_Requirements_2_C)
        with Convention => C;

    vkGetImageSparseMemoryRequirements2:
        vkGetImageSparseMemoryRequirements2_Access;

    type vkGetPhysicalDeviceFeatures2_Access is
        access procedure(Physical_Device: in Vulkan.Physical_Device;
                         Properties: in out Physical_Device_Features_2_C)
        with Convention => C;

    vkGetPhysicalDeviceFeatures2: vkGetPhysicalDeviceFeatures2_Access;

    type vkGetPhysicalDeviceProperties2_Access is
        access procedure(Physical_Device: in Vulkan.Physical_Device;
                         Properties: in out Physical_Device_Properties_2_C)
        with Convention => C;

    vkGetPhysicalDeviceProperties2: vkGetPhysicalDeviceProperties2_Access;

    type vkGetPhysicalDeviceFormatProperties2_Access is
        access procedure(Physical_Device: in Vulkan.Physical_Device;
                         Format: in Vulkan.Format;
                         Format_Properties: in out Format_Properties_2_C)
        with Convention => C;

    vkGetPhysicalDeviceFormatProperties2:
        vkGetPhysicalDeviceFormatProperties2_Access;

    type vkGetPhysicalDeviceImageFormatProperties2_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Image_Format_Info: in
                            Physical_Device_Image_Format_Info_2_C;
                        Image_Format_Properties: in out
                            Image_Format_Properties_2_C) return Result
        with Convention => C;

    vkGetPhysicalDeviceImageFormatProperties2:
        vkGetPhysicalDeviceImageFormatProperties2_Access;

    type vkGetPhysicalDeviceQueueFamilyProperties2_Access is
        access procedure
            (Physical_Device: in Vulkan.Physical_Device;
             Queue_Family_Property_Count: in out Interfaces.Unsigned_32;
             Queue_Family_Properties: access Queue_Family_Properties_2_C)
        with Convention => C;

    vkGetPhysicalDeviceQueueFamilyProperties2:
        vkGetPhysicalDeviceQueueFamilyProperties2_Access;

    type vkGetPhysicalDeviceMemoryProperties2_Access is
        access procedure
            (Physical_Device: in Vulkan.Physical_Device;
             Memory_Properties: out Physical_Device_Memory_Properties_2_C)
        with Convention => C;

    vkGetPhysicalDeviceMemoryProperties2:
        vkGetPhysicalDeviceMemoryProperties2_Access;

    type vkGetPhysicalDeviceSparseImageFormatProperties2_Access is
        access procedure
            (Physical_Device: in Vulkan.Physical_Device;
             Format_Info: in out Physical_Device_Sparse_Image_Format_Info_2_C;
             Property_Count: in out Interfaces.Unsigned_32;
             Properties: access Sparse_Image_Format_Properties_2_C)
        with Convention => C;

    vkGetPhysicalDeviceSparseImageFormatProperties2:
        vkGetPhysicalDeviceSparseImageFormatProperties2_Access;

    type vkTrimCommandPool_Access is
        access procedure(Device: in Vulkan.Device;
                         Command_Pool: in Vulkan.Command_Pool;
                         Flags: in Command_Pool_Trim_Flags)
        with Convention => C;

    vkTrimCommandPool: vkTrimCommandPool_Access;

    type vkGetDeviceQueue2_Access is
        access procedure(Device: in Vulkan.Device;
                         Queue_Info: in out Device_Queue_Info_2_C;
                         Queue: out Vulkan.Queue)
        with Convention => C;

    vkGetDeviceQueue2: vkGetDeviceQueue2_Access;

    type vkCreateSamplerYcbcrConversion_Access is
        access function
            (Device: in Vulkan.Device;
             Create_Info: in Sampler_YCbCr_Conversion_Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             YCbCr_Conversion: out Sampler_YCbCr_Conversion) return Result
        with Convention => C;

    vkCreateSamplerYcbcrConversion: vkCreateSamplerYcbcrConversion_Access;

    type vkDestroySamplerYcbcrConversion_Access is
        access procedure(Device: in Vulkan.Device;
                         YCbCr_Conversion: in Sampler_YCbCr_Conversion;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroySamplerYcbcrConversion: vkDestroySamplerYcbcrConversion_Access;

    type vkCreateDescriptorUpdateTemplate_Access is
        access function
            (Device: in Vulkan.Device;
             Create_Info: in Descriptor_Update_Template_Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Descriptor_Update_Template: out Vulkan.Descriptor_Update_Template)
        return Result
        with Convention => C;

    vkCreateDescriptorUpdateTemplate: vkCreateDescriptorUpdateTemplate_Access;

    type vkDestroyDescriptorUpdateTemplate_Access is
        access procedure
            (Device: in Vulkan.Device;
             Descriptor_Update_Template: in Vulkan.Descriptor_Update_Template;
             Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroyDescriptorUpdateTemplate: vkDestroyDescriptorUpdateTemplate_Access;

    type vkUpdateDescriptorSetWithTemplate_Access is
        access procedure
            (Device: in Vulkan.Device;
             Descriptor_Set: in Vulkan.Descriptor_Set;
             Descriptor_Update_Template: in Vulkan.Descriptor_Update_Template;
             Data: in Interfaces.C.Extensions.void_ptr)
        with Convention => C;

    vkUpdateDescriptorSetWithTemplate: vkUpdateDescriptorSetWithTemplate_Access;

    type vkGetPhysicalDeviceExternalBufferProperties_Access is
        access procedure
            (Physical_Device: in Vulkan.Physical_Device;
             External_Buffer_Info: in Physical_Device_External_Buffer_Info_C;
             External_Buffer_Properties: out External_Buffer_Properties_C)
        with Convention => C;

    vkGetPhysicalDeviceExternalBufferProperties:
        vkGetPhysicalDeviceExternalBufferProperties_Access;

    type vkGetPhysicalDeviceExternalFenceProperties_Access is
        access procedure
            (Physical_Device: in Vulkan.Physical_Device;
             External_Fence_Info: in Physical_Device_External_Fence_Info_C;
             External_Fence_Properties: out External_Fence_Properties_C)
        with Convention => C;

    vkGetPhysicalDeviceExternalFenceProperties:
        vkGetPhysicalDeviceExternalFenceProperties_Access;

    type vkGetPhysicalDeviceExternalSemaphoreProperties_Access is
        access procedure
            (Physical_Device: in Vulkan.Physical_Device;
             External_Semaphore_Info:
                in Physical_Device_External_Semaphore_Info_C;
             External_Semaphore_Properties: out External_Semaphore_Properties_C)
        with Convention => C;

    vkGetPhysicalDeviceExternalSemaphoreProperties:
        vkGetPhysicalDeviceExternalSemaphoreProperties_Access;

    type vkGetDescriptorSetLayoutSupport_Access is
        access procedure (Device: in Vulkan.Device;
                          Create_Info: in C.Descriptor_Set_Layout_Create_Info_C;
                          Support: out Descriptor_Set_Layout_Support_C)
        with Convention => C;

    vkGetDescriptorSetLayoutSupport: vkGetDescriptorSetLayoutSupport_Access;
    
    -- Record conversions and management.
    procedure To_Ada(Ada_Struct: in out Physical_Device_Subgroup_Properties;
                     C_Struct: in Physical_Device_Subgroup_Properties_C);

    function To_C(Struct: in Bind_Buffer_Memory_Info)
        return Bind_Buffer_Memory_Info_C;
    procedure Free(Struct: in out Bind_Buffer_Memory_Info_C);

    function To_C(Struct: in Bind_Image_Memory_Info)
        return Bind_Image_Memory_Info_C;
    procedure Free(Struct: in out Bind_Image_Memory_Info_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_16Bit_Storage_Features;
                     C_Struct: in Physical_Device_16Bit_Storage_Features_C);

    procedure To_Ada(Ada_Struct: in out Memory_Dedicated_Requirements;
                     C_Struct: in Memory_Dedicated_Requirements_C);

    function To_C(Struct: in Memory_Dedicated_Allocate_Info)
        return Memory_Dedicated_Allocate_Info_C;
    procedure Free(Struct: in out Memory_Dedicated_Allocate_Info_C);

    function To_C(Struct: in Memory_Allocate_Flags_Info)
        return Memory_Allocate_Flags_Info_C;
    procedure Free(Struct: in out Memory_Allocate_Flags_Info_C);

    function To_C(Struct: in Device_Group_Render_Pass_Begin_Info)
        return Device_Group_Render_Pass_Begin_Info_C;
    procedure Free(Struct: in out Device_Group_Render_Pass_Begin_Info_C);
    
    function To_C(Struct: in Device_Group_Command_Buffer_Begin_Info)
        return Device_Group_Command_Buffer_Begin_Info_C;
    procedure Free(Struct: in out Device_Group_Command_Buffer_Begin_Info_C);

    function To_C(Struct: in Device_Group_Submit_Info)
        return Device_Group_Submit_Info_C;
    procedure Free(Struct: in out Device_Group_Submit_Info_C);

    function To_C(Struct: in Device_Group_Bind_Sparse_Info)
        return Device_Group_Bind_Sparse_Info_C;
    procedure Free(Struct: in out Device_Group_Bind_Sparse_Info_C);

    function To_C(Struct: in Bind_Buffer_Memory_Device_Group_Info)
        return Bind_Buffer_Memory_Device_Group_Info_C;
    procedure Free(Struct: in out Bind_Buffer_Memory_Device_Group_Info_C);

    function To_C(Struct: in Bind_Image_Memory_Device_Group_Info)
        return Bind_Image_Memory_Device_Group_Info_C;
    procedure Free(Struct: in out Bind_Image_Memory_Device_Group_Info_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Group_Properties;
                     C_Struct: in Physical_Device_Group_Properties_C);

    function To_C(Struct: in Device_Group_Device_Create_Info)
        return Device_Group_Device_Create_Info_C;
    procedure Free(Struct: in out Device_Group_Device_Create_Info_C);

    function To_C(Struct: in Buffer_Memory_Requirements_Info_2)
        return Buffer_Memory_Requirements_Info_2_C;
    procedure Free(Struct: in out Buffer_Memory_Requirements_Info_2_C);

    function To_C(Struct: in Image_Memory_Requirements_Info_2)
        return Image_Memory_Requirements_Info_2_C;
    procedure Free(Struct: in out Image_Memory_Requirements_Info_2_C);

    function To_C(Struct: in Image_Sparse_Memory_Requirements_Info_2)
        return Image_Sparse_Memory_Requirements_Info_2_C;
    procedure Free(Struct: in out Image_Sparse_Memory_Requirements_Info_2_C);

    procedure To_Ada(Ada_Struct: in out Memory_Requirements_2;
                     C_Struct: in Memory_Requirements_2_C);

    procedure To_Ada(Ada_Struct: in out Sparse_Image_Memory_Requirements_2;
                     C_Struct: in Sparse_Image_Memory_Requirements_2_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Features_2;
                     C_Struct: in Physical_Device_Features_2_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Properties_2;
                     C_Struct: in Physical_Device_Properties_2_C);

    procedure To_Ada(Ada_Struct: in out Format_Properties_2;
                     C_Struct: in Format_Properties_2_C);

    procedure To_Ada(Ada_Struct: in out Image_Format_Properties_2;
                     C_Struct: in Image_Format_Properties_2_C);

    function To_C(Struct: in Physical_Device_Image_Format_Info_2)
        return Physical_Device_Image_Format_Info_2_C;
    procedure Free(Struct: in out Physical_Device_Image_Format_Info_2_C);

    procedure To_Ada(Ada_Struct: in out Queue_Family_Properties_2;
                     C_Struct: in Queue_Family_Properties_2_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Memory_Properties_2;
                     C_Struct: in Physical_Device_Memory_Properties_2_C);

    procedure To_Ada(Ada_Struct: in out Sparse_Image_Format_Properties_2;
                     C_Struct: in Sparse_Image_Format_Properties_2_C);

    function To_C(Struct: in Physical_Device_Sparse_Image_Format_Info_2)
        return Physical_Device_Sparse_Image_Format_Info_2_C;
    procedure Free(Struct: in out Physical_Device_Sparse_Image_Format_Info_2_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Point_Clipping_Properties;
         C_Struct: in Physical_Device_Point_Clipping_Properties_C);

    function To_C(Struct: in Render_Pass_Input_Attachment_Aspect_Create_Info)
        return Render_Pass_Input_Attachment_Aspect_Create_Info_C;
    procedure Free
        (Struct: in out Render_Pass_Input_Attachment_Aspect_Create_Info_C);

    function To_C(Struct: in Image_View_Usage_Create_Info)
        return Image_View_Usage_Create_Info_C;
    procedure Free(Struct: in out Image_View_Usage_Create_Info_C);

    function To_C
        (Struct: in Pipeline_Tessellation_Domain_Origin_State_Create_Info)
        return Pipeline_Tessellation_Domain_Origin_State_Create_Info_C;
    procedure Free
        (Struct:
            in out Pipeline_Tessellation_Domain_Origin_State_Create_Info_C);

    function To_C(Struct: in Render_Pass_Multiview_Create_Info)
        return Render_Pass_Multiview_Create_Info_C;
    procedure Free(Struct: in out Render_Pass_Multiview_Create_Info_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Multiview_Features;
                     C_Struct: in Physical_Device_Multiview_Features_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_Multiview_Properties;
                     C_Struct: in Physical_Device_Multiview_Properties_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Variable_Pointer_Features;
         C_Struct: in Physical_Device_Variable_Pointer_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Protected_Memory_Features;
         C_Struct: in Physical_Device_Protected_Memory_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Protected_Memory_Properties;
         C_Struct: in Physical_Device_Protected_Memory_Properties_C);

    function To_C(Struct: in Device_Queue_Info_2) return Device_Queue_Info_2_C;
    procedure Free(Struct: in out Device_Queue_Info_2_C);

    function To_C(Struct: in Protected_Submit_Info)
        return Protected_Submit_Info_C;
    procedure Free(Struct: in out Protected_Submit_Info_C);

    function To_C(Struct: in Sampler_YCbCr_Conversion_Create_Info)
        return Sampler_YCbCr_Conversion_Create_Info_C;
    procedure Free(Struct: in out Sampler_YCbCr_Conversion_Create_Info_C);

    function To_C(Struct: in Sampler_YCbCr_Conversion_Info)
        return Sampler_YCbCr_Conversion_Info_C;
    procedure Free(Struct: in out Sampler_YCbCr_Conversion_Info_C);

    function To_C(Struct: in Bind_Image_Plane_Memory_Info)
        return Bind_Image_Plane_Memory_Info_C;
    procedure Free(Struct: in out Bind_Image_Plane_Memory_Info_C);

    function To_C(Struct: in Image_Plane_Memory_Requirements_Info)
        return Image_Plane_Memory_Requirements_Info_C;
    procedure Free(Struct: in out Image_Plane_Memory_Requirements_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Sampler_YCbCr_Conversion_Features;
         C_Struct: in Physical_Device_Sampler_YCbCr_Conversion_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Sampler_YCbCr_Conversion_Image_Format_Properties;
         C_Struct: in Sampler_YCbCr_Conversion_Image_Format_Properties_C);

    function To_C(Struct: in Descriptor_Update_Template_Create_Info)
        return Descriptor_Update_Template_Create_Info_C;
    procedure Free
        (Struct: in out Descriptor_Update_Template_Create_Info_C);

    function To_C(Struct: in Physical_Device_External_Image_Format_Info)
        return Physical_Device_External_Image_Format_Info_C;
    procedure Free(Struct: in out Physical_Device_External_Image_Format_Info_C);

    procedure To_Ada(Ada_Struct: in out External_Image_Format_Properties;
                     C_Struct: in External_Image_Format_Properties_C);

    function To_C(Struct: in Physical_Device_External_Buffer_Info)
        return Physical_Device_External_Buffer_Info_C;
    procedure Free(Struct: in out Physical_Device_External_Buffer_Info_C);

    procedure To_Ada(Ada_Struct: in out External_Buffer_Properties;
                     C_Struct: in External_Buffer_Properties_C);

    procedure To_Ada(Ada_Struct: in out Physical_Device_ID_Properties;
                     C_Struct: in Physical_Device_ID_Properties_C);

    function To_C(Struct: in External_Memory_Image_Create_Info)
        return External_Memory_Image_Create_Info_C;
    procedure Free(Struct: in out External_Memory_Image_Create_Info_C);

    function To_C(Struct: in External_Memory_Buffer_Create_Info)
        return External_Memory_Buffer_Create_Info_C;
    procedure Free(Struct: in out External_Memory_Buffer_Create_Info_C);

    function To_C(Struct: in Export_Memory_Allocate_Info)
        return Export_Memory_Allocate_Info_C;
    procedure Free(Struct: in out Export_Memory_Allocate_Info_C);

    function To_C(Struct: in Physical_Device_External_Fence_Info)
        return Physical_Device_External_Fence_Info_C;
    procedure Free(Struct: in out Physical_Device_External_Fence_Info_C);

    procedure To_Ada(Ada_Struct: in out External_Fence_Properties;
                     C_Struct: in External_Fence_Properties_C);

    function To_C(Struct: in Export_Fence_Create_Info)
        return Export_Fence_Create_Info_C;
    procedure Free(Struct: in out Export_Fence_Create_Info_C);

    function To_C(Struct: in Export_Semaphore_Create_Info)
        return Export_Semaphore_Create_Info_C;
    procedure Free(Struct: in out Export_Semaphore_Create_Info_C);

    function To_C(Struct: in Physical_Device_External_Semaphore_Info)
        return Physical_Device_External_Semaphore_Info_C;
    procedure Free(Struct: in out Physical_Device_External_Semaphore_Info_C);

    procedure To_Ada(Ada_Struct: in out External_Semaphore_Properties;
                     C_Struct: in External_Semaphore_Properties_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_3_Properties;
         C_Struct: in Physical_Device_Maintenance_3_Properties_C);

    procedure To_Ada(Ada_Struct: in out Descriptor_Set_Layout_Support;
                     C_Struct: in Descriptor_Set_Layout_Support_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Draw_Parameter_Features;
         C_Struct: in Physical_Device_Shader_Draw_Parameter_Features_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_V1_1;


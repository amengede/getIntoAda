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

-- C interface for NV records

with Vulkan.C;
with Vulkan.C_Arrays;
with Vulkan.Extensions.NV;

private package Vulkan.C_NV is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Queue_Family_Checkpoint_Properties_2_Type |
            Checkpoint_Data_2_Type |
            Dedicated_Allocation_Image_Create_Info_Type |
            Dedicated_Allocation_Buffer_Create_Info_Type |
            Dedicated_Allocation_Memory_Allocate_Info_Type |
            Physical_Device_Corner_Sampled_Image_Features_Type |
            Pipeline_Viewport_W_Scaling_State_Create_Info_Type |
            Pipeline_Viewport_Swizzle_State_Create_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Queue_Family_Checkpoint_Properties_2_C is
    record
        Record_Type: Out_Structure_Type :=
            Queue_Family_Checkpoint_Properties_2_Type;
        Next: C.Out_Structure_C_Access;
        Checkpoint_Execution_Stage_Mask: Pipeline_Stage_Flags_2;
    end record
        with Convention => C;

    type Queue_Family_Checkpoint_Properties_2_C_Access is
        access Queue_Family_Checkpoint_Properties_2_C
    with Convention => C;

    type Checkpoint_Data_2_C is
    record
        Record_Type: Out_Structure_Type := Checkpoint_Data_2_Type;
        Next: C.Out_Structure_C_Access;
        Stage: Pipeline_Stage_Flags_2;
        Checkpoint_Marker: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Checkpoint_Data_2_C_Access is access Checkpoint_Data_2_C
        with Convention => C;

    type Dedicated_Allocation_Image_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Dedicated_Allocation_Image_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Dedicated_Allocation: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Dedicated_Allocation_Image_Create_Info_C_Access is
        access Dedicated_Allocation_Image_Create_Info_C
        with Convention => C;

    type Dedicated_Allocation_Buffer_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Dedicated_Allocation_Buffer_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Dedicated_Allocation: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Dedicated_Allocation_Buffer_Create_Info_C_Access is
        access Dedicated_Allocation_Buffer_Create_Info_C
        with Convention => C;

    type Dedicated_Allocation_Memory_Allocate_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Dedicated_Allocation_Memory_Allocate_Info_Type;
        Next: C.In_Structure_C_Access;
        Image: Vulkan.Image;
        Buffer: Vulkan.Buffer;
    end record
        with Convention => C;

    type Dedicated_Allocation_Memory_Allocate_Info_C_Access is
        access Dedicated_Allocation_Memory_Allocate_Info_C
        with Convention => C;

    type Physical_Device_Corner_Sampled_Image_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Corner_Sampled_Image_Features_Type;
        Next: C.Out_Structure_C_Access;
        Corner_Sampled_Image: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Corner_Sampled_Image_Features_C_Access is
        access Physical_Device_Corner_Sampled_Image_Features_C
        with Convention => C;

    package Viewport_W_Scaling_Arrays is
        new C_Arrays(Extensions.NV.Viewport_W_Scaling);

    type Pipeline_Viewport_W_Scaling_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Viewport_W_Scaling_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Viewport_W_Scaling_Enable: Interfaces.Unsigned_32;
        Viewport_Count: Interfaces.Unsigned_32;
        Viewport_W_Scalings: Viewport_W_Scaling_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Viewport_W_Scaling_State_Create_Info_C_Access is
        access Pipeline_Viewport_W_Scaling_State_Create_Info_C
        with Convention => C;

    package Viewport_Swizzle_Arrays is
        new C_Arrays(Extensions.NV.Viewport_Swizzle);

    type Pipeline_Viewport_Swizzle_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Viewport_Swizzle_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.NV.Pipeline_Viewport_Swizzle_State_Create_Flags;
        Viewport_Count: Interfaces.Unsigned_32;
        Viewport_Swizzles: Viewport_Swizzle_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Viewport_Swizzle_State_Create_Info_C_Access is
        access Pipeline_Viewport_Swizzle_State_Create_Info_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada
        (Ada_Struct: in out Extensions.NV.Queue_Family_Checkpoint_Properties_2;
         C_Struct: in Queue_Family_Checkpoint_Properties_2_C);

    procedure To_Ada(Ada_Struct: in out Extensions.NV.Checkpoint_Data_2;
                     C_Struct: in Checkpoint_Data_2_C);

    function To_C
        (Struct: in Extensions.NV.Dedicated_Allocation_Image_Create_Info)
        return Dedicated_Allocation_Image_Create_Info_C;
    procedure Free(Struct: in out Dedicated_Allocation_Image_Create_Info_C);

    function To_C
        (Struct: in Extensions.NV.Dedicated_Allocation_Buffer_Create_Info)
        return Dedicated_Allocation_Buffer_Create_Info_C;
    procedure Free(Struct: in out Dedicated_Allocation_Buffer_Create_Info_C);

    function To_C
        (Struct: in Extensions.NV.Dedicated_Allocation_Memory_Allocate_Info)
        return Dedicated_Allocation_Memory_Allocate_Info_C;
    procedure Free(Struct: in out Dedicated_Allocation_Memory_Allocate_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.NV.Physical_Device_Corner_Sampled_Image_Features;
         C_Struct: in Physical_Device_Corner_Sampled_Image_Features_C);

    function To_C
        (Struct: in Extensions.NV.Pipeline_Viewport_W_Scaling_State_Create_Info)
        return Pipeline_Viewport_W_Scaling_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Viewport_W_Scaling_State_Create_Info_C);

    function To_C
        (Struct: in Extensions.NV.Pipeline_Viewport_Swizzle_State_Create_Info)
        return Pipeline_Viewport_Swizzle_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Viewport_Swizzle_State_Create_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_NV;


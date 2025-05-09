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

-- Nvidia extensions root package

package Vulkan.Extensions.NV is
    -- Enumerations.
    type Viewport_Coordinate_Swizzle is (Positive_X,
                                         Negative_X,
                                         Positive_Y,
                                         Negative_Y,
                                         Positive_Z,
                                         Negative_Z,
                                         Positive_W,
                                         Negative_W)
        with Convention => C;

    for Viewport_Coordinate_Swizzle'Size use 32;

    for Viewport_Coordinate_Swizzle use (Positive_X => 0,
                                         Negative_X => 1,
                                         Positive_Y => 2,
                                         Negative_Y => 3,
                                         Positive_Z => 4,
                                         Negative_Z => 5,
                                         Positive_W => 6,
                                         Negative_W => 7);

    -- Bitfields.
    type Pipeline_Viewport_Swizzle_State_Create_Flags is new Flags;

    Pipeline_Viewport_Swizzle_State_Create_No_Bit:
        constant Pipeline_Viewport_Swizzle_State_Create_Flags := 0;

    -- Records.
    type Queue_Family_Checkpoint_Properties_2 is new Out_Structure
        (Queue_Family_Checkpoint_Properties_2_Type) with
    record
        Checkpoint_Execution_Stage_Mask: Pipeline_Stage_Flags_2 :=
            Pipeline_Stage_2_None;
    end record;

    type Checkpoint_Data_2 is new Out_Structure(Checkpoint_Data_2_Type) with
    record
        Stage: Pipeline_Stage_Flags_2 := Pipeline_Stage_2_None;
        Checkpoint_Marker: Interfaces.C.Extensions.void_ptr;
    end record;

    package Checkpoint_Data_2_Vectors is new Ada.Containers.Vectors
        (Positive, Checkpoint_Data_2);

    type Dedicated_Allocation_Image_Create_Info is new In_Structure
        (Dedicated_Allocation_Image_Create_Info_Type) with
    record
        Dedicated_Allocation: Boolean;
    end record;

    type Dedicated_Allocation_Buffer_Create_Info is new In_Structure
        (Dedicated_Allocation_Buffer_Create_Info_Type) with
    record
        Dedicated_Allocation: Boolean;
    end record;

    type Dedicated_Allocation_Memory_Allocate_Info is new In_Structure
        (Dedicated_Allocation_Memory_Allocate_Info_Type) with
    record
        Image: Vulkan.Image;
        Buffer: Vulkan.Buffer;
    end record;

    type Physical_Device_Corner_Sampled_Image_Features is new Out_Structure
        (Physical_Device_Corner_Sampled_Image_Features_Type) with
    record
        Corner_Sampled_Image: Boolean;
    end record;

    type Viewport_W_Scaling is
    record
        X_Coeff: Float;
        Y_Coeff: Float;
    end record
        with Convention => C;

    package Viewport_W_Scaling_Vectors is new Ada.Containers.Vectors
        (Positive, Viewport_W_Scaling);

    type Pipeline_Viewport_W_Scaling_State_Create_Info is new In_Structure
        (Pipeline_Viewport_W_Scaling_State_Create_Info_Type) with
    record
        Viewport_W_Scaling_Enable: Boolean;
        Viewport_W_Scalings: Viewport_W_Scaling_Vectors.Vector;
    end record;

    type Viewport_Swizzle is
    record
        X: Viewport_Coordinate_Swizzle;
        Y: Viewport_Coordinate_Swizzle;
        Z: Viewport_Coordinate_Swizzle;
        W: Viewport_Coordinate_Swizzle;
    end record
        with Convention => C;

    package Viewport_Swizzle_Vectors is new Ada.Containers.Vectors
        (Positive, Viewport_Swizzle);

    type Pipeline_Viewport_Swizzle_State_Create_Info is new In_Structure
        (Pipeline_Viewport_Swizzle_State_Create_Info_Type) with
    record
        Flags: NV.Pipeline_Viewport_Swizzle_State_Create_Flags :=
            NV.Pipeline_Viewport_Swizzle_State_Create_No_Bit;
        Viewport_Swizzles: Viewport_Swizzle_Vectors.Vector;
    end record;
end Vulkan.Extensions.NV;


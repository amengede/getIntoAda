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

-- AMD extensions root package

package Vulkan.Extensions.AMD is
    -- Enumerations.
    type Rasterization_Order is (Strict,
                                 Relaxed)
        with Convention => C;

    for Rasterization_Order'Size use 32;

    for Rasterization_Order use (Strict => 0,
                                 Relaxed => 1);

    type Shader_Info_Type is (Statistics,
                              Binary,
                              Disassembly)
        with Convention => C;

    for Shader_Info_Type'Size use 32;

    for Shader_Info_Type use (Statistics => 0,
                              Binary => 1,
                              Disassembly => 2);

    -- Records.
    package Sample_Count_Flags_Vectors is new Ada.Containers.Vectors
        (Positive, Sample_Count_Flags);

    type Attachment_Sample_Count_Info is new In_Structure
        (Attachment_Sample_Count_Info_Type) with
    record
        Color_Attachment_Samples: Sample_Count_Flags_Vectors.Vector;
        Depth_Stencil_Attachment_Samples: Sample_Count_Flags :=
            Sample_Count_No_Bit;
    end record;

    type Pipeline_Rasterization_State_Rasterization_Order is new In_Structure
        (Pipeline_Rasterization_State_Rasterization_Order_Type) with
    record
        Rasterization_Order: AMD.Rasterization_Order;
    end record;

    type Texture_LOD_Gather_Format_Properties is new Out_Structure
        (Texture_LOD_Gather_Format_Properties_Type) with
    record
        Supports_Texture_Gather_LOD_Bias: Boolean;
    end record;

    type Shader_Resource_Usage is
    record
        Num_Used_Vgprs: Interfaces.Unsigned_32;
        Num_Used_Sgprs: Interfaces.Unsigned_32;
        LDS_Size_Per_Local_Work_Group: Interfaces.Unsigned_32;
        LDS_Usage_Size_In_Bytes: Interfaces.C.size_t;
        Scratch_Mem_Usage_In_Bytes: Interfaces.C.size_t;
    end record
        with Convention => C;

    type Compute_Work_Group_Size_Array is
        array (1 .. 3) of Interfaces.Unsigned_32
        with Convention => C;

    type Shader_Statistics_Info is
    record
        Shader_Stage_Mask: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Resource_Usage: Shader_Resource_Usage;
        Num_Physical_Vgprs: Interfaces.Unsigned_32;
        Num_Physical_Sgprs: Interfaces.Unsigned_32;
        Num_Available_Vgprs: Interfaces.Unsigned_32;
        Num_Available_Sgprs: Interfaces.Unsigned_32;
        Compute_Work_Group_Size: Compute_Work_Group_Size_Array;
    end record
        with Convention => C;
end Vulkan.Extensions.AMD;


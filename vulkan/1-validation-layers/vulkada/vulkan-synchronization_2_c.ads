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

-- C interfaces for the synchronization 2 extension

with Vulkan.C;

private package Vulkan.Synchronization_2_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Queue_Family_Checkpoint_Properties_2_Type |
            Checkpoint_Data_2_Type;

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

    -- Conversion subprograms.
    procedure To_Ada(Ada_Struct: in out Queue_Family_Checkpoint_Properties_2;
                     C_Struct: in Queue_Family_Checkpoint_Properties_2_C);

    procedure To_Ada(Ada_Struct: in out Checkpoint_Data_2;
                     C_Struct: in Checkpoint_Data_2_C);

    -- Extension record conversions.
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Synchronization_2_C;


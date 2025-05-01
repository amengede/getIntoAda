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

-- C interface for the validation flags extension

with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.Validation_Flags_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type with
        Static_Predicate => Structure in
            Validation_Flags_Type;

    -- C interface records.
    package Validation_Check_Arrays is new C_Arrays(Validation_Check);

    type Validation_Flags_C is
    record
        Record_Type: In_Structure_Type := Validation_Flags_Type;
        Next: C.In_Structure_C_Access;
        Disabled_Validation_Check_Count: Interfaces.Unsigned_32;
        Disabled_Validation_Checks: Validation_Check_Arrays.Pointer;
    end record
        with Convention => C;

    type Validation_Flags_C_Access is access Validation_Flags_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Validation_Flags) return Validation_Flags_C;
    procedure Free(Struct: in out Validation_Flags_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Validation_Flags_C;


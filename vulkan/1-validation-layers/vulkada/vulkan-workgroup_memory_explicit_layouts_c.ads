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

-- C interface for the workgroup memory explicit layout extension

with Vulkan.C;

private package Vulkan.Workgroup_Memory_Explicit_Layouts_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type;

    -- C interface records.
    type Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type;
        Next: C.Out_Structure_C_Access;
        Workgroup_Memory_Explicit_Layout: Interfaces.Unsigned_32;
        Workgroup_Memory_Explicit_Layout_Scalar_Block_Layout:
            Interfaces.Unsigned_32;
        Workgroup_Memory_Explicit_Layout_8_Bit_Access: Interfaces.Unsigned_32;
        Workgroup_Memory_Explicit_Layout_16_Bit_Access: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C_Access is
        access Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada
        (Ada_Struct: in out
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features;
         C_Struct: in
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C);

    -- Extension record conversions.
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Workgroup_Memory_Explicit_Layouts_C;


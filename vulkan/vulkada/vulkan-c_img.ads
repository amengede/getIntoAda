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

-- C interface for IMG records

with Vulkan.C;
with Vulkan.Extensions.IMG;

private package Vulkan.C_IMG is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Relaxed_Line_Rasterization_Features_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Physical_Device_Relaxed_Line_Rasterization_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Relaxed_Line_Rasterization_Features_Type;
        Next: C.Out_Structure_C_Access;
        Relaxed_Line_Rasterization: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Relaxed_Line_Rasterization_Features_C_Access is
        access Physical_Device_Relaxed_Line_Rasterization_Features_C
        with Convention => C;
    
    -- Conversion subprograms.
    procedure To_Ada
        (Ada_Struct:
            in out
             Extensions.IMG.Physical_Device_Relaxed_Line_Rasterization_Features;
         C_Struct: in Physical_Device_Relaxed_Line_Rasterization_Features_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_IMG;


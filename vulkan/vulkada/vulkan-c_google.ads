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

-- C interface for GOOGLE records

with Vulkan.C;
with Vulkan.C_Arrays;
with Vulkan.Extensions.GOOGLE;

private package Vulkan.C_GOOGLE is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Present_Times_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    package Present_Time_Arrays is new C_Arrays(Extensions.GOOGLE.Present_Time);

    type Present_Times_Info_C is
    record
        Record_Type: In_Structure_Type := Present_Times_Info_Type;
        Next: C.In_Structure_C_Access;
        Swapchain_Count: Interfaces.Unsigned_32;
        Times: Present_Time_Arrays.Pointer;
    end record
        with Convention => C;

    type Present_Times_Info_C_Access is access Present_Times_Info_C
        with Convention => C;
    
    -- Conversion subprograms.
    function To_C(Struct: in Extensions.GOOGLE.Present_Times_Info)
        return Present_Times_Info_C;
    procedure Free(Struct: in out Present_Times_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_GOOGLE;


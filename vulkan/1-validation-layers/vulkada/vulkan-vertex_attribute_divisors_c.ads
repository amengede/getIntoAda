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

-- C interface for the vertex attribute divisor extension

with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.Vertex_Attribute_Divisors_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Vertex_Attribute_Divisor_Properties_Type |
            Pipeline_Vertex_Input_Divisor_State_Create_Info_Type |
            Physical_Device_Vertex_Attribute_Divisor_Features_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Physical_Device_Vertex_Attribute_Divisor_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vertex_Attribute_Divisor_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Vertex_Attrib_Divisor: Interfaces.Unsigned_32;
        Supports_Non_Zero_First_Instance: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Properties_C_Access is
        access Physical_Device_Vertex_Attribute_Divisor_Properties_C
        with Convention => C;

    package Vertex_Input_Binding_Divisor_Description_Arrays is new C_Arrays
        (Vertex_Input_Binding_Divisor_Description);

    type Pipeline_Vertex_Input_Divisor_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Vertex_Input_Divisor_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Vertex_Binding_Divisor_Count: Interfaces.Unsigned_32;
        Vertex_Binding_Divisors:
            Vertex_Input_Binding_Divisor_Description_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Vertex_Input_Divisor_State_Create_Info_C_Access is
        access Pipeline_Vertex_Input_Divisor_State_Create_Info_C
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Vertex_Attribute_Divisor_Features_Type;
        Next: C.Out_Structure_C_Access;
        Vertex_Attribute_Instance_Rate_Divisor: Interfaces.Unsigned_32;
        Vertex_Attribute_Instance_Rate_Zero_Divisor: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Vertex_Attribute_Divisor_Features_C_Access is
        access Physical_Device_Vertex_Attribute_Divisor_Features_C
        with Convention => C;

    -- Conversion subprograms.
    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vertex_Attribute_Divisor_Properties;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Properties_C);

    function To_C(Struct: in Pipeline_Vertex_Input_Divisor_State_Create_Info)
        return Pipeline_Vertex_Input_Divisor_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Vertex_Input_Divisor_State_Create_Info_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vertex_Attribute_Divisor_Features;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Features_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Vertex_Attribute_Divisors_C;


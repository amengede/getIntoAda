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

with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Vertex_Attribute_Divisors_C is
    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vertex_Attribute_Divisor_Properties;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Vertex_Attrib_Divisor :=
            C_Struct.Max_Vertex_Attrib_Divisor;
        Ada_Struct.Supports_Non_Zero_First_Instance :=
            Utilities.To_Ada(C_Struct.Supports_Non_Zero_First_Instance);
    end To_Ada;

    function To_C(Struct: in Pipeline_Vertex_Input_Divisor_State_Create_Info)
        return Pipeline_Vertex_Input_Divisor_State_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Vertex_Input_Binding_Divisor_Description_Arrays,
             Vertex_Input_Binding_Divisor_Description_Vectors);

        PVIDSCIC: Pipeline_Vertex_Input_Divisor_State_Create_Info_C;
    begin
        PVIDSCIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(PVIDSCIC.Vertex_Binding_Divisor_Count,
                   Struct.Vertex_Binding_Divisors,
                   PVIDSCIC.Vertex_Binding_Divisors);

        return PVIDSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Vertex_Input_Divisor_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Vertex_Input_Binding_Divisor_Description_Arrays.Free
            (Struct.Vertex_Binding_Divisors);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Vertex_Attribute_Divisor_Features;
         C_Struct: in Physical_Device_Vertex_Attribute_Divisor_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Vertex_Attribute_Instance_Rate_Divisor :=
            Utilities.To_Ada(C_Struct.Vertex_Attribute_Instance_Rate_Divisor);
        Ada_Struct.Vertex_Attribute_Instance_Rate_Zero_Divisor :=
            Utilities.To_Ada
                (C_Struct.Vertex_Attribute_Instance_Rate_Zero_Divisor);
    end To_Ada;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Pipeline_Vertex_Input_Divisor_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                     (Pipeline_Vertex_Input_Divisor_State_Create_Info,
                      Pipeline_Vertex_Input_Divisor_State_Create_Info_C,
                      Pipeline_Vertex_Input_Divisor_State_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Physical_Device_Vertex_Attribute_Divisor_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                 (Physical_Device_Vertex_Attribute_Divisor_Properties,
                  Physical_Device_Vertex_Attribute_Divisor_Properties_C,
                  Physical_Device_Vertex_Attribute_Divisor_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Vertex_Attribute_Divisor_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Physical_Device_Vertex_Attribute_Divisor_Features,
                    Physical_Device_Vertex_Attribute_Divisor_Features_C,
                    Physical_Device_Vertex_Attribute_Divisor_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Vertex_Attribute_Divisor_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                 (C.Out_Structure_C_Access,
                  Physical_Device_Vertex_Attribute_Divisor_Properties_C_Access);
                begin
                    To_Ada
               (Physical_Device_Vertex_Attribute_Divisor_Properties(Ada_Struct),
                To_Access(Next).all);
                end;
            when Physical_Device_Vertex_Attribute_Divisor_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                   (C.Out_Structure_C_Access,
                    Physical_Device_Vertex_Attribute_Divisor_Features_C_Access);
                begin
                    To_Ada
                 (Physical_Device_Vertex_Attribute_Divisor_Features(Ada_Struct),
                  To_Access(Next).all);
                end;
        end case;
    end To_Ada;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case In_Structure(Next.Record_Type) is
            when Pipeline_Vertex_Input_Divisor_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Pipeline_Vertex_Input_Divisor_State_Create_Info_C,
                      Pipeline_Vertex_Input_Divisor_State_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Physical_Device_Vertex_Attribute_Divisor_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                 (Physical_Device_Vertex_Attribute_Divisor_Properties_C,
                  Physical_Device_Vertex_Attribute_Divisor_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Vertex_Attribute_Divisor_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                   (Physical_Device_Vertex_Attribute_Divisor_Features_C,
                    Physical_Device_Vertex_Attribute_Divisor_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Vertex_Attribute_Divisors_C;


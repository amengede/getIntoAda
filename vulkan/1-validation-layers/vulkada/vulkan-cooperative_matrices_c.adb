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

-- C interface for the cooperative matrix extension

with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Cooperative_Matrices_C is
    procedure To_Ada(Ada_Struct: in out Cooperative_Matrix_Properties;
                     C_Struct: in Cooperative_Matrix_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.M_Size := C_Struct.M_Size;
        Ada_Struct.N_Size := C_Struct.N_Size;
        Ada_Struct.K_Size := C_Struct.K_Size;
        Ada_Struct.A_Type := C_Struct.A_Type;
        Ada_Struct.B_Type := C_Struct.B_Type;
        Ada_Struct.C_Type := C_Struct.C_Type;
        Ada_Struct.Result_Type := C_Struct.Result_Type;
        Ada_Struct.Saturating_Accumulation :=
            Utilities.To_Ada(C_Struct.Saturating_Accumulation);
        Ada_Struct.Scope := C_Struct.Scope;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Cooperative_Matrix_Features;
         C_Struct: in Physical_Device_Cooperative_Matrix_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Cooperative_Matrix :=
            Utilities.To_Ada(C_Struct.Cooperative_Matrix);
        Ada_Struct.Cooperative_Matrix_Robust_Buffer_Access :=
            Utilities.To_Ada(C_Struct.Cooperative_Matrix_Robust_Buffer_Access);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Cooperative_Matrix_Properties;
         C_Struct: in Physical_Device_Cooperative_Matrix_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Cooperative_Matrix_Supported_Stages :=
            C_Struct.Cooperative_Matrix_Supported_Stages;
    end To_Ada;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Cooperative_Matrix_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Cooperative_Matrix_Properties,
                        Cooperative_Matrix_Properties_C,
                        Cooperative_Matrix_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Cooperative_Matrix_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Physical_Device_Cooperative_Matrix_Features,
                        Physical_Device_Cooperative_Matrix_Features_C,
                        Physical_Device_Cooperative_Matrix_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Cooperative_Matrix_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Physical_Device_Cooperative_Matrix_Properties,
                        Physical_Device_Cooperative_Matrix_Properties_C,
                        Physical_Device_Cooperative_Matrix_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Structure(Ada_Struct.Record_Type) is
            when Cooperative_Matrix_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Cooperative_Matrix_Properties_C_Access);
                begin
                    To_Ada(Cooperative_Matrix_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Cooperative_Matrix_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Cooperative_Matrix_Features_C_Access);
                begin
                    To_Ada
                       (Physical_Device_Cooperative_Matrix_Features(Ada_Struct),
                        To_Access(Next).all);
                end;
            when Physical_Device_Cooperative_Matrix_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Cooperative_Matrix_Properties_C_Access);
                begin
                    To_Ada
                     (Physical_Device_Cooperative_Matrix_Properties(Ada_Struct),
                      To_Access(Next).all);
                end;
        end case;
    end To_Ada;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Structure(Next.Record_Type) is
            when Cooperative_Matrix_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Cooperative_Matrix_Properties_C,
                         Cooperative_Matrix_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Cooperative_Matrix_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Cooperative_Matrix_Features_C,
                         Physical_Device_Cooperative_Matrix_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Cooperative_Matrix_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Physical_Device_Cooperative_Matrix_Properties_C,
                        Physical_Device_Cooperative_Matrix_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Cooperative_Matrices_C;


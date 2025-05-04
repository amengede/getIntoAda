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

-- C interface for the pipeline executable properties extension

pragma No_Strict_Aliasing;

with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Pipeline_Executable_Properties_C is
    procedure To_Ada
        (Ada_Struct: in out
            Physical_Device_Pipeline_Executable_Properties_Features;
         C_Struct: in
            Physical_Device_Pipeline_Executable_Properties_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Pipeline_Executable_Info :=
            Utilities.To_Ada(C_Struct.Pipeline_Executable_Info);
    end To_Ada;

    function To_C(Struct: in Pipeline_Info) return Pipeline_Info_C is
        PIC: Pipeline_Info_C;
    begin
        PIC.Next := Extension_Records.To_C(Struct.Next);
        PIC.Pipeline := Struct.Pipeline;

        return PIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Pipeline_Executable_Properties;
                     C_Struct: in Pipeline_Executable_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Stages := C_Struct.Stages;
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Name, Interfaces.C.To_Ada(C_Struct.Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Description, Interfaces.C.To_Ada(C_Struct.Description));
        Ada_Struct.Subgroup_Size := C_Struct.Subgroup_Size;
    end To_Ada;

    function To_C(Struct: in Pipeline_Executable_Info)
        return Pipeline_Executable_Info_C is
        PEIC: Pipeline_Executable_Info_C;
    begin
        PEIC.Next := Extension_Records.To_C(Struct.Next);
        PEIC.Pipeline := Struct.Pipeline;
        PEIC.Executable_Index := Struct.Executable_Index;

        return PEIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Executable_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Pipeline_Executable_Statistic;
                     C_Struct: in Pipeline_Executable_Statistic_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Name, Interfaces.C.To_Ada(C_Struct.Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Description, Interfaces.C.To_Ada(C_Struct.Description));
        
        case Ada_Struct.Format is
            when Bool32 =>
                Ada_Struct.Value.B32 := Utilities.To_Ada(C_Struct.B32);
            when Int64 =>
                Ada_Struct.Value.I64 := C_Struct.I64;
            when Uint64 =>
                Ada_Struct.Value.U64 := C_Struct.U64;
            when Float64 =>
                Ada_Struct.Value.F64 := C_Struct.F64;
        end case;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Pipeline_Executable_Internal_Representation;
         C_Struct: in Pipeline_Executable_Internal_Representation_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Name, Interfaces.C.To_Ada(C_Struct.Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Description, Interfaces.C.To_Ada(C_Struct.Description));
        Ada_Struct.Is_Text := Utilities.To_Ada(C_Struct.Is_Text);
        Ada_Struct.Data_Size := C_Struct.Data_Size;
        Ada_Struct.Data := C_Struct.Data;
    end To_Ada;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Pipeline_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Info,
                         Pipeline_Info_C,
                         Pipeline_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Executable_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Executable_Info,
                         Pipeline_Executable_Info_C,
                         Pipeline_Executable_Info_C_Access);
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
            when Physical_Device_Pipeline_Executable_Properties_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
             (Physical_Device_Pipeline_Executable_Properties_Features,
              Physical_Device_Pipeline_Executable_Properties_Features_C,
              Physical_Device_Pipeline_Executable_Properties_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Executable_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Pipeline_Executable_Properties,
                         Pipeline_Executable_Properties_C,
                         Pipeline_Executable_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Executable_Statistic_Type =>
                declare
                    function To_Out_Structure_C_Access is
                        new Ada.Unchecked_Conversion
                            (Pipeline_Executable_Statistic_C_Access,
                             C.Out_Structure_C_Access);

                    Ada_Struct: Pipeline_Executable_Statistic renames
                        Pipeline_Executable_Statistic(Next.all);
                    C_Struct: Pipeline_Executable_Statistic_C_Access;
                    C_Out: C.Out_Structure_C_Access;
                begin
                    C_Struct := new Pipeline_Executable_Statistic_C
                                    (Ada_Struct.Format);
                    C_Out := To_Out_Structure_C_Access(C_Struct);
                    C_Out.Next := Extension_Records.To_C(Next.Next);

                    return C_Out;
                end;
            when Pipeline_Executable_Internal_Representation_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Pipeline_Executable_Internal_Representation,
                         Pipeline_Executable_Internal_Representation_C,
                         Pipeline_Executable_Internal_Representation_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Pipeline_Executable_Properties_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
             (C.Out_Structure_C_Access,
              Physical_Device_Pipeline_Executable_Properties_Features_C_Access);
                begin
                    To_Ada
           (Physical_Device_Pipeline_Executable_Properties_Features(Ada_Struct),
            To_Access(Next).all);
                end;
            when Pipeline_Executable_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Pipeline_Executable_Properties_C_Access);
                begin
                    To_Ada(Pipeline_Executable_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Pipeline_Executable_Statistic_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Pipeline_Executable_Statistic_C_Access);
                begin
                    To_Ada(Pipeline_Executable_Statistic(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Pipeline_Executable_Internal_Representation_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Pipeline_Executable_Internal_Representation_C_Access);
                begin
                    To_Ada
                       (Pipeline_Executable_Internal_Representation(Ada_Struct),
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
            when Pipeline_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Info_C, Pipeline_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Executable_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Executable_Info_C,
                         Pipeline_Executable_Info_C_Access);
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
            when Physical_Device_Pipeline_Executable_Properties_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
             (Physical_Device_Pipeline_Executable_Properties_Features_C,
              Physical_Device_Pipeline_Executable_Properties_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Executable_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Pipeline_Executable_Properties_C,
                         Pipeline_Executable_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Executable_Statistic_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Pipeline_Executable_Statistic_C,
                         Pipeline_Executable_Statistic_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Executable_Internal_Representation_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Pipeline_Executable_Internal_Representation_C,
                         Pipeline_Executable_Internal_Representation_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Pipeline_Executable_Properties_C;


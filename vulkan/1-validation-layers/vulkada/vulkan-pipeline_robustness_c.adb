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

-- C interface for the pipeline robustness extension

with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Pipeline_Robustness_C is
    function To_C(Struct: in Pipeline_Robustness_Create_Info)
        return Pipeline_Robustness_Create_Info_C is
        PRCIC: Pipeline_Robustness_Create_Info_C;
    begin
        PRCIC.Next := Extension_Records.To_C(Struct.Next);
        PRCIC.Storage_Buffers := Struct.Storage_Buffers;
        PRCIC.Uniform_Buffers := Struct.Uniform_Buffers;
        PRCIC.Vertex_Inputs := Struct.Vertex_Inputs;
        PRCIC.Images := Struct.Images;

        return PRCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Robustness_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Robustness_Features;
         C_Struct: in Physical_Device_Pipeline_Robustness_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Pipeline_Robustness :=
            Utilities.To_Ada(C_Struct.Pipeline_Robustness);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Pipeline_Robustness_Properties;
         C_Struct: in Physical_Device_Pipeline_Robustness_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Default_Robustness_Storage_Buffers :=
            C_Struct.Default_Robustness_Storage_Buffers;
        Ada_Struct.Default_Robustness_Uniform_Buffers :=
            C_Struct.Default_Robustness_Uniform_Buffers;
        Ada_Struct.Default_Robustness_Vertex_Inputs :=
            C_Struct.Default_Robustness_Vertex_Inputs;
        Ada_Struct.Default_Robustness_Images :=
            C_Struct.Default_Robustness_Images;
    end To_Ada;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Pipeline_Robustness_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Robustness_Create_Info,
                         Pipeline_Robustness_Create_Info_C,
                         Pipeline_Robustness_Create_Info_C_Access);
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
            when Physical_Device_Pipeline_Robustness_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Pipeline_Robustness_Features,
                         Physical_Device_Pipeline_Robustness_Features_C,
                         Physical_Device_Pipeline_Robustness_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                      (Physical_Device_Pipeline_Robustness_Properties,
                       Physical_Device_Pipeline_Robustness_Properties_C,
                       Physical_Device_Pipeline_Robustness_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Pipeline_Robustness_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Pipeline_Robustness_Features_C_Access);
                begin
                    To_Ada
                      (Physical_Device_Pipeline_Robustness_Features(Ada_Struct),
                       To_Access(Next).all);
                end;
            when Physical_Device_Pipeline_Robustness_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Pipeline_Robustness_Properties_C_Access);
                begin
                    To_Ada
                    (Physical_Device_Pipeline_Robustness_Properties(Ada_Struct),
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
            when Pipeline_Robustness_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Robustness_Create_Info_C,
                         Pipeline_Robustness_Create_Info_C_Access);
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
            when Physical_Device_Pipeline_Robustness_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Pipeline_Robustness_Features_C,
                         Physical_Device_Pipeline_Robustness_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Pipeline_Robustness_Properties_C,
                       Physical_Device_Pipeline_Robustness_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Pipeline_Robustness_C;


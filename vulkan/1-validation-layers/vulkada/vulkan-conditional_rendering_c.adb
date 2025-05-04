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

-- C interface for the conditional rendering extension

with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Conditional_Rendering_C is
    function To_C(Struct: in Conditional_Rendering_Begin_Info)
        return Conditional_Rendering_Begin_Info_C is
        CRBIC: Conditional_Rendering_Begin_Info_C;
    begin
        CRBIC.Next := Extension_Records.To_C(Struct.Next);
        CRBIC.Buffer := Struct.Buffer;
        CRBIC.Offset := Struct.Offset;
        CRBIC.Flags := Struct.Flags;

        return CRBIC;
    end To_C;

    procedure Free(Struct: in out Conditional_Rendering_Begin_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Conditional_Rendering_Features;
         C_Struct: in Physical_Device_Conditional_Rendering_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Conditional_Rendering :=
            Utilities.To_Ada(C_Struct.Conditional_Rendering);
        Ada_Struct.Inherited_Conditional_Rendering :=
            Utilities.To_Ada(C_Struct.Inherited_Conditional_Rendering);
    end To_Ada;

    function To_C
        (Struct: in Command_Buffer_Inheritance_Conditional_Rendering_Info)
        return Command_Buffer_Inheritance_Conditional_Rendering_Info_C is
        CBICRIC: Command_Buffer_Inheritance_Conditional_Rendering_Info_C;
    begin
        CBICRIC.Next := Extension_Records.To_C(Struct.Next);
        CBICRIC.Conditional_Rendering_Enable :=
            Utilities.To_C(Struct.Conditional_Rendering_Enable);

        return CBICRIC;
    end To_C;

    procedure Free
        (Struct:
            in out Command_Buffer_Inheritance_Conditional_Rendering_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Conditional_Rendering_Begin_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Conditional_Rendering_Begin_Info,
                         Conditional_Rendering_Begin_Info_C,
                         Conditional_Rendering_Begin_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Command_Buffer_Inheritance_Conditional_Rendering_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
               (Command_Buffer_Inheritance_Conditional_Rendering_Info,
                Command_Buffer_Inheritance_Conditional_Rendering_Info_C,
                Command_Buffer_Inheritance_Conditional_Rendering_Info_C_Access);
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
            when Physical_Device_Conditional_Rendering_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                      (Physical_Device_Conditional_Rendering_Features,
                       Physical_Device_Conditional_Rendering_Features_C,
                       Physical_Device_Conditional_Rendering_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Conditional_Rendering_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Conditional_Rendering_Features_C_Access);
                begin
                    To_Ada
                    (Physical_Device_Conditional_Rendering_Features(Ada_Struct),
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
            when Conditional_Rendering_Begin_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Conditional_Rendering_Begin_Info_C,
                         Conditional_Rendering_Begin_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Command_Buffer_Inheritance_Conditional_Rendering_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
               (Command_Buffer_Inheritance_Conditional_Rendering_Info_C,
                Command_Buffer_Inheritance_Conditional_Rendering_Info_C_Access);
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
            when Physical_Device_Conditional_Rendering_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Conditional_Rendering_Features_C,
                       Physical_Device_Conditional_Rendering_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Conditional_Rendering_C;


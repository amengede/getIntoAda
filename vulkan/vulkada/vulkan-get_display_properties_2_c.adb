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

-- C interface for the get display properties 2 extension

with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Get_Display_Properties_2_C is
    procedure To_Ada(Ada_Struct: in out Display_Properties_2;
                     C_Struct: in Display_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Display_Properties :=
            Displays_C.To_Ada(C_Struct.Display_Properties);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Display_Plane_Properties_2;
                     C_Struct: in Display_Plane_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Display_Plane_Properties :=
            C_Struct.Display_Plane_Properties;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Display_Mode_Properties_2;
                     C_Struct: in Display_Mode_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Display_Mode_Properties := C_Struct.Display_Mode_Properties;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Display_Plane_Capabilities_2;
                     C_Struct: in Display_Plane_Capabilities_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Capabilities := C_Struct.Capabilities;
    end To_Ada;

    function To_C(Struct: in Display_Plane_Info_2)
        return Display_Plane_Info_2_C is
        DPI2C: Display_Plane_Info_2_C;
    begin
        DPI2C.Next := Extension_Records.To_C(Struct.Next);
        DPI2C.Mode := Struct.Mode;
        DPI2C.Plane_Index := Struct.Plane_Index;

        return DPI2C;
    end To_C;

    procedure Free(Struct: in out Display_Plane_Info_2_C) is
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
            when Display_Plane_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Display_Plane_Info_2,
                         Display_Plane_Info_2_C,
                         Display_Plane_Info_2_C_Access);
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
            when Display_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Display_Properties_2,
                         Display_Properties_2_C,
                         Display_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Plane_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Display_Plane_Properties_2,
                         Display_Plane_Properties_2_C,
                         Display_Plane_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Mode_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Display_Mode_Properties_2,
                         Display_Mode_Properties_2_C,
                         Display_Mode_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Plane_Capabilities_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Display_Plane_Capabilities_2,
                         Display_Plane_Capabilities_2_C,
                         Display_Plane_Capabilities_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Display_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Display_Properties_2_C_Access);
                begin
                    To_Ada(Display_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Display_Plane_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Display_Plane_Properties_2_C_Access);
                begin
                    To_Ada(Display_Plane_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Display_Mode_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Display_Mode_Properties_2_C_Access);
                begin
                    To_Ada(Display_Mode_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Display_Plane_Capabilities_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Display_Plane_Capabilities_2_C_Access);
                begin
                    To_Ada(Display_Plane_Capabilities_2(Ada_Struct),
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
            when Display_Plane_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Plane_Info_2_C, Display_Plane_Info_2_C_Access);
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
            when Display_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Display_Properties_2_C,
                         Display_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Plane_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Display_Plane_Properties_2_C,
                         Display_Plane_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Mode_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Display_Mode_Properties_2_C,
                         Display_Mode_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Plane_Capabilities_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Display_Plane_Capabilities_2_C,
                         Display_Plane_Capabilities_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Get_Display_Properties_2_C;


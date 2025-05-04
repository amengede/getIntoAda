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

-- C interface for the get surface capabilities 2 extension

with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Get_Surface_Capabilities_2_C is
    function To_C(Struct: in Physical_Device_Surface_Info_2)
        return Physical_Device_Surface_Info_2_C is
        PDSI2C: Physical_Device_Surface_Info_2_C;
    begin
        PDSI2C.Next := Extension_Records.To_C(Struct.Next);
        PDSI2C.Surface := Struct.Surface;

        return PDSI2C;
    end To_C;

    procedure Free(Struct: in out Physical_Device_Surface_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Surface_Capabilities_2;
                     C_Struct: in Surface_Capabilities_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Surface_Capabilities := C_Struct.Surface_Capabilities;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Surface_Format_2;
                     C_Struct: in Surface_Format_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Surface_Format := C_Struct.Surface_Format;
    end To_Ada;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Physical_Device_Surface_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Physical_Device_Surface_Info_2,
                         Physical_Device_Surface_Info_2_C,
                         Physical_Device_Surface_Info_2_C_Access);
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
            when Surface_Capabilities_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Surface_Capabilities_2,
                         Surface_Capabilities_2_C,
                         Surface_Capabilities_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Surface_Format_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Surface_Format_2,
                         Surface_Format_2_C,
                         Surface_Format_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Surface_Capabilities_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Surface_Capabilities_2_C_Access);
                begin
                    To_Ada(Surface_Capabilities_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Surface_Format_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Surface_Format_2_C_Access);
                begin
                    To_Ada(Surface_Format_2(Ada_Struct), To_Access(Next).all);
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
            when Physical_Device_Surface_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_Surface_Info_2_C,
                         Physical_Device_Surface_Info_2_C_Access);
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
            when Surface_Capabilities_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Surface_Capabilities_2_C,
                         Surface_Capabilities_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Surface_Format_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Surface_Format_2_C, Surface_Format_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Get_Surface_Capabilities_2_C;


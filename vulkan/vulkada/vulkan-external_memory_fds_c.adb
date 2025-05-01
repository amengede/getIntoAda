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

-- C interface for the external memory FD extension

with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.External_Memory_FDs_C is
    function To_C(Struct: in Import_Memory_FD_Info)
        return Import_Memory_FD_Info_C is
        IMDIC: Import_Memory_FD_Info_C;
    begin
        IMDIC.Next := Extension_Records.To_C(Struct.Next);
        IMDIC.Handle_Type := Struct.Handle_Type;
        IMDIC.FD := Struct.FD;

        return IMDIC;
    end To_C;

    procedure Free(Struct: in out Import_Memory_FD_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Memory_FD_Properties;
                     C_Struct: in Memory_FD_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Memory_Type_Bits := C_Struct.Memory_Type_Bits;
    end To_Ada;

    function To_C(Struct: in Memory_Get_FD_Info) return Memory_Get_FD_Info_C is
        MGFIC: Memory_Get_FD_Info_C;
    begin
        MGFIC.Next := Extension_Records.To_C(Struct.Next);
        MGFIC.Memory := Struct.Memory;
        MGFIC.Handle_Type := Struct.Handle_Type;
        
        return MGFIC;
    end To_C;

    procedure Free(Struct: in out Memory_Get_FD_Info_C) is
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
            when Import_Memory_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Import_Memory_FD_Info,
                         Import_Memory_FD_Info_C,
                         Import_Memory_FD_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Get_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_Get_FD_Info,
                         Memory_Get_FD_Info_C,
                         Memory_Get_FD_Info_C_Access);
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
            when Memory_FD_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Memory_FD_Properties,
                         Memory_FD_Properties_C,
                         Memory_FD_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Memory_FD_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Memory_FD_Properties_C_Access);
                begin
                    To_Ada(Memory_FD_Properties(Ada_Struct),
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
            when Import_Memory_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Import_Memory_FD_Info_C,
                         Import_Memory_FD_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Get_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Memory_Get_FD_Info_C,
                         Memory_Get_FD_Info_C_Access);
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
            when Memory_FD_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Memory_FD_Properties_C,
                         Memory_FD_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.External_Memory_FDs_C;


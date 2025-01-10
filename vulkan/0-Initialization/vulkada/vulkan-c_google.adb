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

-- C interface for GOOGLE records

with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.C_GOOGLE is
    function To_C(Struct: in Extensions.GOOGLE.Present_Times_Info)
        return Present_Times_Info_C is
        procedure To_C_Array is
            new Utilities.To_C_Array(Present_Time_Arrays,
                                     Extensions.GOOGLE.Present_Time_Vectors);

        PTIC: Present_Times_Info_C;
        Count: Interfaces.Unsigned_32;
    begin
        PTIC.Next := Extension_Records.To_C(Struct.Next);
        PTIC.Swapchain_Count := Struct.Swapchain_Count;
        To_C_Array(Count, Struct.Times, PTIC.Times);

        return PTIC;
    end To_C;

    procedure Free(Struct: in out Present_Times_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Present_Time_Arrays.Free(Struct.Times);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Present_Times_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.GOOGLE.Present_Times_Info,
                         Present_Times_Info_C,
                         Present_Times_Info_C_Access);
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
            when others => return null;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when others => null;
        end case;
    end To_Ada;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case In_Structure(Next.Record_Type) is
            when Present_Times_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Present_Times_Info_C,
                         Present_Times_Info_C_Access);
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
            when others => null;
        end case;
    end Free;
end Vulkan.C_GOOGLE;


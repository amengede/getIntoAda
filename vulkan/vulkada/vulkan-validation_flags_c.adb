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

-- C interface for the validation flags extension

with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Validation_Flags_C is
    function To_C(Struct: in Validation_Flags) return Validation_Flags_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Validation_Check_Arrays, Validation_Check_Vectors);

        VFC: Validation_Flags_C;
    begin
        VFC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VFC.Disabled_Validation_Check_Count,
                   Struct.Disabled_Validation_Checks,
                   VFC.Disabled_Validation_Checks);

        return VFC;
    end To_C;

    procedure Free(Struct: in out Validation_Flags_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Validation_Check_Arrays.Free(Struct.Disabled_Validation_Checks);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Validation_Flags_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Validation_Flags,
                         Validation_Flags_C,
                         Validation_Flags_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Structure(Next.Record_Type) is
            when Validation_Flags_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Validation_Flags_C, Validation_Flags_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Validation_Flags_C;


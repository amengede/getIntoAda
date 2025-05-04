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

-- C interface for the external fence FD extension

with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.External_Fence_FDs_C is
    function To_C(Struct: in Import_Fence_FD_Info)
        return Import_Fence_FD_Info_C is
        IFFIC: Import_Fence_FD_Info_C;
    begin
        IFFIC.Next := Extension_Records.To_C(Struct.Next);
        IFFIC.Fence := Struct.Fence;
        IFFIC.Flags := Struct.Flags;
        IFFIC.Handle_Type := Struct.Handle_Type;
        IFFIC.FD := Struct.FD;

        return IFFIC;
    end To_C;

    procedure Free(Struct: in out Import_Fence_FD_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Fence_Get_FD_Info) return Fence_Get_FD_Info_C is
        FGFIC: Fence_Get_FD_Info_C;
    begin
        FGFIC.Next := Extension_Records.To_C(Struct.Next);
        FGFIC.Fence := Struct.Fence;
        FGFIC.Handle_Type := Struct.Handle_Type;

        return FGFIC;
    end To_C;

    procedure Free(Struct: in out Fence_Get_FD_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Import_Fence_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Import_Fence_FD_Info,
                         Import_Fence_FD_Info_C,
                         Import_Fence_FD_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Fence_Get_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Fence_Get_FD_Info,
                         Fence_Get_FD_Info_C,
                         Fence_Get_FD_Info_C_Access);
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
            when Import_Fence_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Import_Fence_FD_Info_C,
                         Import_Fence_FD_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Fence_Get_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Fence_Get_FD_Info_C,
                         Fence_Get_FD_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.External_Fence_FDs_C;


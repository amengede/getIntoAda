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

-- C interface for the external memory extension

with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.External_Memories_C is
    function To_C(Struct: in External_Memory_Image_Create_Info_NV)
        return External_Memory_Image_Create_Info_NV_C is
        EMICINC: External_Memory_Image_Create_Info_NV_C;
    begin
        EMICINC.Next := Extension_Records.To_C(Struct.Next);
        EMICINC.Handle_Types := Struct.Handle_Types;

        return EMICINC;
    end To_C;

    procedure Free(Struct: in out External_Memory_Image_Create_Info_NV_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Export_Memory_Allocate_Info_NV)
        return Export_Memory_Allocate_Info_NV_C is
        EMAINC: Export_Memory_Allocate_Info_NV_C;
    begin
        EMAINC.Next := Extension_Records.To_C(Struct.Next);
        EMAINC.Handle_Types := Struct.Handle_Types;

        return EMAINC;
    end To_C;

    procedure Free(Struct: in out Export_Memory_Allocate_Info_NV_C) is
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
            when External_Memory_Image_Create_Info_NV_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (External_Memory_Image_Create_Info_NV,
                         External_Memory_Image_Create_Info_NV_C,
                         External_Memory_Image_Create_Info_NV_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Export_Memory_Allocate_Info_NV_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Export_Memory_Allocate_Info_NV,
                         Export_Memory_Allocate_Info_NV_C,
                         Export_Memory_Allocate_Info_NV_C_Access);
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
            when External_Memory_Image_Create_Info_NV_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (External_Memory_Image_Create_Info_NV_C,
                         External_Memory_Image_Create_Info_NV_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Export_Memory_Allocate_Info_NV_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Export_Memory_Allocate_Info_NV_C,
                         Export_Memory_Allocate_Info_NV_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.External_Memories_C;


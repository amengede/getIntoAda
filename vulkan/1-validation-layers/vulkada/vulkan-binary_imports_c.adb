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

-- C interface for the binary import extension

with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Binary_Imports_C is
    function To_C(Struct: in Cu_Module_Create_Info)
        return Cu_Module_Create_Info_C is
        CMCIC: Cu_Module_Create_Info_C;
    begin
        CMCIC.Next := Extension_Records.To_C(Struct.Next);
        CMCIC.Data_Size := Struct.Data_Size;
        CMCIC.Data := Struct.Data;

        return CMCIC;
    end To_C;

    procedure Free(Struct: in out Cu_Module_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Cu_Function_Create_Info)
        return Cu_Function_Create_Info_C is
        CFCIC: Cu_Function_Create_Info_C;
    begin
        CFCIC.Next := Extension_Records.To_C(Struct.Next);
        CFCIC.Module := Struct.Module;
        CFCIC.Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Name));

        return CFCIC;
    end To_C;

    procedure Free(Struct: in out Cu_Function_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Name);
    end Free;

    function To_C(Struct: in Cu_Launch_Info) return Cu_Launch_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(Void_Pointer_Arrays,
                                                         Void_Pointer_Vectors);

        CLIC: Cu_Launch_Info_C;
        Count: Interfaces.Unsigned_32;
    begin
        CLIC.Next := Extension_Records.To_C(Struct.Next);
        CLIC.Cu_Function := Struct.Cu_Function;
        CLIC.Grid_Dim_X := Struct.Grid_Dim_X;
        CLIC.Grid_Dim_Y := Struct.Grid_Dim_Y;
        CLIC.Grid_Dim_Z := Struct.Grid_Dim_Z;
        CLIC.Block_Dim_X := Struct.Block_Dim_X;
        CLIC.Block_Dim_Y := Struct.Block_Dim_Y;
        CLIC.Block_Dim_Z := Struct.Block_Dim_Z;
        CLIC.Shared_Mem_Bytes := Struct.Shared_Mem_Bytes;
        To_C_Array(Count, Struct.Params, CLIC.Params);
        CLIC.Param_Count := Interfaces.C.size_t(Count);
        To_C_Array(Count, Struct.Extras, CLIC.Extras);
        CLIC.Extra_Count := Interfaces.C.size_t(Count);

        return CLIC;
    end To_C;

    procedure Free(Struct: in out Cu_Launch_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Void_Pointer_Arrays.Free(Struct.Params);
        Void_Pointer_Arrays.Free(Struct.Extras);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Cu_Module_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Cu_Module_Create_Info,
                         Cu_Module_Create_Info_C,
                         Cu_Module_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Cu_Function_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Cu_Function_Create_Info,
                         Cu_Function_Create_Info_C,
                         Cu_Function_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Cu_Launch_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Cu_Launch_Info,
                         Cu_Launch_Info_C,
                         Cu_Launch_Info_C_Access);
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
            when Cu_Module_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Cu_Module_Create_Info_C,
                         Cu_Module_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Cu_Function_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Cu_Function_Create_Info_C,
                         Cu_Function_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Cu_Launch_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Cu_Launch_Info_C, Cu_Launch_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Binary_Imports_C;


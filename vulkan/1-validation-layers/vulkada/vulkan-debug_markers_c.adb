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

-- C interface for the debug marker extension

with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Debug_Markers_C is
    function To_C(Struct: in Debug_Marker_Object_Name_Info)
        return Debug_Marker_Object_Name_Info_C is
        DMONIC: Debug_Marker_Object_Name_Info_C;
    begin
        DMONIC.Next := Extension_Records.To_C(Struct.Next);
        DMONIC.Object_Type := Struct.Object_Type;
        DMONIC.Object := Struct.Object;
        DMONIC.Object_Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Object_Name));
        
        return DMONIC;
    end To_C;

    procedure Free(Struct: in out Debug_Marker_Object_Name_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Object_Name);
    end Free;

    function To_C(Struct: in Debug_Marker_Object_Tag_Info)
        return Debug_Marker_Object_Tag_Info_C is
        DMOTIC: Debug_Marker_Object_Tag_Info_C;
    begin
        DMOTIC.Next := Extension_Records.To_C(Struct.Next);
        DMOTIC.Object_Type := Struct.Object_Type;
        DMOTIC.Object := Struct.Object;
        DMOTIC.Tag_Name := Struct.Tag_Name;
        DMOTIC.Tag_Size := Struct.Tag_Size;
        DMOTIC.Tag := Struct.Tag;

        return DMOTIC;
    end To_C;

    procedure Free(Struct: in out Debug_Marker_Object_Tag_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Debug_Marker_Marker_Info)
        return Debug_Marker_Marker_Info_C is
        DMMIC: Debug_Marker_Marker_Info_C;
    begin
        DMMIC.Next := Extension_Records.To_C(Struct.Next);
        DMMIC.Marker_Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Marker_Name));
        DMMIC.Color := Struct.Color;

        return DMMIC;
    end To_C;

    procedure Free(Struct: in out Debug_Marker_Marker_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Marker_Name);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Debug_Marker_Object_Name_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Debug_Marker_Object_Name_Info,
                         Debug_Marker_Object_Name_Info_C,
                         Debug_Marker_Object_Name_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Marker_Object_Tag_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Debug_Marker_Object_Tag_Info,
                         Debug_Marker_Object_Tag_Info_C,
                         Debug_Marker_Object_Tag_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Marker_Marker_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Debug_Marker_Marker_Info,
                         Debug_Marker_Marker_Info_C,
                         Debug_Marker_Marker_Info_C_Access);
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
            when Debug_Marker_Object_Name_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Marker_Object_Name_Info_C,
                         Debug_Marker_Object_Name_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Marker_Object_Tag_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Marker_Object_Tag_Info_C,
                         Debug_Marker_Object_Tag_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Marker_Marker_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Marker_Marker_Info_C,
                         Debug_Marker_Marker_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Debug_Markers_C;


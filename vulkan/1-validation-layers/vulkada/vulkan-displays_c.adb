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

-- C interface for the displays extension

with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Displays_C is
    function To_C(Struct: in Display_Mode_Create_Info)
        return Display_Mode_Create_Info_C is
        DMCIC: Display_Mode_Create_Info_C;
    begin
        DMCIC.Next := Extension_Records.To_C(Struct.Next);
        DMCIC.Flags := Struct.Flags;
        DMCIC.Parameters := Struct.Parameters;

        return DMCIC;
    end To_C;

    procedure Free(Struct: in out Display_Mode_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Display_Properties) return Display_Properties_C is
        use type Ada.Strings.Unbounded.Unbounded_String;

        DPC: Display_Properties_C;
    begin
        DPC.Display := Struct.Display;

        if Struct.Display_Name /= "" then
            DPC.Display_Name := Interfaces.C.Strings.New_String
                (Ada.Strings.Unbounded.To_String(Struct.Display_Name));
        end if;

        DPC.Physical_Dimensions := Struct.Physical_Dimensions;
        DPC.Physical_Resolution := Struct.Physical_Resolution;
        DPC.Supported_Transforms := Struct.Supported_Transforms;
        DPC.Plane_Reorder_Possible :=
            Utilities.To_C(Struct.Plane_Reorder_Possible);
        DPC.Persistent_Content := Utilities.To_C(Struct.Persistent_Content);

        return DPC;
    end To_C;

    function To_Ada(DPC: Display_Properties_C) return Display_Properties is
        use type Interfaces.Unsigned_32;
        use type Interfaces.C.Strings.chars_ptr;

        DP: Display_Properties;
    begin
        DP.Display := DPC.Display;

        if DPC.Display_Name /= Interfaces.C.Strings.Null_Ptr then
            DP.Display_Name := Ada.Strings.Unbounded.To_Unbounded_String
                                (Interfaces.C.Strings.Value(DPC.Display_Name));
        end if;

        DP.Physical_Dimensions := DPC.Physical_Dimensions;
        DP.Physical_Resolution := DPC.Physical_Resolution;
        DP.Supported_Transforms := DPC.Supported_Transforms;
        DP.Plane_Reorder_Possible := DPC.Plane_Reorder_Possible /= 0;
        DP.Persistent_Content := DPC.Persistent_Content /= 0;

        return DP;
    end To_Ada;

    procedure Free(Struct: in out Display_Properties_C) is
    begin
        Interfaces.C.Strings.Free(Struct.Display_Name);
    end Free;

    function To_C(Struct: in Display_Surface_Create_Info)
        return Display_Surface_Create_Info_C is
        DSCIC: Display_Surface_Create_Info_C;
    begin
        DSCIC.Next := Extension_Records.To_C(Struct.Next);
        DSCIC.Flags := Struct.Flags;
        DSCIC.Display_Mode := Struct.Display_Mode;
        DSCIC.Plane_Index := Struct.Plane_Index;
        DSCIC.Plane_Stack_Index := Struct.Plane_Stack_Index;
        DSCIC.Transform := Struct.Transform;
        DSCIC.Global_Alpha := Interfaces.C.C_float(Struct.Global_Alpha);
        DSCIC.Alpha_Mode := Struct.Alpha_Mode;
        DSCIC.Image_Extent := Struct.Image_Extent;

        return DSCIC;
    end To_C;

    procedure Free(Struct: in out Display_Surface_Create_Info_C) is
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
            when Display_Mode_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Display_Mode_Create_Info,
                         Display_Mode_Create_Info_C,
                         Display_Mode_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Surface_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Display_Surface_Create_Info,
                         Display_Surface_Create_Info_C,
                         Display_Surface_Create_Info_C_Access);
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
            when Display_Mode_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Mode_Create_Info_C,
                         Display_Mode_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Surface_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Surface_Create_Info_C,
                         Display_Surface_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Displays_C;


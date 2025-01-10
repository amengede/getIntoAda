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

-- C interface for AMD records

with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.C_AMD is
    function To_C(Struct: in Extensions.AMD.Attachment_Sample_Count_Info)
        return Attachment_Sample_Count_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Sample_Count_Flags_Arrays,
             Extensions.AMD.Sample_Count_Flags_Vectors);

        ASCIC: Attachment_Sample_Count_Info_C;
    begin
        ASCIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(ASCIC.Color_Attachment_Count,
                   Struct.Color_Attachment_Samples,
                   ASCIC.Color_Attachment_Samples);
        ASCIC.Depth_Stencil_Attachment_Samples :=
            Struct.Depth_Stencil_Attachment_Samples;

        return ASCIC;
    end To_C;

    procedure Free(Struct: in out Attachment_Sample_Count_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Sample_Count_Flags_Arrays.Free(Struct.Color_Attachment_Samples);
    end Free;

    function To_C
        (Struct:
            in Extensions.AMD.Pipeline_Rasterization_State_Rasterization_Order)
        return Pipeline_Rasterization_State_Rasterization_Order_C is
        PRSROC: Pipeline_Rasterization_State_Rasterization_Order_C;
    begin
        PRSROC.Next := Extension_Records.To_C(Struct.Next);
        PRSROC.Rasterization_Order := Struct.Rasterization_Order;

        return PRSROC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Rasterization_State_Rasterization_Order_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.AMD.Texture_LOD_Gather_Format_Properties;
         C_Struct: in Texture_LOD_Gather_Format_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Supports_Texture_Gather_LOD_Bias :=
            Utilities.To_Ada(C_Struct.Supports_Texture_Gather_LOD_Bias);
    end To_Ada;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Attachment_Sample_Count_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.AMD.Attachment_Sample_Count_Info,
                         Attachment_Sample_Count_Info_C,
                         Attachment_Sample_Count_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Rasterization_State_Rasterization_Order_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
               (Extensions.AMD.Pipeline_Rasterization_State_Rasterization_Order,
                Pipeline_Rasterization_State_Rasterization_Order_C,
                Pipeline_Rasterization_State_Rasterization_Order_C_Access);
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
            when Texture_LOD_Gather_Format_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.AMD.Texture_LOD_Gather_Format_Properties,
                         Texture_LOD_Gather_Format_Properties_C,
                         Texture_LOD_Gather_Format_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Texture_LOD_Gather_Format_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Texture_LOD_Gather_Format_Properties_C_Access);
                begin
                    To_Ada(Extensions.AMD.Texture_LOD_Gather_Format_Properties
                            (Ada_Struct),
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
            when Attachment_Sample_Count_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Attachment_Sample_Count_Info_C,
                         Attachment_Sample_Count_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Rasterization_State_Rasterization_Order_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Pipeline_Rasterization_State_Rasterization_Order_C,
                     Pipeline_Rasterization_State_Rasterization_Order_C_Access);
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
            when Texture_LOD_Gather_Format_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Texture_LOD_Gather_Format_Properties_C,
                         Texture_LOD_Gather_Format_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.C_AMD;


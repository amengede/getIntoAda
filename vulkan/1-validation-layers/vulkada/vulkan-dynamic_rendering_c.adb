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

-- C interface for the dynamic rendering extension

with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Dynamic_Rendering_C is
    function To_C(Struct: in Rendering_Fragment_Shading_Rate_Attachment_Info)
        return Rendering_Fragment_Shading_Rate_Attachment_Info_C is
        RFSRAIC: Rendering_Fragment_Shading_Rate_Attachment_Info_C;
    begin
        RFSRAIC.Next := Extension_Records.To_C(Struct.Next);
        RFSRAIC.Image_View := Struct.Image_View;
        RFSRAIC.Image_Layout := Struct.Image_Layout;
        RFSRAIC.Shading_Rate_Attachment_Texel_Size :=
            Struct.Shading_Rate_Attachment_Texel_Size;

        return RFSRAIC;
    end To_C;

    procedure Free
        (Struct: in out Rendering_Fragment_Shading_Rate_Attachment_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Rendering_Fragment_Density_Map_Attachment_Info)
        return Rendering_Fragment_Density_Map_Attachment_Info_C is
        RFDMAIC: Rendering_Fragment_Density_Map_Attachment_Info_C;
    begin
        RFDMAIC.Next := Extension_Records.To_C(Struct.Next);
        RFDMAIC.Image_View := Struct.Image_View;
        RFDMAIC.Image_Layout := Struct.Image_Layout;

        return RFDMAIC;
    end To_C;

    procedure Free
        (Struct: in out Rendering_Fragment_Density_Map_Attachment_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Attachment_Sample_Count_Info)
        return Attachment_Sample_Count_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Sample_Count_Flags_Arrays, Sample_Count_Flags_Vectors);

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

    function To_C(Struct: in Multiview_Per_View_Attributes_Info)
        return Multiview_Per_View_Attributes_Info_C is
        MPVAIC: Multiview_Per_View_Attributes_Info_C;
    begin
        MPVAIC.Next := Extension_Records.To_C(Struct.Next);
        MPVAIC.Per_View_Attributes :=
            Utilities.To_C(Struct.Per_View_Attributes);
        MPVAIC.Per_View_Attributes_Position_X_Only :=
            Utilities.To_C(Struct.Per_View_Attributes_Position_X_Only);

        return MPVAIC;
    end To_C;

    procedure Free(Struct: in out Multiview_Per_View_Attributes_Info_C) is
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
            when Rendering_Fragment_Shading_Rate_Attachment_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                     (Rendering_Fragment_Shading_Rate_Attachment_Info,
                      Rendering_Fragment_Shading_Rate_Attachment_Info_C,
                      Rendering_Fragment_Shading_Rate_Attachment_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Rendering_Fragment_Density_Map_Attachment_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                      (Rendering_Fragment_Density_Map_Attachment_Info,
                       Rendering_Fragment_Density_Map_Attachment_Info_C,
                       Rendering_Fragment_Density_Map_Attachment_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Attachment_Sample_Count_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Attachment_Sample_Count_Info,
                         Attachment_Sample_Count_Info_C,
                         Attachment_Sample_Count_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Multiview_Per_View_Attributes_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Multiview_Per_View_Attributes_Info,
                         Multiview_Per_View_Attributes_Info_C,
                         Multiview_Per_View_Attributes_Info_C_Access);
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
            when Rendering_Fragment_Shading_Rate_Attachment_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Rendering_Fragment_Shading_Rate_Attachment_Info_C,
                      Rendering_Fragment_Shading_Rate_Attachment_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Rendering_Fragment_Density_Map_Attachment_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                      (Rendering_Fragment_Density_Map_Attachment_Info_C,
                       Rendering_Fragment_Density_Map_Attachment_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Attachment_Sample_Count_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Attachment_Sample_Count_Info_C,
                         Attachment_Sample_Count_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Multiview_Per_View_Attributes_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Multiview_Per_View_Attributes_Info_C,
                         Multiview_Per_View_Attributes_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Dynamic_Rendering_C;


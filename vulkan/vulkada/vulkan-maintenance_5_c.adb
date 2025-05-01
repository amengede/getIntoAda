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

-- C interface for the maintenance 5 extension

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Maintenance_5_C is
    procedure To_Ada(Ada_Struct: in out Physical_Device_Maintenance_5_Features;
                     C_Struct: in Physical_Device_Maintenance_5_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Maintenance_5 := Utilities.To_Ada(C_Struct.Maintenance_5);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_5_Properties;
         C_Struct: in Physical_Device_Maintenance_5_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Early_Fragment_Multisample_Coverage_After_Sample_Counting :=
            Utilities.To_Ada
                (C_Struct.
                    Early_Fragment_Multisample_Coverage_After_Sample_Counting);
        Ada_Struct.Early_Fragment_Sample_Mask_Test_Before_Sample_Counting :=
            Utilities.To_Ada
                (C_Struct.
                    Early_Fragment_Sample_Mask_Test_Before_Sample_Counting);
        Ada_Struct.Depth_Stencil_Swizzle_One_Support :=
            Utilities.To_Ada(C_Struct.Depth_Stencil_Swizzle_One_Support);
        Ada_Struct.Polygon_Mode_Point_Size :=
            Utilities.To_Ada(C_Struct.Polygon_Mode_Point_Size);
        Ada_Struct.Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram :=
            Utilities.To_Ada
                (C_Struct.Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram);
        Ada_Struct.Non_Strict_Wide_Lines_Use_Parallelogram :=
            Utilities.To_Ada(C_Struct.Non_Strict_Wide_Lines_Use_Parallelogram);
    end To_Ada;

    function To_C(Struct: in Rendering_Area_Info)
        return Rendering_Area_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C_V1_2.Format_Arrays,
                                                         Format_Vectors);

        RAIC: Rendering_Area_Info_C;
    begin
        RAIC.Next := Extension_Records.To_C(Struct.Next);
        RAIC.View_Mask := Struct.View_Mask;
        To_C_Array(RAIC.Color_Attachment_Count,
                   Struct.Color_Attachment_Formats,
                   RAIC.Color_Attachment_Formats);
        RAIC.Depth_Attachment_Format := Struct.Depth_Attachment_Format;
        RAIC.Stencil_Attachment_Format := Struct.Stencil_Attachment_Format;

        return RAIC;
    end To_C;

    procedure Free(Struct: in out Rendering_Area_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C_V1_2.Format_Arrays.Free(Struct.Color_Attachment_Formats);
    end Free;

    procedure To_Ada(Ada_Struct: in out Image_Subresource_2;
                     C_Struct: in Image_Subresource_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Image_Subresource := C_Struct.Image_Subresource;
    end To_Ada;

    function To_C(Struct: in Device_Image_Subresource_Info)
        return Device_Image_Subresource_Info_C is
        DISIC: Device_Image_Subresource_Info_C;
    begin
        DISIC.Next := Extension_Records.To_C(Struct.Next);
        DISIC.Create_Info :=
            new C.Image_Create_Info_C'(C.To_C(Struct.Create_Info.all));
        DISIC.Subresource := new Image_Subresource_2_C;
        DISIC.Subresource.Image_Subresource :=
            Struct.Subresource.Image_Subresource;

        return DISIC;
    end To_C;

    procedure Free(Struct: in out Device_Image_Subresource_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (C.Image_Create_Info_C, C.Image_Create_Info_C_Access);
        procedure Free is new Ada.Unchecked_Deallocation
            (Image_Subresource_2_C, Image_Subresource_2_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        C.Free(Struct.Create_Info.all);
        Free(Struct.Create_Info);
        Free(Struct.Subresource);
    end Free;

    procedure To_Ada(Ada_Struct: in out Subresource_Layout_2;
                     C_Struct: in Subresource_Layout_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Subresource_Layout := C_Struct.Subresource_Layout;
    end To_Ada;

    function To_C(Struct: in Pipeline_Create_Flags_2_Create_Info)
        return Pipeline_Create_Flags_2_Create_Info_C is
        PCF2CIC: Pipeline_Create_Flags_2_Create_Info_C;
    begin
        PCF2CIC.Next := Extension_Records.To_C(Struct.Next);
        PCF2CIC.Flags := Struct.Flags;

        return PCF2CIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Create_Flags_2_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Buffer_Usage_Flags_2_Create_Info)
        return Buffer_Usage_Flags_2_Create_Info_C is
        BUF2CIC: Buffer_Usage_Flags_2_Create_Info_C;
    begin
        BUF2CIC.Next := Extension_Records.To_C(Struct.Next);
        BUF2CIC.Flags := Struct.Flags;

        return BUF2CIC;
    end To_C;

    procedure Free(Struct: in out Buffer_Usage_Flags_2_Create_Info_C) is
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
            when Rendering_Area_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Rendering_Area_Info,
                         Rendering_Area_Info_C,
                         Rendering_Area_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Image_Subresource_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Image_Subresource_Info,
                         Device_Image_Subresource_Info_C,
                         Device_Image_Subresource_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Create_Flags_2_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Pipeline_Create_Flags_2_Create_Info,
                         Pipeline_Create_Flags_2_Create_Info_C,
                         Pipeline_Create_Flags_2_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Usage_Flags_2_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Usage_Flags_2_Create_Info,
                         Buffer_Usage_Flags_2_Create_Info_C,
                         Buffer_Usage_Flags_2_Create_Info_C_Access);
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
            when Physical_Device_Maintenance_5_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_5_Features,
                         Physical_Device_Maintenance_5_Features_C,
                         Physical_Device_Maintenance_5_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Maintenance_5_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_5_Properties,
                         Physical_Device_Maintenance_5_Properties_C,
                         Physical_Device_Maintenance_5_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Subresource_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Image_Subresource_2,
                         Image_Subresource_2_C,
                         Image_Subresource_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Subresource_Layout_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Subresource_Layout_2,
                         Subresource_Layout_2_C,
                         Subresource_Layout_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Maintenance_5_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_5_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_5_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Maintenance_5_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_5_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_5_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Image_Subresource_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Image_Subresource_2_C_Access);
                begin
                    To_Ada(Image_Subresource_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Subresource_Layout_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Subresource_Layout_2_C_Access);
                begin
                    To_Ada(Subresource_Layout_2(Ada_Struct),
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
            when Rendering_Area_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Rendering_Area_Info_C,
                         Rendering_Area_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Image_Subresource_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Image_Subresource_Info_C,
                         Device_Image_Subresource_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Create_Flags_2_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Create_Flags_2_Create_Info_C,
                         Pipeline_Create_Flags_2_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Usage_Flags_2_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Buffer_Usage_Flags_2_Create_Info_C,
                         Buffer_Usage_Flags_2_Create_Info_C_Access);
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
            when Physical_Device_Maintenance_5_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_5_Features_C,
                         Physical_Device_Maintenance_5_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Maintenance_5_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_5_Properties_C,
                         Physical_Device_Maintenance_5_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Subresource_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Image_Subresource_2_C, Image_Subresource_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Subresource_Layout_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Subresource_Layout_2_C, Subresource_Layout_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Maintenance_5_C;


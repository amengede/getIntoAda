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

-- C interface to the fragment shading rate extension

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Fragment_Shading_Rates_C is
    function To_C(Struct: in Fragment_Shading_Rate_Attachment_Info)
        return Fragment_Shading_Rate_Attachment_Info_C is
        FSRAIC: Fragment_Shading_Rate_Attachment_Info_C;
    begin
        FSRAIC.Next := Extension_Records.To_C(Struct.Next);

        if Struct.Fragment_Shading_Rate_Attachment /= null then
            FSRAIC.Fragment_Shading_Rate_Attachment :=
                new C_V1_2.Attachment_Reference_2_C'
                    (C_V1_2.To_C(Struct.Fragment_Shading_Rate_Attachment.all));
        end if;

        FSRAIC.Shading_Rate_Attachment_Texel_Size :=
            Struct.Shading_Rate_Attachment_Texel_Size;

        return FSRAIC;
    end To_C;

    procedure Free(Struct: in out Fragment_Shading_Rate_Attachment_Info_C) is
        use type C_V1_2.Attachment_Reference_2_C_Access;

        procedure Free is new Ada.Unchecked_Deallocation
            (C_V1_2.Attachment_Reference_2_C,
             C_V1_2.Attachment_Reference_2_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Fragment_Shading_Rate_Attachment /= null then
            C_V1_2.Free(Struct.Fragment_Shading_Rate_Attachment.all);
            Free(Struct.Fragment_Shading_Rate_Attachment);
        end if;
    end Free;

    function To_C(Struct: in Pipeline_Fragment_Shading_Rate_State_Create_Info)
        return Pipeline_Fragment_Shading_Rate_State_Create_Info_C is
        PFSRSCIC: Pipeline_Fragment_Shading_Rate_State_Create_Info_C;
    begin
        PFSRSCIC.Next := Extension_Records.To_C(Struct.Next);
        PFSRSCIC.Fragment_Size := Struct.Fragment_Size;
        PFSRSCIC.Combiner_Ops := Struct.Combiner_Ops;

        return PFSRSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Fragment_Shading_Rate_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Fragment_Shading_Rate_Features;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Pipeline_Fragment_Shading_Rate :=
            Utilities.To_Ada(C_Struct.Pipeline_Fragment_Shading_Rate);
        Ada_Struct.Primitive_Fragment_Shading_Rate :=
            Utilities.To_Ada(C_Struct.Primitive_Fragment_Shading_Rate);
        Ada_Struct.Attachment_Fragment_Shading_Rate :=
            Utilities.To_Ada(C_Struct.Attachment_Fragment_Shading_Rate);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Fragment_Shading_Rate_Properties;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Min_Fragment_Shading_Rate_Attachment_Texel_Size :=
            C_Struct.Min_Fragment_Shading_Rate_Attachment_Texel_Size;
        Ada_Struct.Max_Fragment_Shading_Rate_Attachment_Texel_Size :=
            C_Struct.Max_Fragment_Shading_Rate_Attachment_Texel_Size;
        Ada_Struct.
            Max_Fragment_Shading_Rate_Attachment_Texel_Size_Aspect_Ratio :=
            C_Struct.
                Max_Fragment_Shading_Rate_Attachment_Texel_Size_Aspect_Ratio;
        Ada_Struct.Primitive_Fragment_Shading_Rate_With_Multiple_Viewports :=
            Utilities.To_Ada
                (C_Struct.
                    Primitive_Fragment_Shading_Rate_With_Multiple_Viewports);
        Ada_Struct.Layered_Shading_Rate_Attachments :=
            Utilities.To_Ada(C_Struct.Layered_Shading_Rate_Attachments);
        Ada_Struct.Fragment_Shading_Rate_Non_Trivial_Combiner_Ops :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_Non_Trivial_Combiner_Ops);
        Ada_Struct.Max_Fragment_Size := C_Struct.Max_Fragment_Size;
        Ada_Struct.Max_Fragment_Size_Aspect_Ratio :=
            C_Struct.Max_Fragment_Size_Aspect_Ratio;
        Ada_Struct.Max_Fragment_Shading_Rate_Coverage_Samples :=
            C_Struct.Max_Fragment_Shading_Rate_Coverage_Samples;
        Ada_Struct.Max_Fragment_Shading_Rate_Rasterization_Samples :=
            C_Struct.Max_Fragment_Shading_Rate_Rasterization_Samples;
        Ada_Struct.Fragment_Shading_Rate_With_Shader_Depth_Stencil_Writes :=
            Utilities.To_Ada
                (C_Struct.
                    Fragment_Shading_Rate_With_Shader_Depth_Stencil_Writes);
        Ada_Struct.Fragment_Shading_Rate_With_Sample_Mask :=
            Utilities.To_Ada(C_Struct.Fragment_Shading_Rate_With_Sample_Mask);
        Ada_Struct.Fragment_Shading_Rate_With_Shader_Sample_Mask :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_With_Shader_Sample_Mask);
        Ada_Struct.Fragment_Shading_Rate_With_Conservative_Rasterization :=
            Utilities.To_Ada
                (C_Struct.
                    Fragment_Shading_Rate_With_Conservative_Rasterization);
        Ada_Struct.Fragment_Shading_Rate_With_Fragment_Shader_Interlock :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_With_Fragment_Shader_Interlock);
        Ada_Struct.Fragment_Shading_Rate_With_Custom_Sample_Locations :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_With_Custom_Sample_Locations);
        Ada_Struct.Fragment_Shading_Rate_Strict_Multiply_Combiner := 
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_Strict_Multiply_Combiner);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Fragment_Shading_Rate;
                     C_Struct: in Physical_Device_Fragment_Shading_Rate_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Sample_Counts := C_Struct.Sample_Counts;
        ADa_Struct.Fragment_Size := C_Struct.Fragment_Size;
    end To_Ada;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Fragment_Shading_Rate_Attachment_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Fragment_Shading_Rate_Attachment_Info,
                         Fragment_Shading_Rate_Attachment_Info_C,
                         Fragment_Shading_Rate_Attachment_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Fragment_Shading_Rate_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                    (Pipeline_Fragment_Shading_Rate_State_Create_Info,
                     Pipeline_Fragment_Shading_Rate_State_Create_Info_C,
                     Pipeline_Fragment_Shading_Rate_State_Create_Info_C_Access);
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
            when Physical_Device_Fragment_Shading_Rate_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                      (Physical_Device_Fragment_Shading_Rate_Features,
                       Physical_Device_Fragment_Shading_Rate_Features_C,
                       Physical_Device_Fragment_Shading_Rate_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Physical_Device_Fragment_Shading_Rate_Properties,
                     Physical_Device_Fragment_Shading_Rate_Properties_C,
                     Physical_Device_Fragment_Shading_Rate_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Fragment_Shading_Rate,
                         Physical_Device_Fragment_Shading_Rate_C,
                         Physical_Device_Fragment_Shading_Rate_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Fragment_Shading_Rate_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Fragment_Shading_Rate_Features_C_Access);
                begin
                    To_Ada
                    (Physical_Device_Fragment_Shading_Rate_Features(Ada_Struct),
                     To_Access(Next).all);
                end;
            when Physical_Device_Fragment_Shading_Rate_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                    (C.Out_Structure_C_Access,
                     Physical_Device_Fragment_Shading_Rate_Properties_C_Access);
                begin
                    To_Ada
                  (Physical_Device_Fragment_Shading_Rate_Properties(Ada_Struct),
                   To_Access(Next).all);
                end;
            when Physical_Device_Fragment_Shading_Rate_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Fragment_Shading_Rate_C_Access);
                begin
                    To_Ada(Physical_Device_Fragment_Shading_Rate(Ada_Struct),
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
            when Fragment_Shading_Rate_Attachment_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Fragment_Shading_Rate_Attachment_Info_C,
                         Fragment_Shading_Rate_Attachment_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Fragment_Shading_Rate_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Pipeline_Fragment_Shading_Rate_State_Create_Info_C,
                     Pipeline_Fragment_Shading_Rate_State_Create_Info_C_Access);
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
            when Physical_Device_Fragment_Shading_Rate_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Fragment_Shading_Rate_Features_C,
                       Physical_Device_Fragment_Shading_Rate_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                    (Physical_Device_Fragment_Shading_Rate_Properties_C,
                     Physical_Device_Fragment_Shading_Rate_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Fragment_Shading_Rate_C,
                         Physical_Device_Fragment_Shading_Rate_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Fragment_Shading_Rates_C;


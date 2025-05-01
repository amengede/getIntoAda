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

-- C interface for the transform feedback extension

with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Transform_Feedbacks_C is
    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Transform_Feedback_Features;
         C_Struct: in Physical_Device_Transform_Feedback_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Transform_Feedback :=
            Utilities.To_Ada(C_Struct.Transform_Feedback);
        Ada_Struct.Geometry_Streams :=
            Utilities.To_Ada(C_Struct.Geometry_Streams);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Transform_Feedback_Properties;
         C_Struct: in Physical_Device_Transform_Feedback_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Transform_Feedback_Streams :=
            C_Struct.Max_Transform_Feedback_Streams;
        Ada_Struct.Max_Transform_Feedback_Buffers :=
            C_Struct.Max_Transform_Feedback_Buffers;
        Ada_Struct.Max_Transform_Feedback_Buffer_Size :=
            C_Struct.Max_Transform_Feedback_Buffer_Size;
        Ada_Struct.Max_Transform_Feedback_Stream_Data_Size :=
            C_Struct.Max_Transform_Feedback_Stream_Data_Size;
        Ada_Struct.Max_Transform_Feedback_Buffer_Data_Size :=
            C_Struct.Max_Transform_Feedback_Buffer_Data_Size;
        Ada_Struct.Max_Transform_Feedback_Buffer_Data_Stride :=
            C_Struct.Max_Transform_Feedback_Buffer_Data_Stride;
        Ada_Struct.Transform_Feedback_Queries :=
            Utilities.To_Ada(C_Struct.Transform_Feedback_Queries);
        Ada_Struct.Transform_Feedback_Streams_Lines_Triangles := 
            Utilities.To_Ada
                (C_Struct.Transform_Feedback_Streams_Lines_Triangles);
        Ada_Struct.Transform_Feedback_Rasterization_Stream_Select :=
            Utilities.To_Ada
                (C_Struct.Transform_Feedback_Rasterization_Stream_Select);
        Ada_Struct.Transform_Feedback_Draw :=
            Utilities.To_Ada(C_Struct.Transform_Feedback_Draw);
    end To_Ada;

    function To_C(Struct: in Pipeline_Rasterization_State_Stream_Create_Info)
        return Pipeline_Rasterization_State_Stream_Create_Info_C is
        PRSSCIC: Pipeline_Rasterization_State_Stream_Create_Info_C;
    begin
        PRSSCIC.Next := Extension_Records.To_C(Struct.Next);
        PRSSCIC.Flags := Struct.Flags;
        PRSSCIC.Rasterization_Stream := Struct.Rasterization_Stream;

        return PRSSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Rasterization_State_Stream_Create_Info_C) is
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
            when Pipeline_Rasterization_State_Stream_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                     (Pipeline_Rasterization_State_Stream_Create_Info,
                      Pipeline_Rasterization_State_Stream_Create_Info_C,
                      Pipeline_Rasterization_State_Stream_Create_Info_C_Access);
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
            when Physical_Device_Transform_Feedback_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Transform_Feedback_Features,
                         Physical_Device_Transform_Feedback_Features_C,
                         Physical_Device_Transform_Feedback_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Transform_Feedback_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Physical_Device_Transform_Feedback_Properties,
                        Physical_Device_Transform_Feedback_Properties_C,
                        Physical_Device_Transform_Feedback_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Transform_Feedback_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Transform_Feedback_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Transform_Feedback_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Transform_Feedback_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Transform_Feedback_Properties_C_Access);
                begin
                    To_Ada
                     (Physical_Device_Transform_Feedback_Properties(Ada_Struct),
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
            when Pipeline_Rasterization_State_Stream_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Pipeline_Rasterization_State_Stream_Create_Info_C,
                      Pipeline_Rasterization_State_Stream_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when others => null;
        end case;
    end Free;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Physical_Device_Transform_Feedback_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Transform_Feedback_Features_C,
                         Physical_Device_Transform_Feedback_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Transform_Feedback_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Physical_Device_Transform_Feedback_Properties_C,
                        Physical_Device_Transform_Feedback_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Transform_Feedbacks_C;


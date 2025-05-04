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

-- C interface for the maintenance 6 extension

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Maintenance_6_C is
    procedure To_Ada(Ada_Struct: in out Physical_Device_Maintenance_6_Features;
                     C_Struct: in Physical_Device_Maintenance_6_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Maintenance_6 := Utilities.To_Ada(C_Struct.Maintenance_6);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_6_Properties;
         C_Struct: in Physical_Device_Maintenance_6_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Block_Texel_View_Compatible_Multiple_Layers :=
            Utilities.To_Ada
                (C_Struct.Block_Texel_View_Compatible_Multiple_Layers);
        Ada_Struct.Max_Combined_Image_Sampler_Descriptor_Count :=
            C_Struct.Max_Combined_Image_Sampler_Descriptor_Count;
        Ada_Struct.Fragment_Shading_Rate_Clamp_Combiner_Inputs :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_Clamp_Combiner_Inputs);
    end To_Ada;

    function To_C(Struct: in Bind_Memory_Status) return Bind_Memory_Status_C is
        BMSC: Bind_Memory_Status_C;
    begin
        BMSC.Next := Extension_Records.To_C(Struct.Next);
        BMSC.Result := Struct.Result;

        return BMSC;
    end To_C;

    procedure Free(Struct: in out Bind_Memory_Status_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Bind_Descriptor_Sets_Info)
        return Bind_Descriptor_Sets_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Descriptor_Set_Arrays, Descriptor_Set_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        BDSIC: Bind_Descriptor_Sets_Info_C;
    begin
        BDSIC.Next := Extension_Records.To_C(Struct.Next);
        BDSIC.Stage_Flags := Struct.Stage_Flags;
        BDSIC.Layout := Struct.Layout;
        BDSIC.First_Set := Struct.First_Set;
        To_C_Array(BDSIC.Descriptor_Set_Count,
                   Struct.Descriptor_Sets,
                   BDSIC.Descriptor_Sets);
        To_C_Array(BDSIC.Dynamic_Offset_Count,
                   Struct.Dynamic_Offsets,
                   BDSIC.Dynamic_Offsets);

        return BDSIC;
    end To_C;

    procedure Free(Struct: in out Bind_Descriptor_Sets_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Descriptor_Set_Arrays.Free(Struct.Descriptor_Sets);
        C.Uint32_t_Arrays.Free(Struct.Dynamic_Offsets);
    end Free;

    function To_C(Struct: in Push_Constants_Info)
        return Push_Constants_Info_C is
        PCIC: Push_Constants_Info_C;
    begin
        PCIC.Next := Extension_Records.To_C(Struct.Next);
        PCIC.Layout := Struct.Layout;
        PCIC.Stage_Flags := Struct.Stage_Flags;
        PCIC.Offset := Struct.Offset;
        PCIC.Size := Struct.Size;
        PCIC.Values := Struct.Values;

        return PCIC;
    end To_C;

    procedure Free(Struct: in out Push_Constants_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Push_Descriptor_Set_Info)
        return Push_Descriptor_Set_Info_C is
        procedure To_C_Array is
            new Utilities.To_C_Array_Convert(Write_Descriptor_Set_C_Arrays,
                                             Write_Descriptor_Set_Vectors,
                                             C.To_C);

        PDSIC: Push_Descriptor_Set_Info_C;
    begin
        PDSIC.Next := Extension_Records.To_C(Struct.Next);
        PDSIC.Stage_Flags := Struct.Stage_Flags;
        PDSIC.Layout := Struct.Layout;
        PDSIC.Set := Struct.Set;
        To_C_Array(PDSIC.Descriptor_Write_Count,
                   Struct.Descriptor_Writes,
                   PDSIC.Descriptor_Writes);

        return PDSIC;
    end To_C;

    procedure Free(Struct: in out Push_Descriptor_Set_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Write_Descriptor_Set_C_Arrays.Free(Struct.Descriptor_Writes,
                                           C.Free'Access);
    end Free;

    function To_C(Struct: in Push_Descriptor_Set_With_Template_Info)
        return Push_Descriptor_Set_With_Template_Info_C is
        PDSWTIC: Push_Descriptor_Set_With_Template_Info_C;
    begin
        PDSWTIC.Next := Extension_Records.To_C(Struct.Next);
        PDSWTIC.Descriptor_Update_Template := Struct.Descriptor_Update_Template;
        PDSWTIC.Layout := Struct.Layout;
        PDSWTIC.Set := Struct.Set;
        PDSWTIC.Data := Struct.Data;

        return PDSWTIC;
    end To_C;

    procedure Free(Struct: in out Push_Descriptor_Set_With_Template_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Set_Descriptor_Buffer_Offsets_Info)
        return Set_Descriptor_Buffer_Offsets_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array(Device_Size_Arrays,
                                                         Device_Size_Vectors);

        SDBOIC: Set_Descriptor_Buffer_Offsets_Info_C;
    begin
        SDBOIC.Next := Extension_Records.To_C(Struct.Next);
        SDBOIC.Stage_Flags := Struct.Stage_Flags;
        SDBOIC.Layout := Struct.Layout;
        SDBOIC.First_Set := Struct.First_Set;
        To_C_Array(SDBOIC.Set_Count,
                   Struct.Buffer_Indices,
                   SDBOIC.Buffer_Indices);
        To_C_Array(SDBOIC.Set_Count,
                   Struct.Offsets,
                   SDBOIC.Offsets);

        return SDBOIC;
    end To_C;

    procedure Free(Struct: in out Set_Descriptor_Buffer_Offsets_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Buffer_Indices);
        Device_Size_Arrays.Free(Struct.Offsets);
    end Free;

    function To_C(Struct: in Bind_Descriptor_Buffer_Embedded_Samplers_Info)
        return Bind_Descriptor_Buffer_Embedded_Samplers_Info_C is
        BDBESIC: Bind_Descriptor_Buffer_Embedded_Samplers_Info_C;
    begin
        BDBESIC.Next := Extension_Records.To_C(Struct.Next);
        BDBESIC.Stage_Flags := Struct.Stage_Flags;
        BDBESIC.Layout := Struct.Layout;
        BDBESIC.Set := Struct.Set;

        return BDBESIC;
    end To_C;

    procedure Free
        (Struct: in out Bind_Descriptor_Buffer_Embedded_Samplers_Info_C) is
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
            when Bind_Memory_Status_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Memory_Status,
                         Bind_Memory_Status_C,
                         Bind_Memory_Status_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Descriptor_Sets_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Descriptor_Sets_Info,
                         Bind_Descriptor_Sets_Info_C,
                         Bind_Descriptor_Sets_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Push_Constants_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Push_Constants_Info,
                         Push_Constants_Info_C,
                         Push_Constants_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Push_Descriptor_Set_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Push_Descriptor_Set_Info,
                         Push_Descriptor_Set_Info_C,
                         Push_Descriptor_Set_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Push_Descriptor_Set_With_Template_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Push_Descriptor_Set_With_Template_Info,
                         Push_Descriptor_Set_With_Template_Info_C,
                         Push_Descriptor_Set_With_Template_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Set_Descriptor_Buffer_Offsets_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Set_Descriptor_Buffer_Offsets_Info,
                         Set_Descriptor_Buffer_Offsets_Info_C,
                         Set_Descriptor_Buffer_Offsets_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                       (Bind_Descriptor_Buffer_Embedded_Samplers_Info,
                        Bind_Descriptor_Buffer_Embedded_Samplers_Info_C,
                        Bind_Descriptor_Buffer_Embedded_Samplers_Info_C_Access);
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
            when Physical_Device_Maintenance_6_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_6_Features,
                         Physical_Device_Maintenance_6_Features_C,
                         Physical_Device_Maintenance_6_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Maintenance_6_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_6_Properties,
                         Physical_Device_Maintenance_6_Properties_C,
                         Physical_Device_Maintenance_6_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Maintenance_6_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_6_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_6_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Maintenance_6_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_6_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_6_Properties(Ada_Struct),
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
            when Bind_Memory_Status_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Memory_Status_C,
                         Bind_Memory_Status_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Descriptor_Sets_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Descriptor_Sets_Info_C,
                         Bind_Descriptor_Sets_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Push_Constants_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Push_Constants_Info_C, Push_Constants_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Push_Descriptor_Set_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Push_Descriptor_Set_Info_C,
                         Push_Descriptor_Set_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Push_Descriptor_Set_With_Template_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Push_Descriptor_Set_With_Template_Info_C,
                         Push_Descriptor_Set_With_Template_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Set_Descriptor_Buffer_Offsets_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Set_Descriptor_Buffer_Offsets_Info_C,
                         Set_Descriptor_Buffer_Offsets_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Bind_Descriptor_Buffer_Embedded_Samplers_Info_C,
                        Bind_Descriptor_Buffer_Embedded_Samplers_Info_C_Access);
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
            when Physical_Device_Maintenance_6_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_6_Features_C,
                         Physical_Device_Maintenance_6_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Maintenance_6_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_6_Properties_C,
                         Physical_Device_Maintenance_6_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Maintenance_6_C;


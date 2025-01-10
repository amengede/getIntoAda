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

-- C interface for NVX records

with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.C_NVX is
    function To_C(Struct: in Extensions.NVX.Multiview_Per_View_Attributes_Info)
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

    function To_C(Struct: in Extensions.NVX.Cu_Module_Create_Info)
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

    function To_C(Struct: in Extensions.NVX.Cu_Function_Create_Info)
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

    function To_C(Struct: in Extensions.NVX.Cu_Launch_Info)
        return Cu_Launch_Info_C is
        procedure To_C_Array is
            new Utilities.To_C_Array(Void_Pointer_Arrays,
                                     Extensions.NVX.Void_Pointer_Vectors);

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

    function To_C(Struct: in Extensions.NVX.Image_View_Handle_Info)
        return Image_View_Handle_Info_C is
        IVHIC: Image_View_Handle_Info_C;
    begin
        IVHIC.Next := Extension_Records.To_C(Struct.Next);
        IVHIC.Image_View := Struct.Image_View;
        IVHIC.Descriptor_Type := Struct.Descriptor_Type;
        IVHIC.Sampler := Struct.Sampler;

        return IVHIC;
    end To_C;

    procedure Free(Struct: in out Image_View_Handle_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.NVX.Image_View_Address_Properties;
         C_Struct: in Image_View_Address_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Device_Address := C_Struct.Device_Address;
        Ada_Struct.Size := C_Struct.Size;
    end To_Ada;

    procedure To_Ada
       (Ada_Struct:
            in out
        Extensions.NVX.Physical_Device_Multiview_Per_View_Attributes_Properties;
         C_Struct: 
            in Physical_Device_Multiview_Per_View_Attributes_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Per_View_Position_All_Components :=
            Utilities.To_Ada(C_Struct.Per_View_Position_All_Components);
    end To_Ada;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Multiview_Per_View_Attributes_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.NVX.Multiview_Per_View_Attributes_Info,
                         Multiview_Per_View_Attributes_Info_C,
                         Multiview_Per_View_Attributes_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Cu_Module_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.NVX.Cu_Module_Create_Info,
                         Cu_Module_Create_Info_C,
                         Cu_Module_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Cu_Function_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.NVX.Cu_Function_Create_Info,
                         Cu_Function_Create_Info_C,
                         Cu_Function_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Cu_Launch_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.NVX.Cu_Launch_Info,
                         Cu_Launch_Info_C,
                         Cu_Launch_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_View_Handle_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.NVX.Image_View_Handle_Info,
                         Image_View_Handle_Info_C,
                         Image_View_Handle_Info_C_Access);
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
            when Image_View_Address_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.NVX.Image_View_Address_Properties,
                         Image_View_Address_Properties_C,
                         Image_View_Address_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when
                Physical_Device_Multiview_Per_View_Attributes_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
       (Extensions.NVX.Physical_Device_Multiview_Per_View_Attributes_Properties,
        Physical_Device_Multiview_Per_View_Attributes_Properties_C,
        Physical_Device_Multiview_Per_View_Attributes_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Image_View_Address_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Image_View_Address_Properties_C_Access);
                begin
                    To_Ada(Extensions.NVX.Image_View_Address_Properties
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when
                Physical_Device_Multiview_Per_View_Attributes_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
            (C.Out_Structure_C_Access,
             Physical_Device_Multiview_Per_View_Attributes_Properties_C_Access);
                begin
                    To_Ada
        (Extensions.NVX.Physical_Device_Multiview_Per_View_Attributes_Properties
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
            when Multiview_Per_View_Attributes_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Multiview_Per_View_Attributes_Info_C,
                         Multiview_Per_View_Attributes_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
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
            when Image_View_Handle_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_View_Handle_Info_C,
                         Image_View_Handle_Info_C_Access);
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
            when Image_View_Address_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Image_View_Address_Properties_C,
                         Image_View_Address_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when
                Physical_Device_Multiview_Per_View_Attributes_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
            (Physical_Device_Multiview_Per_View_Attributes_Properties_C,
             Physical_Device_Multiview_Per_View_Attributes_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.C_NVX;


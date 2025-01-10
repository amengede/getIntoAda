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

-- C interface for NV records

with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.C_NV is
    procedure To_Ada
        (Ada_Struct: in out Extensions.NV.Queue_Family_Checkpoint_Properties_2;
         C_Struct: in Queue_Family_Checkpoint_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Checkpoint_Execution_Stage_Mask :=
            C_Struct.Checkpoint_Execution_Stage_Mask;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Extensions.NV.Checkpoint_Data_2;
                     C_Struct: in Checkpoint_Data_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Stage := C_Struct.Stage;
        Ada_Struct.Checkpoint_Marker := C_Struct.Checkpoint_Marker;
    end To_Ada;

    function To_C
        (Struct: in Extensions.NV.Dedicated_Allocation_Image_Create_Info)
        return Dedicated_Allocation_Image_Create_Info_C is
        DAICIC: Dedicated_Allocation_Image_Create_Info_C;
    begin
        DAICIC.Next := Extension_Records.To_C(Struct.Next);
        DAICIC.Dedicated_Allocation :=
            Utilities.To_C(Struct.Dedicated_Allocation);

        return DAICIC;
    end To_C;

    procedure Free(Struct: in out Dedicated_Allocation_Image_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.NV.Dedicated_Allocation_Buffer_Create_Info)
        return Dedicated_Allocation_Buffer_Create_Info_C is
        DABCIC: Dedicated_Allocation_Buffer_Create_Info_C;
    begin
        DABCIC.Next := Extension_Records.To_C(Struct.Next);
        DABCIC.Dedicated_Allocation :=
            Utilities.To_C(Struct.Dedicated_Allocation);

        return DABCIC;
    end To_C;

    procedure Free(Struct: in out Dedicated_Allocation_Buffer_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.NV.Dedicated_Allocation_Memory_Allocate_Info)
        return Dedicated_Allocation_Memory_Allocate_Info_C is
        DAMAIC: Dedicated_Allocation_Memory_Allocate_Info_C;
    begin
        DAMAIC.Next := Extension_Records.To_C(Struct.Next);
        DAMAIC.Image := Struct.Image;
        DAMAIC.Buffer := Struct.Buffer;

        return DAMAIC;
    end To_C;

    procedure Free
        (Struct: in out Dedicated_Allocation_Memory_Allocate_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.NV.Physical_Device_Corner_Sampled_Image_Features;
         C_Struct: in Physical_Device_Corner_Sampled_Image_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Corner_Sampled_Image :=
            Utilities.To_Ada(C_Struct.Corner_Sampled_Image);
    end To_Ada;

    function To_C
        (Struct: in Extensions.NV.Pipeline_Viewport_W_Scaling_State_Create_Info)
        return Pipeline_Viewport_W_Scaling_State_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Viewport_W_Scaling_Arrays,
             Extensions.NV.Viewport_W_Scaling_Vectors);

        PVWSSCIC: Pipeline_Viewport_W_Scaling_State_Create_Info_C;
    begin
        PVWSSCIC.Next := Extension_Records.To_C(Struct.Next);
        PVWSSCIC.Viewport_W_Scaling_Enable :=
            Utilities.To_C(Struct.Viewport_W_Scaling_Enable);
        To_C_Array(PVWSSCIC.Viewport_Count,
                   Struct.Viewport_W_Scalings,
                   PVWSSCIC.Viewport_W_Scalings);

        return PVWSSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Viewport_W_Scaling_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Viewport_W_Scaling_Arrays.Free(Struct.Viewport_W_Scalings);
    end Free;

    function To_C
        (Struct: in Extensions.NV.Pipeline_Viewport_Swizzle_State_Create_Info)
        return Pipeline_Viewport_Swizzle_State_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Viewport_Swizzle_Arrays, Extensions.NV.Viewport_Swizzle_Vectors);

        PVSSCIC: Pipeline_Viewport_Swizzle_State_Create_Info_C;
    begin
        PVSSCIC.Next := Extension_Records.To_C(Struct.Next);
        PVSSCIC.Flags := Struct.Flags;
        To_C_Array(PVSSCIC.Viewport_Count,
                   Struct.Viewport_Swizzles,
                   PVSSCIC.Viewport_Swizzles);

        return PVSSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Viewport_Swizzle_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Viewport_Swizzle_Arrays.Free(Struct.Viewport_Swizzles);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Dedicated_Allocation_Image_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.NV.Dedicated_Allocation_Image_Create_Info,
                         Dedicated_Allocation_Image_Create_Info_C,
                         Dedicated_Allocation_Image_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Dedicated_Allocation_Buffer_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.NV.Dedicated_Allocation_Buffer_Create_Info,
                         Dedicated_Allocation_Buffer_Create_Info_C,
                         Dedicated_Allocation_Buffer_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Dedicated_Allocation_Memory_Allocate_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                       (Extensions.NV.Dedicated_Allocation_Memory_Allocate_Info,
                        Dedicated_Allocation_Memory_Allocate_Info_C,
                        Dedicated_Allocation_Memory_Allocate_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Viewport_W_Scaling_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                   (Extensions.NV.Pipeline_Viewport_W_Scaling_State_Create_Info,
                    Pipeline_Viewport_W_Scaling_State_Create_Info_C,
                    Pipeline_Viewport_W_Scaling_State_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Viewport_Swizzle_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                     (Extensions.NV.Pipeline_Viewport_Swizzle_State_Create_Info,
                      Pipeline_Viewport_Swizzle_State_Create_Info_C,
                      Pipeline_Viewport_Swizzle_State_Create_Info_C_Access);
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
            when Queue_Family_Checkpoint_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.NV.Queue_Family_Checkpoint_Properties_2,
                         Queue_Family_Checkpoint_Properties_2_C,
                         Queue_Family_Checkpoint_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Checkpoint_Data_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.NV.Checkpoint_Data_2,
                         Checkpoint_Data_2_C,
                         Checkpoint_Data_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Corner_Sampled_Image_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Extensions.NV.Physical_Device_Corner_Sampled_Image_Features,
                    Physical_Device_Corner_Sampled_Image_Features_C,
                    Physical_Device_Corner_Sampled_Image_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Queue_Family_Checkpoint_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Queue_Family_Checkpoint_Properties_2_C_Access);
                begin
                    To_Ada(Extensions.NV.Queue_Family_Checkpoint_Properties_2
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Checkpoint_Data_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Checkpoint_Data_2_C_Access);
                begin
                    To_Ada(Extensions.NV.Checkpoint_Data_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Corner_Sampled_Image_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Corner_Sampled_Image_Features_C_Access);
                begin
                   To_Ada
                    (Extensions.NV.Physical_Device_Corner_Sampled_Image_Features
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
            when Dedicated_Allocation_Image_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Dedicated_Allocation_Image_Create_Info_C,
                         Dedicated_Allocation_Image_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Dedicated_Allocation_Buffer_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Dedicated_Allocation_Buffer_Create_Info_C,
                         Dedicated_Allocation_Buffer_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Dedicated_Allocation_Memory_Allocate_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Dedicated_Allocation_Memory_Allocate_Info_C,
                         Dedicated_Allocation_Memory_Allocate_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Viewport_W_Scaling_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Pipeline_Viewport_W_Scaling_State_Create_Info_C,
                        Pipeline_Viewport_W_Scaling_State_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Viewport_Swizzle_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Viewport_Swizzle_State_Create_Info_C,
                         Pipeline_Viewport_Swizzle_State_Create_Info_C_Access);
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
            when Queue_Family_Checkpoint_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Queue_Family_Checkpoint_Properties_2_C,
                         Queue_Family_Checkpoint_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Checkpoint_Data_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Checkpoint_Data_2_C,
                         Checkpoint_Data_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Corner_Sampled_Image_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Physical_Device_Corner_Sampled_Image_Features_C,
                        Physical_Device_Corner_Sampled_Image_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.C_NV;


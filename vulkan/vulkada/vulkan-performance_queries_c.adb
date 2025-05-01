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

-- C interface for the performance query extension

with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.Performance_Queries_C is
    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Performance_Query_Features;
         C_Struct: in Physical_Device_Performance_Query_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Performance_Counter_Query_Pools :=
            Utilities.To_Ada(C_Struct.Performance_Counter_Query_Pools);
        Ada_Struct.Performance_Counter_Multiple_Query_Pools :=
            Utilities.To_Ada(C_Struct.Performance_Counter_Multiple_Query_Pools);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Performance_Query_Properties;
         C_Struct: in Physical_Device_Performance_Query_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Allow_Command_Buffer_Query_Copies :=
            Utilities.To_Ada(C_Struct.Allow_Command_Buffer_Query_Copies);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Performance_Counter;
                     C_Struct: in Performance_Counter_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Unit := C_Struct.Unit;
        Ada_Struct.Scope := C_Struct.Scope;
        Ada_Struct.Storage := C_Struct.Storage;
        Ada_Struct.UUID := C_Struct.UUID;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Performance_Counter_Description;
                     C_Struct: in Performance_Counter_Description_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Flags := C_Struct.Flags;
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Name, Interfaces.C.To_Ada(C_Struct.Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Category, Interfaces.C.To_Ada(C_Struct.Category));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Description, Interfaces.C.To_Ada(C_Struct.Description));
    end To_Ada;

    function To_C(Struct: in Query_Pool_Performance_Create_Info)
        return Query_Pool_Performance_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        QPPCIC: Query_Pool_Performance_Create_Info_C;
    begin
        QPPCIC.Next := Extension_Records.To_C(Struct.Next);
        QPPCIC.Queue_Family_Index := Struct.Queue_Family_Index;
        To_C_Array(QPPCIC.Counter_Index_Count,
                   Struct.Counter_Indices,
                   QPPCIC.Counter_Indices);

        return QPPCIC;
    end To_C;

    procedure Free(Struct: in out Query_Pool_Performance_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Counter_Indices);
    end Free;

    function To_C(Struct: in Acquire_Profiling_Lock_Info)
        return Acquire_Profiling_Lock_Info_C is
        APLIC: Acquire_Profiling_Lock_Info_C;
    begin
        APLIC.Next := Extension_Records.To_C(Struct.Next);
        APLIC.Flags := Struct.Flags;
        APLIC.Timeout := Struct.Timeout;

        return APLIC;
    end To_C;

    procedure Free(Struct: in out Acquire_Profiling_Lock_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Performance_Query_Submit_Info)
        return Performance_Query_Submit_Info_C is
        PQSIC: Performance_Query_Submit_Info_C;
    begin
        PQSIC.Next := Extension_Records.To_C(Struct.Next);
        PQSIC.Counter_Pass_Index := Struct.Counter_Pass_Index;

        return PQSIC;
    end To_C;

    procedure Free(Struct: in out Performance_Query_Submit_Info_C) is
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
            when Query_Pool_Performance_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Query_Pool_Performance_Create_Info,
                         Query_Pool_Performance_Create_Info_C,
                         Query_Pool_Performance_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Acquire_Profiling_Lock_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Acquire_Profiling_Lock_Info,
                         Acquire_Profiling_Lock_Info_C,
                         Acquire_Profiling_Lock_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Performance_Query_Submit_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Performance_Query_Submit_Info,
                         Performance_Query_Submit_Info_C,
                         Performance_Query_Submit_Info_C_Access);
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
            when Physical_Device_Performance_Query_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Performance_Query_Features,
                         Physical_Device_Performance_Query_Features_C,
                         Physical_Device_Performance_Query_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Performance_Query_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Performance_Query_Properties,
                         Physical_Device_Performance_Query_Properties_C,
                         Physical_Device_Performance_Query_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Performance_Counter_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Performance_Counter,
                         Performance_Counter_C,
                         Performance_Counter_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Performance_Counter_Description_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Performance_Counter_Description,
                         Performance_Counter_Description_C,
                         Performance_Counter_Description_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Performance_Query_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Performance_Query_Features_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Performance_Query_Features(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Physical_Device_Performance_Query_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Performance_Query_Properties_C_Access);
                begin
                    To_Ada
                      (Physical_Device_Performance_Query_Properties(Ada_Struct),
                       To_Access(Next).all);
                end;
            when Performance_Counter_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Performance_Counter_C_Access);
                begin
                    To_Ada(Performance_Counter(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Performance_Counter_Description_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Performance_Counter_Description_C_Access);
                begin
                    To_Ada(Performance_Counter_Description(Ada_Struct),
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
            when Query_Pool_Performance_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Query_Pool_Performance_Create_Info_C,
                         Query_Pool_Performance_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Acquire_Profiling_Lock_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Acquire_Profiling_Lock_Info_C,
                         Acquire_Profiling_Lock_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Performance_Query_Submit_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Performance_Query_Submit_Info_C,
                         Performance_Query_Submit_Info_C_Access);
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
            when Physical_Device_Performance_Query_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Performance_Query_Features_C,
                         Physical_Device_Performance_Query_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Performance_Query_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Performance_Query_Properties_C,
                         Physical_Device_Performance_Query_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Performance_Counter_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Performance_Counter_C, Performance_Counter_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Performance_Counter_Description_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Performance_Counter_Description_C,
                         Performance_Counter_Description_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Performance_Queries_C;


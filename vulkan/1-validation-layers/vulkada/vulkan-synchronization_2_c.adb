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

-- C interfaces for the synchronization 2 extension

with Ada.Unchecked_Conversion;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Synchronization_2_C is
    procedure To_Ada(Ada_Struct: in out Queue_Family_Checkpoint_Properties_2;
                     C_Struct: in Queue_Family_Checkpoint_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Checkpoint_Execution_Stage_Mask :=
            C_Struct.Checkpoint_Execution_Stage_Mask;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Checkpoint_Data_2;
                     C_Struct: in Checkpoint_Data_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Stage := C_Struct.Stage;
        Ada_Struct.Checkpoint_Marker := C_Struct.Checkpoint_Marker;
    end To_Ada;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Queue_Family_Checkpoint_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Queue_Family_Checkpoint_Properties_2,
                         Queue_Family_Checkpoint_Properties_2_C,
                         Queue_Family_Checkpoint_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Checkpoint_Data_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Checkpoint_Data_2,
                         Checkpoint_Data_2_C,
                         Checkpoint_Data_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Structure(Ada_Struct.Record_Type) is
            when Queue_Family_Checkpoint_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Queue_Family_Checkpoint_Properties_2_C_Access);
                begin
                    To_Ada(Queue_Family_Checkpoint_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Checkpoint_Data_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Checkpoint_Data_2_C_Access);
                begin
                    To_Ada(Checkpoint_Data_2(Ada_Struct),
                           To_Access(Next).all);
                end;
        end case;
    end To_Ada;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Structure(Next.Record_Type) is
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
        end case;
    end Free;
end Vulkan.Synchronization_2_C;


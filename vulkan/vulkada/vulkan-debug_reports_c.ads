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

-- C interface for the debug report extension

with Interfaces.C.Strings;
with Vulkan.C;

private package Vulkan.Debug_Reports_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Debug_Report_Callback_Create_Info_Type;

    -- C interface records.
    type Debug_Report_Callback_Function_C is
        access function
            (Flags: in Debug_Report_Flags;
             Object_Type: in Debug_Report_Object_Type;
             Object: in Object_Handle;
             Location: in Interfaces.C.size_t;
             Message_Code: in Interfaces.Integer_32;
             Layer_Prefix, Message: in Interfaces.C.Strings.chars_ptr;
             User_Data: in Interfaces.C.Extensions.void_ptr)
        return Interfaces.Unsigned_32
        with Convention => C;

    type Debug_Report_Callback_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Debug_Report_Callback_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Debug_Report_Flags;
        Callback: Debug_Report_Callback_Function_C;
        User_Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Debug_Report_Callback_Create_Info_C_Access is
        access Debug_Report_Callback_Create_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C(Struct: in Debug_Report_Callback_Create_Info)
        return Debug_Report_Callback_Create_Info_C;
    procedure Free(Struct: in out Debug_Report_Callback_Create_Info_C);

    -- Extension record conversions.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    procedure Free(Next: in out C.In_Structure_C_Access);
end Vulkan.Debug_Reports_C;


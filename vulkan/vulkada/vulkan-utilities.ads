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

-- Copyright 2025 Phaser Cat Games LLC

-- Internal utilities package

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.Utilities is
    use type Ada.Containers.Count_Type;
    use type Interfaces.C.Strings.chars_ptr;

    -- Convert an array of extension properties from C to Ada.
    type C_Extension_Properties_Array is array (Positive range <>)
        of aliased C.Extension_Properties_C
        with Convention => C;

    function To_Ada(Properties: in C_Extension_Properties_Array)
        return Extension_Properties_Vectors.Vector
        with Post => Properties'Length = To_Ada'Result.Length;

    -- Convert an array of layer properties from C to Ada.
    type C_Layer_Properties_Array is array (Positive range <>)
        of aliased C.Layer_Properties_C
        with Convention => C;

    function To_Ada(Properties: in C_Layer_Properties_Array)
        return Layer_Properties_Vectors.Vector
        with Post => Properties'Length = To_Ada'Result.Length;

    -- Create and destroy arrays of C strings.
    function To_C(Strings: in String_Vectors.Vector)
        return Interfaces.C.Strings.chars_ptr_array
        with Post => Strings.Length = To_C'Result'Length;

    procedure Free(Strings: in out Interfaces.C.Strings.chars_ptr_array)
        with Post => (for all S of Strings =>
                        S = Interfaces.C.Strings.null_ptr);

    -- Convert from a C string to an unbounded string.
    function To_Unbounded_String(C_String: in Interfaces.C.Strings.chars_ptr)
        return Ada.Strings.Unbounded.Unbounded_String;

    -- Process those two-stage array request functions.
    generic
        type Arg_1(<>) is limited private;
        with package Vectors is new Ada.Containers.Vectors(<>);
        with function C_Get_Array(First: in Arg_1;
                                  Count: in out Interfaces.Unsigned_32;
                                  Items: access Vectors.Element_Type)
                                    return Result;
    function Get_Array_1(First: in Arg_1) return Vectors.Vector;

    generic
        type Arg_1(<>) is limited private;
        with package Vectors is new Ada.Containers.Vectors(<>);
        with procedure C_Get_Array(First: in Arg_1;
                                   Count: in out Interfaces.Unsigned_32;
                                   Items: access Vectors.Element_Type);
    function Get_Array_1_Proc(First: in Arg_1) return Vectors.Vector;

    generic
        type Arg_1(<>) is limited private;
        type Arg_2(<>) is limited private;
        with package Vectors is new Ada.Containers.Vectors(<>);
        with function C_Get_Array(First: in Arg_1;
                                  Second: in Arg_2;
                                  Count: in out Interfaces.Unsigned_32;
                                  Items: access Vectors.Element_Type)
                                    return Result;
    function Get_Array_2(First: in Arg_1;
                         Second: in Arg_2) return Vectors.Vector;

    generic
        type Arg_1(<>) is limited private;
        type Arg_2(<>) is limited private;
        with package Vectors is new Ada.Containers.Vectors(<>);
        with procedure C_Get_Array(First: in Arg_1;
                                   Second: in Arg_2;
                                   Count: in out Interfaces.Unsigned_32;
                                   Items: access Vectors.Element_Type);
    function Get_Array_2_Proc(First: in Arg_1;
                              Second: in Arg_2) return Vectors.Vector;

    -- Type conversions needed in multiple areas.
    function To_Ada(PDFC: in C.Physical_Device_Features_C)
        return Physical_Device_Features;

    function To_Ada(PDPC: in C.Physical_Device_Properties_C)
        return Physical_Device_Properties;

    function To_Ada(PDMPC: in C.Physical_Device_Memory_Properties_C)
        return Physical_Device_Memory_Properties;

    -- Boolean conversions.
    function To_C(Flag: in Boolean) return Interfaces.Unsigned_32
        with Inline,
             Post => To_C'Result in 0 | 1;
    function To_Ada(Flag: in Interfaces.Unsigned_32) return Boolean
        with Inline;

    -- Convert a vector to a C array.
    generic
        with package C_Array is new C_Arrays(<>);
        with package Vectors is new Ada.Containers.Vectors(Positive,
                                                           C_Array.Element,
                                                           <>);
    procedure To_C_Array(C_Count: out Interfaces.Unsigned_32;
                         Vector: in Vectors.Vector;
                         C_Pointer: out C_Array.Pointer);

    -- Convert a vector to a C array with a conversion function.
    generic
        with package C_Array is new C_Arrays(<>);
        with package Vectors is new Ada.Containers.Vectors(Positive, <>);
        with function To_C(Struct: in Vectors.Element_Type)
            return C_Array.Element is <>;
    procedure To_C_Array_Convert(C_Count: out Interfaces.Unsigned_32;
                                 Vector: in Vectors.Vector;
                                 C_Pointer: out C_Array.Pointer);

    -- The same but for indefinite vectors.
    generic
        with package C_Array is new C_Arrays(<>);
        with package Vectors is
            new Ada.Containers.Indefinite_Vectors(Positive, <>);
        with function To_C(Struct: in Vectors.Element_Type)
            return C_Array.Element is <>;
    procedure To_C_Array_Convert_Indefinite(C_Count: out Interfaces.Unsigned_32;
                                            Vector: in Vectors.Vector;
                                            C_Pointer: out C_Array.Pointer);

    -- Extension structure generics.
    generic
        type Ada_Record(<>) is new In_Structure with private;
        type C_Structure(<>) is private;
        type C_Structure_Access is access C_Structure;
        with function To_C(Struct: in Ada_Record) return C_Structure is <>;
    function Make_In_Struct(Next: in In_Structure_Access)
        return C.In_Structure_C_Access;

    generic
        type Ada_Record(<>) is new Out_Structure with private;
        type C_Structure is private;
        type C_Structure_Access is access C_Structure;
    function Make_Out_Struct(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;

    generic
        type Struct_Type(<>) is private;
        type Struct_Access is access Struct_Type;
        with procedure Free(S: in out Struct_Type) is <>;
    procedure Free_In_Struct(Next: in out C.In_Structure_C_Access);

    generic
        type Struct_Type(<>) is private;
        type Struct_Access is access Struct_Type;
    procedure Free_Out_Struct(Next: in out C.Out_Structure_C_Access);

    -- Forwarding wrappers for command procedures.
    generic
        type Input_Type(<>) is limited private;
        C_Proc: access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                                 Input: in Input_Type);
    procedure Forward(Command_Buffer: in Vulkan.Command_Buffer;
                      Input: in Input_Type)
        with Inline;

    generic
        type Input_Struct(<>) is limited private;
        type Input_Struct_C(<>) is limited private;
        with function To_C(Input: in Input_Struct) return Input_Struct_C;
        with procedure Free(Input: in out Input_Struct_C);
        C_Proc: access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                                 Input: in Input_Struct_C);
    procedure Forward_Convert(Command_Buffer: in Vulkan.Command_Buffer;
                              Input: in Input_Struct);

    generic
        C_Proc: access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                                 Flag: in Interfaces.Unsigned_32);
    procedure Forward_Boolean(Command_Buffer: in Vulkan.Command_Buffer;
                              Flag: in Boolean)
        with Inline;
end Vulkan.Utilities;


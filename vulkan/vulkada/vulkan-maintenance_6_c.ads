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

with Vulkan.C;
with Vulkan.C_Arrays;

private package Vulkan.Maintenance_6_C is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Physical_Device_Maintenance_6_Features_Type |
            Physical_Device_Maintenance_6_Properties_Type |
            Bind_Memory_Status_Type |
            Bind_Descriptor_Sets_Info_Type |
            Push_Constants_Info_Type |
            Push_Descriptor_Set_Info_Type |
            Push_Descriptor_Set_With_Template_Info_Type |
            Set_Descriptor_Buffer_Offsets_Info_Type |
            Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Physical_Device_Maintenance_6_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_6_Features_Type;
        Next: C.Out_Structure_C_Access;
        Maintenance_6: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_6_Features_C_Access is
        access Physical_Device_Maintenance_6_Features_C
        with Convention => C;

    type Physical_Device_Maintenance_6_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Maintenance_6_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Block_Texel_View_Compatible_Multiple_Layers: Interfaces.Unsigned_32;
        Max_Combined_Image_Sampler_Descriptor_Count: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Clamp_Combiner_Inputs: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Maintenance_6_Properties_C_Access is
        access Physical_Device_Maintenance_6_Properties_C
        with Convention => C;
    
    type Bind_Memory_Status_C is
    record
        Record_Type: In_Structure_Type := Bind_Memory_Status_Type;
        Next: C.In_Structure_C_Access;
        Result: Result_Access;
    end record
        with Convention => C;

    type Bind_Memory_Status_C_Access is access Bind_Memory_Status_C
        with Convention => C;

    package Descriptor_Set_Arrays is new C_Arrays(Descriptor_Set);

    type Bind_Descriptor_Sets_Info_C is
    record
        Record_Type: In_Structure_Type := Bind_Descriptor_Sets_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        First_Set: Interfaces.Unsigned_32;
        Descriptor_Set_Count: Interfaces.Unsigned_32;
        Descriptor_Sets: Descriptor_Set_Arrays.Pointer;
        Dynamic_Offset_Count: Interfaces.Unsigned_32;
        Dynamic_Offsets: C.Uint32_t_Arrays.Pointer;
    end record
        with Convention => C;

    type Bind_Descriptor_Sets_Info_C_Access is
        access Bind_Descriptor_Sets_Info_C
        with Convention => C;

    type Push_Constants_Info_C is
    record
        Record_Type: In_Structure_Type := Push_Constants_Info_Type;
        Next: C.In_Structure_C_Access;
        Layout: Pipeline_Layout;
        Stage_Flags: Shader_Stage_Flags;
        Offset: Interfaces.Unsigned_32;
        Size: Interfaces.Unsigned_32;
        Values: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Push_Constants_Info_C_Access is access Push_Constants_Info_C
        with Convention => C;

    package Write_Descriptor_Set_C_Arrays is
        new C_Arrays(C.Write_Descriptor_Set_C);

    type Push_Descriptor_Set_Info_C is
    record
        Record_Type: In_Structure_Type := Push_Descriptor_Set_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
        Descriptor_Write_Count: Interfaces.Unsigned_32;
        Descriptor_Writes: Write_Descriptor_Set_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Push_Descriptor_Set_Info_C_Access is
        access Push_Descriptor_Set_Info_C
        with Convention => C;

    type Push_Descriptor_Set_With_Template_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Push_Descriptor_Set_With_Template_Info_Type;
        Next: C.In_Structure_C_Access;
        Descriptor_Update_Template: Vulkan.Descriptor_Update_Template;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
        Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Push_Descriptor_Set_With_Template_Info_C_Access is
        access Push_Descriptor_Set_With_Template_Info_C
        with Convention => C;

    package Device_Size_Arrays is new C_Arrays(Device_Size);

    type Set_Descriptor_Buffer_Offsets_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Set_Descriptor_Buffer_Offsets_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        First_Set: Interfaces.Unsigned_32;
        Set_Count: Interfaces.Unsigned_32;
        Buffer_Indices: C.Uint32_t_Arrays.Pointer;
        Offsets: Device_Size_Arrays.Pointer;
    end record
        with Convention => C;

    type Set_Descriptor_Buffer_Offsets_Info_C_Access is
        access Set_Descriptor_Buffer_Offsets_Info_C
        with Convention => C;

    type Bind_Descriptor_Buffer_Embedded_Samplers_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type;
        Next: C.In_Structure_C_Access;
        Stage_Flags: Shader_Stage_Flags;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Bind_Descriptor_Buffer_Embedded_Samplers_Info_C_Access is
        access Bind_Descriptor_Buffer_Embedded_Samplers_Info_C
        with Convention => C;
        
    -- Conversion subprograms.
    procedure To_Ada(Ada_Struct: in out Physical_Device_Maintenance_6_Features;
                     C_Struct: in Physical_Device_Maintenance_6_Features_C);

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_6_Properties;
         C_Struct: in Physical_Device_Maintenance_6_Properties_C);

    function To_C(Struct: in Bind_Memory_Status) return Bind_Memory_Status_C;
    procedure Free(Struct: in out Bind_Memory_Status_C);

    function To_C(Struct: in Bind_Descriptor_Sets_Info)
        return Bind_Descriptor_Sets_Info_C;
    procedure Free(Struct: in out Bind_Descriptor_Sets_Info_C);

    function To_C(Struct: in Push_Constants_Info) return Push_Constants_Info_C;
    procedure Free(Struct: in out Push_Constants_Info_C);

    function To_C(Struct: in Push_Descriptor_Set_Info)
        return Push_Descriptor_Set_Info_C;
    procedure Free(Struct: in out Push_Descriptor_Set_Info_C);

    function To_C(Struct: in Push_Descriptor_Set_With_Template_Info)
        return Push_Descriptor_Set_With_Template_Info_C;
    procedure Free(Struct: in out Push_Descriptor_Set_With_Template_Info_C);

    function To_C(Struct: in Set_Descriptor_Buffer_Offsets_Info)
        return Set_Descriptor_Buffer_Offsets_Info_C;
    procedure Free(Struct: in out Set_Descriptor_Buffer_Offsets_Info_C);

    function To_C(Struct: in Bind_Descriptor_Buffer_Embedded_Samplers_Info)
        return Bind_Descriptor_Buffer_Embedded_Samplers_Info_C;
    procedure Free
        (Struct: in out Bind_Descriptor_Buffer_Embedded_Samplers_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.Maintenance_6_C;


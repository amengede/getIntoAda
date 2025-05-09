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

-- Core Vulkan subprograms

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with Vulkan.Utilities;
with Vulkan.Exceptions;
with Vulkan.C;
with Vulkan.C_V1_1;
with Vulkan.C_V1_2;
with Vulkan.C_V1_3;
with Vulkan.C_V1_4;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Vulkan.Core is
    -- Common creation function.
    function Create(Create_Info: in Instance_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Instance: out Vulkan.Instance) return Result;

    -- Common code for Enumerate_Instance_Extension_Properties.
    function Enumerate_Instance_Extension_Properties
        (Layer_Name: in Interfaces.C.Strings.char_array_access)
        return Extension_Properties_Vectors.Vector;

    function Create(Create_Info: in Instance_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Instance: out Vulkan.Instance)
        return Result is
    begin
        return Create(Create_Info, Allocator'Access, Instance);
    end Create;

    function Create(Create_Info: in Instance_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Instance is
        Instance: Vulkan.Instance;
    begin
        Exceptions.Check(Create(Create_Info, Allocator'Access, Instance));

        return Instance;
    end Create;

    function Create(Create_Info: in Instance_Create_Info;
                    Instance: out Vulkan.Instance) return Result is
    begin
        return Create(Create_Info, null, Instance);
    end Create;

    function Create(Create_Info: in Instance_Create_Info) return Instance is
        Instance: Vulkan.Instance;
    begin
        Exceptions.Check(Create(Create_Info, null, Instance));

        return Instance;
    end Create;

    procedure Destroy(Instance: in out Vulkan.Instance;
                      Allocator: aliased in Allocation_Callbacks) is
    begin
        C.vkDestroyInstance(Instance, Allocator'Access);
        Instance := No_Instance;
    end Destroy;

    procedure Destroy(Instance: in out Vulkan.Instance) is
    begin
        C.vkDestroyInstance(Instance, null);
        Instance := No_Instance;
    end Destroy;

    function Get_Proc_Addr(Instance: in Vulkan.Instance;
                           Name: in String) return Proc is
        function To_Proc is
            new Ada.Unchecked_Conversion(C.Void_Function, Proc);

        Raw_Proc: C.Void_Function;
        C_Name: aliased Interfaces.C.char_array := Interfaces.C.To_C(Name);
    begin
        Raw_Proc := C.vkGetInstanceProcAddr(Instance, C_Name'Unchecked_Access);

        --  Put_Line ("Loaded " & Name & ", got " & To_Proc (Raw_Proc)'Image);
        return To_Proc(Raw_Proc);
    end Get_Proc_Addr;

    function Enumerate_Instance_Extension_Properties(Layer_Name: in String)
        return Extension_Properties_Vectors.Vector is
        C_Layer_Name: aliased Interfaces.C.char_array :=
            Interfaces.C.To_C(Layer_Name);
    begin
        return Enumerate_Instance_Extension_Properties
                (C_Layer_Name'Unchecked_Access);
    end Enumerate_Instance_Extension_Properties;

    function Enumerate_Instance_Extension_Properties
        return Extension_Properties_Vectors.Vector is
    begin
        return Enumerate_Instance_Extension_Properties(null);
    end Enumerate_Instance_Extension_Properties;

    function Enumerate_Instance_Layer_Properties
        return Layer_Properties_Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
    begin
        if C.vkEnumerateInstanceLayerProperties(Count, null) /= Success then
            return Layer_Properties_Vectors.Empty_Vector;
        end if;

        if Count = 0 then
            return Layer_Properties_Vectors.Empty_Vector;
        end if;

        declare
            Properties: Utilities.C_Layer_Properties_Array
                        (1 .. Positive(Count));
        begin
            if C.vkEnumerateInstanceLayerProperties
                (Count, Properties(1)'Access) /= Success then
                return Layer_Properties_Vectors.Empty_Vector;
            end if;

            return Utilities.To_Ada(Properties);
        end;
    end Enumerate_Instance_Layer_Properties;  

    function Enumerate_Instance_Version return Version_Number is
        use type C_V1_1.vkEnumerateInstanceVersion_Access;

        Version: Version_Number;
    begin
        if C_V1_1.vkEnumerateInstanceVersion = null then
            return API_Version_1_0;
        end if;

        C_V1_1.vkEnumerateInstanceVersion(Version);

        return Version;
    end Enumerate_Instance_Version;

    function Create(Create_Info: in Instance_Create_Info;
                    Allocator: access constant Allocation_Callbacks;
                    Instance: out Vulkan.Instance) return Result is
        Create_Info_C: C.Instance_Create_Info_C := C.To_C(Create_Info);
        Layer_Names: Interfaces.C.Strings.chars_ptr_array :=
            Utilities.To_C(Create_Info.Enabled_Layer_Names);
        Extension_Names: Interfaces.C.Strings.chars_ptr_array :=
            Utilities.To_C(Create_Info.Enabled_Extension_Names);
        Result: Vulkan.Result;
    begin
        if Layer_Names'Length > 0 then
            Create_Info_C.Enabled_Layer_Names :=
                Layer_Names(Layer_Names'First)'Unchecked_Access;
        end if;

        if Extension_Names'Length > 0 then
            Create_Info_C.Enabled_Extension_Names :=
                Extension_Names(Extension_Names'First)'Unchecked_Access;
        end if;

        Result := C.vkCreateInstance(Create_Info_C,
                                     Allocator,
                                     Instance);
        Utilities.Free(Extension_Names);
        Utilities.Free(Layer_Names);
        C.Free(Create_Info_C);

        if Result = Vulkan.Success and Create_Info.Application_Info /= null then
            if Create_Info.Application_Info.API_Version >= API_Version_1_4 then
                goto Load_1_4;
            end if;

            if Create_Info.Application_Info.API_Version >= API_Version_1_3 then
                goto Load_1_3;
            end if;

            if Create_Info.Application_Info.API_Version >= API_Version_1_2 then
                goto Load_1_2;
            end if;

            if Create_Info.Application_Info.API_Version >= API_Version_1_1 then
                goto Load_1_1;
            end if;

            return Vulkan.Success;

            <<Load_1_4>>
            C_V1_4.Load(Instance);
            <<Load_1_3>>
            C_V1_3.Load(Instance);
            <<Load_1_2>>
            C_V1_2.Load(Instance);
            <<Load_1_1>>
            C_V1_1.Load(Instance);
        end if;

        return Result;
    end Create;

    function Enumerate_Instance_Extension_Properties
        (Layer_Name: in Interfaces.C.Strings.char_array_access)
        return Extension_Properties_Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
    begin
        if C.vkEnumerateInstanceExtensionProperties
            (Layer_Name, Count, null) /= Success then
            return Extension_Properties_Vectors.Empty_Vector;
        end if;

        if Count = 0 then
            return Extension_Properties_Vectors.Empty_Vector;
        end if;

        declare
            Properties: Utilities.C_Extension_Properties_Array
                (1 .. Positive(Count));
        begin
            if C.vkEnumerateInstanceExtensionProperties
                (Layer_Name, Count, Properties(1)'Access) /= Success then
                return Extension_Properties_Vectors.Empty_Vector;
            end if;

            return Utilities.To_Ada(Properties);
        end;
    end Enumerate_Instance_Extension_Properties;
end Vulkan.Core;


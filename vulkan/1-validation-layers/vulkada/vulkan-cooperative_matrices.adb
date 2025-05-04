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

-- Operations for the cooperative matrix extension

with Vulkan.Cooperative_Matrices_C;
with Vulkan.Core;
with Vulkan.Exceptions;
with Vulkan.Extension_Records;

package body Vulkan.Cooperative_Matrices is
    -- Loaded extension functions.
    type vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Property_Count: in out Interfaces.Unsigned_32;
             Properties:
                access Cooperative_Matrices_C.Cooperative_Matrix_Properties_C)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR:
        vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR_Access);
    begin
        Load(vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR,
             "vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR");
    end Load_Extension;
    
    function Physical_Device_Cooperative_Matrix_Properties_Count
        (Physical_Device: in Vulkan.Physical_Device)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR
            (Physical_Device, Count, null));

        return Count;
    end Physical_Device_Cooperative_Matrix_Properties_Count;

    function Get_Physical_Device_Cooperative_Matrix_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Properties: in out Cooperative_Matrix_Properties_Vectors.Vector)
        return Result is
    begin
        if Properties.Is_Empty then
            return Success;
        end if;

        declare
            Count: Interfaces.Unsigned_32 :=
                Interfaces.Unsigned_32(Properties.Length);
            C_Properties: array (1 .. Positive(Count)) of aliased
                Cooperative_Matrices_C.Cooperative_Matrix_Properties_C
                with Convention => C;
            Result: Vulkan.Result;
        begin
            for X in C_Properties'Range loop
                C_Properties(X).Next :=
                    Extension_Records.To_C(Properties(X).Next);
            end loop;

            Result := vkGetPhysicalDeviceCooperativeMatrixPropertiesKHR
                        (Physical_Device, Count, C_Properties(1)'Access);

            for X in C_Properties'Range loop
                Cooperative_Matrices_C.To_Ada(Properties(X), C_Properties(X));
                Extension_Records.Free(C_Properties(X).Next);
            end loop;

            return Result;
        end;
    end Get_Physical_Device_Cooperative_Matrix_Properties;

    function Get_Physical_Device_Cooperative_Matrix_Properties
        (Physical_Device: in Vulkan.Physical_Device)
        return Cooperative_Matrix_Properties_Vectors.Vector is
        Properties: Cooperative_Matrix_Properties_Vectors.Vector;
        Property: Cooperative_Matrix_Properties;
    begin
        Properties.Append
            (Property, Ada.Containers.Count_Type
                (Physical_Device_Cooperative_Matrix_Properties_Count
                    (Physical_Device)));
        Exceptions.Check(Get_Physical_Device_Cooperative_Matrix_Properties
            (Physical_Device, Properties));

        return Properties;
    end Get_Physical_Device_Cooperative_Matrix_Properties;
end Vulkan.Cooperative_Matrices;


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

-- Operations for the fragment shading rate extension

with Vulkan.C_KHR;
with Vulkan.Core;
with Vulkan.Extension_Records;
with Vulkan.Exceptions;

package body Vulkan.Extensions.KHR_Fragment_Shading_Rate is
    -- Loaded extension functions.
    type vkGetPhysicalDeviceFragmentShadingRatesKHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Fragment_Shading_Rate_Count: in out Interfaces.Unsigned_32;
             Fragment_Shading_Rates:
                access C_KHR.Physical_Device_Fragment_Shading_Rate_C)
        return Result
        with Convention => C;

    vkGetPhysicalDeviceFragmentShadingRatesKHR:
        vkGetPhysicalDeviceFragmentShadingRatesKHR_Access;

    type vkCmdSetFragmentShadingRateKHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Fragment_Size: in Extent_2D;
             Combiner_Ops: KHR.Fragment_Shading_Rate_Combiner_Op_Array)
        with Convention => C;

    vkCmdSetFragmentShadingRateKHR: vkCmdSetFragmentShadingRateKHR_Access;

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
            (vkGetPhysicalDeviceFragmentShadingRatesKHR_Access);
        procedure Load is new Load_Pointer
            (vkCmdSetFragmentShadingRateKHR_Access);
    begin
        Load(vkGetPhysicalDeviceFragmentShadingRatesKHR,
             "vkGetPhysicalDeviceFragmentShadingRatesKHR");
        Load(vkCmdSetFragmentShadingRateKHR, "vkCmdSetFragmentShadingRateKHR");
    end Load_Extension;

    function Fragment_Shading_Rates_Count
        (Physical_Device: in Vulkan.Physical_Device)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check
            (vkGetPhysicalDeviceFragmentShadingRatesKHR(Physical_Device,
                                                        Count,
                                                        null));

        return Count;
    end Fragment_Shading_Rates_Count;

    function Get_Fragment_Shading_Rates
        (Physical_Device: in Vulkan.Physical_Device;
         Fragment_Shading_Rates:
            in out KHR.Physical_Device_Fragment_Shading_Rate_Vectors.Vector)
        return Result is
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Fragment_Shading_Rates.Length);
        C_Rates: array (1 .. Positive(Count)) of aliased
            C_KHR.Physical_Device_Fragment_Shading_Rate_C;
        Result: Vulkan.Result;
    begin
        for X in C_Rates'Range loop
            C_Rates(X).Next :=
                Extension_Records.To_C(Fragment_Shading_Rates(X).Next);
        end loop;

        Result := vkGetPhysicalDeviceFragmentShadingRatesKHR(Physical_Device,
                                                             Count,
                                                             C_Rates(1)'Access);

        for X in C_Rates'Range loop
            C_KHR.To_Ada(Fragment_Shading_Rates(X), C_Rates(X));
            Extension_Records.Free(C_Rates(X).Next);
        end loop;

        return Result;
    end Get_Fragment_Shading_Rates;
    
    function Get_Fragment_Shading_Rates
        (Physical_Device: in Vulkan.Physical_Device)
        return KHR.Physical_Device_Fragment_Shading_Rate_Vectors.Vector is
        Rates: KHR.Physical_Device_Fragment_Shading_Rate_Vectors.Vector;
        Rate: KHR.Physical_Device_Fragment_Shading_Rate;
        Count: Interfaces.Unsigned_32 :=
            Fragment_Shading_Rates_Count(Physical_Device);
    begin
        Rates.Append(Rate, Ada.Containers.Count_Type(Count));
        Exceptions.Check(Get_Fragment_Shading_Rates(Physical_Device, Rates));

        return Rates;
    end Get_Fragment_Shading_Rates;

    procedure Set_Fragment_Shading_Rate
        (Command_Buffer: in Vulkan.Command_Buffer;
         Fragment_Size: in Extent_2D;
         Combiner_Ops: in KHR.Fragment_Shading_Rate_Combiner_Op_Array) is
    begin
        vkCmdSetFragmentShadingRateKHR(Command_Buffer,
                                       Fragment_Size,
                                       Combiner_Ops);
    end Set_Fragment_Shading_Rate;
end Vulkan.Extensions.KHR_Fragment_Shading_Rate;


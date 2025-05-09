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

-- Operations for the calibrated timestamps extension

with Vulkan.Core;
with Vulkan.C_KHR;
with Vulkan.Exceptions;

package body Vulkan.Extensions.KHR_Calibrated_Timestamps is
    -- Loaded extension functions.
    type vkGetPhysicalDeviceCalibrateableTimeDomainsKHR_Access is
        access function(Physical_Device: in Vulkan.Physical_Device;
                        Time_Domain_Count: in out Interfaces.Unsigned_32;
                        Time_Domains: access KHR.Time_Domain) return Result
        with Convention => C;

    vkGetPhysicalDeviceCalibrateableTimeDomainsKHR:
        vkGetPhysicalDeviceCalibrateableTimeDomainsKHR_Access;

    type vkGetCalibratedTimestampsKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Timestamp_Count: in Interfaces.Unsigned_32;
             Timestamp_Infos: access C_KHR.Calibrated_Timestamp_Info_C;
             Timestamps: access Interfaces.Unsigned_64;
             Max_Deviation: out Interfaces.Unsigned_64) return Result
        with Convention => C;

    vkGetCalibratedTimestampsKHR: vkGetCalibratedTimestampsKHR_Access;

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
            (vkGetPhysicalDeviceCalibrateableTimeDomainsKHR_Access);
        procedure Load is new Load_Pointer(vkGetCalibratedTimestampsKHR_Access);
    begin
        Load(vkGetPhysicalDeviceCalibrateableTimeDomainsKHR,
             "vkGetPhysicalDeviceCalibrateableTimeDomainsKHR");
        Load(vkGetCalibratedTimestampsKHR, "vkGetCalibratedTimestampsKHR");
    end Load_Extension;
    
    function Physical_Device_Calibrateable_Time_Domains_Count
        (Physical_Device: in Vulkan.Physical_Device)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check
            (vkGetPhysicalDeviceCalibrateableTimeDomainsKHR
                (Physical_Device, Count, null));

        return Count;
    end Physical_Device_Calibrateable_Time_Domains_Count;
    
    function Get_Physical_Device_Calibrateable_Time_Domains
        (Physical_Device: in Vulkan.Physical_Device;
         Time_Domains: out KHR.Time_Domain_Vectors.Vector) return Result is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 :=
            Physical_Device_Calibrateable_Time_Domains_Count(Physical_Device);
    begin
        Time_Domains.Clear;

        if Count = 0 then
            return Success;
        end if;

        declare
            Values: array (1 .. Positive(Count)) of aliased KHR.Time_Domain
                with Convention => C;
            Result: Vulkan.Result;
        begin
            Result := vkGetPhysicalDeviceCalibrateableTimeDomainsKHR
                (Physical_Device, Count, Values(1)'Access);

            if Result = Success then
                for TD of Values loop
                    Time_Domains.Append(TD);
                end loop;
            end if;

            return Result;
        end;
    end Get_Physical_Device_Calibrateable_Time_Domains;

    function Get_Physical_Device_Calibrateable_Time_Domains
        (Physical_Device: in Vulkan.Physical_Device)
        return KHR.Time_Domain_Vectors.Vector is
        Values: KHR.Time_Domain_Vectors.Vector;
    begin
        Exceptions.Check
            (Get_Physical_Device_Calibrateable_Time_Domains
                (Physical_Device, Values));

        return Values;
    end Get_Physical_Device_Calibrateable_Time_Domains;
    
    function Get_Calibrated_Timestamps
        (Device: in Vulkan.Device;
         Timestamp_Infos: in KHR.Calibrated_Timestamp_Info_Vectors.Vector;
         Timestamps: out KHR.Unsigned_64_Vectors.Vector;
         Max_Deviation: out Interfaces.Unsigned_64) return Result is
        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Timestamp_Infos.Length);
        C_Infos: array (1 .. Positive(Count)) of aliased
            C_KHR.Calibrated_Timestamp_Info_C
            with Convention => C;
        Timestamp_Values: array (1 .. Positive(Count)) of aliased
            Interfaces.Unsigned_64
            with Convention => C;
        Result: Vulkan.Result;
    begin
        Timestamps.Clear;

        for X in C_Infos'Range loop
            C_Infos(X) := C_KHR.To_C(Timestamp_Infos(X));
        end loop;

        Result := vkGetCalibratedTimestampsKHR(Device,
                                               Count,
                                               C_Infos(1)'Access,
                                               Timestamp_Values(1)'Access,
                                               Max_Deviation);

        if Result = Success then
            for TV of Timestamp_Values loop
                Timestamps.Append(TV);
            end loop;
        end if;

        for Info of C_Infos loop
            C_KHR.Free(Info);
        end loop;

        return Result;
    end Get_Calibrated_Timestamps;
end Vulkan.Extensions.KHR_Calibrated_Timestamps;


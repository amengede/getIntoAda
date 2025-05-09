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

-- Operations for the display timing extension

with Vulkan.Core;
with Vulkan.Exceptions;

package body Vulkan.Extensions.GOOGLE_Display_Timing is
    -- Loaded extension functions.
    type vkGetRefreshCycleDurationGOOGLE_Access is
        access function
            (Device: in Vulkan.Device;
             Swapchain: in KHR.Swapchain;
             Display_Timing_Properties: out GOOGLE.Refresh_Cycle_Duration)
        return Result
        with Convention => C;

    vkGetRefreshCycleDurationGOOGLE: vkGetRefreshCycleDurationGOOGLE_Access;

    type vkGetPastPresentationTimingGOOGLE_Access is
        access function
            (Device: in Vulkan.Device;
             Swapchain: in KHR.Swapchain;
             Presentation_Timing_Count: in out Interfaces.Unsigned_32;
             Presentation_Timings: access GOOGLE.Past_Presentation_Timing)
        return Result
        with Convention => C;

    vkGetPastPresentationTimingGOOGLE: vkGetPastPresentationTimingGOOGLE_Access;

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
            (vkGetRefreshCycleDurationGOOGLE_Access);
        procedure Load is new Load_Pointer
            (vkGetPastPresentationTimingGOOGLE_Access);
    begin
        Load(vkGetRefreshCycleDurationGOOGLE,
             "vkGetRefreshCycleDurationGOOGLE");
        Load(vkGetPastPresentationTimingGOOGLE,
             "vkGetPastPresentationTimingGOOGLE");
    end Load_Extension;
    
    function Get_Refresh_Cycle_Duration
        (Device: in Vulkan.Device;
         Swapchain: in KHR.Swapchain;
         Display_Timing_Properties: out GOOGLE.Refresh_Cycle_Duration)
        return Result is
    begin
        return vkGetRefreshCycleDurationGOOGLE(Device,
                                               Swapchain,
                                               Display_Timing_Properties);
    end Get_Refresh_Cycle_Duration;
    
    function Get_Refresh_Cycle_Duration(Device: in Vulkan.Device;
                                        Swapchain: in KHR.Swapchain)
         return GOOGLE.Refresh_Cycle_Duration is
        Duration: GOOGLE.Refresh_Cycle_Duration;
    begin
        Exceptions.Check(Get_Refresh_Cycle_Duration(Device, 
                                                    Swapchain,
                                                    Duration));

        return Duration;
    end Get_Refresh_Cycle_Duration;

    function Get_Past_Presentation_Timing_Count(Device: in Vulkan.Device;
                                                Swapchain: in KHR.Swapchain)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(vkGetPastPresentationTimingGOOGLE(Device,
                                                           Swapchain,
                                                           Count,
                                                           null));

        return Count;
    end Get_Past_Presentation_Timing_Count;

    function Get_Past_Presentation_Timing
        (Device: in Vulkan.Device;
         Swapchain: in KHR.Swapchain;
         Presentation_Timings:
            in out GOOGLE.Past_Presentation_Timing_Vectors.Vector)
        return Result is
        Timing: array (1 .. Positive(Presentation_Timings.Length))
                    of aliased GOOGLE.Past_Presentation_Timing;
        Count: Interfaces.Unsigned_32 := Timing'Length;
        Result: Vulkan.Result;
    begin
        for X in Timing'Range loop
            Timing(X) := Presentation_Timings(X);
        end loop;

        Result := vkGetPastPresentationTimingGOOGLE(Device,
                                                    Swapchain,
                                                    Count,
                                                    Timing(1)'Access);

        if Result not in Error_Result then
            for X in Timing'Range loop
                Presentation_Timings(X) := Timing(X);
            end loop;
        end if;

        return Result;
    end Get_Past_Presentation_Timing;
    
    function Get_Past_Presentation_Timing(Device: in Vulkan.Device;
                                          Swapchain: in KHR.Swapchain)
        return GOOGLE.Past_Presentation_Timing_Vectors.Vector is
        Count: Interfaces.Unsigned_32 := Get_Past_Presentation_Timing_Count
                (Device, Swapchain);
        Timings: GOOGLE.Past_Presentation_Timing_Vectors.Vector;
    begin
        Timings.Set_Length(Ada.Containers.Count_Type(Count));
        Exceptions.Check(Get_Past_Presentation_Timing(Device,
                                                      Swapchain,
                                                      Timings));

        return Timings;
    end Get_Past_Presentation_Timing;
end Vulkan.Extensions.GOOGLE_Display_Timing;


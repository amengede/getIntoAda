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

-- Semaphore related subprogram

with Vulkan.C_V1_2;
with Vulkan.Exceptions;

package body Vulkan.Semaphores is
    function Get_Counter_Value(Device: in Vulkan.Device;
                               Semaphore: in Vulkan.Semaphore;
                               Value: out Semaphore_Value) return Result is
    begin
        return C_V1_2.vkGetSemaphoreCounterValue(Device, Semaphore, Value);
    end Get_Counter_Value;

    function Get_Counter_Value(Device: in Vulkan.Device;
                               Semaphore: in Vulkan.Semaphore)
        return Semaphore_Value is
        Value: Semaphore_Value;
    begin
        Exceptions.Check(Get_Counter_Value(Device, Semaphore, Value));

        return Value;
    end Get_Counter_Value;
    
    function Wait(Device: in Vulkan.Device;
                  Wait_Info: in Semaphore_Wait_Info;
                  Timeout: in Interfaces.Unsigned_64) return Result is
        Wait_Info_C: C_V1_2.Semaphore_Wait_Info_C := C_V1_2.To_C(Wait_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_2.vkWaitSemaphores(Device, Wait_Info_C, Timeout);
        C_V1_2.Free(Wait_Info_C);

        return Result;
    end Wait;

    function Signal(Device: in Vulkan.Device;
                    Signal_Info: in Semaphore_Signal_Info) return Result is
        Signal_Info_C: C_V1_2.Semaphore_Signal_Info_C :=
            C_V1_2.To_C(Signal_Info);
        Result: Vulkan.Result;
    begin
        Result := C_V1_2.vkSignalSemaphore(Device, Signal_Info_C);
        C_V1_2.Free(Signal_Info_C);

        return Result;
    end Signal;

    procedure Signal(Device: in Vulkan.Device;
                     Signal_Info: in Semaphore_Signal_Info) is
    begin
        Exceptions.Check(Signal(Device, Signal_Info));
    end Signal;
end Vulkan.Semaphores;


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

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Semaphores is
    use type Ada.Containers.Count_Type;

    -- vkCreateSemaphore
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Semaphore_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Semaphore: out Vulkan.Semaphore) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Semaphore /= No_Semaphore);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Semaphore_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Semaphore
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Semaphore;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Semaphore_Create_Info;
                    Semaphore: out Vulkan.Semaphore) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Semaphore /= No_Semaphore);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Semaphore_Create_Info)
        return Semaphore
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Semaphore;

    -- vkDestroySemaphore
    procedure Destroy(Device: in Vulkan.Device;
                      Semaphore: in out Vulkan.Semaphore;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Semaphore = No_Semaphore;

    procedure Destroy(Device: in Vulkan.Device;
                      Semaphore: in out Vulkan.Semaphore)
        with Inline,
             Pre => Device /= No_Device,
             Post => Semaphore = No_Semaphore;

    -- Vulkan 1.2
    -- vkGetSemaphoreCounterValue
    function Get_Counter_Value(Device: in Vulkan.Device;
                               Semaphore: in Vulkan.Semaphore;
                               Value: out Semaphore_Value) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Semaphore /= No_Semaphore,
             Post => Get_Counter_Value'Result in Success |
                                                 Out_Of_Host_Memory |
                                                 Out_Of_Device_Memory |
                                                 Device_Lost;

    function Get_Counter_Value(Device: in Vulkan.Device;
                               Semaphore: in Vulkan.Semaphore)
        return Semaphore_Value
        with Inline,
             Pre => Device /= No_Device and
                    Semaphore /= No_Semaphore;

    -- vkWaitSemaphores
    function Wait(Device: in Vulkan.Device;
                  Wait_Info: in Semaphore_Wait_Info;
                  Timeout: in Interfaces.Unsigned_64) return Result
        with Pre => Device /= No_Device and
                    Wait_Info.Semaphores.Length = Wait_Info.Values.Length and
                    not Wait_Info.Semaphores.Is_Empty and
                    (for all Semaphore of Wait_Info.Semaphores =>
                        Semaphore /= No_Semaphore),
             Post => Wait'Result in Success |
                                    Vulkan.Timeout |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Device_Lost;

    -- vkSignalSemaphore
    function Signal(Device: in Vulkan.Device;
                    Signal_Info: in Semaphore_Signal_Info) return Result
        with Pre => Device /= No_Device and
                    Signal_Info.Semaphore /= No_Semaphore,
             Post => Signal'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory;

    procedure Signal(Device: in Vulkan.Device;
                     Signal_Info: in Semaphore_Signal_Info)
        with Inline,
             Pre => Device /= No_Device and
                    Signal_Info.Semaphore /= No_Semaphore;

private
    package Semaphores_Common is
        new Objects_Common(Semaphore_Create_Info,
                           C.Semaphore_Create_Info_C,
                           Semaphore,
                           No_Semaphore,
                           C.To_C,
                           C.Free,
                           C.vkCreateSemaphore,
                           C.vkDestroySemaphore);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Semaphore_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Semaphore: out Vulkan.Semaphore) return Result
        renames Semaphores_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Semaphore_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Semaphore
        renames Semaphores_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Semaphore_Create_Info;
                    Semaphore: out Vulkan.Semaphore) return Result
        renames Semaphores_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Semaphore_Create_Info)
        return Semaphore
        renames Semaphores_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Semaphore: in out Vulkan.Semaphore;
                      Allocator: aliased in Allocation_Callbacks)
        renames Semaphores_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Semaphore: in out Vulkan.Semaphore)
        renames Semaphores_Common.Destroy;
end Vulkan.Semaphores;


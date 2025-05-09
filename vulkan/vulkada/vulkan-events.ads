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

-- Event related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Events is
    -- vkCreateEvent
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Event_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Event: out Vulkan.Event) return Result
        with Inline,
             Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Event /= No_Event);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Event_Create_Info;
                    Allocator: aliased in Allocation_Callbacks) return Event
        with Inline,
             Pre => Device /= No_Device,
             Post => Create'Result /= No_Event;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Event_Create_Info;
                    Event: out Vulkan.Event) return Result
        with Inline,
             Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Event /= No_Event);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Event_Create_Info) return Event
        with Inline,
             Pre => Device /= No_Device,
             Post => Create'Result /= No_Event;

    -- vkDestroyEvent
    procedure Destroy(Device: in Vulkan.Device;
                      Event: in out Vulkan.Event;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Event = No_Event;

    procedure Destroy(Device: in Vulkan.Device; Event: in out Vulkan.Event)
        with Inline,
             Pre => Device /= No_Device,
             Post => Event = No_Event;

    -- vkGetEventStatus
    function Get_Status(Device: in Vulkan.Device;
                        Event: in Vulkan.Event) return Result
        with Pre => Device /= No_Device and
                    Event /= No_Event,
             Post => Get_Status'Result in Event_Set |
                                          Event_Reset |
                                          Out_Of_Host_Memory |
                                          Out_Of_Device_Memory |
                                          Device_Lost;

    -- vkSetEvent
    function Set(Device: in Vulkan.Device; Event: in Vulkan.Event) return Result
        with Pre => Device /= No_Device and
                    Event /= No_Event,
             Post => Set'Result in Success |
                                   Out_Of_Host_Memory |
                                   Out_Of_Device_Memory;

    procedure Set(Device: in Vulkan.Device; Event: in Vulkan.Event)
        with Inline,
             Pre => Device /= No_Device and
                    Event /= No_Event;

    -- vkResetEvent
    function Reset(Device: in Vulkan.Device;
                   Event: in Vulkan.Event) return Result
        with Pre => Device /= No_Device and
                    Event /= No_Event,
             Post => Reset'Result in Success |
                                     Out_Of_Device_Memory;

    procedure Reset(Device: in Vulkan.Device; Event: in Vulkan.Event)
        with Inline,
             Pre => Device /= No_Device and
                    Event /= No_Event;

private
    package Events_Common is
        new Objects_Common(Event_Create_Info,
                           C.Event_Create_Info_C,
                           Event,
                           No_Event,
                           C.To_C,
                           C.Free,
                           C.vkCreateEvent,
                           C.vkDestroyEvent);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Event_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Event: out Vulkan.Event) return Result
        renames Events_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Event_Create_Info;
                    Allocator: aliased in Allocation_Callbacks) return Event
        renames Events_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Event_Create_Info;
                    Event: out Vulkan.Event) return Result
        renames Events_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Event_Create_Info) return Event
        renames Events_Common.Create;

    procedure Destroy(Device: in Vulkan.Device; Event: in out Vulkan.Event;
                      Allocator: aliased in Allocation_Callbacks)
        renames Events_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device; Event: in out Vulkan.Event)
        renames Events_Common.Destroy;

    function Get_Status(Device: in Vulkan.Device;
                        Event: in Vulkan.Event) return Result
        renames C.vkGetEventStatus;
    
    function Set(Device: in Vulkan.Device; Event: in Vulkan.Event) return Result
        renames C.vkSetEvent;
    
    function Reset(Device: in Vulkan.Device;
                   Event: in Vulkan.Event) return Result
        renames C.vkResetEvent;
end Vulkan.Events;


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

with Vulkan.Exceptions;

package body Vulkan.Events is
    procedure Set(Device: in Vulkan.Device; Event: in Vulkan.Event) is
    begin
        Exceptions.Check(Set(Device, Event));
    end Set;
    
    procedure Reset(Device: in Vulkan.Device; Event: in Vulkan.Event) is
    begin
        Exceptions.Check(Reset(Device, Event));
    end Reset;
end Vulkan.Events;


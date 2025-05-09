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

-- Private data slot subprograms

with Vulkan.Exceptions;

package body Vulkan.Private_Data_Slots is
    package body Data_Access is
        function Set(Device: in Vulkan.Device;
                     Object_Handle: in Traits.Object;
                     Private_Data_Slot: in Vulkan.Private_Data_Slot;
                     Data: in Interfaces.Unsigned_64) return Result is
        begin
            return C_V1_3.vkSetPrivateData(Device,
                                           Traits.Object_Type,
                                           Vulkan.Object_Handle(Object_Handle),
                                           Private_Data_Slot,
                                           Data);
        end Set;

        procedure Set(Device: in Vulkan.Device;
                      Object_Handle: in Traits.Object;
                      Private_Data_Slot: in Vulkan.Private_Data_Slot;
                      Data: in Interfaces.Unsigned_64) is
        begin
            Exceptions.Check(Set(Device,
                                 Object_Handle,
                                 Private_Data_Slot,
                                 Data));
        end Set;
        
        function Get(Device: in Vulkan.Device;
                     Object_Handle: in Traits.Object;
                     Private_Data_Slot: in Vulkan.Private_Data_Slot)
            return Interfaces.Unsigned_64 is
            Data: Interfaces.Unsigned_64;
        begin
            C_V1_3.vkGetPrivateData(Device,
                                    Traits.Object_Type,
                                    Vulkan.Object_Handle(Object_Handle),
                                    Private_Data_Slot,
                                    Data);

            return Data;
        end Get;
    end Data_Access;
end Vulkan.Private_Data_Slots;


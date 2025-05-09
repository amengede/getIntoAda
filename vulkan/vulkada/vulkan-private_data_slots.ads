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

with Vulkan.Object_Traits;

private with Vulkan.Objects_Common_Access;
private with Vulkan.C_V1_3;

package Vulkan.Private_Data_Slots is
    -- Vulkan 1.3
    -- vkCreatePrivateDataSlot
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Private_Data_Slot_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Private_Data_Slot: out Vulkan.Private_Data_Slot)
        return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory and
                     (if Create'Result = Success then
                        Private_Data_Slot /= No_Private_Data_Slot);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Private_Data_Slot_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Private_Data_Slot
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Private_Data_Slot;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Private_Data_Slot_Create_Info;
                    Private_Data_Slot: out Vulkan.Private_Data_Slot)
        return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory and
                     (if Create'Result = Success then
                        Private_Data_Slot /= No_Private_Data_Slot);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Private_Data_Slot_Create_Info)
        return Private_Data_Slot
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Private_Data_Slot;

    -- vkDestroyPrivateDataSlot
    procedure Destroy(Device: in Vulkan.Device;
                      Private_Data_Slot: in out Vulkan.Private_Data_Slot;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Private_Data_Slot = No_Private_Data_Slot;

    procedure Destroy(Device: in Vulkan.Device;
                      Private_Data_Slot: in out Vulkan.Private_Data_Slot)
        with Inline,
             Pre => Device /= No_Device,
             Post => Private_Data_Slot = No_Private_Data_Slot;

    generic
        with package Traits is new Vulkan.Object_Traits.Traits(<>);
    package Data_Access is
        use type Traits.Object;

        -- vkSetPrivateData
        function Set(Device: in Vulkan.Device;
                     Object_Handle: in Traits.Object;
                     Private_Data_Slot: in Vulkan.Private_Data_Slot;
                     Data: in Interfaces.Unsigned_64) return Result
            with Inline,
                 Pre => Device /= No_Device and
                        Object_Handle /= Traits.No_Object and
                        Private_Data_Slot /= No_Private_Data_Slot,
                 Post => Set'Result in Success |
                                       Out_Of_Host_Memory;

        procedure Set(Device: in Vulkan.Device;
                      Object_Handle: in Traits.Object;
                      Private_Data_Slot: in Vulkan.Private_Data_Slot;
                      Data: in Interfaces.Unsigned_64)
            with Inline,
                 Pre => Device /= No_Device and
                        Object_Handle /= Traits.No_Object and
                        Private_Data_Slot /= No_Private_Data_Slot;

        -- vkGetPrivateData
        function Get(Device: in Vulkan.Device;
                     Object_Handle: in Traits.Object;
                     Private_Data_Slot: in Vulkan.Private_Data_Slot)
            return Interfaces.Unsigned_64
            with Inline,
                 Pre => Device /= No_Device and
                        Object_Handle /= Traits.No_Object and
                        Private_Data_Slot /= No_Private_Data_Slot;
    end Data_Access;

private
    package Private_Data_Slots_Common is new Objects_Common_Access
        (Private_Data_Slot_Create_Info,
         C_V1_3.Private_Data_Slot_Create_Info_C,
         Private_Data_Slot,
         No_Private_Data_Slot,
         C_V1_3.To_C,
         C_V1_3.Free,
         C_V1_3.vkCreatePrivateDataSlot_Access,
         C_V1_3.vkDestroyPrivateDataSlot_Access,
         C_V1_3.vkCreatePrivateDataSlot,
         C_V1_3.vkDestroyPrivateDataSlot);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Private_Data_Slot_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Private_Data_Slot: out Vulkan.Private_Data_Slot)
        return Result
        renames Private_Data_Slots_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Private_Data_Slot_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Private_Data_Slot
        renames Private_Data_Slots_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Private_Data_Slot_Create_Info;
                    Private_Data_Slot: out Vulkan.Private_Data_Slot)
        return Result
        renames Private_Data_Slots_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Private_Data_Slot_Create_Info)
        return Private_Data_Slot
        renames Private_Data_Slots_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Private_Data_Slot: in out Vulkan.Private_Data_Slot;
                      Allocator: aliased in Allocation_Callbacks)
        renames Private_Data_Slots_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Private_Data_Slot: in out Vulkan.Private_Data_Slot)
        renames Private_Data_Slots_Common.Destroy;
end Vulkan.Private_Data_Slots;


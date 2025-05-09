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

-- Generic callback mappings

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Containers.Ordered_Maps;
with Interfaces.C.Extensions;

private generic
    type Object_Type is new Object_Handle;
    type User_Data is private;
    type Callback_Function is private;
package Vulkan.Callback_Marshallers is
    -- Internal marshalling data.
    type Marshaller is
    record
        Data: User_Data;
        Callback: Callback_Function;
    end record;

    type Marshaller_Access is access Marshaller;

    procedure Free is new Ada.Unchecked_Deallocation(Marshaller,
                                                     Marshaller_Access);

    function To_Marshaller is
        new Ada.Unchecked_Conversion(Interfaces.C.Extensions.void_ptr,
                                     Marshaller_Access);

    -- Storage for the current marshallers.
    package Marshaller_Maps is new Ada.Containers.Ordered_Maps
        (Object_Type, Marshaller_Access);

    -- Registered marshallers.
    protected type Marshallers is
        procedure Register(Object: in Object_Type;
                           Callback: in not null Marshaller_Access);

        procedure Remove(Object: in Object_Type);

    private
        Callbacks: Marshaller_Maps.Map;
    end Marshallers;
end Vulkan.Callback_Marshallers;


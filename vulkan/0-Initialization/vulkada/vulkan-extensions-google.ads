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

-- Google extensions root package

package Vulkan.Extensions.GOOGLE is
    -- Records.
    type Refresh_Cycle_Duration is
    record
        Refresh_Duration: Interfaces.Unsigned_64;
    end record
        with Convention => C;

    type Past_Presentation_Timing is
    record
        Present_ID: Interfaces.Unsigned_32;
        Desired_Present_Time: Interfaces.Unsigned_64;
        Actual_Present_Time: Interfaces.Unsigned_64;
        Earliest_Present_Time: Interfaces.Unsigned_64;
        Present_Margin: Interfaces.Unsigned_64;
    end record
        with Convention => C;

    package Past_Presentation_Timing_Vectors is new Ada.Containers.Vectors
        (Positive, Past_Presentation_Timing);

    type Present_Time is
    record
        Present_ID: Interfaces.Unsigned_32;
        Desired_Present_Time: Interfaces.Unsigned_64;
    end record
        with Convention => C;

    package Present_Time_Vectors is new Ada.Containers.Vectors
        (Positive, Present_Time);

    type Present_Times_Info is new In_Structure(Present_Times_Info_Type) with
    record
        Swapchain_Count: Interfaces.Unsigned_32;
        Times: Present_Time_Vectors.Vector;
    end record;
end Vulkan.Extensions.GOOGLE;


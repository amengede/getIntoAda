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

-- Operations for the direct mode display extension

with Vulkan.Extensions.KHR;

package Vulkan.Extensions.EXT_Direct_Mode_Display is
    use type KHR.Display;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkReleaseDisplayEXT
    function Release(Physical_Device: in Vulkan.Physical_Device;
                     Display: in KHR.Display) return Result
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display,
             Post => Release'Result in Success;

    procedure Release(Physical_Device: in Vulkan.Physical_Device;
                      Display: in KHR.Display)
        with Inline,
             Pre => Physical_Device /= No_Physical_Device and
                    Display /= KHR.No_Display;
end Vulkan.Extensions.EXT_Direct_Mode_Display;


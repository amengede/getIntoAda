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

-- Operations for the conditional rendering extension

with Vulkan.Extensions.EXT;

package Vulkan.Extensions.EXT_Conditional_Rendering is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdBeginConditionalRenderingEXT
    procedure Begin_Conditional_Rendering
        (Command_Buffer: in Vulkan.Command_Buffer;
         Conditional_Rendering_Begin: in EXT.Conditional_Rendering_Begin_Info)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    Conditional_Rendering_Begin.Buffer /= No_Buffer and
                    Conditional_Rendering_Begin.Offset rem 4 = 0;

    -- vkCmdEndConditionalRenderingEXT
    procedure End_Conditional_Rendering
        (Command_Buffer: in Vulkan.Command_Buffer)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;
end Vulkan.Extensions.EXT_Conditional_Rendering;


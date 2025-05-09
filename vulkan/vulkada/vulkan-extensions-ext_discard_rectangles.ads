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

-- Operations for the discard rectangles extension

with Vulkan.Extensions.EXT;

package Vulkan.Extensions.EXT_Discard_Rectangles is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCmdSetDiscardRectangleEXT
    procedure Set_Discard_Rectangle
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Discard_Rectangle: in Interfaces.Unsigned_32;
         Discard_Rectangles: in Rect_2D_Vectors.Vector)
        with Pre => Command_Buffer /= No_Command_Buffer and
                    not Discard_Rectangles.Is_Empty and
                    (for all Rect of Discard_Rectangles =>
                        Rect.Offset.X >= 0 and Rect.Offset.Y >= 0);

    procedure Set_Discard_Rectangle
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Discard_Rectangle: in Interfaces.Unsigned_32;
         Discard_Rectangle: in Rect_2D)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer and
                    Discard_Rectangle.Offset.X >= 0 and
                    Discard_Rectangle.Offset.Y >= 0;

    -- vkCmdSetDiscardRectangleEnableEXT
    procedure Set_Discard_Rectangle_Enable
        (Command_Buffer: in Vulkan.Command_Buffer;
         Discard_Rectangle_Enable: in Boolean)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdSetDiscardRectangleModeEXT
    procedure Set_Discard_Rectangle_Mode
        (Command_Buffer: in Vulkan.Command_Buffer;
         Discard_Rectangle_Mode: in EXT.Discard_Rectangle_Mode)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;
end Vulkan.Extensions.EXT_Discard_Rectangles;


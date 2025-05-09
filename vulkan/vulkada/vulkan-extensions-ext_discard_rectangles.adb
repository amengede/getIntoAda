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

with Vulkan.Core;
with Vulkan.Utilities;

package body Vulkan.Extensions.EXT_Discard_Rectangles is
    -- Loaded extension functions.
    type vkCmdSetDiscardRectangleEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         First_Discard_Rectangle,
                         Discard_Rectangle_Count: in Interfaces.Unsigned_32;
                         Discard_Rectangles: access constant Rect_2D)
        with Convention => C;

    vkCmdSetDiscardRectangleEXT: vkCmdSetDiscardRectangleEXT_Access;
   
    type vkCmdSetDiscardRectangleEnableEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Discard_Rectangle_Enable: in Interfaces.Unsigned_32)
        with Convention => C;

    vkCmdSetDiscardRectangleEnableEXT: vkCmdSetDiscardRectangleEnableEXT_Access;

    type vkCmdSetDiscardRectangleModeEXT_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Discard_Rectangle_Mode: in EXT.Discard_Rectangle_Mode)
        with Convention => C;

    vkCmdSetDiscardRectangleModeEXT: vkCmdSetDiscardRectangleModeEXT_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdSetDiscardRectangleEXT_Access);
        procedure Load is new Load_Pointer
            (vkCmdSetDiscardRectangleEnableEXT_Access);
        procedure Load is new Load_Pointer
            (vkCmdSetDiscardRectangleModeEXT_Access);
    begin
        Load(vkCmdSetDiscardRectangleEXT, "vkCmdSetDiscardRectangleEXT");
        Load(vkCmdSetDiscardRectangleEnableEXT,
             "vkCmdSetDiscardRectangleEnableEXT");
        Load(vkCmdSetDiscardRectangleModeEXT,
             "vkCmdSetDiscardRectangleModeEXT");
    end Load_Extension;

    procedure Set_Discard_Rectangle
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Discard_Rectangle: in Interfaces.Unsigned_32;
         Discard_Rectangles: in Rect_2D_Vectors.Vector) is
        C_Rects: array (1 .. Positive(Discard_Rectangles.Length))
                    of aliased Rect_2D;
    begin
        for X in C_Rects'Range loop
            C_Rects(X) := Discard_Rectangles(X);
        end loop;

        vkCmdSetDiscardRectangleEXT(Command_Buffer,
                                    First_Discard_Rectangle,
                                    C_Rects'Length,
                                    C_Rects(1)'Access);
    end Set_Discard_Rectangle;

    procedure Set_Discard_Rectangle
        (Command_Buffer: in Vulkan.Command_Buffer;
         First_Discard_Rectangle: in Interfaces.Unsigned_32;
         Discard_Rectangle: in Rect_2D) is
        Local_Rect: aliased Rect_2D := Discard_Rectangle;
    begin
        vkCmdSetDiscardRectangleEXT(Command_Buffer,
                                    First_Discard_Rectangle,
                                    1,
                                    Local_Rect'Access);
    end Set_Discard_Rectangle;

    procedure Set_Discard_Rectangle_Enable
        (Command_Buffer: in Vulkan.Command_Buffer;
         Discard_Rectangle_Enable: in Boolean) is
    begin
        vkCmdSetDiscardRectangleEnableEXT
            (Command_Buffer, Utilities.To_C(Discard_Rectangle_Enable));
    end Set_Discard_Rectangle_Enable;

    procedure Set_Discard_Rectangle_Mode
        (Command_Buffer: in Vulkan.Command_Buffer;
         Discard_Rectangle_Mode: in EXT.Discard_Rectangle_Mode) is
    begin
        vkCmdSetDiscardRectangleModeEXT(Command_Buffer,
                                        Discard_Rectangle_Mode);
    end Set_Discard_Rectangle_Mode;
end Vulkan.Extensions.EXT_Discard_Rectangles;


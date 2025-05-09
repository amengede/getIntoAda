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

-- Operations for the conditional rendering extension

with Vulkan.Core;
with Vulkan.C_EXT;
with Vulkan.Extension_Records;

package body Vulkan.Extensions.EXT_Conditional_Rendering is
    -- Loaded extension functions.
    type vkCmdBeginConditionalRenderingEXT_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Conditional_Rendering_Begin:
                 in C_EXT.Conditional_Rendering_Begin_Info_C)
        with Convention => C;

    vkCmdBeginConditionalRenderingEXT: vkCmdBeginConditionalRenderingEXT_Access;

    type vkCmdEndConditionalRenderingEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer)
        with Convention => c;

    vkCmdEndConditionalRenderingEXT: vkCmdEndConditionalRenderingEXT_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer
            (vkCmdBeginConditionalRenderingEXT_Access);
        procedure Load is new Load_Pointer
            (vkCmdEndConditionalRenderingEXT_Access);
    begin
        Load(vkCmdBeginConditionalRenderingEXT,
             "vkCmdBeginConditionalRenderingEXT");
        Load(vkCmdEndConditionalRenderingEXT,
             "vkCmdEndConditionalRenderingEXT");
    end Load_Extension;

    procedure Begin_Conditional_Rendering
        (Command_Buffer: in Vulkan.Command_Buffer;
         Conditional_Rendering_Begin:
            in EXT.Conditional_Rendering_Begin_Info) is
        Info_C: C_EXT.Conditional_Rendering_Begin_Info_C :=
            C_EXT.To_C(Conditional_Rendering_Begin);
    begin
        vkCmdBeginConditionalRenderingEXT(Command_Buffer, Info_C);
        C_EXT.Free(Info_C);
    end Begin_Conditional_Rendering;
    
    procedure End_Conditional_Rendering
        (Command_Buffer: in Vulkan.Command_Buffer) is
    begin
        vkCmdEndConditionalRenderingEXT(Command_Buffer);
    end End_Conditional_Rendering;
end Vulkan.Extensions.EXT_Conditional_Rendering;


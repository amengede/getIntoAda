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

-- Operation for the push descriptor extension

with Vulkan.Core;
with Vulkan.Push_Descriptors_C;
with Vulkan.C;

package body Vulkan.Push_Descriptors is
    -- Loaded extension functions.
    type vkCmdPushDescriptorSetKHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Pipeline_Bind_Point: in Vulkan.Pipeline_Bind_Point;
             Layout: in Pipeline_Layout;
             Set, Descriptor_Write_Count: in Interfaces.Unsigned_32;
             Descriptor_Writes: access constant C.Write_Descriptor_Set_C)
        with Convention => C;

    vkCmdPushDescriptorSetKHR: vkCmdPushDescriptorSetKHR_Access;

    type vkCmdPushDescriptorSetWithTemplateKHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Descriptor_Update_Template: in Vulkan.Descriptor_Update_Template;
             Layout: in Pipeline_Layout;
             Set: in Interfaces.Unsigned_32;
             Data: in Interfaces.C.Extensions.void_ptr)
        with Convention => C;

    vkCmdPushDescriptorSetWithTemplateKHR:
        vkCmdPushDescriptorSetWithTemplateKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdPushDescriptorSetKHR_Access);
        procedure Load is new Load_Pointer
            (vkCmdPushDescriptorSetWithTemplateKHR_Access);
    begin
        Load(vkCmdPushDescriptorSetKHR, "vkCmdPushDescriptorSetKHR");
        Load(vkCmdPushDescriptorSetWithTemplateKHR,
             "vkCmdPushDescriptorSetWithTemplateKHR");
    end Load_Extension;

    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Bind_Point: in Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   Set: in Interfaces.Unsigned_32;
                   Descriptor_Writes: in Write_Descriptor_Set_Vectors.Vector) is
        C_Descriptor_Writes: array (1 .. Positive(Descriptor_Writes.Length))
            of aliased C.Write_Descriptor_Set_C;
    begin
        for X in C_Descriptor_Writes'Range loop
            C_Descriptor_Writes(X) := C.To_C(Descriptor_Writes(X));
        end loop;

        vkCmdPushDescriptorSetKHR(Command_Buffer,
                                  Bind_Point,
                                  Layout,
                                  Set,
                                  C_Descriptor_Writes'Length,
                                  C_Descriptor_Writes(1)'Access);

        for Write of C_Descriptor_Writes loop
            C.Free(Write);
        end loop;
    end Push;

    procedure Push(Command_Buffer: in Vulkan.Command_Buffer;
                   Bind_Point: in Pipeline_Bind_Point;
                   Layout: in Pipeline_Layout;
                   Set: in Interfaces.Unsigned_32;
                   Descriptor_Write: in Write_Descriptor_Set) is
    begin
        Push(Command_Buffer,
             Bind_Point,
             Layout,
             Set,
             Write_Descriptor_Set_Vectors.To_Vector(Descriptor_Write, 1));
    end Push;
    
    procedure Push
        (Command_Buffer: in Vulkan.Command_Buffer;
         Descriptor_Update_Template: in Vulkan.Descriptor_Update_Template;
         Layout: in Pipeline_Layout;
         Set: in Interfaces.Unsigned_32;
         Data: in Interfaces.C.Extensions.void_ptr) is
    begin
        vkCmdPushDescriptorSetWithTemplateKHR(Command_Buffer,
                                              Descriptor_Update_Template,
                                              Layout,
                                              Set,
                                              Data);
    end Push;
end Vulkan.Push_Descriptors;


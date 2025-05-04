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

-- Framebuffer related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Framebuffers is
    use type Interfaces.Unsigned_32;

    -- vkCreateFramebuffer
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Framebuffer_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Framebuffer: out Vulkan.Framebuffer) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Render_Pass /= No_Render_Pass and
                    (for all Image_View of Create_Info.Attachments =>
                        Image_View /= No_Image_View) and
                    Create_Info.Width > 0 and
                    Create_Info.Height > 0 and
                    Create_Info.Layers > 0,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Framebuffer /= No_Framebuffer);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Framebuffer_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Framebuffer
        with Pre => Device /= No_Device and
                    Create_Info.Render_Pass /= No_Render_Pass and
                    (for all Image_View of Create_Info.Attachments =>
                        Image_View /= No_Image_View) and
                    Create_Info.Width > 0 and
                    Create_Info.Height > 0 and
                    Create_Info.Layers > 0,
             Post => Create'Result /= No_Framebuffer;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Framebuffer_Create_Info;
                    Framebuffer: out Vulkan.Framebuffer) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Render_Pass /= No_Render_Pass and
                    (for all Image_View of Create_Info.Attachments =>
                        Image_View /= No_Image_View) and
                    Create_Info.Width > 0 and
                    Create_Info.Height > 0 and
                    Create_Info.Layers > 0,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Framebuffer /= No_Framebuffer);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Framebuffer_Create_Info)
        return Framebuffer
        with Pre => Device /= No_Device and
                    Create_Info.Render_Pass /= No_Render_Pass and
                    (for all Image_View of Create_Info.Attachments =>
                        Image_View /= No_Image_View) and
                    Create_Info.Width > 0 and
                    Create_Info.Height > 0 and
                    Create_Info.Layers > 0,
             Post => Create'Result /= No_Framebuffer;

    -- vkDestroyFramebuffer
    procedure Destroy(Device: in Vulkan.Device;
                      Framebuffer: in out Vulkan.Framebuffer;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Framebuffer = No_Framebuffer;

    procedure Destroy(Device: in Vulkan.Device;
                      Framebuffer: in out Vulkan.Framebuffer)
        with Inline,
             Pre => Device /= No_Device,
             Post => Framebuffer = No_Framebuffer;

private
    package Framebuffers_Common is
        new Objects_Common(Framebuffer_Create_Info,
                           C.Framebuffer_Create_Info_C,
                           Framebuffer,
                           No_Framebuffer,
                           C.To_C,
                           C.Free,
                           C.vkCreateFramebuffer,
                           C.vkDestroyFramebuffer);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Framebuffer_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Framebuffer: out Vulkan.Framebuffer) return Result
        renames Framebuffers_Common.Create;

    function Create(Device: in Vulkan.Device;
                Create_Info: in Framebuffer_Create_Info;
                Allocator: aliased in Allocation_Callbacks)
                    return Framebuffer
        renames Framebuffers_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Framebuffer_Create_Info;
                    Framebuffer: out Vulkan.Framebuffer) return Result
        renames Framebuffers_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Framebuffer_Create_Info)
                        return Framebuffer
        renames Framebuffers_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Framebuffer: in out Vulkan.Framebuffer;
                      Allocator: aliased in Allocation_Callbacks)
        renames Framebuffers_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Framebuffer: in out Vulkan.Framebuffer)
        renames Framebuffers_Common.Destroy;
end Vulkan.Framebuffers;


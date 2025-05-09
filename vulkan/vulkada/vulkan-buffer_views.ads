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

-- Buffer view related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Buffer_Views is
    -- vkCreateBufferView
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_View_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Buffer_View: out Vulkan.Buffer_View) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Buffer /= No_Buffer and
                    Create_Info.View_Range /= 0,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Buffer_View /= No_Buffer_View);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_View_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Buffer_View
        with Pre => Device /= No_Device and
                    Create_Info.Buffer /= No_Buffer and
                    Create_Info.View_Range /= 0,
             Post => Create'Result /= No_Buffer_View;
                     
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_View_Create_Info;
                    Buffer_View: out Vulkan.Buffer_View) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Buffer /= No_Buffer and
                    Create_Info.View_Range /= 0,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Buffer_View /= No_Buffer_View);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_View_Create_Info) return Buffer_View
        with Pre => Device /= No_Device and
                    Create_Info.Buffer /= No_Buffer and
                    Create_Info.View_Range /= 0,
             Post => Create'Result /= No_Buffer_View;

    -- vkDestroyBufferView
    procedure Destroy(Device: in Vulkan.Device;
                      Buffer_View: in out Vulkan.Buffer_View;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Buffer_View = No_Buffer_View;

    procedure Destroy(Device: in Vulkan.Device;
                      Buffer_View: in out Vulkan.Buffer_View)
        with Inline,
             Pre => Device /= No_Device,
             Post => Buffer_View = No_Buffer_View;

private
    package Buffer_Views_Common is
        new Objects_Common(Buffer_View_Create_Info,
                           C.Buffer_View_Create_Info_C,
                           Buffer_View,
                           No_Buffer_View,
                           C.To_C,
                           C.Free,
                           C.vkCreateBufferView,
                           C.vkDestroyBufferView);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_View_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Buffer_View: out Vulkan.Buffer_View) return Result
        renames Buffer_Views_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_View_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Buffer_View
        renames Buffer_Views_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_View_Create_Info;
                    Buffer_View: out Vulkan.Buffer_View) return Result
        renames Buffer_Views_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Buffer_View_Create_Info) return Buffer_View
        renames Buffer_Views_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Buffer_View: in out Vulkan.Buffer_View;
                      Allocator: aliased in Allocation_Callbacks)
        renames Buffer_Views_Common.Destroy;                      

    procedure Destroy(Device: in Vulkan.Device;
                      Buffer_View: in out Vulkan.Buffer_View)
        renames Buffer_Views_Common.Destroy;
end Vulkan.Buffer_Views;


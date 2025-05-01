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

-- Image view related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Image_Views is
    -- vkCreateImageView
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_View_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Image_View: out Vulkan.Image_View) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Image /= No_Image,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Image_View /= No_Image_View);
 
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_View_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Image_View
        with Pre => Device /= No_Device and
                    Create_Info.Image /= No_Image,
             Post => Create'Result /= No_Image_View;
            
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_View_Create_Info;
                    Image_View: out Vulkan.Image_View) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Image /= No_Image,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        Image_View /= No_Image_View);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_View_Create_Info)
        return Image_View
        with Pre => Device /= No_Device and
                    Create_Info.Image /= No_Image,
             Post => Create'Result /= No_Image_View;

    -- vkDestroyImageView
    procedure Destroy(Device: in Vulkan.Device;
                      Image_View: in out Vulkan.Image_View;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Image_View = No_Image_View;

    procedure Destroy(Device: in Vulkan.Device;
                      Image_View: in out Vulkan.Image_View)
        with Inline,
             Pre => Device /= No_Device,
             Post => Image_View = No_Image_View;

private
    package Image_Views_Common is
        new Objects_Common(Image_View_Create_Info,
                           C.Image_View_Create_Info_C,
                           Image_View,
                           No_Image_View,
                           C.To_C,
                           C.Free,
                           C.vkCreateImageView,
                           C.vkDestroyImageView);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_View_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Image_View: out Vulkan.Image_View) return Result
        renames Image_Views_Common.Create;
             
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_View_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Image_View
        renames Image_Views_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_View_Create_Info;
                    Image_View: out Vulkan.Image_View) return Result
        renames Image_Views_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Image_View_Create_Info)
                        return Image_View
        renames Image_Views_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Image_View: in out Vulkan.Image_View;
                      Allocator: aliased in Allocation_Callbacks)
        renames Image_Views_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Image_View: in out Vulkan.Image_View)
        renames Image_Views_Common.Destroy;
end Vulkan.Image_Views;


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

-- Various properties of object handles useful in generics

with Vulkan.Extensions.KHR;
with Vulkan.Extensions.EXT;
with Vulkan.Extensions.NVX;

package Vulkan.Object_Traits is
    generic
        type Object is new Object_Handle;
        Object_Type: in Vulkan.Object_Type;
        No_Object: in Object;
    package Traits is
    end Traits;

    package Instance is new Traits(Vulkan.Instance,
                                   Instance_Object_Type,
                                   No_Instance);
    package Physical_Device is new Traits(Vulkan.Physical_Device,
                                          Physical_Device_Object_Type,
                                          No_Physical_Device);
    package Device is new Traits(Vulkan.Device, Device_Object_Type, No_Device);
    package Queue is new Traits(Vulkan.Queue, Queue_Object_Type, No_Queue);
    package Semaphore is new Traits(Vulkan.Semaphore,
                                    Semaphore_Object_Type,
                                    No_Semaphore);
    package Command_Buffer is new Traits(Vulkan.Command_Buffer,
                                         Command_Buffer_Object_Type,
                                         No_Command_Buffer);
    package Fence is new Traits(Vulkan.Fence, Fence_Object_Type, No_Fence);
    package Device_Memory is new Traits(Vulkan.Device_Memory,
                                        Device_Memory_Object_Type,
                                        No_Device_Memory);
    package Buffer is new Traits(Vulkan.Buffer, Buffer_Object_Type, No_Buffer);
    package Image is new Traits(Vulkan.Image, Image_Object_Type, No_Image);
    package Event is new Traits(Vulkan.Event, Event_Object_Type, No_Event);
    package Query_Pool is new Traits(Vulkan.Query_Pool,
                                     Query_Pool_Object_Type,
                                     No_Query_Pool);
    package Buffer_View is new Traits(Vulkan.Buffer_View,
                                      Buffer_View_Object_Type,
                                      No_Buffer_View);
    package Image_View is new Traits(Vulkan.Image_View,
                                     Image_View_Object_Type,
                                     No_Image_View);
    package Shader_Module is new Traits(Vulkan.Shader_Module,
                                        Shader_Module_Object_Type,
                                        No_Shader_Module);
    package Pipeline_Cache is new Traits(Vulkan.Pipeline_Cache,
                                         Pipeline_Cache_Object_Type,
                                         No_Pipeline_Cache);
    package Pipeline_Layout is new Traits(Vulkan.Pipeline_Layout,
                                          Pipeline_Layout_Object_Type,
                                          No_Pipeline_Layout);
    package Render_Pass is new Traits(Vulkan.Render_Pass,
                                      Render_Pass_Object_Type,
                                      No_Render_Pass);
    package Pipeline is new Traits(Vulkan.Pipeline,
                                   Pipeline_Object_Type,
                                   No_Pipeline);
    package Descriptor_Set_Layout is new Traits
        (Vulkan.Descriptor_Set_Layout,
         Descriptor_Set_Layout_Object_Type,
         No_Descriptor_Set_Layout);
    package Sampler is new Traits(Vulkan.Sampler,
                                  Sampler_Object_Type,
                                  No_Sampler);
    package Descriptor_Pool is new Traits(Vulkan.Descriptor_Pool,
                                          Descriptor_Pool_Object_Type,
                                          No_Descriptor_Pool);
    package Descriptor_Set is new Traits(Vulkan.Descriptor_Set,
                                         Descriptor_Set_Object_Type,
                                         No_Descriptor_Set);
    package Framebuffer is new Traits(Vulkan.Framebuffer,
                                      Framebuffer_Object_Type,
                                      No_Framebuffer);
    package Command_Pool is new Traits(Vulkan.Command_Pool,
                                       Command_Pool_Object_Type,
                                       No_Command_Pool);
    -- Vulkan 1.1
    package Sampler_YCbCr_Conversion is new Traits
        (Vulkan.Sampler_YCbCr_Conversion,
         Sampler_YCbCr_Conversion_Object_Type,
         No_Sampler_YCbCr_Conversion);
    package Descriptor_Update_Template is new Traits
        (Vulkan.Descriptor_Update_Template,
         Descriptor_Update_Template_Object_Type,
         No_Descriptor_Update_Template);
    -- Vulkan 1.3
    package Private_Data_Slot is new Traits(Vulkan.Private_Data_Slot,
                                            Private_Data_Slot_Object_Type,
                                            No_Private_Data_Slot);
    -- Extensions
    package Surface is new Traits(Extensions.KHR.Surface,
                                  Surface_Object_Type,
                                  Extensions.KHR.No_Surface);
    package Swapchain is new Traits(Extensions.KHR.Swapchain,
                                    Swapchain_Object_Type,
                                    Extensions.KHR.No_Swapchain);
    package Display is new Traits(Extensions.KHR.Display,
                                  Display_Object_Type,
                                  Extensions.KHR.No_Display);
    package Display_Mode is new Traits(Extensions.KHR.Display_Mode,
                                       Display_Mode_Object_Type,
                                       Extensions.KHR.No_Display_Mode);
    package Video_Session is new Traits(Extensions.KHR.Video_Session,
                                        Video_Session_Object_Type,
                                        Extensions.KHR.No_Video_Session);
    package Video_Session_Parameters is new Traits
        (Extensions.KHR.Video_Session_Parameters,
         Video_Session_Parameters_Object_Type,
         Extensions.KHR.No_Video_Session_Parameters);
    package Cu_Module is new Traits(Extensions.NVX.Cu_Module,
                                    Cu_Module_Object_Type,
                                    Extensions.NVX.No_Cu_Module);
    package Cu_Function is new Traits(Extensions.NVX.Cu_Function,
                                      Cu_Function_Object_Type,
                                      Extensions.NVX.No_Cu_Function);
    package Debug_Utils_Messenger is new Traits
        (Extensions.EXT.Debug_Utils_Messenger,
         Debug_Utils_Messenger_Object_Type,
         Extensions.EXT.No_Debug_Utils_Messenger);
end Vulkan.Object_Traits;


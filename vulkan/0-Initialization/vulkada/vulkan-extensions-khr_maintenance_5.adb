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

-- Operations for the maintenance 5 extension

with Vulkan.Core;
with Vulkan.C_KHR;
with Vulkan.Extension_Records;

package body Vulkan.Extensions.KHR_Maintenance_5 is
    -- Loaded extension functions.
    type vkCmdBindIndexBuffer2KHR_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer;
                         Buffer: in Vulkan.Buffer;
                         Offset, Size: in Device_Size;
                         Index_Type: in Vulkan.Index_Type)
        with Convention => C;

    vkCmdBindIndexBuffer2KHR: vkCmdBindIndexBuffer2KHR_Access;

    type vkGetRenderingAreaGranularityKHR_Access is
        access procedure(Device: in Vulkan.Device;
                         Rendering_Area_Info: in C_KHR.Rendering_Area_Info_C;
                         Granularity: out Extent_2D)
        with Convention => C;

    vkGetRenderingAreaGranularityKHR: vkGetRenderingAreaGranularityKHR_Access;

    type vkGetDeviceImageSubresourceLayoutKHR_Access is
        access procedure(Device: in Vulkan.Device;
                         Info: in C_KHR.Device_Image_Subresource_Info_C;
                         Layout: out C_KHR.Subresource_Layout_2_C)
        with Convention => C;

    vkGetDeviceImageSubresourceLayoutKHR:
        vkGetDeviceImageSubresourceLayoutKHR_Access;

    type vkGetImageSubresourceLayout2KHR_Access is
        access procedure(Device: in Vulkan.Device;
                         Image: in Vulkan.Image;
                         Subresource: in C_KHR.Image_Subresource_2_C;
                         Layout: out C_KHR.Subresource_Layout_2_C)
        with Convention => C;

    vkGetImageSubresourceLayout2KHR: vkGetImageSubresourceLayout2KHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkCmdBindIndexBuffer2KHR_Access);
        procedure Load is new Load_Pointer
            (vkGetRenderingAreaGranularityKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetDeviceImageSubresourceLayoutKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetImageSubresourceLayout2KHR_Access);
    begin
        Load(vkCmdBindIndexBuffer2KHR, "vkCmdBindIndexBuffer2KHR");
        Load(vkGetRenderingAreaGranularityKHR,
             "vkGetRenderingAreaGranularityKHR");
        Load(vkGetDeviceImageSubresourceLayoutKHR,
             "vkGetDeviceImageSubresourceLayoutKHR");
        Load(vkGetImageSubresourceLayout2KHR,
             "vkGetImageSubresourceLayout2KHR");
    end Load_Extension;

    procedure Bind(Command_Buffer: in Vulkan.Command_Buffer;
                   Buffer: in Vulkan.Buffer;
                   Offset, Size: in Device_Size;
                   Index_Type: in Vulkan.Index_Type) is
    begin
        vkCmdBindIndexBuffer2KHR(Command_Buffer,
                                 Buffer,
                                 Offset,
                                 Size,
                                 Index_Type);
    end Bind;

    function Get_Granularity(Device: in Vulkan.Device;
                             Rendering_Area_Info: in KHR.Rendering_Area_Info)
        return Extent_2D is
        Info_C: C_KHR.Rendering_Area_Info_C := C_KHR.To_C(Rendering_Area_Info);
        Granularity: Extent_2D;
    begin
        vkGetRenderingAreaGranularityKHR(Device, Info_C, Granularity);
        C_KHR.Free(Info_C);

        return Granularity;
    end Get_Granularity;

    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Info: in KHR.Device_Image_Subresource_Info;
                                     Layout: in out KHR.Subresource_Layout_2) is
        Info_C: C_KHR.Device_Image_Subresource_Info_C := C_KHR.To_C(InfO);
        Layout_C: C_KHR.Subresource_Layout_2_C;
    begin
        Layout_C.Next := Extension_Records.To_C(Layout.Next);
        vkGetDeviceImageSubresourceLayoutKHR(Device, Info_C, Layout_C);
        
        C_KHR.Free(Info_C);
        C_KHR.To_Ada(Layout, Layout_C);
        Extension_Records.Free(Layout_C.Next);
    end Get_Subresource_Layout;
    
    function Get_Subresource_Layout(Device: in Vulkan.Device;
                                    Info: in KHR.Device_Image_Subresource_Info)
        return KHR.Subresource_Layout_2 is
        Layout: KHR.Subresource_Layout_2;
    begin
        Get_Subresource_Layout(Device, Info, Layout);

        return Layout;
    end Get_Subresource_Layout;

    procedure Get_Subresource_Layout(Device: in Vulkan.Device;
                                     Image: in Vulkan.Image;
                                     Info: in KHR.Image_Subresource_2;
                                     Layout: in out KHR.Subresource_Layout_2) is
        Info_C: C_KHR.Image_Subresource_2_C;
        Layout_C: C_KHR.Subresource_Layout_2_C;
    begin
        Info_C.Next := Extension_Records.To_C(Info.Next);
        Info_C.Image_Subresource := Info.Image_Subresource;
        Layout_C.Next := Extension_Records.To_C(Layout.Next);
        vkGetImageSubresourceLayout2KHR(Device, Image, Info_C, Layout_C);

        Extension_Records.Free(Info_C.Next);
        C_KHR.To_Ada(Layout, Layout_C);
        Extension_Records.Free(Layout_C.Next);
    end Get_Subresource_Layout;

    function Get_Subresource_Layout(Device: in Vulkan.Device;
                                    Image: in Vulkan.Image;
                                    Info: in KHR.Image_Subresource_2)
        return KHR.Subresource_Layout_2 is
        Layout: KHR.Subresource_Layout_2;
    begin
        Get_Subresource_Layout(Device, Image, Info, Layout);

        return Layout;
    end Get_Subresource_Layout;
end Vulkan.Extensions.KHR_Maintenance_5;


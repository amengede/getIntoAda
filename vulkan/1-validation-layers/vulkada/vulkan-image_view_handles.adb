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

-- Operations for the image view handle extension

with Vulkan.Core;
with Vulkan.Image_View_Handles_C;
with Vulkan.Extension_Records;
with Vulkan.Exceptions;

package body Vulkan.Image_View_Handles is
    -- Loaded extension functions.
    type vkGetImageViewHandleNVX_Access is
        access function(Device: in Vulkan.Device;
                        Info: in Image_View_Handles_C.Image_View_Handle_Info_C)
            return Interfaces.Unsigned_32
        with Convention => C;

    vkGetImageViewHandleNVX: vkGetImageViewHandleNVX_Access;

    type vkGetImageViewAddressNVX_Access is
        access function
            (Device: in Vulkan.Device;
             Image_View: in Vulkan.Image_View;
             Properties:
                in Image_View_Handles_C.Image_View_Address_Properties_C)
        return Result
        with Convention => C;

    vkGetImageViewAddressNVX: vkGetImageViewAddressNVX_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkGetImageViewHandleNVX_Access);
        procedure Load is new Load_Pointer(vkGetImageViewAddressNVX_Access);
    begin
        Load(vkGetImageViewHandleNVX, "vkGetImageViewHandleNVX");
        Load(vkGetImageViewAddressNVX, "vkGetImageViewAddressNVX");
    end Load_Extension;

    function Get_Image_View_Handle(Device: in Vulkan.Device;
                                   Info: in Image_View_Handle_Info)
        return Interfaces.Unsigned_32 is
        Info_C: Image_View_Handles_C.Image_View_Handle_Info_C :=
            Image_View_Handles_C.To_C(Info);
        Handle: Interfaces.Unsigned_32;
    begin
        Handle := vkGetImageViewHandleNVX(Device, Info_C);
        Image_View_Handles_C.Free(Info_C);

        return Handle;
    end Get_Image_View_Handle;

    function Get_Image_View_Address
        (Device: in Vulkan.Device;
         Image_View: in Vulkan.Image_View;
         Properties: in out Image_View_Address_Properties) return Result is
        Properties_C: Image_View_Handles_C.Image_View_Address_Properties_C;
        Result: Vulkan.Result;
    begin
        Properties_C.Next := Extension_Records.To_C(Properties.Next);
        Result := vkGetImageViewAddressNVX(Device, Image_View, Properties_C);
        Image_View_Handles_C.To_Ada(Properties, Properties_C);
        Extension_Records.Free(Properties_C.Next);

        return Result;
    end Get_Image_View_Address;

    procedure Get_Image_View_Address
        (Device: in Vulkan.Device;
         Image_View: in Vulkan.Image_View;
         Properties: in out Image_View_Address_Properties) is
    begin
        Exceptions.Check(Get_Image_View_Address(Device, 
                                                Image_View,
                                                Properties));
    end Get_Image_View_Address;

    function Get_Image_View_Address(Device: in Vulkan.Device;
                                    Image_View: in Vulkan.Image_View)
        return Image_View_Address_Properties is
        Properties: Image_View_Address_Properties;
    begin
        Get_Image_View_Address(Device, Image_View, Properties);

        return Properties;
    end Get_Image_View_Address;
end Vulkan.Image_View_Handles;


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

-- Operations for the debug marker extension

package Vulkan.Debug_Markers is
    use type System.Address;
    use type Interfaces.C.size_t;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkDebugMarkerSetObjectTagEXT
    function Set_Object_Tag(Device: in Vulkan.Device;
                            Tag_Info: in Debug_Marker_Object_Tag_Info)
        return Result
        with Pre => Device /= No_Device and
                    Tag_Info.Object /= No_Object_Handle and
                    Tag_Info.Tag_Size > 0 and
                    Tag_Info.Tag /= System.Null_Address,
             Post => Set_Object_Tag'Result in Success |
                                              Out_Of_Host_Memory |
                                              Out_Of_Device_Memory;

    procedure Set_Object_Tag(Device: in Vulkan.Device;
                             Tag_Info: in Debug_Marker_Object_Tag_Info)
        with Pre => Device /= No_Device and
                    Tag_Info.Object /= No_Object_Handle and
                    Tag_Info.Tag_Size > 0 and
                    Tag_Info.Tag /= System.Null_Address;

    -- vkDebugMarkerSetObjectNameEXT
    function Set_Object_Name(Device: in Vulkan.Device;
                             Name_Info: in Debug_Marker_Object_Name_Info)
        return Result
        with Pre => Device /= No_Device and
                    Name_Info.Object /= No_Object_Handle,
             Post => Set_Object_Name'Result in Success |
                                               Out_Of_Host_Memory |
                                               Out_Of_Device_Memory;

    procedure Set_Object_Name(Device: in Vulkan.Device;
                              Name_Info: in Debug_Marker_Object_Name_Info)
        with Pre => Device /= No_Device and
                    Name_Info.Object /= No_Object_Handle;

    -- vkCmdDebugMarkerBeginEXT
    procedure Begin_Debug_Marker(Command_Buffer: in Vulkan.Command_Buffer;
                                 Marker_Info: in Debug_Marker_Marker_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdDebugMarkerEndEXT
    procedure End_Debug_Marker(Command_Buffer: in Vulkan.Command_Buffer)
        with Inline,
             Pre => Command_Buffer /= No_Command_Buffer;

    -- vkCmdDebugMarkerInsertEXT
    procedure Insert(Command_Buffer: in Vulkan.Command_Buffer;
                     Marker_Info: in Debug_Marker_Marker_Info)
        with Pre => Command_Buffer /= No_Command_Buffer;
end Vulkan.Debug_Markers;


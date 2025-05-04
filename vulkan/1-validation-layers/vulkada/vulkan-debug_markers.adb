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

with Vulkan.Core;
with Vulkan.Debug_Markers_C;
with Vulkan.Exceptions;

package body Vulkan.Debug_Markers is
    -- Loaded extension functions.
    type vkDebugMarkerSetObjectTagEXT_Access is
        access function
            (Device: in Vulkan.Device;
             Tag_Info: in Debug_Markers_C.Debug_Marker_Object_Tag_Info_C)
        return Result
        with Convention => C;

    vkDebugMarkerSetObjectTagEXT: vkDebugMarkerSetObjectTagEXT_Access;

    type vkDebugMarkerSetObjectNameEXT_Access is
        access function
            (Device: in Vulkan.Device;
             Name_Info: in Debug_Markers_C.Debug_Marker_Object_Name_Info_C)
        return Result
        with Convention => C;

    vkDebugMarkerSetObjectNameEXT: vkDebugMarkerSetObjectNameEXT_Access;

    type vkCmdDebugMarkerBeginEXT_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Marker_Info: in Debug_Markers_C.Debug_Marker_Marker_Info_C)
        with Convention => C;

    vkCmdDebugMarkerBeginEXT: vkCmdDebugMarkerBeginEXT_Access;

    type vkCmdDebugMarkerEndEXT_Access is
        access procedure(Command_Buffer: in Vulkan.Command_Buffer)
        with Convention => C;

    vkCmdDebugMarkerEndEXT: vkCmdDebugMarkerEndEXT_Access;

    type vkCmdDebugMarkerInsertEXT_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Marker_Info: in Debug_Markers_C.Debug_Marker_Marker_Info_C)
        with Convention => C;

    vkCmdDebugMarkerInsertEXT: vkCmdDebugMarkerInsertEXT_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer(vkDebugMarkerSetObjectTagEXT_Access);
        procedure Load is
            new Load_Pointer(vkDebugMarkerSetObjectNameEXT_Access);
        procedure Load is new Load_Pointer(vkCmdDebugMarkerBeginEXT_Access);
        procedure Load is new Load_Pointer(vkCmdDebugMarkerEndEXT_Access);
        procedure Load is new Load_Pointer(vkCmdDebugMarkerInsertEXT_Access);
    begin
        Load(vkDebugMarkerSetObjectTagEXT, "vkDebugMarkerSetObjectTagEXT");
        Load(vkDebugMarkerSetObjectNameEXT, "vkDebugMarkerSetObjectNameEXT");
        Load(vkCmdDebugMarkerBeginEXT, "vkCmdDebugMarkerBeginEXT");
        Load(vkCmdDebugMarkerEndEXT, "vkCmdDebugMarkerEndEXT");
        Load(vkCmdDebugMarkerInsertEXT, "vkCmdDebugMarkerInsertEXT");
    end Load_Extension;
    
    function Set_Object_Tag(Device: in Vulkan.Device;
                            Tag_Info: in Debug_Marker_Object_Tag_Info)
        return Result is
        Info_C: Debug_Markers_C.Debug_Marker_Object_Tag_Info_C :=
            Debug_Markers_C.To_C(Tag_Info);
        Result: Vulkan.Result;
    begin
        Result := vkDebugMarkerSetObjectTagEXT(Device, Info_C);
        Debug_Markers_C.Free(Info_C);

        return Result;
    end Set_Object_Tag;

    procedure Set_Object_Tag(Device: in Vulkan.Device;
                             Tag_Info: in Debug_Marker_Object_Tag_Info) is
    begin
        Exceptions.Check(Set_Object_Tag(Device, Tag_Info));
    end Set_Object_Tag;

    function Set_Object_Name(Device: in Vulkan.Device;
                             Name_Info: in Debug_Marker_Object_Name_Info)
        return Result is
        Info_C: Debug_Markers_C.Debug_Marker_Object_Name_Info_C :=
            Debug_Markers_C.To_C(Name_Info);
        Result: Vulkan.Result;
    begin
        Result := vkDebugMarkerSetObjectNameEXT(Device, Info_C);
        Debug_Markers_C.Free(Info_C);

        return Result;
    end Set_Object_Name;
    
    procedure Set_Object_Name(Device: in Vulkan.Device;
                              Name_Info: in Debug_Marker_Object_Name_Info) is
    begin
        Exceptions.Check(Set_Object_Name(Device, Name_Info));
    end Set_Object_Name;
    
    procedure Begin_Debug_Marker(Command_Buffer: in Vulkan.Command_Buffer;
                                 Marker_Info: in Debug_Marker_Marker_Info) is
        Info_C: Debug_Markers_C.Debug_Marker_Marker_Info_C :=
            Debug_Markers_C.To_C(Marker_Info);
    begin
        vkCmdDebugMarkerBeginEXT(Command_Buffer, Info_C);
        Debug_Markers_C.Free(Info_C);
    end Begin_Debug_Marker;

    procedure End_Debug_Marker(Command_Buffer: in Vulkan.Command_Buffer) is
    begin
        vkCmdDebugMarkerEndEXT(Command_Buffer);
    end End_Debug_Marker;
    
    procedure Insert(Command_Buffer: in Vulkan.Command_Buffer;
                     Marker_Info: in Debug_Marker_Marker_Info) is
        Info_C: Debug_Markers_C.Debug_Marker_Marker_Info_C :=
            Debug_Markers_C.To_C(Marker_Info);
    begin
        vkCmdDebugMarkerInsertEXT(Command_Buffer, Info_C);
        Debug_Markers_C.Free(Info_C);
    end Insert;
end Vulkan.Debug_Markers;


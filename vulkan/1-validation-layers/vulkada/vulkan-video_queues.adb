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

-- Operations for the video queues extension

with Vulkan.Video_Queues_C;
with Vulkan.Objects_Common_Access;
with Vulkan.Core;
with Vulkan.Extension_Records;
with Vulkan.Exceptions;

package body Vulkan.Video_Queues is
    -- Loaded extension functions.
    type vkGetPhysicalDeviceVideoCapabilitiesKHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Video_Profile: in Video_Queues_C.Video_Profile_Info_C;
             Capabilities: in out Video_Queues_C.Video_Capabilities_C)
                return Result
        with Convention => C;

    vkGetPhysicalDeviceVideoCapabilitiesKHR:
        vkGetPhysicalDeviceVideoCapabilitiesKHR_Access;

    type vkGetPhysicalDeviceVideoFormatPropertiesKHR_Access is
        access function
            (Physical_Device: in Vulkan.Physical_Device;
             Video_Format_Info: in
                Video_Queues_C.Physical_Device_Video_Format_Info_C;
             Video_Format_Property_Count: in out Interfaces.Unsigned_32;
             Video_Format_Properties:
                access Video_Queues_C.Video_Format_Properties_C) return Result
        with Convention => C;

    vkGetPhysicalDeviceVideoFormatPropertiesKHR:
        vkGetPhysicalDeviceVideoFormatPropertiesKHR_Access;

    type vkCreateVideoSessionKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Create_Info: in Video_Queues_C.Video_Session_Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Video_Session: out Vulkan.Video_Session) return Result
        with Convention => C;

    vkCreateVideoSessionKHR: vkCreateVideoSessionKHR_Access;

    type vkDestroyVideoSessionKHR_Access is
        access procedure(Device: in Vulkan.Device;
                         Video_Session: in Vulkan.Video_Session;
                         Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroyVideoSessionKHR: vkDestroyVideoSessionKHR_Access;

    type vkGetVideoSessionMemoryRequirementsKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Video_Session: in Vulkan.Video_Session;
             Memory_Requirements_Count: in out Interfaces.Unsigned_32;
             Memory_Requirements:
                access Video_Queues_C.Video_Session_Memory_Requirements_C)
                return Result
        with Convention => C;

    vkGetVideoSessionMemoryRequirementsKHR:
        vkGetVideoSessionMemoryRequirementsKHR_Access;

    type vkBindVideoSessionMemoryKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Video_Session: in Vulkan.Video_Session;
             Bind_Session_Memory_Info_Count: in Interfaces.Unsigned_32;
             Memory_Requirements:
                access Video_Queues_C.Bind_Video_Session_Memory_Info_C)
                return Result
        with Convention => C;

    vkBindVideoSessionMemoryKHR: vkBindVideoSessionMemoryKHR_Access;

    type vkCreateVideoSessionParametersKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Create_Info:
                in Video_Queues_C.Video_Session_Parameters_Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Video_Session_Parameters: out Vulkan.Video_Session_Parameters)
                return Result
        with Convention => C;

    vkCreateVideoSessionParametersKHR: vkCreateVideoSessionParametersKHR_Access;

    type vkUpdateVideoSessionParametersKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Video_Session_Parameters: in Vulkan.Video_Session_Parameters;
             Update_Info:
                in Video_Queues_C.Video_Session_Parameters_Update_Info_C)
                return Result
        with Convention => C;

    vkUpdateVideoSessionParametersKHR: vkUpdateVideoSessionParametersKHR_Access;

    type vkDestroyVideoSessionParametersKHR_Access is
        access procedure
            (Device: in Vulkan.Device;
             Video_Session_Parameters: in Vulkan.Video_Session_Parameters;
             Allocator: access constant Allocation_Callbacks)
        with Convention => C;

    vkDestroyVideoSessionParametersKHR:
        vkDestroyVideoSessionParametersKHR_Access;

    type vkCmdBeginVideoCodingKHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Begin_Info: in Video_Queues_C.Video_Begin_Coding_Info_C)
        with Convention => C;

    vkCmdBeginVideoCodingKHR: vkCmdBeginVideoCodingKHR_Access;

    type vkCmdEndVideoCodingKHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             End_Coding_Info: in Video_Queues_C.Video_End_Coding_Info_C)
        with Convention => C;

    vkCmdEndVideoCodingKHR: vkCmdEndVideoCodingKHR_Access;

    type vkCmdControlVideoCodingKHR_Access is
        access procedure
            (Command_Buffer: in Vulkan.Command_Buffer;
             Coding_Control_Info: in Video_Queues_C.Video_Coding_Control_Info_C)
        with Convention => C;

    vkCmdControlVideoCodingKHR: vkCmdControlVideoCodingKHR_Access;

    package Video_Sessions_Common is new Objects_Common_Access
        (Video_Session_Create_Info,
         Video_Queues_C.Video_Session_Create_Info_C,
         Video_Session,
         No_Video_Session,
         Video_Queues_C.To_C,
         Video_Queues_C.Free,
         vkCreateVideoSessionKHR_Access,
         vkDestroyVideoSessionKHR_Access,
         vkCreateVideoSessionKHR,
         vkDestroyVideoSessionKHR);

    package Video_Session_Parameters_Common is new Objects_Common_Access
        (Video_Session_Parameters_Create_Info,
         Video_Queues_C.Video_Session_Parameters_Create_Info_C,
         Video_Session_Parameters,
         No_Video_Session_Parameters,
         Video_Queues_C.To_C,
         Video_Queues_C.Free,
         vkCreateVideoSessionParametersKHR_Access,
         vkDestroyVideoSessionParametersKHR_Access,
         vkCreateVideoSessionParametersKHR,
         vkDestroyVideoSessionParametersKHR);

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
            (vkGetPhysicalDeviceVideoCapabilitiesKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPhysicalDeviceVideoFormatPropertiesKHR_Access);
        procedure Load is new Load_Pointer(vkCreateVideoSessionKHR_Access);
        procedure Load is new Load_Pointer(vkDestroyVideoSessionKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetVideoSessionMemoryRequirementsKHR_Access);
        procedure Load is new Load_Pointer(vkBindVideoSessionMemoryKHR_Access);
        procedure Load is new Load_Pointer
            (vkCreateVideoSessionParametersKHR_Access);
        procedure Load is new Load_Pointer
            (vkUpdateVideoSessionParametersKHR_Access);
        procedure Load is new Load_Pointer
            (vkDestroyVideoSessionParametersKHR_Access);
        procedure Load is new Load_Pointer(vkCmdBeginVideoCodingKHR_Access);
        procedure Load is new Load_Pointer(vkCmdEndVideoCodingKHR_Access);
        procedure Load is new Load_Pointer(vkCmdControlVideoCodingKHR_Access);
    begin
        Load(vkGetPhysicalDeviceVideoCapabilitiesKHR,
             "vkGetPhysicalDeviceVideoCapabilitiesKHR");
        Load(vkGetPhysicalDeviceVideoFormatPropertiesKHR,
             "vkGetPhysicalDeviceVideoFormatPropertiesKHR");
        Load(vkCreateVideoSessionKHR, "vkCreateVideoSessionKHR");
        Load(vkDestroyVideoSessionKHR, "vkDestroyVideoSessionKHR");
        Load(vkGetVideoSessionMemoryRequirementsKHR,
             "vkGetVideoSessionMemoryRequirementsKHR");
        Load(vkBindVideoSessionMemoryKHR, "vkBindVideoSessionMemoryKHR");
        Load(vkCreateVideoSessionParametersKHR,
             "vkCreateVideoSessionParametersKHR");
        Load(vkUpdateVideoSessionParametersKHR,
             "vkUpdateVideoSessionParametersKHR");
        Load(vkDestroyVideoSessionParametersKHR,
             "vkDestroyVideoSessionParametersKHR");
        Load(vkCmdBeginVideoCodingKHR, "vkCmdBeginVideoCodingKHR");
        Load(vkCmdEndVideoCodingKHR, "vkCmdEndVideoCodingKHR");
        Load(vkCmdControlVideoCodingKHR, "vkCmdControllerVideoCodingKHR");
    end Load_Extension;
   
    function Get_Physical_Device_Video_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Profile: in Video_Profile_Info;
         Capabilities: in out Video_Capabilities) return Result is
        Video_Profile_C: Video_Queues_C.Video_Profile_Info_C :=
            Video_Queues_C.To_C(Video_Profile);
        Capabilities_C: Video_Queues_C.Video_Capabilities_C;
        Result: Vulkan.Result;
    begin
        Capabilities_C.Next := Extension_Records.To_C(Capabilities.Next);
        Result := vkGetPhysicalDeviceVideoCapabilitiesKHR(Physical_Device,
                                                          Video_Profile_C,
                                                          Capabilities_C);
        Video_Queues_C.Free(Video_Profile_C);
        Video_Queues_C.To_Ada(Capabilities, Capabilities_C);
        Extension_Records.Free(Capabilities_C.Next);

        return Result;
    end Get_Physical_Device_Video_Capabilities;
    
    function Get_Physical_Device_Video_Capabilities
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Profile: in Video_Profile_Info) return Video_Capabilities is
        Capabilities: Video_Capabilities;
    begin
        Exceptions.Check
            (Get_Physical_Device_Video_Capabilities(Physical_Device,
                                                    Video_Profile,
                                                    Capabilities));

        return Capabilities;
    end Get_Physical_Device_Video_Capabilities;
    
    function Physical_Device_Video_Format_Properties_Count
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Format_Info: in Physical_Device_Video_Format_Info)
        return Interfaces.Unsigned_32 is
        Video_Format_Info_C:
            Video_Queues_C.Physical_Device_Video_Format_Info_C :=
                Video_Queues_C.To_C(Video_Format_Info);
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(vkGetPhysicalDeviceVideoFormatPropertiesKHR
            (Physical_Device, Video_Format_Info_C, Count, null));
        Video_Queues_C.Free(Video_Format_Info_C);

        return Count;

    exception
        when others =>
            Video_Queues_C.Free(Video_Format_Info_C);

            raise;
    end Physical_Device_Video_Format_Properties_Count;
    
    function Get_Physical_Device_Video_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Format_Info: in Physical_Device_Video_Format_Info;
         Video_Format_Properties: in out Video_Format_Properties_Vectors.Vector)
        return Result is
        Video_Format_Info_C:
            Video_Queues_C.Physical_Device_Video_Format_Info_C :=
                Video_Queues_C.To_C(Video_Format_Info);
        Count: Interfaces.Unsigned_32 := 0;
        Result: Vulkan.Result;
    begin
        Result := vkGetPhysicalDeviceVideoFormatPropertiesKHR
            (Physical_Device, Video_Format_Info_C, Count, null);

        if Result not in Success | Incomplete then
            Video_Queues_C.Free(Video_Format_Info_C);

            return Result;
        end if;

        declare
            use type Interfaces.Unsigned_32;

            C_Properties: array (1 .. Count) of aliased
                Video_Queues_C.Video_Format_Properties_C
                with Convention => C;
            Index: Interfaces.Unsigned_32 := 1;
        begin
            for P of Video_Format_Properties loop
                C_Properties(Index).Next := Extension_Records.To_C(P.Next);
                Index := Index + 1;
            end loop;

            Result := vkGetPhysicalDeviceVideoFormatPropertiesKHR
                (Physical_Device,
                 Video_Format_Info_C,
                 Count,
                 C_Properties(1)'Unchecked_Access);

            Video_Queues_C.Free(Video_Format_Info_C);

            if Result not in Success | Incomplete then
                for CP of C_Properties loop
                    Extension_Records.Free(CP.Next);
                end loop;

                return Result;
            end if;

            Index := 1;

            for P of Video_Format_Properties loop
                Video_Queues_C.To_Ada(P, C_Properties(Index));
                Extension_Records.Free(C_Properties(Index).Next);
                Index := Index + 1;
            end loop;
        end;

        return Result;
    end Get_Physical_Device_Video_Format_Properties;

    function Get_Physical_Device_Video_Format_Properties
        (Physical_Device: in Vulkan.Physical_Device;
         Video_Format_Info: in Physical_Device_Video_Format_Info)
        return Video_Format_Properties_Vectors.Vector is
        Properties: Video_Format_Properties_Vectors.Vector;
        Item: Video_Format_Properties;
        Count: Interfaces.Unsigned_32;
    begin
        Count := Physical_Device_Video_Format_Properties_Count
            (Physical_Device, Video_Format_Info);
        Properties.Append(Item, Ada.Containers.Count_Type(Count));
        Exceptions.Check(Get_Physical_Device_Video_Format_Properties
            (Physical_Device, Video_Format_Info, Properties));

        return Properties;
    end Get_Physical_Device_Video_Format_Properties;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Video_Session: out Vulkan.Video_Session) return Result
        renames Video_Sessions_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Video_Session renames Video_Sessions_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Create_Info;
                    Video_Session: out Vulkan.Video_Session) return Result
        renames Video_Sessions_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Create_Info)
        return Video_Session renames Video_Sessions_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Video_Session: in out Vulkan.Video_Session;
                      Allocator: aliased in Allocation_Callbacks)
        renames Video_Sessions_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Video_Session: in out Vulkan.Video_Session)
        renames Video_Sessions_Common.Destroy;

    function Video_Session_Memory_Requirements_Count
        (Device: in Vulkan.Device;
         Video_Session: in Vulkan.Video_Session)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(vkGetVideoSessionMemoryRequirementsKHR(Device,
                                                                Video_Session,
                                                                Count,
                                                                null));
        
        return Count;
    end Video_Session_Memory_Requirements_Count;

    function Get_Video_Session_Memory_Requirements
        (Device: in Vulkan.Device;
         Video_Session: in Vulkan.Video_Session;
         Requirements: in out Video_Session_Memory_Requirements_Vectors.Vector)
        return Result is
        Count: Interfaces.Unsigned_32;
        Result: Vulkan.Result;
    begin
        Count := Video_Session_Memory_Requirements_Count(Device, Video_Session);
        
        declare
            use type Interfaces.Unsigned_32;

            C_Requirements: array (1 .. Count) of aliased
                Video_Queues_C.Video_Session_Memory_Requirements_C
                with Convention => C;
            Index: Interfaces.Unsigned_32 := 1;
        begin
            for R of Requirements loop
                C_Requirements(Index).Next := Extension_Records.To_C(R.Next);
                Index := Index + 1;
            end loop;

            Result := vkGetVideoSessionMemoryRequirementsKHR
                (Device,
                 Video_Session,
                 Count,
                 C_Requirements(1)'Unchecked_Access);
            
            if Result not in Success | Incomplete then
                for CR of C_Requirements loop
                    Extension_Records.Free(CR.Next);
                end loop;

                return Result;
            end if;

            Index := 1;

            for R of Requirements loop
                Video_Queues_C.To_Ada(R, C_Requirements(Index));
                Extension_Records.Free(C_Requirements(Index).Next);
                Index := Index + 1;
            end loop;
        end;

        return Result;
    end Get_Video_Session_Memory_Requirements;

    function Get_Video_Session_Memory_Requirements
        (Device: in Vulkan.Device;
         Video_Session: in Vulkan.Video_Session)
        return Video_Session_Memory_Requirements_Vectors.Vector is
        Requirements: Video_Session_Memory_Requirements_Vectors.Vector;
        Item: Video_Session_Memory_Requirements;
        Count: Interfaces.Unsigned_32;
    begin
        Count := Video_Session_Memory_Requirements_Count(Device, Video_Session);
        Requirements.Append(Item, Ada.Containers.Count_Type(Count));
        Exceptions.Check(Get_Video_Session_Memory_Requirements(Device,
                                                               Video_Session,
                                                               Requirements));

        return Requirements;
    end Get_Video_Session_Memory_Requirements;
    
    function Bind(Device: in Vulkan.Device;
                  Video_Session: in Vulkan.Video_Session;
                  Bind_Session_Memory_Infos:
                    in Bind_Video_Session_Memory_Info_Vectors.Vector)
        return Result is
        C_Infos: array (1 .. Positive(Bind_Session_Memory_Infos.Length)) of
                    aliased Video_Queues_C.Bind_Video_Session_Memory_Info_C;
        Result: Vulkan.Result;
    begin
        for X in C_Infos'Range loop
            C_Infos(X) := Video_Queues_C.To_C(Bind_Session_Memory_Infos(X));
        end loop;

        Result := vkBindVideoSessionMemoryKHR
            (Device,
             Video_Session,
             Interfaces.Unsigned_32(C_Infos'Length),
             C_Infos(1)'Unchecked_Access);

        for CI of C_Infos loop
            Video_Queues_C.Free(CI);
        end loop;

        return Result;
    end Bind;
    
    procedure Bind(Device: in Vulkan.Device;
                   Video_Session: in Vulkan.Video_Session;
                   Bind_Session_Memory_Infos:
                    in Bind_Video_Session_Memory_Info_Vectors.Vector) is
    begin
        Exceptions.Check(Bind(Device,
                              Video_Session,
                              Bind_Session_Memory_Infos));
    end Bind;

    function Bind(Device: in Vulkan.Device;
                  Video_Session: in Vulkan.Video_Session;
                  Bind_Session_Memory_Info: in Bind_Video_Session_Memory_Info)
        return Result is
    begin
        return Bind(Device,
                    Video_Session,
                    Bind_Video_Session_Memory_Info_Vectors.To_Vector
                        (Bind_Session_Memory_Info, 1));
    end Bind;
    
    procedure Bind
        (Device: in Vulkan.Device;
         Video_Session: in Vulkan.Video_Session;
         Bind_Session_Memory_Info: in Bind_Video_Session_Memory_Info) is
    begin
        Exceptions.Check(Bind(Device, Video_Session, Bind_Session_Memory_Info));
    end Bind;

    function Create
        (Device: in Vulkan.Device;
         Create_Info: in Video_Session_Parameters_Create_Info;
         Allocator: aliased in Allocation_Callbacks;
         Video_Session_Parameters: out Vulkan.Video_Session_Parameters)
        return Result renames Video_Session_Parameters_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Parameters_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Video_Session_Parameters
        renames Video_Session_Parameters_Common.Create;

    function Create
        (Device: in Vulkan.Device;
         Create_Info: in Video_Session_Parameters_Create_Info;
         Video_Session_Parameters: out Vulkan.Video_Session_Parameters)
        return Result renames Video_Session_Parameters_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Video_Session_Parameters_Create_Info)
        return Video_Session_Parameters
        renames Video_Session_Parameters_Common.Create;
    
    function Update
        (Device: in Vulkan.Device;
         Video_Session_Parameters: in Vulkan.Video_Session_Parameters;
         Update_Info: in Video_Session_Parameters_Update_Info) return Result is
        Update_Info_C: Video_Queues_C.Video_Session_Parameters_Update_Info_C :=
            Video_Queues_C.To_C(Update_Info);
        Result: Vulkan.Result;
    begin
        Result := vkUpdateVideoSessionParametersKHR(Device,
                                                    Video_Session_Parameters,
                                                    Update_Info_C);
        Video_Queues_C.Free(Update_Info_C);

        return Result;
    end Update;
    
    procedure Update
        (Device: in Vulkan.Device;
         Video_Session_Parameters: in Vulkan.Video_Session_Parameters;
         Update_Info: in Video_Session_Parameters_Update_Info) is
    begin
        Exceptions.Check(Update(Device, Video_Session_Parameters, Update_Info));
    end Update;

    procedure Destroy
        (Device: in Vulkan.Device;
         Video_Session_Parameters: in out Vulkan.Video_Session_Parameters;
         Allocator: aliased in Allocation_Callbacks)
        renames Video_Session_Parameters_Common.Destroy;

    procedure Destroy
        (Device: in Vulkan.Device;
         Video_Session_Parameters: in out Vulkan.Video_Session_Parameters)
        renames Video_Session_Parameters_Common.Destroy;
    
    procedure Begin_Video_Coding(Command_Buffer: in Vulkan.Command_Buffer;
                                 Begin_Info: in Video_Begin_Coding_Info) is
        Begin_Info_C: Video_Queues_C.Video_Begin_Coding_Info_C :=
            Video_Queues_C.To_C(Begin_Info);
    begin
        vkCmdBeginVideoCodingKHR(Command_Buffer, Begin_Info_C);
        Video_Queues_C.Free(Begin_Info_C);
    end Begin_Video_Coding;

    procedure End_Video_Coding(Command_Buffer: in Vulkan.Command_Buffer;
                               End_Coding_Info: in Video_End_Coding_Info) is
        End_Coding_Info_C: Video_Queues_C.Video_End_Coding_Info_C :=
            Video_Queues_C.To_C(End_Coding_Info);
    begin
        vkCmdEndVideoCodingKHR(Command_Buffer, End_Coding_Info_C);
        Video_Queues_C.Free(End_Coding_Info_C);
    end End_Video_Coding;

    procedure Control_Video_Coding
        (Command_Buffer: in Vulkan.Command_Buffer;
         Coding_Control_Info: in Video_Coding_Control_Info) is
        Coding_Control_Info_C: Video_Queues_C.Video_Coding_Control_Info_C :=
            Video_Queues_C.To_C(Coding_Control_Info);
    begin
        vkCmdControlVideoCodingKHR(Command_Buffer, Coding_Control_Info_C);
        Video_Queues_C.Free(Coding_Control_Info_C);
    end Control_Video_Coding;
end Vulkan.Video_Queues;


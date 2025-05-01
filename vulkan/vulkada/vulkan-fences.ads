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

-- Fence related subprograms

private with Vulkan.Objects_Common;
private with Vulkan.C;

package Vulkan.Fences is
    -- vkCreateFence
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Fence_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Fence: out Vulkan.Fence) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Fence /= No_Fence);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Fence_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Fence
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Fence;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Fence_Create_Info;
                    Fence: out Vulkan.Fence) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then Fence /= No_Fence);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Fence_Create_Info) return Fence
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Fence;

    -- vkDestroyFence
    procedure Destroy(Device: in Vulkan.Device;
                      Fence: in out Vulkan.Fence;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Fence = No_Fence;

    procedure Destroy(Device: in Vulkan.Device;
                      Fence: in out Vulkan.Fence)
        with Inline,
             Pre => Device /= No_Device,
             Post => Fence = No_Fence;

    -- vkResetFences
    function Reset(Device: in Vulkan.Device;
                   Fences: in Fence_Vectors.Vector) return Result
        with Inline,
             Pre => Device /= No_Device and
                    (for all Fence of Fences => Fence /= No_Fence),
             Post => Reset'Result in Success |
                                     Out_Of_Device_Memory;

    procedure Reset(Device: in Vulkan.Device;
                    Fences: in Fence_Vectors.Vector)
        with Inline,
             Pre => Device /= No_Device and
                    (for all Fence of Fences => Fence /= No_Fence);

    function Reset(Device: in Vulkan.Device;
                   Fence: in Vulkan.Fence) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Fence /= No_Fence,
             Post => Reset'Result in Success |
                                     Out_Of_Device_Memory;

    procedure Reset(Device: in Vulkan.Device;
                    Fence: in Vulkan.Fence)
        with Inline,
             Pre => Device /= No_Device and
                    Fence /= No_Fence;

    -- vkGetFenceStatus
    function Get_Status(Device: in Vulkan.Device;
                        Fence: in Vulkan.Fence) return Result
        with Inline,
             Pre => Device /= No_Device and
                    Fence /= No_Fence,
             Post => Get_Status'Result in Success |
                                          Not_Ready |
                                          Out_Of_Host_Memory |
                                          Out_Of_Device_Memory |
                                          Device_Lost;

    -- vkWaitForFences
    function Wait(Device: in Vulkan.Device;
                  Fences: in Fence_Vectors.Vector;
                  Wait_All: in Boolean;
                  Timeout: in Interfaces.Unsigned_64) return Result
        with Inline,
             Pre => Device /= No_Device and
                    not Fences.Is_Empty and
                    (for all Fence of Fences => Fence /= No_Fence),
             Post => Wait'Result in Success |
                                    Vulkan.Timeout |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Device_Lost;

    function Wait(Device: in Vulkan.Device;
                  Fence: in Vulkan.Fence;
                  Timeout: in Interfaces.Unsigned_64) return Result
        with Pre => Device /= No_Device and
                    Fence /= No_Fence,
             Post => Wait'Result in Success |
                                    Vulkan.Timeout |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Device_Lost;

private
    package Fences_Common is
        new Objects_Common(Fence_Create_Info,
                           C.Fence_Create_Info_C,
                           Fence,
                           No_Fence,
                           C.To_C,
                           C.Free,
                           C.vkCreateFence,
                           C.vkDestroyFence);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Fence_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Fence: out Vulkan.Fence) return Result
        renames Fences_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Fence_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Fence
        renames Fences_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Fence_Create_Info;
                    Fence: out Vulkan.Fence) return Result
        renames Fences_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Fence_Create_Info) return Fence
        renames Fences_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Fence: in out Vulkan.Fence;
                      Allocator: aliased in Allocation_Callbacks)
        renames Fences_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Fence: in out Vulkan.Fence)
        renames Fences_Common.Destroy;
    
    function Get_Status(Device: in Vulkan.Device;
                        Fence: in Vulkan.Fence) return Result
        renames C.vkGetFenceStatus;
end Vulkan.Fences;


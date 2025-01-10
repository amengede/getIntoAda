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

-- Queue related subprograms

private with Vulkan.C;

package Vulkan.Queues is
    use type Ada.Containers.Count_Type;

    -- vkGetDeviceQueue
    function Get(Device: in Vulkan.Device;
                 Family_Index: in Queue_Family_Index;
                 Index: in Interfaces.Unsigned_32) return Queue
        with Inline,
             Pre => Device /= No_Device;

    -- vkQueueSubmit
    function Submit(Queue: in Vulkan.Queue;
                    Submits: in Submit_Info_Vectors.Vector;
                    Fence: in Vulkan.Fence := No_Fence) return Result
        with Pre => Queue /= No_Queue and
                    (for all Submit of Submits =>
                        Submit.Wait_Semaphores.Length =
                        Submit.Wait_Dst_Stage_Mask.Length and
                        (for all Semaphore of Submit.Wait_Semaphores =>
                            Semaphore /= No_Semaphore) and
                        (for all Buffer of Submit.Command_Buffers =>
                            Buffer /= No_Command_Buffer) and
                        (for all Semaphore of Submit.Signal_Semaphores =>
                            Semaphore /= No_Semaphore)),
             Post => Submit'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Device_Lost;

    procedure Submit(Queue: in Vulkan.Queue;
                     Submits: in Submit_Info_Vectors.Vector;
                     Fence: in Vulkan.Fence := No_Fence)
        with Inline,
             Pre => Queue /= No_Queue and
                    (for all Submit of Submits =>
                        Submit.Wait_Semaphores.Length =
                        Submit.Wait_Dst_Stage_Mask.Length and
                        (for all Semaphore of Submit.Wait_Semaphores =>
                            Semaphore /= No_Semaphore) and
                        (for all Buffer of Submit.Command_Buffers =>
                            Buffer /= No_Command_Buffer) and
                        (for all Semaphore of Submit.Signal_Semaphores =>
                            Semaphore /= No_Semaphore));

    function Submit(Queue: in Vulkan.Queue;
                    Submit_Info: in Vulkan.Submit_Info;
                    Fence: in Vulkan.Fence := No_Fence) return Result
        with Pre => Queue /= No_Queue and
                    Submit_Info.Wait_Semaphores.Length =
                    Submit_Info.Wait_Dst_Stage_Mask.Length and
                    (for all Semaphore of Submit_Info.Wait_Semaphores =>
                        Semaphore /= No_Semaphore) and
                    (for all Buffer of Submit_Info.Command_Buffers =>
                        Buffer /= No_Command_Buffer) and
                    (for all Semaphore of Submit_Info.Signal_Semaphores =>
                        Semaphore /= No_Semaphore),
             Post => Submit'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Device_Lost;

    procedure Submit(Queue: in Vulkan.Queue;
                     Submit_Info: in Vulkan.Submit_Info;
                     Fence: in Vulkan.Fence := No_Fence)
        with Inline,
             Pre => Queue /= No_Queue and
                    Submit_Info.Wait_Semaphores.Length =
                    Submit_Info.Wait_Dst_Stage_Mask.Length and
                    (for all Semaphore of Submit_Info.Wait_Semaphores =>
                        Semaphore /= No_Semaphore) and
                    (for all Buffer of Submit_Info.Command_Buffers =>
                        Buffer /= No_Command_Buffer) and
                    (for all Semaphore of Submit_Info.Signal_Semaphores =>
                        Semaphore /= No_Semaphore);

    function Submit(Queue: in Vulkan.Queue;
                    Fence: in Vulkan.Fence := No_Fence) return Result
        with Inline,
             Pre => Queue /= No_Queue,
             Post => Submit'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Device_Lost;

    procedure Submit(Queue: in Vulkan.Queue;
                     Fence: in Vulkan.Fence := No_Fence)
        with Inline,
             Pre => Queue /= No_Queue;

    -- vkQueueWaitIdle
    function Wait_Idle(Queue: in Vulkan.Queue) return Result
        with Pre => Queue /= No_Queue,
             Post => Wait_Idle'Result in Success |
                                         Out_Of_Host_Memory |
                                         Out_Of_Device_Memory |
                                         Device_Lost;

    procedure Wait_Idle(Queue: in Vulkan.Queue)
        with Inline,
             Pre => Queue /= No_Queue;

    -- vkQueueBindSparse
    function Bind(Queue: in Vulkan.Queue;
                  Bind_Info: in Bind_Sparse_Info_Vectors.Vector;
                  Fence: in Vulkan.Fence := No_Fence) return Result
        with Pre => Queue /= No_Queue,
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Device_Lost;

    procedure Bind(Queue: in Vulkan.Queue;
                   Bind_Info: in Bind_Sparse_Info_Vectors.Vector;
                   Fence: in Vulkan.Fence := No_Fence)
        with Inline,
             Pre => Queue /= No_Queue;

    function Bind(Queue: in Vulkan.Queue;
                  Bind_Info: in Bind_Sparse_Info;
                  Fence: in Vulkan.Fence := No_Fence) return Result
        with Pre => Queue /= No_Queue,
             Post => Bind'Result in Success |
                                    Out_Of_Host_Memory |
                                    Out_Of_Device_Memory |
                                    Device_Lost;

    procedure Bind(Queue: in Vulkan.Queue;
                   Bind_Info: in Bind_Sparse_Info;
                   Fence: in Vulkan.Fence := No_Fence)
        with Inline,
             Pre => Queue /= No_Queue;

    -- Vulkan 1.1
    -- vkGetDeviceQueue2
    function Get(Device: in Vulkan.Device;
                 Queue_Info: in Device_Queue_Info_2) return Queue
        with Pre => Device /= No_Device;

    -- Vulkan 1.3
    -- vkQueueSubmit2
    function Submit(Queue: in Vulkan.Queue;
                    Submits: in Submit_Info_2_Vectors.Vector;
                    Fence: in Vulkan.Fence := No_Fence) return Result
        with Pre => Queue /= No_Queue and
                    (for all Submit of Submits =>
                        (for all Info of Submit.Wait_Semaphore_Infos =>
                            Info.Semaphore /= No_Semaphore) and
                        (for all Info of Submit.Command_Buffer_Infos =>
                            Info.Command_Buffer /= No_Command_Buffer) and
                        (for all Info of Submit.Signal_Semaphore_Infos =>
                            Info.Semaphore /= No_Semaphore)),
             Post => Submit'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Device_Lost;

    procedure Submit(Queue: in Vulkan.Queue;
                     Submits: in Submit_Info_2_Vectors.Vector;
                     Fence: in Vulkan.Fence := No_Fence)
        with Inline,
             Pre => Queue /= No_Queue and
                    (for all Submit of Submits =>
                        (for all Info of Submit.Wait_Semaphore_Infos =>
                            Info.Semaphore /= No_Semaphore) and
                        (for all Info of Submit.Command_Buffer_Infos =>
                            Info.Command_Buffer /= No_Command_Buffer) and
                        (for all Info of Submit.Signal_Semaphore_Infos =>
                            Info.Semaphore /= No_Semaphore));

    function Submit(Queue: in Vulkan.Queue;
                    Submit_Info: in Submit_Info_2;
                    Fence: in Vulkan.Fence := No_Fence) return Result
        with Inline,
             Pre => Queue /= No_Queue and
                    (for all Info of Submit_Info.Wait_Semaphore_Infos =>
                        Info.Semaphore /= No_Semaphore) and
                    (for all Info of Submit_Info.Command_Buffer_Infos =>
                        Info.Command_Buffer /= No_Command_Buffer) and
                    (for all Info of Submit_Info.Signal_Semaphore_Infos =>
                        Info.Semaphore /= No_Semaphore),
             Post => Submit'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Device_Lost;

    procedure Submit(Queue: in Vulkan.Queue;
                     Submit_Info: in Submit_Info_2;
                     Fence: in Vulkan.Fence := No_Fence)
        with Inline,
             Pre => Queue /= No_Queue and
                    (for all Info of Submit_Info.Wait_Semaphore_Infos =>
                        Info.Semaphore /= No_Semaphore) and
                    (for all Info of Submit_Info.Command_Buffer_Infos =>
                        Info.Command_Buffer /= No_Command_Buffer) and
                    (for all Info of Submit_Info.Signal_Semaphore_Infos =>
                        Info.Semaphore /= No_Semaphore);

private
    function Wait_Idle(Queue: in Vulkan.Queue) return Result
        renames C.vkQueueWaitIdle;
end Vulkan.Queues;


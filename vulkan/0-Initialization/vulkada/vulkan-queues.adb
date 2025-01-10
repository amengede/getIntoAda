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

with Vulkan.Exceptions;
with Vulkan.C_V1_1;
with Vulkan.C_V1_3;

package body Vulkan.Queues is
    function Get(Device: in Vulkan.Device;
                 Family_Index: in Queue_Family_Index;
                 Index: in Interfaces.Unsigned_32) return Queue is
        New_Queue: Queue := No_Queue;
    begin
        C.vkGetDeviceQueue(Device, Family_Index, Index, New_Queue);

        return New_Queue;
    end Get;

    function Submit(Queue: in Vulkan.Queue;
                    Submits: in Submit_Info_Vectors.Vector;
                    Fence: in Vulkan.Fence := No_Fence) return Result is
        C_Submits: array (1 .. Positive(Submits.Length)) of aliased C.Submit_Info_C
            with Convention => C;
        Result: Vulkan.Result;
    begin
        for X in C_Submits'Range loop
            C_Submits(X) := C.To_C(Submits(X));
        end loop;

        Result := C.vkQueueSubmit(Queue,
                                  C_Submits'Length,
                                  C_Submits(1)'Access,
                                  Fence);

        for Submit of C_Submits loop
            C.Free(Submit);
        end loop;

        return Result;
    end Submit;

    procedure Submit(Queue: in Vulkan.Queue;
                     Submits: in Submit_Info_Vectors.Vector;
                     Fence: in Vulkan.Fence := No_Fence) is
    begin
        Exceptions.Check(Submit(Queue, Submits, Fence));
    end Submit;

    function Submit(Queue: in Vulkan.Queue;
                    Submit_Info: in Vulkan.Submit_Info;
                    Fence: in Vulkan.Fence := No_Fence) return Result is
        C_Submit: aliased C.Submit_Info_C := C.To_C(Submit_Info);
        Result: Vulkan.Result;
    begin
        Result := C.vkQueueSubmit(Queue, 1, C_Submit'Access, Fence);
        C.Free(C_Submit);

        return Result;
    end Submit;
                                  
    procedure Submit(Queue: in Vulkan.Queue;
                     Submit_Info: in Vulkan.Submit_Info;
                     Fence: in Vulkan.Fence := No_Fence) is
    begin
        Exceptions.Check(Submit(Queue, Submit_Info, Fence));
    end Submit;

    function Submit(Queue: in Vulkan.Queue;
                    Fence: in Vulkan.Fence := No_Fence) return Result is
    begin
        return C.vkQueueSubmit(Queue, 0, null, Fence);
    end Submit;

    procedure Submit(Queue: in Vulkan.Queue;
                     Fence: in Vulkan.Fence := No_Fence) is
    begin
        Exceptions.Check(Submit(Queue, Fence));
    end Submit;

    procedure Wait_Idle(Queue: in Vulkan.Queue) is
    begin
        Exceptions.Check(Wait_Idle(Queue));
    end Wait_Idle;

    function Bind(Queue: in Vulkan.Queue;
                  Bind_Info: in Bind_Sparse_Info_Vectors.Vector;
                  Fence: in Vulkan.Fence := No_Fence) return Result is
        Result: Vulkan.Result;
        Bind_Info_C: C.Bind_Sparse_Info_Array(1 .. Natural(Bind_Info.Length));
        Index: Natural := 1;
    begin
        for Bind of Bind_Info loop
            Bind_Info_C(Index) := C.To_C(Bind);
        end loop;

        Result := C.vkQueueBindSparse(Queue,
                                      Interfaces.Unsigned_32(Bind_Info.Length),
                                      Bind_Info_C(1)'Access,
                                      Fence);

        for Bind of Bind_Info_C loop
            C.Free(Bind);
        end loop;

        return Result;
    end Bind;

    procedure Bind(Queue: in Vulkan.Queue;
                   Bind_Info: in Bind_Sparse_Info_Vectors.Vector;
                   Fence: in Vulkan.Fence := No_Fence) is
    begin
        Exceptions.Check(Bind(Queue, Bind_Info, Fence));
    end Bind;

    function Bind(Queue: in Vulkan.Queue;
                  Bind_Info: in Bind_Sparse_Info;
                  Fence: in Vulkan.Fence := No_Fence) return Result is
        Bind_Info_C: aliased C.Bind_Sparse_Info_C := C.To_C(Bind_Info);
        Result: Vulkan.Result;
    begin
        Result := C.vkQueueBindSparse(Queue,
                                      1,
                                      Bind_Info_C'Access,
                                      Fence);
        C.Free(Bind_Info_C);

        return Result;
    end Bind;

    procedure Bind(Queue: in Vulkan.Queue;
                   Bind_Info: in Bind_Sparse_Info;
                   Fence: in Vulkan.Fence := No_Fence) is
    begin
        Exceptions.Check(Bind(Queue, Bind_Info, Fence));
    end Bind;

    function Get(Device: in Vulkan.Device;
                 Queue_Info: in Device_Queue_Info_2) return Queue is
        C_Queue_Info: C_V1_1.Device_Queue_Info_2_C := C_V1_1.To_C(Queue_Info);
        Queue: Vulkan.Queue;
    begin
        C_V1_1.vkGetDeviceQueue2(Device, C_Queue_Info, Queue);
        C_V1_1.Free(C_Queue_Info);

        return Queue;
    end Get;

    function Submit(Queue: in Vulkan.Queue;
                    Submits: in Submit_Info_2_Vectors.Vector;
                    Fence: in Vulkan.Fence := No_Fence) return Result is
    begin
        if Submits.Is_Empty then
            return C_V1_3.vkQueueSubmit2(Queue, 0, null, Fence);
        end if;

        declare
            C_Submits: array (1 .. Positive(Submits.Length)) of
                aliased C_V1_3.Submit_Info_2_C;
            Result: Vulkan.Result;
        begin
            for X in C_Submits'Range loop
                C_Submits(X) := C_V1_3.To_C(Submits(X));
            end loop;

            Result := C_V1_3.vkQueueSubmit2(Queue,
                                            C_Submits'Length, 
                                            C_Submits(1)'Access,
                                            Fence);

            for Submit of C_Submits loop
                C_V1_3.Free(Submit);
            end loop;

            return Result;
        end;
    end Submit;

    procedure Submit(Queue: in Vulkan.Queue;
                     Submits: in Submit_Info_2_Vectors.Vector;
                     Fence: in Vulkan.Fence := No_Fence) is
    begin
        Exceptions.Check(Submit(Queue, Submits, Fence));
    end Submit;
    
    function Submit(Queue: in Vulkan.Queue;
                    Submit_Info: in Submit_Info_2;
                    Fence: in Vulkan.Fence := No_Fence) return Result is
    begin
        return Submit(Queue,
                      Submit_Info_2_Vectors.To_Vector(Submit_Info, 1),
                      Fence);
    end Submit;

    procedure Submit(Queue: in Vulkan.Queue;
                     Submit_Info: in Submit_Info_2;
                     Fence: in Vulkan.Fence := No_Fence) is
    begin
        Exceptions.Check(Submit(Queue, Submit_Info, Fence));
    end Submit;
end Vulkan.Queues;


with Queues;

package Constants is
    -- Screen Width
    Width : constant := 640;
    Width_Float : constant := 640.0;
    -- Screen Height
    Height : constant := 480;
    Height_Float : constant := 480.0;

    type Command_Buffer_Index is mod 2 ** 20;

    package Work_Queues is new Queues (T => Command_Buffer_Index);
    Work_Queue : Work_Queues.Queue;

    Worker_Thread_Count : constant Integer := 4;
end Constants;

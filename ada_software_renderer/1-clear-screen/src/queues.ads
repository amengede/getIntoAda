generic
    type T is private;
package Queues is
    type Queue_Index is mod 16;
    type Queue_Data is array (Queue_Index) of T;

    protected type Queue is
        entry Insert (Item : in T; Succeeded : out Boolean);
        entry Remove (Item : out T; Succeeded : out Boolean);
        private
            Items : Queue_Data;
            Read_Pos : Queue_Index := 0;
            Write_Pos : Queue_Index := 0;
        end Queue;

end Queues;

with Ada.Text_IO; use Ada.Text_IO;

procedure Threading is

   type Queue_Index is mod 16;
   type Queue_Data is array (Queue_Index) of Integer;

   protected type Queue is
   entry Insert (Item : in Integer; Succeeded : out Boolean);
   entry Remove (Item : out Integer; Succeeded : out Boolean);
   private
      Items : Queue_Data;
      Read_Pos : Queue_Index := 0;
      Write_Pos : Queue_Index := 0;
   end Queue;

   protected body Queue is
   entry Insert (Item : in Integer; Succeeded : out Boolean)
      when True is
   begin
      if (Write_Pos + 1) = Read_Pos then
         Succeeded := False;
         return;
      end if;

      Items (Write_Pos) := Item;
      Write_Pos := Write_Pos + 1;
      Succeeded := True;
   end Insert;

   entry Remove (Item : out Integer; Succeeded : out Boolean)
      when True is
         begin
            if Write_Pos = Read_Pos then
               Succeeded := False;
               return;
            end if;
            Item := Items (Read_Pos);
            Read_Pos := Read_Pos + 1;
            Succeeded := True;
         end Remove;

   end Queue;

   type Queue_Ptr is access Queue;

   Work_Queue : Queue;

   Worker_Thread_Count : constant Integer := 4;
   type Thread_Flags is array (1 .. Worker_Thread_Count) of Boolean
       with Volatile_Components;
   type Status_Ptr is access all Thread_Flags;

   Thread_Running : Thread_Flags := [False, False, False, False];
   Job_Running : Boolean := False with Volatile;
   Program_Running : Boolean := True with Volatile;

   task type Worker_Thread is
      entry Run (Id : Integer);
   end Worker_Thread;

   task body Worker_Thread is
      My_Id : Integer;
      Current_Job : Integer;
      Got_Work : Boolean;
   begin
      accept Run (Id : Integer) do
         My_Id := Id;
      end;
      Put_Line ("Thread " & My_ID'Image & " spawned.");
      while (Program_Running) loop
         -- program is running, wait for a job
         while (Job_Running) loop
            -- job is running, try to find a task to complete
            Work_Queue.Remove (Current_Job, Got_Work);
            Thread_Running (My_Id) := Got_Work;
            if Got_Work = False then
               goto Continue;
            end if;
            -- Got a job! Run it!
            Put_Line ("Job " & Current_Job'Image
                      & " completed by thread " & My_Id'Image);
            Flush;
            <<Continue>>
         end loop;
      end loop;
      Put_Line ("Thread " & My_ID'Image & " finished.");
   end Worker_Thread;

   procedure Wait is
   begin
       while (Thread_Running (1) = True or
              Thread_Running (2) = True or
              Thread_Running (3) = True or
              Thread_Running (4) = True) loop
           null;
       end loop;
   end Wait;

   Worker_Threads : array (1 .. Worker_Thread_Count) of Worker_Thread;
   Job_Enqueued : Boolean := False;
   Ticks : Queue_Index := 0;

begin

   -- Launch Worker threads
   for I in 1 .. Worker_Thread_Count loop
      Worker_Threads (I).Run (Id => I);
   end loop;

   -- Tell Worker threads to start a job
   Job_Running := True;

   -- Enqueue a whole lot of jobs
   for I in 1 .. 32 loop
      Job_Enqueued := False;
      while Job_Enqueued = False loop
         Work_Queue.Insert (I, Job_Enqueued);
      end loop;
   end loop;

   -- Wait for all threads to be done
   Wait;

   -- Tell Worker threads job is over
   Job_Running := False;

   Flush;
   Put_Line ("---- Starting another batch of work ----");
   Flush;

   -- Tell Worker threads to start a job
   Job_Running := True;

   -- Enqueue a whole lot of jobs
   for I in 64 .. 96 loop
      Job_Enqueued := False;
      while Job_Enqueued = False loop
         -- Put_Line ("Try to queue up job" & I'Image);
         Work_Queue.Insert (I, Job_Enqueued);
      end loop;
   end loop;

   -- Wait for all threads to be done
   Wait;

   -- Tell Worker threads job is over
   Job_Running := False;

   -- Tell Worker threads the program is over
   Program_Running := False;
end Threading;

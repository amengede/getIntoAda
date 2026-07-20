with Ada.Text_IO; use Ada.Text_IO;

package body Clocks is

    procedure Initialize (Self : in out Clock) is
    begin
        Self.Last_Time := Ticks;
    end Initialize;

    procedure Tick (Self : in out Clock; Frame_Rate : Natural) is
        Ellapsed : constant Milliseconds_Long := Ticks - Self.Last_Time;
        Frame_Time : constant Milliseconds_Long :=
            1000 / Milliseconds_Long (Frame_Rate);
        Required_Delay : constant Milliseconds_Long := Frame_Time - Ellapsed;
    begin
        if Required_Delay > 0 and Required_Delay <= Frame_Time then
            --  Put_Line ("Waiting for " & Required_Delay'Image & " ms.");
            Wait_Delay (Required_Delay);
        end if;

        Self.Last_Time := Ticks;
    end Tick;

end Clocks;

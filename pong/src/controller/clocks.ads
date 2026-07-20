with SDL.Timers; use SDL.Timers;

package Clocks is
    type Clock is tagged private;
    --  @brief A clock which can enforce a framerate limit

    procedure Initialize (Self : in out Clock);
    --  @brief Start the clock
    --  @param Self clock to start

    procedure Tick (Self : in out Clock; Frame_Rate : Natural);
    --  @brief Update the clock, waiting if necessary to enforce the framerate
    --  @param Self clock to update
    --  @param Frame_Rate Framerate to enforce
private
    type Clock is tagged record
        Last_Time : Milliseconds_Long := 0;
    end record;
end Clocks;

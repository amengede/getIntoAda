with Clocks;
with SDL.Video.Windows;

package Controllers is
    type Controller is abstract tagged limited
        record
            Clock : Clocks.Clock;
        end record;

    procedure Start (Self : in out Controller;
                     Window : SDL.Video.Windows.Window) is abstract;
end Controllers;

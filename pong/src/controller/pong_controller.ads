with Controllers;
with Pong_Model;
with Pong_View;
with SDL.Video.Windows;

package Pong_Controller is
    type Controller is new Controllers.Controller with private;

    procedure Initialize (Self : in out Controller;
                          Window : SDL.Video.Windows.Window);

    overriding
    procedure Start (Self : in out Controller;
                    Window : SDL.Video.Windows.Window);
private
    type Controller is new Controllers.Controller with
        record
            Model : Pong_Model.Model;
            View : Pong_View.View;
        end record;
end Pong_Controller;

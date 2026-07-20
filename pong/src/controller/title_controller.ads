with Controllers;
with Title_View;
with SDL.Video.Windows;

package Title_Controller is
    type Controller is limited new Controllers.Controller with
        record
            View : Title_View.View;
        end record;

    procedure Initialize (Self : in out Controller;
                          Window : SDL.Video.Windows.Window);

    overriding
    procedure Start (Self : in out Controller;
                    Window : SDL.Video.Windows.Window);

end Title_Controller;

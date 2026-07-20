with SDL.Events.Events;
with SDL.Events.Keyboards;

package body Title_Controller is
    procedure Initialize (Self : in out Controller;
                          Window : SDL.Video.Windows.Window) is
    begin
        Self.View.Initialize (Window => Window);
    end Initialize;

    overriding
    procedure Start (Self : in out Controller;
                    Window : SDL.Video.Windows.Window) is
        Running : Boolean := True;
        Event : SDL.Events.Events.Events;
    begin
        Self.Clock.Initialize;
        while Running loop

            while SDL.Events.Events.Poll (Event) loop
                case Event.Common.Event_Type is
                    when SDL.Events.Quit =>
                        Running := False;
                    when SDL.Events.Keyboards.Key_Down =>
                        Running := False;
                    when others =>
                        null;
                end case;
            end loop;

            Self.View.Update (Window);

            Self.Clock.Tick (60);

        end loop;
    end Start;
end Title_Controller;

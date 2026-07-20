with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Inputs.Keyboards;
with Paddles;
with Ada.Text_IO; use Ada.Text_IO;

package body Pong_Controller is
    procedure Initialize (Self : in out Controller;
                          Window : SDL.Video.Windows.Window) is
    begin
        Self.Model.Initialize;
        Self.View.Initialize (Window => Window);
    end Initialize;

    overriding
    procedure Start (Self : in out Controller;
                     Window : SDL.Video.Windows.Window) is
        use type SDL.Events.Keyboards.Key_Codes;
        Running : Boolean := True;
        Event : SDL.Events.Events.Events;
        Keys : constant SDL.Inputs.Keyboards.Key_State_Access :=
            SDL.Inputs.Keyboards.Get_State;
    begin
        Self.Clock.Initialize;
        while Running loop

            while SDL.Events.Events.Poll (Event) loop
                case Event.Common.Event_Type is
                    when SDL.Events.Quit =>
                        Running := False;
                    when SDL.Events.Keyboards.Key_Down =>
                        if Event.Keyboard.Key_Sym.Key_Code =
                            SDL.Events.Keyboards.Code_Escape
                        then
                            Running := False;
                        end if;
                    when others =>
                        null;
                end case;
            end loop;

            Self.Model.Update;

            if Keys (SDL.Events.Keyboards.Scan_Code_W) then
                Self.Model.Move_Player (Paddles.Player_1, -2);
            end if;
            if Keys (SDL.Events.Keyboards.Scan_Code_S) then
                Self.Model.Move_Player (Paddles.Player_1, 2);
            end if;

            if Keys (SDL.Events.Keyboards.Scan_Code_O) then
                Self.Model.Move_Player (Paddles.Player_2, -2);
            end if;
            if Keys (SDL.Events.Keyboards.Scan_Code_L) then
                Self.Model.Move_Player (Paddles.Player_2, 2);
            end if;

            Self.View.Update (Self.Model.Get_Visible_Set,
                              Self.Model.Get_Scores,
                              Window);

            Self.Clock.Tick (60);

        end loop;
    end Start;
end Pong_Controller;

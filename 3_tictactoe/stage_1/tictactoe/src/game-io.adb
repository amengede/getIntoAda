with Ada.Text_IO; use Ada.Text_IO;

package body Game.IO is

    procedure Display_Board(
        Game_State : Board;
        Current_Player : String) is
    begin
        Put_Line (Horizontal_Line);

        Put_Line ("It is player " & Current_Player & "'s turn.");

        for I in 1 .. 3 loop
            Put_Line (
                Game_State (I, 1) 
                & Vertical_Stroke & Game_State (I, 2) 
                & Vertical_Stroke & Game_State (I, 3));
            
            if I < 3 then
                Put_Line (Board_Line);
            end if;
        end loop;
    end Display_Board;
end Game.IO;
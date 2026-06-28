with Ada.Text_IO; use Ada.Text_IO;

package body Game.IO is

    procedure Display_Board(
        Game_State : Board;
        Current_Player : Game_Piece) is
        Piece : Game_Piece;
    begin
        Put_Line (Horizontal_Line);

        Put_Line ("It is " & Current_Player'Image & "'s turn.");

        for I in 1 .. 3 loop
            for J in 1 .. 3 loop
                Piece := Game_State (I, J);
                if Piece /= Blank then
                    Put (Piece'Image);
                else
                    Put (" ");
                end if;
                if J /= 3 then
                    Put (Vertical_Stroke);
                end if;
            end loop;
            Put_Line ("");
            if I < 3 then
                Put_Line (Board_Line);
            end if;
        end loop;
    end Display_Board;
end Game.IO;

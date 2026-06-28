with Game;
with Game.IO;

procedure Tictactoe is
   use Game;
   use Game.IO;

   Game_State : Board := (
       (Blank, Blank, Blank),
       (Blank, Blank, Blank),
       (Blank, Blank, Blank));
   Current_Player : Game_Piece := O;
   Current_Move : Move;
begin
    while True loop
        Display_Board (Game_State, Current_Player);
        Current_Move.Piece := Current_Player;
        Get_Coordinates (Current_Move);
        Game_State (Current_Move.Row, Current_Move.Column) := Current_Player;

        if Current_Player = O then
            Current_Player := X;
        else
            Current_Player := O;
        end if;
    end loop;
end Tictactoe;

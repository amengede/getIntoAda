with Game;
with Game.IO;

procedure Tictactoe is
   use Game;
   use Game.IO;

   Game_State : constant Board := (
       (Blank, Blank, Blank),
       (Blank, Blank, Blank),
       (Blank, Blank, Blank));
   Current_Player : constant Game_Piece := O;
begin
   Display_Board (Game_State, Current_Player);
end Tictactoe;

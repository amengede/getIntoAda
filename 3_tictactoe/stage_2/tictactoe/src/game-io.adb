with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Game.IO is

    function Valid_Coordinate (
        Letter : Character) return Boolean is
        Test_Digit : Integer;
    begin
        if not Is_Digit (Letter) then
            return False;
        end if;

        Test_Digit := Character'Pos(Letter) - Character'Pos('0');

        return Test_Digit in Integer(Coordinate'First) .. Integer(Coordinate'Last);
    end Valid_Coordinate;

    function Valid_Move (
        Test_Move : String) return Boolean is
    begin
        if Test_Move'Length /= 3 then
            return False;
        end if;

        if not Valid_Coordinate (Test_Move (1)) or 
            not Valid_Coordinate (Test_Move (3)) then
            return False;
        end if;

        return True;
    end Valid_Move;

    procedure Display_Board(
        Game_State : Board;
        Current_Player : Game_Piece) is
        Piece : Game_Piece;
    begin
        Put_Line (Horizontal_Line);

        Put_Line ("It is " & Current_Player'Image & "'s turn.");

        for I in Coordinate'Range loop
            for J in Coordinate'Range loop
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

    procedure Get_Coordinates (
        Pending_Move : in out Move) is
        User_Response : Unbounded_String;
        Row : String := "1";
        Column : String := "1";
    begin
        while True loop
            Put_Line ("Enter your move: ");
            User_Response := To_Unbounded_String (Get_Line);
            exit when Valid_Move (To_String (User_Response));
        end loop;

        Row := To_String (Unbounded_Slice (User_Response, 1, 1));
        Column := To_String (Unbounded_Slice (User_Response, 3, 3));

        Pending_Move.Row := Coordinate'Value (Row);
        Pending_Move.Column := Coordinate'Value (Column);
    end Get_Coordinates;
end Game.IO;

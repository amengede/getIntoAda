package Game.IO is

    --  Constants
    Horizontal_Line : constant String := "------------------------";
    Board_Line : constant String := "-+-+-";
    Vertical_Stroke : constant String := "|";

    --  Procedures
    procedure Display_Board (
        Game_State : Board;
        Current_Player : Game_Piece);

    procedure Get_Coordinates (
        Pending_Move : in out Move);
end Game.IO;

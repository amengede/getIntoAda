package Game is

    --  Types
    type Game_Piece is (X, O, Blank);
    type Board is Array (1 .. 3, 1 .. 3) of Game_Piece;

end Game;

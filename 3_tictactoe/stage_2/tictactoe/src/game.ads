package Game is

    --  Types
    type Coordinate is new Integer range 1 .. 3;
    type Game_Piece is (X, O, Blank);
    type Board is Array (Coordinate, Coordinate) of Game_Piece;
    type Move is record
        Piece: Game_Piece;
        Row, Column: Coordinate;
    end record;

end Game;

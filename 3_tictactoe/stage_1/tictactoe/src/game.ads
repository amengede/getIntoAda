package Game is

    --  Types
    type Board is Array (1 .. 3, 1 .. 3) of String (1 .. 1);

    --  Constants
    Blank : constant String := " ";
    O : constant String := "O";
    X : constant String := "X";

end Game;
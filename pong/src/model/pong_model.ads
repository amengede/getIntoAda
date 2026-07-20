with Rectangles;
with Balls;
with Paddles;
package Pong_Model is
    type Model is tagged private;
    --  @brief Models the objects in a game of Pong

    type Score_Array is array (Paddles.Player_ID) of Natural;

    procedure Initialize (Self : in out Model);
    --  @brief Initialize a new Pong Model
    --  @param Self the model to initialize

    procedure Update (Self : in out Model);
    --  @brief Update a Pong Model
    --  @brief Self the Model to update

    procedure Move_Player (Self : in out Model;
                           Player : Paddles.Player_ID;
                           Dy : Integer);
    --  @brief Move a player in the game
    --  @param Self Model to update
    --  @param Player ID of player to update
    --  @param Dy vertical movement amount

    function Get_Visible_Set (Self : Model)
        return Rectangles.Rectangle_Vectors.Vector;
    --  @brief What can we see?
    --  @param Self Model to query
    --  @return The list of visible objects

    function Get_Scores (Self : in out Model) return Score_Array;
    --  @brief What are the scores of the two players?
    --  @param Self Model to query
    --  @return The scores of both players

private
    procedure Start_Round (Self : in out Model);

    type Paddle_Array is array (Paddles.Player_ID) of Paddles.Paddle;

    type Model is tagged
        record
            Ball : Balls.Ball;
            Players : Paddle_Array;
            Top_Boundary, Bottom_Boundary,
            Left_Boundary, Right_Boundary : aliased Rectangles.Rectangle;
            Ball_Restricted, Paddle_Restricted,
            Visible : Rectangles.Rectangle_Vectors.Vector;
        end record;
end Pong_Model;

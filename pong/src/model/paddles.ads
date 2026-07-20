with Rectangles;
package Paddles is
    type Paddle is tagged private;
    --  @brief A moveable paddle.

    type Player_ID is (Player_1, Player_2);

    procedure Initialize (Self : in out Paddle;
                          X, Y : Integer; ID : Player_ID);
    --  @brief Initialize a new Paddle
    --  @param Self Paddle to initialize
    --  @param X Paddle's X-Coordinate
    --  @param Y Paddle's Y-Coordinate
    --  @param ID ID of the player controlling this Paddle

    procedure Increase_Score (Self : in out Paddle);
    --  @brief Increase a paddle's score
    --  @param Self Paddle to modify

    function Get_Score (Self : Paddle) return Natural;
    --  @brief Get a paddle's score
    --  @param Self paddle to query
    --  @return The paddle's score

    function Get_Rectangle (Self : in out Paddle)
        return Rectangles.Rectangle_Ptr;
    --  @brief Get the Paddle's rectangle
    --  @param Self Paddle to query
    --  @return A handle to the paddle's rectangle
private

    type Paddle is tagged
        record
            Rectangle : aliased Rectangles.Rectangle;
            ID : Player_ID;
            Score : Natural := 0;
        end record;
    --  @field Rectangle Bounding Box
    --  @field ID ID of the player controlling this Paddle
    --  @field Score Score of the player controlling this Paddle

    WIDTH : constant := 16;
    --  @brief Width of a paddle

    HEIGHT : constant := 64;
    --  @brief Height of a paddle
end Paddles;

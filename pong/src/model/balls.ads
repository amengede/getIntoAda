with Rectangles;
package Balls is

    type Ball is tagged private;
    --  @brief A ball!

    procedure Initialize (Self : in out Ball);
    --  @brief Initialize a new Ball
    --  @param Self Ball to initialize

    procedure Serve (Self : in out Ball);
    --  @brief Place the ball at the center of the court and give it a push.
    --  @param Self the ball to serve

    procedure Update (Self : in out Ball;
                      Restricted : Rectangles.Rectangle_Vectors.Vector);
    --  @brief Update a ball
    --  @param Self the ball to update
    --  @param Restricted List of objects to bounce off of

    function Get_Rectangle (Self : in out Ball)
        return Rectangles.Rectangle_Ptr;
    --  @brief Get the ball's rectangle
    --  @param Ball Ball to query
    --  @return A handle to the ball's rectangle
private

    type Ball is tagged record
        Rectangle : aliased Rectangles.Rectangle;
        X_Velocity : Integer := 0;
        Y_Velocity : Integer := 0;
    end record;
    --  @field Rectangle Bounding box for the ball
    --  @field X_Velocity X component of the ball's velocity
    --  @field Y_Velocity Y component of the ball's velocity

    SIZE : constant := 16;
    --  @brief Size of a ball
end Balls;

with Views;
with Rectangles;
with Pong_Model;
with SDL.Video.Windows;

package Pong_View is
    type View is new Views.View with null record;

    procedure Update (Self : in out View;
                      Visible : Rectangles.Rectangle_Vectors.Vector;
                      Scores : Pong_Model.Score_Array;
                     Window : SDL.Video.Windows.Window);
    --  @brief Draw everything
    --  @param Self View to draw to
    --  @param Visible List of visible rectangles
    --  @param Scores Array of player scores
end Pong_View;

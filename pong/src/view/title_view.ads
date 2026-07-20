with SDL.Video.Windows;
with Views;

package Title_View is
    type View is new Views.View with null record;

    procedure Update (Self : in out View;
                      Window : SDL.Video.Windows.Window);
    --  @brief Refresh the display
    --  @param Self view to refresh
end Title_View;

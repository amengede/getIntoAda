-------------------------------------------------------------------------------
--  SDL Backend
--
--  Utility functions for dealing with SDL.
-------------------------------------------------------------------------------
with SDL;
with SDL.Video.Windows;

package SDL_Backend is

    package Windows renames SDL.Video.Windows;

    type Window_Ptr is access Windows.Window;

    ---------------------------------------------------------------------------
    --  Build a Window
    --  Window: to be populated with the created window
    --  Width: requested window width
    --  Height: requested window height
    --  Title: title to display
    ---------------------------------------------------------------------------
    procedure Build_Window (
        Window : in out Windows.Window;
        Width : SDL.Positive_Dimension;
        Height : SDL.Positive_Dimension;
        Title : String);

end SDL_Backend;
-------------------------------------------------------------------------------

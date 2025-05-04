with SDL.Video.Windows.Makers;

package body SDL_Backend is

    package Makers renames SDL.Video.Windows.Makers;

    procedure Build_Window (
        Window : in out Windows.Window;
        Width : SDL.Positive_Dimension;
        Height : SDL.Positive_Dimension;
        Title : String) is
    begin
        Makers.Create (
            Win => Window,
            Title => Title,
            Size => SDL.Positive_Sizes'(Width, Height),
            Position => SDL.Natural_Coordinates'(0, 0),
            Flags => 0);
    end Build_Window;

end SDL_Backend;

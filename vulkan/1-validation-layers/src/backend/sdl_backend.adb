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

    function Get_SDL_Extensions (
        Window : Windows.Window) return SDL_Vulkan.Extension_Name_Arrays is
    begin
        SDL_Vulkan.Load_Library;
        return SDL_Vulkan.Get_Instance_Extensions (Window);
    end Get_SDL_Extensions;
end SDL_Backend;

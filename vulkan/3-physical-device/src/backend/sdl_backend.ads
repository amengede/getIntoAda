-------------------------------------------------------------------------------
--  SDL Backend
--
--  Utility functions for dealing with SDL.
-------------------------------------------------------------------------------
with SDL;
with SDL.Video.Windows;
with SDL.Video.Vulkan;
with Vulkan;

package SDL_Backend is

    package SDL_Vulkan is new SDL.Video.Vulkan (
        Instance_Address_Type => Vulkan.Instance,
        Instance_Null => Vulkan.No_Instance,
        Surface_Type => Vulkan.Object_Handle);

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

    function Get_SDL_Extensions (
        Window : Windows.Window) return SDL_Vulkan.Extension_Name_Arrays;

end SDL_Backend;
-------------------------------------------------------------------------------

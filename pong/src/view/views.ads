with SDL.Video.Windows;
with SDL.Video.Renderers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.TTFs;
with Rectangles;

package Views is

    package Windows renames SDL.Video.Windows;
    package Renderers renames SDL.Video.Renderers;
    package Fonts renames SDL.TTFs;
    package Textures renames SDL.Video.Textures;
    package Surfaces renames SDL.Video.Surfaces;

    type View is tagged limited
        record
            Renderer : Renderers.Renderer;
            Font : Fonts.Fonts;
        end record;
    --  @brief A general renderer
    --  @field Window window to draw to
    --  @field Renderer SDL renderer struct to do drawing
    --  @field Font Font library to draw text

    procedure Initialize (Self : in out View;
                         Window : Windows.Window);
    --  @brief Initialize a new view
    --  @param Self View to initialize
    --  @param Window Window to draw to

    procedure Clear_Screen (Self : in out View);
    --  @brief Clear the screen to black
    --  @param Self View to clear

    procedure Draw_Rectangle (Self : in out View;
                              Rect : in out Rectangles.Rectangle);
    --  @brief Draw a rectangle
    --  @param Self View to draw to
    --  @param Rect Rectangle to draw

    procedure Draw_Text (Self : in out View;
                         Text : String; X, Y : Integer);
    --  @brief Draw a piece of text
    --  @param Self View to draw to
    --  @param Text Text to draw
    --  @param X X-Coordinate for top-left of text
    --  @param Y Y-Coordinate for top-left of text

    procedure Finish_Drawing (Self : in out View; Window : Windows.Window);
    --  @brief Update the display to present new frame
    --  @param Self View to update

end Views;

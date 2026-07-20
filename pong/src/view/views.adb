with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.TTFs.Makers;
with Constants; use Constants;
with SDL.Video.Rectangles;

package body Views is

    procedure Initialize (Self : in out View;
                         Window : Windows.Window) is
    begin
        Renderers.Makers.Create (Self.Renderer,
                                 Window.Get_Surface);
        Fonts.Makers.Create (Self.Font, File_Name => "font.ttf",
                             Point_Size => 32);
    end Initialize;

    procedure Clear_Screen (Self : in out View) is
    begin
        Self.Renderer.Set_Draw_Colour (BLACK);
        Self.Renderer.Fill (Rectangle =>
                            SDL.Video.Rectangles.Rectangle'(0, 0,
                                                           WIDTH, HEIGHT));
    end Clear_Screen;

    procedure Draw_Rectangle (Self : in out View;
                              Rect : in out Rectangles.Rectangle) is
    begin
        Self.Renderer.Set_Draw_Colour (Rect.Get_Colour);
        Self.Renderer.Fill (Rect.Get_Geometry);
    end Draw_Rectangle;

    procedure Draw_Text (Self : in out View;
                         Text : String; X, Y : Integer) is
        Surface : constant Surfaces.Surface :=
            Fonts.Render_Solid (Self.Font, Text => Text, Colour => WHITE);
        Size : constant SDL.Sizes := Surface.Size;
        Texture : Textures.Texture;
    begin
        Textures.Makers.Create (Texture, Self.Renderer, Surface);
        Renderers.Copy (Self.Renderer, Texture,
                        (Float (X), Float (Y),
                         Float (Size.Width), Float (Size.Height)));
    end Draw_Text;

    procedure Finish_Drawing (Self : in out View;
                              Window : Windows.Window) is
    begin
        Window.Update_Surface;
    end Finish_Drawing;

end Views;

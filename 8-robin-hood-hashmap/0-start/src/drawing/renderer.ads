--  Dependencies
with SDL.Video.Windows;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.TTFs;
with Common; use Common;

package Renderer is

   --  Rename dependencies for quality of life
   package Rectangles renames SDL.Video.Rectangles;
   package Renderers renames SDL.Video.Renderers;
   package Surfaces renames SDL.Video.Surfaces;
   package Textures renames SDL.Video.Textures;
   package TTFs renames SDL.TTFs;

   --  Variables
   Width   : constant := 800;
   Height  : constant := 800;
   Window   : SDL.Video.Windows.Window;
   Renderer : Renderers.Renderer;
   Font : TTFs.Fonts;

   --  Procedures
   function Initialize return Boolean;
   procedure Clear_Screen;
   procedure Render (Head : Person_List.Link);
   procedure Update;
   procedure Finalize;

end Renderer;
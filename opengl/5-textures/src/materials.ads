with GL.Objects.Textures;
with SDL.Video.Surfaces;
with SDL.Images;

package Materials is
    package Textures renames GL.Objects.Textures;
    package Surfaces renames SDL.Video.Surfaces;
    package Images renames SDL.Images;

    procedure Initialize;

    function From_Image (Filename : String) return Textures.Texture;

    procedure Finalize;
end Materials;

with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with Meshes;
with GL.Objects.Textures;

package Renderer is
    package Programs renames GL.Objects.Programs;
    package Shaders renames GL.Objects.Shaders;
    package Vertex_Arrays renames GL.Objects.Vertex_Arrays;
    package Textures renames GL.Objects.Textures;

    procedure Initialize;

    procedure Draw;

    procedure  Finalize;
private
    procedure Clear_Screen;

    function Make_Shader_Module (Filename : String;
                                 Module_Type : Shaders.Shader_Type)
        return Shaders.Shader;

    function Make_Shader_Program (Vertex_Filename,
                                  Fragment_Filename : String)
        return Programs.Program;

    Shader : Programs.Program;
    Triangle_Mesh : Meshes.Mesh;
    Texture : Textures.Texture;
end Renderer;

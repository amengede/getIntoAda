with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;

package Renderer is

    package Programs renames GL.Objects.Programs;
    package Shaders renames GL.Objects.Shaders;
    package Vertex_Arrays renames GL.Objects.Vertex_Arrays;

    procedure Initialize;

    procedure Draw;

private
    procedure Clear_Screen;

    function Make_Shader_Module (Filename : String;
                                 Module_Type : Shaders.Shader_Type)
        return Shaders.Shader;

    function Make_Shader_Program (Vertex_Filename,
                                  Fragment_Filename : String)
        return Programs.Program;

    Shader : Programs.Program;
    Dummy_VAO : Vertex_Arrays.Vertex_Array_Object;
end Renderer;

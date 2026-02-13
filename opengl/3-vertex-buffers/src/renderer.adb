with GL.Buffers;
with GL.Context;
with Ada.Text_IO; use Ada.Text_IO;
with GL.Types;
with Ada.Strings.Unbounded;

package body Renderer is

    procedure Initialize is
    begin
        Put_Line (GL.Context.Version_String);
        GL.Buffers.Set_Color_Clear_Value ([0.5, 0.0, 0.25, 1.0]);
        Shader := Make_Shader_Program ("shaders/vertex.txt",
                                       "shaders/fragment.txt");
        Triangle_Mesh := Meshes.Make_RGB_Triangle;
    end Initialize;

    procedure Draw is
        use GL.Types;
    begin
        Clear_Screen;
        Shader.Use_Program;
        Triangle_Mesh.Vertex_Array_Object.Bind;
        Vertex_Arrays.Draw_Arrays (Mode => Triangles,
                                   First => 0,
                                   Count => Triangle_Mesh.Vertex_Count);
    end Draw;

    procedure Clear_Screen is
        Flags : constant GL.Buffers.Buffer_Bits :=
            GL.Buffers.Buffer_Bits'(Depth => False,
                                    Accum => False,
                                    Stencil => False,
                                    Color => True);
    begin
        GL.Buffers.Clear (Flags);
    end Clear_Screen;

    function Make_Shader_Module (Filename : String;
                                 Module_Type : Shaders.Shader_Type)
        return Shaders.Shader is
        Module : Shaders.Shader (Module_Type);
        Source_File : File_Type;
        Source_Code : Ada.Strings.Unbounded.Unbounded_String;
    begin

        -- Load source code
        Open (Source_File, In_File, Filename);
        while not End_Of_File (Source_File) loop
            Ada.Strings.Unbounded.Append (Source_Code,
                                          Get_Line (Source_File) &
                                          Character'Val (10));
        end loop;
        Close (Source_File);

        -- Build module
        Module.Initialize_Id;
        Module.Set_Source (Ada.Strings.Unbounded.To_String (Source_Code));
        Module.Compile;

        -- Error reporting
        if not Module.Compile_Status then
            Put_Line (Filename & " compilation failed.");
            Put_Line (Shaders.Info_Log (Module));
        end if;

        return Module;
    end Make_Shader_Module;

    function Make_Shader_Program (Vertex_Filename,
                                  Fragment_Filename : String)
        return Programs.Program is
        Program : Programs.Program;
    begin
        Program.Initialize_Id;
        Program.Attach (Make_Shader_Module (Vertex_Filename,
                                            Shaders.Vertex_Shader));
        Program.Attach (Make_Shader_Module (Fragment_Filename,
                                            Shaders.Fragment_Shader));
        Program.Link;
        if not Program.Link_Status then
            Put_Line ("Module Linking failed.");
            Put_Line (Programs.Info_Log (Program));
        end if;

        Shaders.Release_Shader_Compiler;
        return Program;
    end Make_Shader_Program;

end Renderer;

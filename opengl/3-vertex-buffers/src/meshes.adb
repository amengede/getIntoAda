with GL.Attributes;

package body Meshes is
    function Make_RGB_Triangle return Mesh is
        package Attributes renames GL.Attributes;
        use Types;
        Vertices : constant Vertex_Data (0 .. 2) :=
            [Vertex'(X => -0.75, Y => -0.75, Color_ID => 0),
            Vertex'(X => 0.75, Y => -0.75, Color_ID => 1),
            Vertex'(X => 0.0, Y => 0.75, Color_ID => 2)];
        Final_Mesh : Mesh;
    begin
        -- Initialize
        Final_Mesh.Vertex_Array_Object.Initialize_Id;
        Final_Mesh.Buffer.Initialize_Id;
        Final_Mesh.Vertex_Count := 3;

        -- Bind everything
        Final_Mesh.Vertex_Array_Object.Bind;
        Buffers.Bind (Target => Buffers.Array_Buffer,
                      Object => Final_Mesh.Buffer);

        -- Load data
        Upload (Target => Buffers.Array_Buffer,
                Data => Vertices,
                Usage => Buffers.Static_Draw);

        -- Attribute 0: Position
        Attributes.Enable_Vertex_Attrib_Array (0);
        Attributes.Set_Vertex_Attrib_Pointer (Index => 0,
                                              Count => 2,
                                              Kind => Single_Type,
                                              Normalized => False,
                                              Stride => 9,
                                              Offset => 0);

        -- Attribute 1: Color Index
        Attributes.Enable_Vertex_Attrib_Array (1);
        Attributes.Set_Vertex_Integer_Attrib_Pointer (Index => 1,
                                                      Count => 1,
                                                      Kind => Ubyte_Type,
                                                      Stride => 9,
                                                      Offset => 8);

        -- Done!
        return Final_Mesh;
    end Make_RGB_Triangle;
end Meshes;

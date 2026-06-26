with GL.Attributes;

package body Meshes is
    function Make_RGB_Triangle return Mesh is
        package Attributes renames GL.Attributes;
        use Types;
        use Buffers;
        Vertices : constant Vertex_Data (0 .. 2) :=
            [Vertex'(X => -0.75, Y => -0.75, Color_ID => 0),
            Vertex'(X => 0.75, Y => -0.75, Color_ID => 1),
            Vertex'(X => 0.0, Y => 0.75, Color_ID => 2)];
        Indices : constant Index_Data (0 .. 2) :=
            [0, 1, 2];
        Final_Mesh : Mesh;
        Buffer_Size : constant Types.Long := 3 * 9 + 3;
        Usage : constant Storage_Usage_Bits := Storage_Usage_Bits'(
            Map_Read => False,
            Map_Write => False,
            Map_Persistent => False,
            Map_Coherent => False,
            Dynamic_Storage => True,
            Client_Storage => False,
            Unused => False);
    begin
        -- Initialize
        Final_Mesh.Vertex_Array_Object.Initialize_Id;
        Final_Mesh.Buffer.Initialize_Id;
        Final_Mesh.Index_Buffer_Offset := 3 * 9;
        Final_Mesh.Index_Count := 3;

        -- Bind everything
        Final_Mesh.Vertex_Array_Object.Bind;
        Buffers.Bind (Target => Buffers.Array_Buffer,
                      Object => Final_Mesh.Buffer);
        Buffers.Bind (Target => Buffers.Element_Array_Buffer,
                    Object => Final_Mesh.Buffer);

        -- Load data
        Buffers.Allocate (
            Target => Buffers.Array_Buffer,
            Number_Of_Bytes => Buffer_Size,
            Flags => Usage);
        Upload (Target => Buffers.Array_Buffer,
                Offset => 0,
                Data => Vertices);
        Upload (Target => Buffers.Element_Array_Buffer,
            Offset => 3 * 9,
            Data => Indices);

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

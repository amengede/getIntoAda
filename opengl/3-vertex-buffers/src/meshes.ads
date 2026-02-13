with GL.Types;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with Interfaces.C.Pointers;

package Meshes is

    package Types renames GL.Types;
    package Buffers renames GL.Objects.Buffers;
    package Vertex_Arrays renames GL.Objects.Vertex_Arrays;

    type Mesh is record
        Buffer : Buffers.Buffer;
        Vertex_Array_Object : Vertex_Arrays.Vertex_Array_Object;
        Vertex_Count : Types.Size := 0;
    end record;

    function Make_RGB_Triangle return Mesh;

private

    type Vertex is record
        X, Y : Types.Single;
        Color_ID : Types.UByte;
    end record with Object_Size => 72;

    type Vertex_Data is array(Natural range <>) of aliased Vertex;

    package Vertex_Pointers is
        new Interfaces.C.Pointers(Index => Natural,
                                  Element => Vertex,
                                  Element_Array => Vertex_Data,
                                  Default_Terminator => Vertex'(0.0, 0.0, 0));
        procedure Upload is new Buffers.Load_To_Buffer (Vertex_Pointers);
    end Meshes;
